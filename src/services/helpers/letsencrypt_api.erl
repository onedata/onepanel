%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Functions for communicating with the Let's Encrypt service
%%% via its ACME protocol API.
%%% @end
%%%-------------------------------------------------------------------
-module(letsencrypt_api).
-author("Wojciech Geisler").

-include("modules/models.hrl").
-include("modules/errors.hrl").
-include("names.hrl").

-include_lib("ctool/include/logging.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

% Staging server
-define(STAGING_DIRECTORY_URL, application:get_env(onepanel,
    letsencrypt_directory_url_staging, undefined)).
% Production server
-define(PRODUCTION_DIRECTORY_URL, application:get_env(onepanel, letsencrypt_directory_url,
    "https://acme-v01.api.letsencrypt.org/directory")).

-define(CERT_PATH, onepanel_env:get(web_cert_file)).
-define(LETSENCRYPT_KEYS_DIR, application:get_env(?APP_NAME, letsencrypt_keys_dir,
    % default
    filename:join(filename:dirname(?CERT_PATH), "letsencrypt/"))).

% Name of the txt record used for authorization.
% See <https://tools.ietf.org/id/draft-ietf-acme-acme-07.html#rfc.section.8.5>
-define(LETSENCRYPT_TXT_NAME, <<"_acme-challenge">>).
% Short ttl to mitigate problems when quickly obtaining new certificate
-define(LETSENCRYPT_TXT_TTL, 1).

% Filenames for temporary Let's Encrypt account keys
-define(PRIVATE_RSA_KEY, "letsencrypt_private_key.pem").
-define(PUBLIC_RSA_KEY, "letsencrypt_public_key.pem").

% See run_certification_flow doc for possible values
-define(DEFAULT_MODE, application:get_env(onepanel, letsencrypt_mode, full)).

% Interval between polls about authorization status
-define(LE_POLL_INTERVAL, timer:seconds(3)).
% Number of polls about authorization status
-define(LE_POLL_ATTEMPTS, 10).
% Number of failed request retries
-define(GET_RETRIES, 3).
-define(POST_RETRIES, 4).

-define(HTTP_OPTS,
    [{connect_timeout, timer:seconds(15)}, {recv_timeout, timer:seconds(15)}]).

% Record for the endpoints directory presented by letsencrypt
-record(directory, {
    key_change :: binary(),
    new_authz :: binary(),
    new_cert :: binary(),
    new_reg :: binary()
}).

% record for storing progress in the certification process
-record(flow_state, {
    service :: module(),

    % ACME server directory URL
    directory_url :: string(),

    % ACME server directory
    directory = undefined :: undefined | #directory{},

    % Every Let's Encrypt response contains a nonce for one-time use.
    % This is a pool for unused nonces.
    nonces = [] :: [nonce()],

    % URL for checking domain authorization status
    authz_uri = undefined :: undefined | binary(),

    % Common Name for the certificate
    domain :: binary(),

    % Whether to save obtained certificate on disk
    save_cert :: boolean(),

    % Paths for saving resulting key and cert
    cert_path :: string(),
    cert_private_key_path :: string(),
    chain_path :: string(),

    % Directory for keys used in communication with Let's Encrypt
    jws_keys_dir :: string()
}).

-type token() :: binary().
-type nonce() :: binary().
-type url() :: http_client:url().
-type pem() :: binary().

-export([run_certification_flow/2, run_certification_flow/3]).
-export([clean_keys/0, clean_keys/1]).

%%%===================================================================
%%% Public API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Performs all stages of obtaining new certificate from Let's Encrypt.
%% Certificate paths are obtained from app config.
%% Plugin is a service module for interacting with the oneprovider or onezone.
%% @end
%%--------------------------------------------------------------------
-spec run_certification_flow(Domain :: binary(), Plugin :: module()) -> ok | no_return().
run_certification_flow(Domain, Plugin) ->
    run_certification_flow(Domain, Plugin, ?DEFAULT_MODE).

%%--------------------------------------------------------------------
%% @doc
%% Performs all stages of obtaining new certificate from Let's Encrypt.
%% Mode parameter can be:
%% - dry        - run full certification procedure against staging server but do NOT
%%                save obtained certificate
%% - staging    - obtain certificate from the staging server
%% - production - obtain certificate from the main Let's Encrypt server
%% - full       - check certification success as with dry and then obtain certificate
%%                from the production server
%% @end
%%--------------------------------------------------------------------
-spec run_certification_flow(Domain :: binary(), Plugin :: module(),
    Mode :: dry | staging | production | full) -> ok | no_return().
run_certification_flow(Domain, Plugin, Mode) ->

    Mode2 = case {Mode, ?STAGING_DIRECTORY_URL} of
        {production, _} -> production;
        {full, undefined} ->
            ?info("No staging ACME server URL defined. Skipping staging run and running in production mode."),
            production;
        {full, _} ->
            ?info("Let's Encrypt: Starting test run against staging server before actual Let's"
            "Encrypt certification"),
            full;
        {_, undefined} -> throw("No staging ACME server URL defined. Cannot perform test run"),
            throw({error, no_letsencrypt_staging_server});
        {_, _} -> Mode
    end,

    % For logging purposes
    ModeName = case Mode2 of
        full -> staging;
        _ -> Mode2
    end,

    DirectoryURL = case Mode2 of
        production -> ?PRODUCTION_DIRECTORY_URL;
        _ -> ?STAGING_DIRECTORY_URL
    end,
    KeysDir = filename:join(?LETSENCRYPT_KEYS_DIR, atom_to_list(ModeName)),
    SaveCert = Mode2 == production orelse Mode2 == staging,

    CertPath = onepanel_env:get(web_cert_file),
    KeyPath = onepanel_env:get(web_key_file),
    ChainPath = onepanel_env:get(web_cert_chain_file),

    case SaveCert of
        true -> ensure_files_access([CertPath, KeyPath, ChainPath]);
        false -> ok
    end,

    % passing state around is useful for managing anti-replay nonces
    State = #flow_state{
        service = Plugin,
        directory_url = DirectoryURL,
        jws_keys_dir = KeysDir,
        cert_path = CertPath,
        cert_private_key_path = KeyPath,
        chain_path = ChainPath,
        save_cert = Mode2 == production orelse Mode2 == staging,
        domain = Domain},

    try
        case read_keys(KeysDir) of
            {ok, _, _} ->
                ?info("Let's Encrypt: reusing account from \"~s\"", [filename:absname(KeysDir)]);
            error ->
                ?info("Let's Encrypt: generating new Let's Encrypt account keys"),
                ok = generate_keys(KeysDir)
        end,

        ?info("Let's Encrypt ~s run: get endpoints", [ModeName]),
        {ok, State2} = get_directory(State),
        ?info("Let's Encrypt ~s run: register account", [ModeName]),
        {ok, State3} = register_account(State2),
        ?info("Let's Encrypt ~s run: authorize for the domain ~s", [ModeName, Domain]),
        {ok, State4} = authorize(State3),
        ?info("Let's Encrypt ~s run: poll authorization", [ModeName]),
        {ok, State5} = poll_authorization(State4),
        ?info("Let's Encrypt ~s run: obtain certificate", [ModeName]),
        {ok, _State6} = get_certificate(State5)
    after
        catch clean_txt_record(Plugin)
    end,

    case Mode2 of
        full -> run_certification_flow(Domain, Plugin, production);
        _ -> ok
    end.


%%%===================================================================
%%% Let's Encrypt API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads the endpoints directory and stores it in flow state.
%% @end
%%--------------------------------------------------------------------
-spec get_directory(#flow_state{}) -> {ok, #flow_state{}}.
get_directory(#flow_state{directory_url = URL} = State) ->
    {ok, #{} = DirectoryMap, _, State2} = http_get(URL, State),
    {ok, State2#flow_state{directory = decode_directory(DirectoryMap)}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates new account used for communication with Let's Encrypt.
%% @end
%%--------------------------------------------------------------------
-spec register_account(#flow_state{}) -> {ok, #flow_state{}}.
register_account(#flow_state{service = Service} = State) ->
    #flow_state{directory = #directory{new_reg = NewRegURL}} = State,

    Contact = case Service:get_admin_email(#{}) of
        undefined -> [];
        Email -> [<<"mailto:", Email/binary>>]
    end,
    Payload = #{resource => <<"new-reg">>, contact => Contact},


    % Let's Encrypt incorrectly returns 409 on registering existing account
    {ok, _, Headers, State2} = http_post(NewRegURL, Payload, [201, 409], State),

    case Headers of
        #{<<"Link">> := TermsHeader, <<"Location">> := AgreementURL} ->
            % extract usage terms URL needed to send ToS agreement request
            % example input:
            % <https://letsencrypt.org/documents/LE-SA-v1.2-November-15-2017.pdf>; rel="terms-of-service"
            {match, [TermsURL]} =
                re:run(TermsHeader, <<"^<(.*)>">>, [{capture, [1], binary}]),

            agree_to_terms(AgreementURL, TermsURL, State2);
        _ ->
            % existing account, no agreement needed
            {ok, State2}
    end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Accepts the Subscriber Agreement.
%% @end
%%--------------------------------------------------------------------
-spec agree_to_terms(url(), url(), #flow_state{}) -> {ok, #flow_state{}}.
agree_to_terms(URL, TermsURL, State) ->
    Payload = #{<<"resource">> => <<"reg">>,
        <<"agreement">> => TermsURL},
    {ok, _, _, State2} = http_post(URL, Payload, 202, State),
    {ok, State2}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates authorization record used to confirm control of a domain.
%% Proceeds to perform requested authorization challenge.
%% @end
%%--------------------------------------------------------------------
authorize(#flow_state{domain = Domain} = State) ->
    #flow_state{directory = #directory{new_authz = NewAuthzURL}} = State,
    Payload = #{<<"resource">> => <<"new-authz">>,
        <<"identifier">> => #{
            <<"type">> => <<"dns">>, % the only available resource type
            <<"value">> => Domain}
    },

    {ok, Response, _, State2} = http_post(NewAuthzURL, Payload, 201, State),
    #{<<"challenges">> := Challenges} = Response,

    % should be only one matching
    [DNSChallenge] = lists:filter(fun
        (#{<<"type">> := <<"dns-01">>}) -> true;
        (_) -> false
    end, Challenges),

    #{<<"token">> := ChallengeToken,
        <<"uri">> := ChallengeURI} = DNSChallenge,

    handle_challenge(ChallengeURI, ChallengeToken, State2).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles fullfilling authorization challange.
%% @end
%%--------------------------------------------------------------------
handle_challenge(URI, Token, #flow_state{service = Service} = State) ->
    AuthString = make_auth_string(Token, State),
    TxtValue = base64url:encode(crypto:hash(sha256, AuthString)),

    % requires oz connection
    ok = Service:set_txt_record(#{txt_name => ?LETSENCRYPT_TXT_NAME,
        txt_value => TxtValue, txt_ttl => ?LETSENCRYPT_TXT_TTL}),

    Payload = #{<<"resource">> => <<"challenge">>,
        <<"type">> => <<"dns-01">>,
        <<"keyAuthorization">> => AuthString},
    {ok, _, _, State2} = http_post(URI, Payload, 202, State),
    {ok, State2#flow_state{authz_uri = URI}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if Let's Encrypt has accepted the challenge response.
%% @end
%%--------------------------------------------------------------------
-spec poll_authorization(State :: #flow_state{}, Attempts :: non_neg_integer()) ->
    {ok, #flow_state{}} | no_return().
poll_authorization(#flow_state{authz_uri = URI} = _State, 0) when is_binary(
    URI) ->
    ?error("Let's Encrypt authorization timed out"),
    ?throw_error(?ERR_LETSENCRYPT(
        "authorization", "Let's Encrypt authorization timed out"));
poll_authorization(#flow_state{authz_uri = URI} = State, Attempts) when is_binary(URI) ->
    {ok, #{<<"status">> := ChallengeStatus} = Body, _, State2} = http_get(URI, State),
    case ChallengeStatus of
        <<"pending">> ->
            timer:sleep(?LE_POLL_INTERVAL),
            poll_authorization(State2, Attempts - 1);
        <<"valid">> -> {ok, State2};
        <<"invalid">> ->
            ?error("Let's Encrypt did not accept challenge response: ~p", [Body]),
            ?throw_error(?ERR_LETSENCRYPT_AUTHORIZATION(
                "Let's encrypt could not authorize domain."))
    end.
poll_authorization(#flow_state{authz_uri = URI} = State) when is_binary(URI) ->
    poll_authorization(State, ?LE_POLL_ATTEMPTS).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Request and download the certificate.
%% @end
%%--------------------------------------------------------------------
-spec get_certificate(#flow_state{}) -> {ok, #flow_state{}}.
get_certificate(#flow_state{domain = Domain} = State) ->
    #flow_state{directory = #directory{new_cert = NewCertURL}} = State,

    {ok, CSRPem, KeyPem} = onepanel_ssl:generate_csr_and_key(binary_to_list(Domain)),
    [{'CertificationRequest', CSRDer, not_encrypted}] = public_key:pem_decode(CSRPem),
    CSRB64 = base64url:encode(CSRDer),

    Payload = #{<<"resource">> => <<"new-cert">>, <<"csr">> => CSRB64},

    {ok, {raw, CertDer}, Headers, State2} = http_post(NewCertURL, Payload, 201, State),
    CertPem = public_key:pem_encode([{'Certificate', CertDer, not_encrypted}]),

    case State2#flow_state.save_cert of
        true ->
            {ok, State3, ChainPem} = get_chain_certificate(State2, maps:get(<<"Link">>, Headers)),
            save_cert(State3, CertPem, KeyPem, ChainPem),

            ?info("Let's Encrypt: saved new certificate at ~s",
                [filename:absname(State3#flow_state.cert_path)]),
            {ok, State3};
        _ -> {ok, State2}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Parses given content of "Link" header as returned by Let's Encrypt
%% and downloads the CA certificates it is pointing to.
%% @end
%%--------------------------------------------------------------------
-spec get_chain_certificate(State :: #flow_state{}, Link :: binary()) ->
    {ok, #flow_state{}, pem()}.
get_chain_certificate(State, Link) ->
    % example Link value:
    % <https://acme-v01.api.letsencrypt.org/acme/issuer-cert>;rel=\"up\"
    {match, [ChainURL]} =
        re:run(Link, <<"^<(.*)>;rel=\"up\"">>, [{capture, [1], binary}]),

    {ok, {raw, ChainDer}, _, State2} = http_get(ChainURL, State),
    ChainPem = public_key:pem_encode([{'Certificate', ChainDer, not_encrypted}]),

    {ok, State2, ChainPem}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Performs a GET request and returns response body and headers.
%% If the body is a JSON it is decoded, otherwise the raw response is
%% returned.
%% Adds received nonce to the pool.
%% @end
%%--------------------------------------------------------------------
-spec http_get(url(), #flow_state{}) ->
    {ok, Response :: map() | {raw, binary()}, http_client:headers(), #flow_state{}} |
    no_return().
http_get(URL, State) ->
    http_get(URL, State, ?GET_RETRIES).
-spec http_get(url(), #flow_state{}, Attempts :: non_neg_integer()) ->
    {ok, Response :: map() | {raw, binary()}, http_client:headers(), #flow_state{}} |
    no_return().
http_get(_URL, _State, 0) ->
    ?ERR_LETSENCRYPT(<<"connection">>, "Could not connect to Let's Encrypt.");
http_get(URL, State, Attempts) ->
    case http_client:get(URL, #{}, <<>>, ?HTTP_OPTS) of
        {ok, _Status, #{<<"Replay-Nonce">> := Nonce} = Headers, BodyRaw} ->
            Body = try
                json_utils:decode(BodyRaw)
            catch
                throw:invalid_json -> {raw, BodyRaw}
            end,
            {ok, Body, Headers, push_nonce(Nonce, State)};
        {error, Reason} ->
            ?error("Error performing GET request to ~s: ~p", [URL, {error, Reason}]),
            http_get(URL, State, Attempts - 1)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Performs post request with given Payload packaged as JWS.
%% Verifies response status and decodes body as JSON.
%% If returned body cannot be parsed as JSON it is returned unchanged.
%% @end
%%--------------------------------------------------------------------
-spec http_post(
    URL :: url(), Payload :: term(),
    OkCodes :: Status | [Status], State :: #flow_state{}
) ->
    {ok, Response, Headers, #flow_state{}} | {error, Reason :: term(), #flow_state{}}
    when
    Status :: [http_client:code()],
    Response :: map() | {raw, binary()},
    Headers :: http_client:headers().
http_post(URL, Payload, OkCodes, #flow_state{} = State) ->
    http_post(URL, Payload, OkCodes, State, ?POST_RETRIES).


-spec http_post(URL :: url(), Payload :: term(),
    OkCodes :: Status | [Status], State :: #flow_state{},
    Retrues :: non_neg_integer()) ->
    {ok, Response, Headers, #flow_state{}} | {error, Reason :: term(), #flow_state{}}
    when
    Status :: [http_client:code()],
    Response :: map() | {raw, binary()},
    Headers :: http_client:headers().
http_post(URL, Payload, OkCode, State, Attempts)
    when is_integer(OkCode) ->
    http_post(URL, Payload, [OkCode], State, Attempts);

http_post(_URL, _Payload, _OkCodes, _State, 0) ->
    ?ERR_LETSENCRYPT(<<"connection">>, "Could not connect to Let's Encrypt.");

http_post(URL, Payload, OkCodes, #flow_state{} = State, Attempts) ->
    {ok, Body, State2} = encode(Payload, State),

    case http_client:post(URL, #{}, Body, ?HTTP_OPTS) of
        {ok, Status, #{<<"Replay-Nonce">> := Nonce} = Headers, ResponseRaw} ->
            Response = try
                json_utils:decode(ResponseRaw)
            catch
                throw:invalid_json -> {raw, ResponseRaw}
            end,

            case lists:member(Status, OkCodes) of
                true ->
                    {ok, Response, Headers, push_nonce(Nonce, State2)};
                false ->
                    OkCodesStr = lists:join(" or ", lists:map(fun erlang:integer_to_list/1, OkCodes)),
                    % Identify errors deserving customized handling
                    case {Status, Response} of
                        {400, #{<<"type">> := <<"urn:acme:error:badNonce">>}} ->
                            % badNonce - retry with newly received nonce
                            http_post(URL, Payload, OkCodes, push_nonce(Nonce, State2),
                                Attempts - 1);
                        {400, #{
                            <<"type">> := <<"urn:acme:error:connection">> = ErrorType,
                            <<"detail">> := ErrorMessage}} ->
                            % Handled as a special case to provide explanation for the user
                            ?error("Let's Encrypt response status: ~B, expected ~s~n"
                            "Response headers: ~p~nResponse body:~p",
                                [Status, OkCodesStr, Headers, Response]),
                            ?throw_error(?ERR_LETSENCRYPT_AUTHORIZATION(ErrorMessage));
                        {429, #{<<"type">> := ErrorType, <<"detail">> := ErrorMessage}} ->
                            % Rate limits reached error
                            % Handled as a special case to provide explanation for the user
                            ?error("Let's Encrypt limit reached. Response headers: ~p~nResponse body:~p",
                                [Headers, Response]),
                            ?throw_error(?ERR_LETSENCRYPT_LIMIT(ErrorType, ErrorMessage));
                        {_, #{<<"type">> := ErrorType, <<"detail">> := ErrorMessage}} ->
                            ?error("Let's Encrypt response status: ~B, expected ~s~n"
                            "Response headers: ~p~nResponse body:~p",
                                [Status, OkCodesStr, Headers, Response]),
                            ?throw_error(?ERR_LETSENCRYPT(ErrorType, ErrorMessage));
                        {_, Response} ->
                            ?error("Let's Encrypt response status: ~B, expected ~s~n"
                            "Response headers: ~p~nResponse body:~p",
                                [Status, OkCodesStr, Headers, Response]),
                            ?throw_error(
                                ?ERR_LETSENCRYPT(<<"">>, "Unexpected Let's Encrypt response"))
                    end
            end;

        {error, Reason} ->
            ?error("Error performing POST request to ~s: ~p", [URL, {error, Reason}]),
            http_post(URL, Payload, OkCodes, State2, Attempts - 1)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Loads keys used for communication with Let's Encrypt
%% @end
%%--------------------------------------------------------------------
-spec read_keys(KeyPath :: string()) ->
    {ok, #'RSAPrivateKey'{}, #'RSAPublicKey'{}} | error.
read_keys(KeysDir) ->
    PrivateKeyFile = filename:join([KeysDir, ?PRIVATE_RSA_KEY]),
    PublicKeyFile = filename:join([KeysDir, ?PUBLIC_RSA_KEY]),

    case {
        onepanel_jws:load_key(PrivateKeyFile),
        onepanel_jws:load_key(PublicKeyFile)
    } of
        {{ok, #'RSAPrivateKey'{} = Key}, {ok, #'RSAPublicKey'{} = Pubkey}} ->
            {ok, Key, Pubkey};
        _ -> error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Generates keys for communication with Let's Encrypt.
%% @end
%%--------------------------------------------------------------------
-spec generate_keys(KeysDir :: string()) -> ok.
generate_keys(KeysDir) ->
    % trailing slash necessary to create the directory itself
    % and not only parent directories
    ok = filelib:ensure_dir(KeysDir ++ "/"),

    PrivateKeyFile = filename:join([KeysDir, ?PRIVATE_RSA_KEY]),
    PublicKeyFile = filename:join([KeysDir, ?PUBLIC_RSA_KEY]),
    onepanel_jws:generate_keys(PrivateKeyFile, PublicKeyFile).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes keys temporary directory.
%% @end
%%--------------------------------------------------------------------
-spec clean_keys() -> ok.
clean_keys() ->
    lists:foreach(fun(Mode) ->
        Dir = filename:join(?LETSENCRYPT_KEYS_DIR, Mode),
        clean_keys(Dir)
    end, [dry, staging, production]).

-spec clean_keys(KeysDir :: string()) -> ok.
clean_keys(KeysDir) ->
    lists:foreach(fun(File) ->
        Path = filename:join(KeysDir, File),
        case filelib:is_regular(Path) of
            true -> file:delete(Path);
            _ -> ok
        end
    end, [?PRIVATE_RSA_KEY, ?PUBLIC_RSA_KEY]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes txt record set for Let's Encrypt authorization
%% @end
%%--------------------------------------------------------------------
-spec clean_txt_record(Service :: module()) -> ok.
clean_txt_record(Service) ->
    ok = Service:remove_txt_record(
        #{txt_name => ?LETSENCRYPT_TXT_NAME}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns nonce from the pool or performs get request to obtain a new one.
%% @end
%%--------------------------------------------------------------------
-spec pop_nonce(#flow_state{}) -> {nonce(), #flow_state{}}.
pop_nonce(#flow_state{nonces = [Nonce | Rest]} = State) ->
    {Nonce, State#flow_state{nonces = Rest}};
pop_nonce(#flow_state{nonces = []} = State) ->
    {ok, _, _, #flow_state{} = State2} =
        http_get(State#flow_state.directory_url, State),
    pop_nonce(State2).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds nonce to the pool.
%% @end
%%--------------------------------------------------------------------
-spec push_nonce(nonce(), #flow_state{}) -> #flow_state{}.
push_nonce(Nonce, #flow_state{nonces = Nonces} = State) ->
    State#flow_state{nonces = [Nonce | Nonces]}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns Payload encoded as JWS JSON Serialization,
%% adding headers required by Let's Encrypt API.
%% @end
%%--------------------------------------------------------------------
-spec encode(map(), #flow_state{}) -> {ok, binary(), #flow_state{}}.
encode(Payload, #flow_state{jws_keys_dir = KeysDir} = State) ->
    {ok, Key, Pubkey} = read_keys(KeysDir),
    {Nonce, State2} = pop_nonce(State),
    JWK = onepanel_jws:key_to_jwk_map(Pubkey),
    ProtectedHeader = #{<<"jwk">> => JWK, <<"nonce">> => Nonce},

    {ok, BodyMap} = onepanel_jws:sign(Payload, ProtectedHeader, Key),
    {ok, json_utils:encode(BodyMap), State2}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates authorization string needed to fullfill dns challenge.
%% @end
%%--------------------------------------------------------------------
-spec make_auth_string(token(), #flow_state{}) -> binary().
make_auth_string(Token, #flow_state{jws_keys_dir = KeysDir}) ->
    {ok, Key, _} = read_keys(KeysDir),
    Thumbprint = onepanel_jws:thumbprint(Key),
    <<Token/binary, ".", Thumbprint/binary>>.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates directory map to record.
%% @end
%%--------------------------------------------------------------------
-spec decode_directory(map()) -> #directory{}.
decode_directory(Map) ->
    #directory{
        key_change = maps:get(<<"key-change">>, Map),
        new_authz = maps:get(<<"new-authz">>, Map),
        new_cert = maps:get(<<"new-cert">>, Map),
        new_reg = maps:get(<<"new-reg">>, Map)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Saves given certificates PEMs on all onepanel nodes.
%% @end
%%--------------------------------------------------------------------
-spec save_cert(State :: #flow_state{},
    CertPem :: onepanel_ssl:pem(), KeyPem :: onepanel_ssl:pem(), ChainPem :: onepanel_ssl:pem()) ->
    ok.
save_cert(#flow_state{
    cert_private_key_path = KeyPath,
    cert_path = CertPath,
    chain_path = ChainPath}, CertPem, KeyPem, ChainPem) ->
    Nodes = service_onepanel:get_nodes(),

    ok = utils:save_file_on_hosts(Nodes, KeyPath, KeyPem),
    ok = utils:save_file_on_hosts(Nodes, CertPath, CertPem),
    ok = utils:save_file_on_hosts(Nodes, ChainPath, ChainPem).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures all given paths are writable. Creates files if they
%% do not exist.
%% @end
%%--------------------------------------------------------------------
-spec ensure_files_access([string()]) -> ok | no_return().
ensure_files_access(Paths) ->
    lists:foreach(fun(Path) ->
        case check_write_access(Path) of
            ok -> ok;
            {error, Reason} -> ?throw_error(?ERR_FILE_ACCESS(Path, Reason))
        end
    end, Paths).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% If Path points to exisiting file checks if it can be opened for writing.
%% Otherwise attempts to create file at given path.
%% @end
%%--------------------------------------------------------------------
-spec check_write_access(Path :: string()) -> ok | {error, Reason :: term()}.
check_write_access(Path) ->
    case filelib:ensure_dir(Path) of
        ok ->
            case file:open(Path, [write, read]) of
                {ok, File} -> ok = file:close(File);
                Error -> Error
            end;
        Error -> Error
    end.
