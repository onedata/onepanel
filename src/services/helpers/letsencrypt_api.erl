%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Functions for communicating with the Let's Encrypt service
%%% via its ACME protocol API.
%%% See <https://tools.ietf.org/html/rfc8555> for the protocol description.
%%% @end
%%%-------------------------------------------------------------------
-module(letsencrypt_api).
-author("Wojciech Geisler").

-include("modules/models.hrl").
-include("modules/errors.hrl").
-include("modules/onepanel_dns.hrl").
-include("names.hrl").

-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").
-include_lib("kernel/include/inet.hrl").

-define(STAGING_DIRECTORY_URL, application:get_env(?APP_NAME,
    letsencrypt_directory_url_staging, undefined)).
-define(PRODUCTION_DIRECTORY_URL, application:get_env(?APP_NAME,
    letsencrypt_directory_url, "https://acme-v02.api.letsencrypt.org/directory")).

-define(CERT_PATH, onepanel_env:get(web_cert_file)).
-define(LETSENCRYPT_KEYS_DIR, application:get_env(?APP_NAME, letsencrypt_keys_dir,
    % default
    filename:join(filename:dirname(?CERT_PATH), "letsencrypt/"))).

% Name of the txt record used for authorization.
% Source: <https://tools.ietf.org/html/rfc8555#section-8.4>
-define(LETSENCRYPT_TXT_NAME, <<"_acme-challenge">>).
% Short ttl to mitigate problems when quickly obtaining new certificate
-define(LETSENCRYPT_TXT_TTL, 1).

% Filenames for temporary Let's Encrypt account keys
-define(PRIVATE_RSA_KEY, "letsencrypt_private_key.pem").
-define(PUBLIC_RSA_KEY, "letsencrypt_public_key.pem").

% See {@link run_certification_flow/3} documentation for possible values
-define(DEFAULT_MODE, application:get_env(?APP_NAME, letsencrypt_mode, full)).

% Interval between polls about authorization status
-define(LE_POLL_INTERVAL, timer:seconds(3)).
% Number of polls about authorization status
-define(LE_POLL_ATTEMPTS, 10).

% Number of polls to DNS.
% Selected so that it is longer than default soa_minimum in Onezone DNS
-define(WAIT_FOR_TXT_ATTEMPTS, 65).
% Delay between polls to DNS.
-define(WAIT_FOR_TXT_DELAY, timer:seconds(2)).
% DNS servers used to verify TXT record at onezone
-define(DNS_SERVERS, application:get_env(?APP_NAME,
    letsencrypt_dns_verification_servers, [{8, 8, 8, 8}])).

% Number of failed request retries
-define(GET_RETRIES, 3).
-define(POST_RETRIES, 4).

-define(HTTP_OPTS, [{
    connect_timeout, timer:seconds(30)},
    {recv_timeout, timer:seconds(30)},
    {ssl_options, [{cacerts, cert_utils:load_ders_in_dir(onepanel_env:get(cacerts_dir))}]}
]).


% Record for the endpoints directory presented by Let's Encrypt
-record(directory, {
    key_change :: binary(),
    new_account :: binary(),
    new_nonce :: binary(),
    new_order :: binary(),
    revoke_cert :: binary()
}).

% Record describing a domain authorization challenge
-record(challenge, {
    type :: challenge_type(),
    url :: binary(),
    token :: binary()
}).


-record(authorization, {
    identifier :: #{type := binary(), value := binary()},
    challenges :: [#challenge{}]
}).

% Record for storing progress in the certification process
-record(flow_state, {
    %% Settings
    % Callback module
    service :: plugin_service(),
    % Common Name for the certificate
    domain :: binary(),
    % Whether to save obtained certificate on disk
    save_cert :: boolean(),
    % Paths for saving resulting key and cert
    cert_path :: string(),
    cert_private_key_path :: string(),
    chain_path :: string(),
    % Directory for keys used in communication with Let's Encrypt
    jws_keys_dir :: string(),
    % Mode of the current run (used in logs)
    current_mode :: run_mode(),
    % ACME server directory URL
    directory_url :: string(),

    %% Fields written once per certification flow
    % ACME server directory contents
    directory = undefined :: #directory{} | undefined,
    % URL containing account id. Obtained during registration,
    % used for identifying public key used for signing messages.
    account_url = undefined :: url() | undefined,

    %% Current context
    % URL of the currently processed order
    order_url = undefined :: url() | undefined,
    % URL at which CSR should be sent after validating authorizations
    finalize_url = undefined :: url() | undefined,
    % list of authorizations necessary to validate the current order
    authorizations = undefined :: [#authorization{}] | undefined,
    % currently processed challenge
    challenge = undefined :: #challenge{} | undefined,

    %% Nonce pool
    % Each request uses a nonce from here and adds new one sent by server.
    nonces = [] :: [nonce()]
}).


% @formatter:off
-type one_or_many(Type) :: Type | [Type].

-type url() :: string() | binary().
-type pem() :: binary().

-type run_mode() :: dry | staging | production | full.
-type challenge_type() :: dns | http | unknown.
-type plugin_service() :: service_op_worker | service_oz_worker.
-type token() :: binary().
-type nonce() :: binary().

-type api_response() :: #{binary() => json_utils:json_term()} | {raw, binary()}.
-type request_result() :: {ok, api_response(), http_client:headers(), #flow_state{}}.
% @formatter:on

-export_type([challenge_type/0]).

-export([run_certification_flow/2]).
-export([challenge_types/0]).

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
    run_certification_flow(Domain, Plugin, resolve_run_mode()).


%%--------------------------------------------------------------------
%% @doc
%% Returns implemented authorization challenge types, in order
%% of prefence - later challenges are attempted if previous failed.
%% @end
%%--------------------------------------------------------------------
-spec challenge_types() -> [challenge_type()].
challenge_types() ->
    [http, dns].


%%%===================================================================
%%% Certification flow functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
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
    Mode :: run_mode()) -> ok | no_return().
run_certification_flow(Domain, Plugin, Mode) ->
    % When called with 'full' mode, first complete the staging flow
    % and then recurse with Mode 'production'.
    CurrentMode = case Mode of
        full -> staging;
        _ -> Mode
    end,

    {ok, State} = initial_state(Domain, Plugin, Mode, CurrentMode),
    KeysDir = State#flow_state.jws_keys_dir,

    case Mode of
        dry -> ok;
        _ -> ensure_files_access(State)
    end,

    AccountExists = case read_keys(KeysDir) of
        {ok, _, _} ->
            ?info("Let's Encrypt ~s run: reusing account from \"~s\"", [CurrentMode, filename:absname(KeysDir)]),
            true;
        _ ->
            ?info("Let's Encrypt ~s run: generating new Let's Encrypt account keys", [CurrentMode]),
            false
    end,

    try
        AccountExists orelse generate_keys(KeysDir),
        ?info("Let's Encrypt ~s run: get endpoints", [CurrentMode]),
        {ok, State2} = get_directory(State),
        ?info("Let's Encrypt ~s run: register account", [CurrentMode]),
        {ok, State3} = register_account(State2),
        {ok, _State4} = attempt_certification(State3)
    catch
        Type:Error:Stacktrace ->
            case AccountExists of
                true -> ok;
                false ->
                    % if error occurred before current account has completed
                    % any successful certification, it can be safely
                    % deleted. This avoids counting against per-account invalid
                    % authorization rate limit.
                    catch clean_keys(KeysDir)
            end,
            erlang:raise(Type, Error, Stacktrace)
    end,
    % Remove TXT record only on success to ease debugging
    catch clean_txt_record(Plugin),

    case Mode of
        full -> run_certification_flow(Domain, Plugin, production);
        _ -> ok
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads the endpoints directory and stores it in flow state.
%% @end
%%--------------------------------------------------------------------
-spec get_directory(#flow_state{}) -> {ok, #flow_state{}}.
get_directory(#flow_state{directory_url = URL} = State) ->
    {ok, ?HTTP_200_OK, _, DirectoryMap} = http_get(URL, ?GET_RETRIES),
    {ok, State#flow_state{directory = decode_directory(DirectoryMap)}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates new account used for communication with Let's Encrypt.
%% @end
%%--------------------------------------------------------------------
-spec register_account(#flow_state{}) -> {ok, #flow_state{}}.
register_account(#flow_state{service = Service} = State) ->
    #flow_state{directory = #directory{new_account = NewAccountUrl}} = State,

    Payload = #{termsOfServiceAgreed => true},
    Payload2 = case Service:get_admin_email() of
        undefined -> Payload;
        Email -> Payload#{contact => [<<"mailto:", Email/binary>>]}
    end,

    % 200 is expected when existing account is reused
    {ok, _, Headers, State2} = post(NewAccountUrl, Payload2, [?HTTP_200_OK, ?HTTP_201_CREATED], State),
    {ok, State2#flow_state{account_url = maps:get(<<"Location">>, Headers)}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to obtain a certificate using each of available authorization
%% types until success.
%% @end
%%--------------------------------------------------------------------
-spec attempt_certification(#flow_state{}) -> {ok, #flow_state{}}.
attempt_certification(#flow_state{service = Service} = State) ->
    Result = lists:foldl(fun
        (_ChallengeType, {ok, _StateAcc} = Prev) ->
            Prev;
        (ChallengeType, {_PrevError, StateAcc} = Prev) ->
            try
                case Service:supports_letsencrypt_challenge(ChallengeType) of
                    true -> obtain_certificate(ChallengeType, StateAcc);
                    false -> Prev
                end
            catch
                throw:Error ->
                    {Error, reset_nonce_pool(StateAcc)}
            end
    % internal server error: there should never be a case that no challenge
    % is supported by the plugin module, and no better error reason is thrown
    % from supports_letsencrypt_challenge.
    end, {?ERROR_INTERNAL_SERVER_ERROR, State}, challenge_types()),

    case Result of
        {ok, NewState} -> {ok, NewState};
        {Error, _} -> throw(Error)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the process of obtaining new certificate using provided
%% challenge type for authorization.
%% Requires Let's Encrypt account to be already created.
%% @end
%%--------------------------------------------------------------------
-spec obtain_certificate(challenge_type(), #flow_state{}) -> {ok, #flow_state{}}.
obtain_certificate(ChallengeType, State) ->
    CurrentMode = State#flow_state.current_mode,
    ?info("Let's Encrypt ~s run: attempt ~s challenge", [CurrentMode, ChallengeType]),
    {ok, State2} = create_order(State),
    {ok, State3} = fulfill_authorizations(ChallengeType, State2),

    ?info("Let's Encrypt ~s run: obtain certificate", [CurrentMode]),
    fetch_certificate(State3).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates order resource representing a request to issue a certificate.
%% @end
%%--------------------------------------------------------------------
-spec create_order(#flow_state{}) -> {ok, #flow_state{}}.
create_order(#flow_state{domain = Domain} = State) ->
    #flow_state{directory = #directory{new_order = NewOrderURL}} = State,
    Payload = #{<<"identifiers">> => [#{
        <<"type">> => <<"dns">>,
        <<"value">> => Domain
    }]},

    {ok, Response, Headers, State2} = post(NewOrderURL, Payload, ?HTTP_201_CREATED, State),
    #{
        <<"authorizations">> := AuthorizationURLs,
        <<"finalize">> := FinalizeURL
    } = Response,

    {ok, State3} = fetch_authorizations(AuthorizationURLs, State2),
    {ok, State3#flow_state{
        finalize_url = FinalizeURL,
        order_url = maps:get(<<"Location">>, Headers)
    }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Obtains authorization objects given by URLs.
%% @end
%%--------------------------------------------------------------------
-spec fetch_authorizations(AuthorizationURLs :: [url()], State :: #flow_state{}) ->
    {ok, #flow_state{}}.
fetch_authorizations(AuthorizationURLs, State) ->
    State2 = lists:foldl(fun(AuthURL, StateAcc) ->
        AuthzAcc = StateAcc#flow_state.authorizations,
        {ok, Authz, StateAcc2} = fetch_authorization(AuthURL, StateAcc),
        StateAcc2#flow_state{authorizations = [Authz | AuthzAcc]}
    end, State#flow_state{authorizations = []}, AuthorizationURLs),
    {ok, State2}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Obtains authorization object given by URL.
%% @end
%%--------------------------------------------------------------------
-spec fetch_authorization(url(), #flow_state{}) ->
    {ok, #authorization{}, #flow_state{}}.
fetch_authorization(URL, State) ->
    {ok, Body, _, State2} = post_as_get(URL, State),

    #{<<"type">> := IdType,
        <<"value">> := IdValue} = maps:get(<<"identifier">>, Body),
    ChallengeList = maps:get(<<"challenges">>, Body),

    Authorization = #authorization{
        challenges = lists:filtermap(fun parse_challenge/1, ChallengeList),
        identifier = #{type => IdType, value => IdValue}
    },
    {ok, Authorization, State2}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Brings each authorization to "valid" state by fulfilling one
%% challenge for each.
%%
%% Although the authorizations list returned by Let's Encrypt is expected
%% to contain 1 item since there is 1 "identifier" requested,
%% it is treated as arbitrary length here for robustness.
%% @end
%%--------------------------------------------------------------------
-spec fulfill_authorizations(challenge_type(), #flow_state{}) -> {ok, #flow_state{}}.
fulfill_authorizations(ChallengeType, #flow_state{authorizations = Authorizations} = State) ->
    lists:foldl(fun(Authz, {ok, StateAcc}) ->
        fulfill_challenge(ChallengeType, Authz, StateAcc)
    end, {ok, State}, Authorizations).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Enacts challenge response and waits for it to be accepted
%% by the Let's Encrypt server.
%% @end
%%--------------------------------------------------------------------
-spec fulfill_challenge(challenge_type(), #authorization{}, #flow_state{}) ->
    {ok, #flow_state{}}.
fulfill_challenge(ChallengeType, #authorization{challenges = Challenges}, State) ->
    {ok, Challenge} = find_challenge(ChallengeType, Challenges),
    {ok, State2} = handle_challenge(State#flow_state{challenge = Challenge}),
    {ok, State3, _} = poll_status(Challenge#challenge.url, State2),
    {ok, State3}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Selects challenge of given type.
%% @end
%%--------------------------------------------------------------------
-spec find_challenge(Type :: challenge_type(), ChallengeList :: [#challenge{}]) ->
    {ok, #challenge{}} | {error, _}.
find_challenge(Type, ChallengeList) ->
    case [Ch || Ch = #challenge{type = T} <- ChallengeList, T == Type] of
        [Challenge | _] -> {ok, Challenge};
        [] ->
            ?warning("Let's Encrypt did not offer challenge type ~s", [Type]),
            throw(?ERROR_LETS_ENCRYPT_RESPONSE(undefined, str_utils:format_bin(
                "Let's Encrypt did not offer challenge type ~s", [Type])))
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets up challenge response and triggers validation.
%% @end
%%--------------------------------------------------------------------
-spec handle_challenge(State :: #flow_state{}) -> {ok, #flow_state{}}.
handle_challenge(#flow_state{challenge = #challenge{type = http}} = State) ->
    #flow_state{
        challenge = #challenge{
            token = Token,
            url = URL
        },
        service = Service
    } = State,
    AuthString = make_auth_string(Token, State),

    ok = Service:set_http_record(Token, AuthString),

    {ok, _, _, State2} = post(URL, #{}, ?HTTP_200_OK, State),
    {ok, State2};

handle_challenge(#flow_state{challenge = #challenge{type = dns}} = State) ->
    #flow_state{
        challenge = #challenge{
            token = Token,
            url = URL
        },
        service = Service
    } = State,
    AuthString = make_auth_string(Token, State),
    TxtValue = base64url:encode(crypto:hash(sha256, AuthString)),
    ok = Service:set_txt_record(#{txt_name => ?LETSENCRYPT_TXT_NAME,
        txt_value => TxtValue, txt_ttl => ?LETSENCRYPT_TXT_TTL}),

    % Do not fail here even if TXT cannot be resolved
    % as there is no harm in attempting certification anyway
    wait_for_txt_propagation(?LETSENCRYPT_TXT_NAME, State#flow_state.domain,
        TxtValue, State#flow_state.service),

    {ok, _, _, State2} = post(URL, #{}, ?HTTP_200_OK, State),
    {ok, State2}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for challenge or order resource to have status "valid".
%% @end
%%--------------------------------------------------------------------
-spec poll_status(url(), State :: #flow_state{}) ->
    {ok, #flow_state{}, LastResult :: api_response()} | no_return().
poll_status(URL, #flow_state{} = State) ->
    poll_status(URL, State, ?LE_POLL_ATTEMPTS).


-spec poll_status(url(), State :: #flow_state{}, Attempts :: non_neg_integer()) ->
    {ok, #flow_state{}, LastResult :: api_response()} | no_return().
poll_status(_URL, #flow_state{} = _State, 0) ->
    ?error("Let's Encrypt authorization timed out"),
    throw(?ERROR_TIMEOUT);
poll_status(URL, #flow_state{} = State, Attempts) ->
    {ok, #{<<"status">> := Status} = Body, _, State2} = post_as_get(URL, State),
    case Status of
        <<"valid">> ->
            {ok, State2, Body};
        <<"pending">> ->
            timer:sleep(?LE_POLL_INTERVAL),
            poll_status(URL, State2, Attempts - 1);
        <<"processing">> ->
            timer:sleep(?LE_POLL_INTERVAL),
            poll_status(URL, State2, Attempts - 1);
        <<"invalid">> ->
            ?error("Let's Encrypt did not accept challenge response: ~p", [Body]),
            {ErrorObject, Message} = case Body of
                #{<<"error">> := #{<<"detail">> := Description} = Error} -> {Error, Description};
                _ -> {undefined, <<"Let's encrypt could not authorize domain.">>}
            end,
            throw(?ERROR_LETS_ENCRYPT_RESPONSE(ErrorObject, Message))
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Request and download the certificate.
%% @end
%%--------------------------------------------------------------------
-spec fetch_certificate(#flow_state{}) -> {ok, #flow_state{}}.
fetch_certificate(#flow_state{order_url = OrderURL} = State) ->
    {ok, State2, OrderObject, KeyPem} = request_certificate(State),

    {CertURL, State4} = case OrderObject of
        #{<<"status">> := <<"valid">>, <<"certificate">> := URL} ->
            {URL, State2};
        #{<<"status">> := <<"processing">>} ->
            % Order status will change to "valid" when certificate is ready for download
            {ok, State3, #{<<"certificate">> := URL}} = poll_status(OrderURL, State2),
            {URL, State3}
    end,
    {ok, {raw, CertAndChainPem}, _, State5} = post_as_get(CertURL, State4),

    case State5#flow_state.save_cert of
        true ->
            save_cert(CertAndChainPem, KeyPem, State5),
            ?info("Let's Encrypt ~s run: saved new certificate at ~s",
                [State#flow_state.current_mode, filename:absname(State5#flow_state.cert_path)]),
            {ok, State5};
        _ ->
            {ok, State5}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends Certificate Signing Request to the server to create a certificate.
%% @end
%%--------------------------------------------------------------------
-spec request_certificate(#flow_state{}) ->
    {ok, #flow_state{}, Order :: map(), pem()}.
request_certificate(State) ->
    #flow_state{domain = Domain, finalize_url = FinalizeURL} = State,

    % The CSR does not contain Alt Name extension request, but Let's Encrypt
    % adds one for the requested domain, which is the desired result.
    {ok, CSRPem, KeyPem} = onepanel_cert:generate_csr_and_key(binary_to_list(Domain)),
    [{'CertificationRequest', CSRDer, not_encrypted}] = public_key:pem_decode(CSRPem),
    CSRB64 = base64url:encode(CSRDer),

    Payload = #{<<"csr">> => CSRB64},
    {ok, OrderObject, _, State2} = post(FinalizeURL, Payload, ?HTTP_200_OK, State),
    {ok, State2, OrderObject, KeyPem}.


%%%===================================================================
%%% Helper functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns run mode, depending on application settings.
%% @end
%%--------------------------------------------------------------------
-spec resolve_run_mode() -> run_mode().
resolve_run_mode() ->
    ModeEnv = onepanel_env:get(letsencrypt_mode, ?APP_NAME, full),
    case {ModeEnv, ?STAGING_DIRECTORY_URL} of
        {production, _} -> production;
        {full, undefined} ->
            ?info("No staging ACME server URL defined. Skipping staging run and running in production mode."),
            production;
        {full, _} ->
            ?info("Let's Encrypt: Starting test run against staging server before actual "
            "Let's Encrypt certification"),
            full;
        {_, undefined} ->
            ?error("No staging server URL defined for Let's Encrypt. Cannot perform ~s run",
                [ModeEnv]),
            error(no_letsencrypt_staging_server);
        {StagingOrDry, _} ->
            StagingOrDry
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepares the #flow_state{} record.
%% Mode - target mode
%% CurrentMode - mode of the current run
%% @end
%%--------------------------------------------------------------------
-spec initial_state(Domain :: binary(), Plugin :: plugin_service(),
    Mode :: run_mode(), CurrentMode :: run_mode()) -> {ok, #flow_state{}}.
initial_state(Domain, Plugin, Mode, CurrentMode) ->
    CertPath = onepanel_env:get(web_cert_file),
    KeyPath = onepanel_env:get(web_key_file),
    ChainPath = onepanel_env:get(web_cert_chain_file),
    KeysDir = filename:join(?LETSENCRYPT_KEYS_DIR, atom_to_list(CurrentMode)),

    % save cert only in the final run and not in 'dry' mode
    SaveCert = Mode == production orelse Mode == staging,

    DirectoryURL = case CurrentMode of
        production -> ?PRODUCTION_DIRECTORY_URL;
        _ -> ?STAGING_DIRECTORY_URL
    end,

    % passing state around is useful for managing anti-replay nonces
    {ok, #flow_state{
        service = Plugin,
        directory_url = DirectoryURL,
        jws_keys_dir = KeysDir,
        cert_path = CertPath,
        cert_private_key_path = KeyPath,
        chain_path = ChainPath,
        save_cert = SaveCert,
        domain = Domain,
        current_mode = CurrentMode
    }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates directory map to record.
%% @end
%%--------------------------------------------------------------------
-spec decode_directory(map()) -> #directory{}.
decode_directory(Map) ->
    #directory{
        key_change = maps:get(<<"keyChange">>, Map),
        new_account = maps:get(<<"newAccount">>, Map),
        new_nonce = maps:get(<<"newNonce">>, Map),
        new_order = maps:get(<<"newOrder">>, Map),
        revoke_cert = maps:get(<<"revokeCert">>, Map)
    }.


%%%===================================================================
%%% Connection and encoding functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs HTTP GET, retrying upon connection errors.
%% @end
%%--------------------------------------------------------------------
-spec http_get(url(), Attempts :: non_neg_integer()) ->
    {ok, http_client:code(), http_client:headers(), api_response()} | no_return().
http_get(_URL, 0) ->
    throw(?ERROR_LETS_ENCRYPT_NOT_REACHABLE);

http_get(URL, Attempts) ->
    case http_client:get(URL, #{}, <<>>, ?HTTP_OPTS) of
        {ok, Status, Headers, BodyRaw} ->
            {ok, Status, Headers, decode_body(BodyRaw)};
        {error, Reason} ->
            ?error("Error performing GET request to ~s: ~p", [URL, {error, Reason}]),
            http_get(URL, Attempts - 1)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Performs a POST-as-GET request, that is a request with
%% empty payload intended to fetch a resource while providing
%% authentication via JWS-signed body.
%% @end
%%--------------------------------------------------------------------
-spec post_as_get(url(), #flow_state{}) -> request_result() | no_return().
post_as_get(URL, State) ->
    post(URL, <<>>, ?HTTP_200_OK, State).


%%--------------------------------------------------------------------
%% @doc
%% Performs post request with given Payload packaged as JWS.
%% Verifies response status and decodes body as JSON.
%% If returned body cannot be parsed as JSON it is returned as {raw, binary()}.
%% @end
%%--------------------------------------------------------------------
-spec post(url(), Payload :: term(), OkCodes :: one_or_many(http_client:code()),
    State :: #flow_state{}) -> request_result() | no_return().
post(URL, Payload, OkCodes, #flow_state{} = State) ->
    post(URL, Payload, OkCodes, State, ?POST_RETRIES).


-spec post(url(), Payload :: term(), OkCodes :: one_or_many(http_client:code()),
    #flow_state{}, Attempts :: non_neg_integer()) -> request_result() | no_return().
post(URL, Payload, OkCode, State, Attempts) when is_integer(OkCode) ->
    post(URL, Payload, [OkCode], State, Attempts);

post(_URL, _Payload, _OkCodes, _State, 0) ->
    throw(?ERROR_LETS_ENCRYPT_NOT_REACHABLE);

post(URL, Payload, OkCodes, #flow_state{} = State, Attempts) ->
    {ok, Body, State2} = encode(URL, Payload, State),
    ContentType = <<"application/jose+json">>,
    ReqHeaders = #{?HDR_CONTENT_TYPE => ContentType},

    case http_client:post(URL, ReqHeaders, Body, ?HTTP_OPTS) of
        {ok, Status, Headers, ResponseRaw} ->
            Response = decode_body(ResponseRaw),
            case lists:member(Status, OkCodes) of
                true ->
                    {ok, Response, Headers, push_nonce(Headers, State2)};
                false ->
                    OkCodesStr = onepanel_utils:join(OkCodes, <<" or ">>),
                    case {Status, Response} of
                        {?HTTP_400_BAD_REQUEST, #{<<"type">> := <<"urn:ietf:params:acme:error:badNonce">>}} ->
                            % badNonce - retry with newly received nonce
                            post(URL, Payload, OkCodes, push_nonce(Headers, State2), Attempts - 1);
                        {_, #{<<"type">> := _, <<"detail">> := ErrorMessage} = Error} ->
                            ?error("Let's Encrypt response status: ~B, expected ~s~n"
                            "Response headers: ~p~nResponse body:~p",
                                [Status, OkCodesStr, Headers, Response]),
                            throw(?ERROR_LETS_ENCRYPT_RESPONSE(Error, ErrorMessage));
                        {Code, Response} ->
                            ?error("Let's Encrypt response status: ~B, expected ~s~n"
                            "Response headers: ~p~nResponse body:~p",
                                [Status, OkCodesStr, Headers, Response]),
                            throw(?ERROR_LETS_ENCRYPT_RESPONSE(undefined, str_utils:format_bin(
                                "Unexpected Let's Encrypt response with HTTP code ~b", [Code])))
                    end
            end;

        {error, Reason} ->
            ?error("Error performing POST request to ~s: ~p", [URL, {error, Reason}]),
            post(URL, Payload, OkCodes, State2, Attempts - 1)
    end.


%% @private
-spec decode_body(Body :: binary()) -> api_response().
decode_body(Body) ->
    try
        json_utils:decode(Body)
    catch
        _:invalid_json -> {raw, Body}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns Payload encoded as JWS JSON Serialization,
%% adding headers required by Let's Encrypt API.
%% @end
%%--------------------------------------------------------------------
-spec encode(url(), map() | binary(), #flow_state{}) -> {ok, binary(), #flow_state{}}.
encode(URL, Payload, #flow_state{jws_keys_dir = KeysDir} = State) ->
    {ok, Key, _} = read_keys(KeysDir),
    {Nonce, State2} = pop_nonce(State),
    KeyHeader = get_jwk_or_kid(State2),
    ProtectedHeader = KeyHeader#{<<"nonce">> => Nonce, <<"url">> => URL},

    {ok, BodyMap} = onepanel_jws:sign(Payload, ProtectedHeader, Key),
    {ok, json_utils:encode(BodyMap), State2}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates jwk or kid part of the JWS protected header.
%% "jwk" field is to be used during account creation, afterwards "kid"
%% should be sent containing account url returned by the server.
%% @end
%%--------------------------------------------------------------------
-spec get_jwk_or_kid(#flow_state{}) -> map().
get_jwk_or_kid(#flow_state{account_url = undefined, jws_keys_dir = KeysDir}) ->
    {ok, _, Pubkey} = read_keys(KeysDir),
    #{<<"jwk">> => onepanel_jws:key_to_jwk_map(Pubkey)};

get_jwk_or_kid(#flow_state{account_url = AccountUrl}) when is_binary(AccountUrl) ->
    #{<<"kid">> => AccountUrl}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns nonce from the pool or performs HEAD request to obtain a new one.
%% @end
%%--------------------------------------------------------------------
-spec pop_nonce(#flow_state{}) -> {nonce(), #flow_state{}}.
pop_nonce(#flow_state{nonces = [Nonce | Rest]} = State) ->
    {Nonce, State#flow_state{nonces = Rest}};
pop_nonce(#flow_state{nonces = [], directory = Directory} = State) ->
    {ok, ?HTTP_200_OK, Headers} = http_client:request(head, Directory#directory.new_nonce, #{}, <<>>, ?HTTP_OPTS),
    pop_nonce(push_nonce(Headers, State)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds nonce retrieved from response headers to the nonce pool.
%% @end
%%--------------------------------------------------------------------
-spec push_nonce(cowboy:http_headers(), #flow_state{}) -> #flow_state{}.
push_nonce(#{<<"Replay-Nonce">> := Nonce}, #flow_state{nonces = Nonces} = State) ->
    State#flow_state{nonces = [Nonce | Nonces]}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Empties the nonce pool. New nonce will be obtained in next call
%% to {@link pop_nonce/1} or {@link post/5}.
%% @end
%%--------------------------------------------------------------------
-spec reset_nonce_pool(#flow_state{}) -> #flow_state{}.
reset_nonce_pool(State) ->
    State#flow_state{nonces = []}.


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
-spec clean_keys(KeysDir :: string()) -> ok.
clean_keys(KeysDir) ->
    lists:foreach(fun(File) ->
        Path = filename:join(KeysDir, File),
        case filelib:is_regular(Path) of
            true -> file:delete(Path);
            _ -> ok
        end
    end, [?PRIVATE_RSA_KEY, ?PUBLIC_RSA_KEY]).


%%%===================================================================
%%% Challenge helpers
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates challenge description returned by the Let's Encrypt API.
%% Return type matches that of {@link lists:filtermap/2} fun.
%% @end
%%--------------------------------------------------------------------
-spec parse_challenge(ChallengeMap :: #{binary() := _}) ->
    {true, #challenge{}} | false.
parse_challenge(#{
    <<"type">> := Type,
    <<"token">> := Token,
    <<"url">> := URL}) ->
    TypeAtom = challenge_type_to_atom(Type),
    {true, #challenge{token = Token, url = URL, type = TypeAtom}};

parse_challenge(_) -> false.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates challenge type used by the Let's Encrypt API.
%% @end
%%--------------------------------------------------------------------
-spec challenge_type_to_atom(binary()) -> challenge_type().
challenge_type_to_atom(<<"dns-01">>) -> dns;
challenge_type_to_atom(<<"http-01">>) -> http;
challenge_type_to_atom(_) -> unknown.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for the expected DNS txt record to be resolvable.
%% TxtName and Domain are used to construct the query.
%% Expected is the correct content.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_txt_propagation(TxtName :: binary(), Domain :: binary(),
    Expected :: binary(), Plugin :: module()) -> ok | {error, timeout}.
wait_for_txt_propagation(TxtName, Domain, Expected, Plugin) ->
    Query = binary:bin_to_list(<<TxtName/binary, ".", Domain/binary>>),
    Validator = fun(#dns_check{summary = ok}) -> ok end,

    ?info("Let's Encrypt: Waiting for txt record valued ~p at ~s", [Expected, Query]),
    try
        ZoneDomain = Plugin:get_dns_server(),
        {ok, IP} = inet:getaddr(ZoneDomain, inet),

        % check that onezone responds with correct txt record
        onepanel_utils:wait_until(onepanel_dns, check_all,
            [[Expected], [Query], txt, [IP]],
            {validator, Validator}, ?WAIT_FOR_TXT_ATTEMPTS, ?WAIT_FOR_TXT_DELAY),

        % check that txt record is visible globally in DNS
        onepanel_utils:wait_until(onepanel_dns, check_all,
            [[Expected], [Query], txt, ?DNS_SERVERS],
            {validator, Validator}, ?WAIT_FOR_TXT_ATTEMPTS, ?WAIT_FOR_TXT_DELAY)
    catch _:_ ->
        ?warning("Cannot verify that txt DNS record needed by Let's Encrypt client "
        "was set in Onezone DNS. Continuing anyway. If certification fails, "
        "contact your Onezone administrator."),
        {error, timeout}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes txt record set for Let's Encrypt authorization
%% @end
%%--------------------------------------------------------------------
-spec clean_txt_record(Service :: module()) -> ok.
clean_txt_record(Service) ->
    ok = Service:remove_txt_record(#{txt_name => ?LETSENCRYPT_TXT_NAME}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates authorization string needed to fulfill http or dns challenge.
%% @end
%%--------------------------------------------------------------------
-spec make_auth_string(token(), #flow_state{}) -> binary().
make_auth_string(Token, #flow_state{jws_keys_dir = KeysDir}) ->
    {ok, Key, _} = read_keys(KeysDir),
    Thumbprint = onepanel_jws:thumbprint(Key),
    <<Token/binary, ".", Thumbprint/binary>>.


%%%===================================================================
%%% Certificate file helpers
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Saves given certificates PEMs on all onepanel nodes.
%% @end
%%--------------------------------------------------------------------
-spec save_cert(CertAndChainPem :: onepanel_cert:pem(),
    KeyPem :: onepanel_cert:pem(), #flow_state{}) -> ok.
save_cert(CertAndChainPem, KeyPem, State) ->
    #flow_state{
        cert_private_key_path = KeyPath,
        cert_path = CertPath,
        chain_path = ChainPath
    } = State,
    Nodes = nodes:all(?SERVICE_PANEL),

    {ok, CertPem, ChainPem} = split_chain(CertAndChainPem),
    ok = utils:save_file_on_hosts(Nodes, KeyPath, KeyPem),
    ok = utils:save_file_on_hosts(Nodes, CertPath, CertPem),
    ok = utils:save_file_on_hosts(Nodes, ChainPath, ChainPem).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Split pem data containing the main certificate and its
%% chain certificates.
%% @end
%%--------------------------------------------------------------------
-spec split_chain(PemData :: pem()) ->
    {ok, EndCert :: pem(), ChainCerts :: pem()}.
split_chain(PemData) ->
    [CertDer | ChainDers] = cert_utils:pem_to_ders(PemData),
    CertPem = cert_utils:ders_to_pem(CertDer),
    ChainPem = cert_utils:ders_to_pem(ChainDers),
    {ok, CertPem, ChainPem}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures all certificate and key paths are writable.
%% Creates files if they do not exist.
%% @end
%%--------------------------------------------------------------------
-spec ensure_files_access(#flow_state{} | [string()]) -> ok | no_return().
ensure_files_access(#flow_state{} = State) ->
    ensure_files_access([
        State#flow_state.cert_path,
        State#flow_state.cert_private_key_path,
        State#flow_state.chain_path
    ]);
ensure_files_access(Paths) ->
    lists:foreach(fun(Path) ->
        case check_write_access(Path) of
            ok -> ok;
            {error, Reason} -> throw(?ERROR_FILE_ACCESS(Path, Reason))
        end
    end, Paths).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% If Path points to existing file checks if it can be opened for writing.
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
