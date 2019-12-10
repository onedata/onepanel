%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains utility functions for REST API tests.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_test_rest).
-author("Krzysztof Trzepla").

-include("authentication.hrl").
-include("onepanel_test_utils.hrl").
-include("onepanel_test_rest.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").

%% API
-export([auth_request/4, auth_request/5, auth_request/6, auth_request/7,
    noauth_request/3, noauth_request/4, noauth_request/5, noauth_request/6]).
-export([assert_body_fields/2, assert_body_values/2, assert_body/2]).
-export([set_default_passphrase/1, mock_token_authentication/1,
    oz_token_auth/1, oz_token_auth/2, oz_token_auth/3,
    obtain_local_token/2, construct_token/1]).

-type config() :: string() | proplists:proplist().
-type endpoint() :: http_client:url() | {noprefix, http_client:url()}.
-type method() :: http_client:method().
-type auth() :: {Username :: binary(), Password :: binary()} |
                (Passphrase :: binary()) |
                {cookie, Name :: binary(), Value :: binary()} |
                {token, Token :: binary()} |
                none.
-type headers() :: http_client:headers().
-type body() :: http_client:body().
-type response() :: {ok, Code :: http_client:code(), Headers :: headers(), Body :: body()} |
                    {error, Reason :: term()}.

-export_type([auth/0]).

%%%===================================================================
%%% REST client
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv auth_request(HostOrConfig, Endpoint, Method, Username, Password, <<>>)
%% @end
%%--------------------------------------------------------------------
-spec auth_request(HostOrConfig :: config(), Endpoint :: endpoint(),
    Method :: method(), Auth :: auth()) -> Response :: response().
auth_request(HostOrConfig, Endpoint, Method, Auth) ->
    auth_request(HostOrConfig, Endpoint, Method, Auth, <<>>).


%%--------------------------------------------------------------------
%% @doc @equiv auth_request(HostOrConfig, Endpoint, Method, Username, Password,
%% [], Body)
%% @end
%%--------------------------------------------------------------------
-spec auth_request(HostOrConfig :: config(), Endpoint :: endpoint(),
    Method :: method(), Auth :: auth(), Body :: body()) -> Response :: response().
auth_request(HostOrConfig, Endpoint, Method, Auth, Body) ->
    auth_request(HostOrConfig, Endpoint, Method, Auth, [], Body).


%%--------------------------------------------------------------------
%% @doc @equiv auth_request(HostOrConfig, 9443, Endpoint, Method, Username,
%% Password, Headers, Body)
%% @end
%%--------------------------------------------------------------------
-spec auth_request(HostOrConfig :: config(), Endpoint :: endpoint(),
    Method :: method(), Auth :: auth(), Headers :: headers(), Body :: body()) ->
    Response :: response().
auth_request(HostOrConfig, Endpoint, Method, Auth, Headers, Body) ->
    auth_request(HostOrConfig, 9443, Endpoint, Method, Auth, Headers, Body).


%%--------------------------------------------------------------------
%% @doc Executes noauth_request/6 with authorization header.
%% Provided authorization may be a list, in which case the request
%% is asserted to return identical code and body for all of them
%% (for this to work, the operation must be idempotent).
%% @end
%%--------------------------------------------------------------------
-spec auth_request(HostOrConfig :: config(), Port :: integer(), Endpoint :: endpoint(),
    Method :: method(), Auth :: auth() | [auth()], Headers :: headers(), Body :: body()) ->
    Response :: response().
auth_request(HostOrConfig, Port, Endpoint, Method, none, Headers, Body) ->
    noauth_request(HostOrConfig, Port, Endpoint, Method, Headers, Body);

auth_request(HostOrConfig, Port, Endpoint, Method, {cookie, Name, Value},
    Headers, Body) ->
    NewHeaders = [{<<"cookie">>, <<Name/binary, "=", Value/binary>>}
        | Headers],
    noauth_request(HostOrConfig, Port, Endpoint, Method, NewHeaders, Body);

auth_request(HostOrConfig, Port, Endpoint, Method, {token, Token},
    Headers, Body) ->
    NewHeaders = [{?HDR_X_AUTH_TOKEN, Token} | Headers],
    noauth_request(HostOrConfig, Port, Endpoint, Method, NewHeaders, Body);

auth_request(HostOrConfig, Port, Endpoint, Method, <<Passphrase/binary>>,
    Headers, Body) ->
    Username = ?LOCAL_USERNAME,
    auth_request(HostOrConfig, Port, Endpoint, Method, {Username, Passphrase}, Headers, Body);

auth_request(HostOrConfig, Port, Endpoint, Method, {Username, Password}, Headers, Body) ->
    NewHeaders = [onepanel_utils:get_basic_auth_header(Username, Password) | Headers],
    noauth_request(HostOrConfig, Port, Endpoint, Method, NewHeaders, Body);

auth_request(HostOrConfig, Port, Endpoint, Method, [_ | _] = Auths, Headers, Body) ->
    {Result, _LastAuth} = lists:foldl(fun
        (Auth, first) ->
            {auth_request(HostOrConfig, Port, Endpoint, Method, Auth, Headers, Body), Auth};
        (Auth, {{ok, PrevCode, _RespHeaders, PrevBody} = Previous, PrevAuth}) ->
            case auth_request(HostOrConfig, Port, Endpoint, Method, Auth, Headers, Body) of
                {ok, PrevCode, _, PrevBody} = Result ->
                    {Result, Auth};
                {ok, _OtherCode, _, _OtherBody} = Result ->
                    ct:pal("Result of rest call with auth ~p differs from previous with auth ~p~n"
                    ++ "Was: ~p~nIs: ~p", [Auth, PrevAuth, Previous, Result]),
                    ?assertMatch({ok, PrevCode, _, PrevBody}, Result);
                Error ->
                    {Error, PrevAuth}
            end;
        (_Auth, {Error, PrevAuth}) ->
            {Error, PrevAuth}
    end, first, Auths),
    Result.


%%--------------------------------------------------------------------
%% @doc @equiv noauth_request(HostOrConfig, Endpoint, Method, [])
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(HostOrConfig :: config(), Endpoint :: endpoint(),
    Method :: method()) -> Response :: response().
noauth_request(HostOrConfig, Endpoint, Method) ->
    noauth_request(HostOrConfig, Endpoint, Method, []).


%%--------------------------------------------------------------------
%% @doc @equiv noauth_request(HostOrConfig, Endpoint, Method, [], Body)
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(HostOrConfig :: config(), Endpoint :: endpoint(),
    Method :: method(), Body :: body()) -> Response :: response().
noauth_request(HostOrConfig, Endpoint, Method, Body) ->
    noauth_request(HostOrConfig, Endpoint, Method, [], Body).


%%--------------------------------------------------------------------
%% @doc @equiv noauth_request(HostOrConfig, 9443, Endpoint, Method, Headers, Body)
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(HostOrConfig :: config(), Endpoint :: endpoint(),
    Method :: method(), Headers :: headers(), Body :: body()) ->
    Response :: response().
noauth_request(HostOrConfig, Endpoint, Method, Headers, Body) ->
    noauth_request(HostOrConfig, 9443, Endpoint, Method, Headers, Body).


%%--------------------------------------------------------------------
%% @doc Performs HTTP request.
%% @end
%%--------------------------------------------------------------------
-spec noauth_request(HostOrConfig :: config(), Port :: integer(),
    Endpoint :: endpoint(), Method :: method(), Headers :: headers(),
    Body :: body()) -> Response :: response().
noauth_request(HostOrConfig, Port, {noprefix, Path}, Method, Headers, Body) ->
    Host = case io_lib:printable_unicode_list(HostOrConfig) of
        true -> HostOrConfig;
        false -> utils:random_element(?config(all_hosts, HostOrConfig))
    end,
    NewHeaders = [
        {?HDR_CONTENT_TYPE, <<"application/json">>} |
        Headers
    ],
    Url = onepanel_utils:join(["https://", Host, ":", Port, Path]),
    JsonBody = json_utils:encode(Body),
    http_client:request(Method, Url, maps:from_list(NewHeaders), JsonBody,
        [{ssl_options, [{secure, false}]}]);

noauth_request(HostOrConfig, Port, Endpoint, Method, Headers, Body) ->
    Path = str_utils:format("/api/v3/onepanel~s", [Endpoint]),
    noauth_request(HostOrConfig, Port, {noprefix, Path}, Method, Headers, Body).


%%--------------------------------------------------------------------
%% @doc Checks whether the response body contains specified fields.
%% @end
%%--------------------------------------------------------------------
-spec assert_body_fields(JsonBody :: binary(), Fields :: [binary()]) -> ok.
assert_body_fields(JsonBody, Fields) ->
    onepanel_test_utils:assert_fields(json_utils:decode(JsonBody), Fields).


%%--------------------------------------------------------------------
%% @doc Checks whether the response body contains specified fields and their
%% values match the expected ones.
%% @end
%%--------------------------------------------------------------------
-spec assert_body_values(JsonBody :: binary(),
    Values :: [{binary(), any()}] | #{binary() => any()}) -> ok.
assert_body_values(JsonBody, Values) when is_map(Values) ->
    assert_body_values(JsonBody, maps:to_list(Values));

assert_body_values(JsonBody, Values) when is_list(Values) ->
    onepanel_test_utils:assert_values(json_utils:decode(JsonBody), Values).


%%--------------------------------------------------------------------
%% @doc Checks whether the response body matches the expected one.
%% @end
%%--------------------------------------------------------------------
-spec assert_body(JsonBody :: binary(), Body :: any()) -> ok.
assert_body(JsonBody, Body) ->
    ?assertEqual(Body, json_utils:decode(JsonBody)).


%%%===================================================================
%%% Mock helpers
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc On all nodes sets up emergency passphrase used
%% by the ?ROOT_AUTHS and ?OZ_OR_ROOT_AUTHS macros.
%% @end
%%--------------------------------------------------------------------
set_default_passphrase(Config) ->
    Validator = fun(Result) ->
        ?assertMatch(ok, Result)
    end,
    Results = ?callAll(Config, emergency_passphrase, set, [?EMERGENCY_PASSPHRASE]),
    lists:foreach(Validator, Results).


%%--------------------------------------------------------------------
%% @doc Sets up mock of onezone_tokens module to accept tokens with
%% encoded client information, as created by the onepanel_test_rest utilities.
%% Requires hook `{?LOAD_MODULES, [onepanel_test_rest]}` to work correctly.
%% @end
%%--------------------------------------------------------------------
-spec mock_token_authentication(ConfigOrNodes :: proplists:proplist() | [node()]) -> ok.
mock_token_authentication([{_, _} | _] = Config) ->
    mock_token_authentication(?config(all_nodes, Config));

mock_token_authentication(Nodes) ->
    test_utils:mock_new(Nodes, [onezone_tokens]),
    test_utils:mock_expect(Nodes, onezone_tokens, authenticate_user, fun
        (<<"valid-token:", ClientB64/binary>> = Token, _PeerIp) ->
            ClientBin = base64:decode(ClientB64),
            Client = erlang:binary_to_term(ClientBin),
            case onepanel_env:get_cluster_type() of
                oneprovider -> Client#client{zone_auth = {rest, {token, Token}}};
                onezone -> Client#client{zone_auth = {rpc, opaque_client}}
            end;
        (_BadToken, _PeerIp) -> ?ERROR_TOKEN_INVALID
    end).


%%--------------------------------------------------------------------
%% @doc Generates a token which can be decoded by mocked
%% onezone tokens module to simulate obtaining user info.
%% Authenticated user has no privileges in the cluster.
%% @end
%%--------------------------------------------------------------------
-spec oz_token_auth(Name :: binary()) -> auth().
oz_token_auth(Name) ->
    oz_token_auth(Name, []).

%%--------------------------------------------------------------------
%% @doc Generates a token which can be decoded by mocked
%% onezone tokens module to obtain user info.
%% Derives user id from Name.
%% @end
%%--------------------------------------------------------------------
-spec oz_token_auth(Name :: binary(),
    Privileges :: [privileges:cluster_privilege()]) -> auth().
oz_token_auth(Name, Privileges) ->
    oz_token_auth(<<Name/binary, "Id">>, Name, Privileges).

%%--------------------------------------------------------------------
%% @doc Generates a token which can be decoded by mocked
%% onezone tokens module to obtain user info.
%% @end
%%--------------------------------------------------------------------
-spec oz_token_auth(UserId :: binary(), Name :: binary(),
    Privileges :: [privileges:cluster_privilege()]) -> auth().
oz_token_auth(UserId, Name, Privileges) ->
    Client = erlang:term_to_binary(#client{
        role = member, privileges = Privileges,
        user = #user_details{id = UserId, username = Name, full_name = Name}
    }),
    ClientB64 = base64:encode(Client),
    {token, <<"valid-token:", ClientB64/binary>>}.


%%--------------------------------------------------------------------
%% @doc Obtains authentication token linked with a local user's session.
%% @end
%%--------------------------------------------------------------------
-spec obtain_local_token(HostOrConfig :: config(),
    {Username :: binary(), Auth :: auth()}) -> auth().
obtain_local_token(HostOrConfig, Auth) ->
    {ok, _, #{<<"set-cookie">> := CookieHeader}, _} = ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _},
        auth_request(HostOrConfig, {noprefix, "/login"}, post, Auth)),

    SessionCookieKey = gui_session_plugin:session_cookie_key(),
    Cookies = hackney_cookie:parse_cookie(CookieHeader),
    {SessionCookieKey, SessionCookie} = proplists:lookup(SessionCookieKey, Cookies),

    SessionAuth = {cookie, SessionCookieKey, SessionCookie},
    {ok, _, _, TokenJson} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
        auth_request(HostOrConfig, {noprefix, "/gui-preauthorize"}, post, SessionAuth)),

    #{<<"token">> := Token} = json_utils:decode(TokenJson),
    {token, Token}.


-spec construct_token([caveats:caveat()]) -> tokens:serialized().
construct_token(Caveats) ->
    {ok, Token} = tokens:serialize(tokens:construct(#token{
        subject = ?SUB(user, <<"userId">>),
        onezone_domain = <<"someonezone.test">>,
        persistent = false,
        id = str_utils:rand_hex(16)}, <<"someSecret">>, Caveats)),
    Token.
