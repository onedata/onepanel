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
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([auth_request/4, auth_request/5, auth_request/6, auth_request/7,
    noauth_request/3, noauth_request/4, noauth_request/5, noauth_request/6]).
-export([assert_body_fields/2, assert_body_values/2, assert_body/2]).
-export([mock_token_authentication/1,
    oz_token_auth/1, oz_token_auth/2, oz_token_auth/3,
    obtain_local_token/3]).

-type config() :: string() | proplists:proplist().
-type endpoint() :: http_client:url() | {noprefix, http_client:url()}.
-type method() :: http_client:method().
-type auth() :: {Username :: binary(), Password :: binary()} |
                {cookie, Cookie :: binary()} |
                {cookie, Name :: binary(), Value :: binary()} |
                {token, Token :: binary()} |
                none.
-type headers() :: http_client:headers().
-type body() :: http_client:body().
-type code() :: http_client:code().
-type response() :: {ok, Code :: code(), Headers :: headers(), Body :: body()} |
                    {error, Reason :: term()}.

%%%===================================================================
%%% API functions
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
%% is asserted to return identical code and body for all of them.
%% @end
%%--------------------------------------------------------------------
-spec auth_request(HostOrConfig :: config(), Port :: integer(), Endpoint :: endpoint(),
    Method :: method(), Auth :: auth() | [auth()], Headers :: headers(), Body :: body()) ->
    Response :: response().
auth_request(HostOrConfig, Port, Endpoint, Method, none, Headers, Body) ->
    noauth_request(HostOrConfig, Port, Endpoint, Method, Headers, Body);

auth_request(HostOrConfig, Port, Endpoint, Method, {cookie, SessionId},
    Headers, Body) ->
    % @fixme does it even work
    NewHeaders = [{<<"cookie">>, <<"sessionId=", SessionId/binary>>}
        | Headers],
    noauth_request(HostOrConfig, Port, Endpoint, Method, NewHeaders, Body);

auth_request(HostOrConfig, Port, Endpoint, Method, {cookie, Name, Value},
    Headers, Body) ->
    NewHeaders = [{<<"cookie">>, <<Name/binary, "=", Value/binary>>}
        | Headers],
    noauth_request(HostOrConfig, Port, Endpoint, Method, NewHeaders, Body);

auth_request(HostOrConfig, Port, Endpoint, Method, {token, Token},
    Headers, Body) ->
    NewHeaders = [{<<"x-auth-token">>, Token} | Headers],
    noauth_request(HostOrConfig, Port, Endpoint, Method, NewHeaders, Body);

auth_request(HostOrConfig, Port, Endpoint, Method, {Username, Password},
    Headers, Body) ->
    NewHeaders = [onepanel_utils:get_basic_auth_header(Username, Password)
        | Headers],
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
noauth_request(HostOrConfig, Port, {noprefix, Endpoint}, Method, Headers, Body) ->
    Host = case io_lib:printable_unicode_list(HostOrConfig) of
        true -> HostOrConfig;
        false -> utils:random_element(?config(onepanel_hosts, HostOrConfig))
    end,
    NewHeaders = [
        {<<"content-type">>, <<"application/json">>} |
        Headers
    ],
    Url = onepanel_utils:join(["https://", Host, ":", Port, Endpoint]),
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
-spec assert_body_values(JsonBody :: binary(), Values :: [{binary(), any()}]) -> ok.
assert_body_values(JsonBody, Values) ->
    onepanel_test_utils:assert_values(json_utils:decode(JsonBody), Values).


%%--------------------------------------------------------------------
%% @doc Checks whether the response body matches the expected one.
%% @end
%%--------------------------------------------------------------------
-spec assert_body(JsonBody :: binary(), Body :: any()) -> ok.
assert_body(JsonBody, Body) ->
    ?assertEqual(Body, json_utils:decode(JsonBody)).


-spec mock_token_authentication(ConfigOrNodes :: proplists:proplist() | [node()]) -> ok.
mock_token_authentication([{_, _} | _] = Config) ->
    mock_token_authentication(?config(nodes, Config));

mock_token_authentication(Nodes) ->
    test_utils:mock_new(Nodes, [zone_tokens]),
    test_utils:mock_expect(Nodes, zone_tokens, authenticate_user, fun
        (<<"valid-token:", ClientB64/binary>> = Token) ->
            ClientBin = base64:decode(ClientB64),
            Client = erlang:binary_to_term(ClientBin),
            case onepanel_env:get_cluster_type() of
                oneprovider -> Client#client{zone_auth = {rest, {access_token, Token}}};
                onezone -> Client#client{zone_auth = {rpc, opaque_client}}
            end
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
        user = #user_details{id = UserId, name = Name}
    }),
    ClientB64 = base64:encode(Client),
    {token, <<"valid-token:", ClientB64/binary>>}.


%%--------------------------------------------------------------------
%% @doc Obtains authentication token linked with a local user's session.
%% @end
%%--------------------------------------------------------------------
-spec obtain_local_token(HostOrConfig :: config(),
    Username :: binary(), Password :: binary()) -> auth().
obtain_local_token(HostOrConfig, Username, Password) ->
    {ok, _, #{<<"set-cookie">> := CookieHeader}, _} = ?assertMatch({ok, 204, _, _},
        auth_request(HostOrConfig, {noprefix, "/login"}, post, {Username, Password})),

    SessionCookieKey = gui_session_plugin:session_cookie_key(),
    Cookies = hackney_cookie:parse_cookie(CookieHeader),
    {SessionCookieKey, SessionCookie} = proplists:lookup(SessionCookieKey, Cookies),

    SessionAuth = {cookie, SessionCookieKey, SessionCookie},
    {ok, _, _, TokenJson} = ?assertMatch({ok, 200, _, _},
        auth_request(HostOrConfig, {noprefix, "/gui-token"}, post, SessionAuth)),

    #{<<"token">> := Token} = json_utils:decode(TokenJson),
    {token, Token}.

