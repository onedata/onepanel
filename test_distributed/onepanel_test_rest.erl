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

-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([auth_request/4, auth_request/5, auth_request/6, auth_request/7,
    noauth_request/3, noauth_request/4, noauth_request/5, noauth_request/6]).
-export([assert_body_fields/2, assert_body_values/2, assert_body/2]).

-type config() :: string() | proplists:proplist().
-type endpoint() :: http_client:url().
-type method() :: http_client:method().
-type auth() :: {Username :: binary(), Password :: binary()} |
                {cookie, Cookie :: binary()}.
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
%% @doc Executes noauth_request/6 with basic authorization header.
%% @end
%%--------------------------------------------------------------------
-spec auth_request(HostOrConfig :: config(), Port :: integer(), Endpoint :: endpoint(),
    Method :: method(), Auth :: auth(), Headers :: headers(), Body :: body()) ->
    Response :: response().
auth_request(HostOrConfig, Port, Endpoint, Method, {cookie, SessionId}, Headers,
    Body) ->
    NewHeaders = [
        {<<"cookie">>, <<"sessionId=", SessionId/binary>>} |
        Headers
    ],
    noauth_request(HostOrConfig, Port, Endpoint, Method, NewHeaders, Body);
auth_request(HostOrConfig, Port, Endpoint, Method, {Username, Password}, Headers,
    Body) ->
    NewHeaders = [
        onepanel_utils:get_basic_auth_header(Username, Password) |
        Headers
    ],
    noauth_request(HostOrConfig, Port, Endpoint, Method, NewHeaders, Body).


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
noauth_request(HostOrConfig, Port, Endpoint, Method, Headers, Body) ->
    Host = case io_lib:printable_unicode_list(HostOrConfig) of
        true -> HostOrConfig;
        false -> utils:random_element(?config(onepanel_hosts, HostOrConfig))
    end,
    NewHeaders = [
        {<<"content-type">>, <<"application/json">>} |
        Headers
    ],
    Prefix = "/api/v3/onepanel",
    Url = onepanel_utils:join(["https://", Host, ":", Port, Prefix, Endpoint]),
    JsonBody = json_utils:encode(Body),
    http_client:request(Method, Url, maps:from_list(NewHeaders), JsonBody, [{ssl_options, [{secure, false}]}]).


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