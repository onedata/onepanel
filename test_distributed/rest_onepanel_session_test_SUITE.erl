%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of 'onepanel_session' module.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_onepanel_session_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    post_should_return_unauthorized_error/1,
    post_should_return_forbidden_error/1,
    post_should_create_session/1,
    get_should_return_session/1,
    get_should_return_not_found_error/1,
    delete_should_remove_session/1,
    delete_should_return_not_found_error/1,
    session_should_authenticate_user/1,
    session_should_not_authenticate_user/1,
    session_should_expire/1
]).

-define(ADMIN_USER_NAME, <<"admin1">>).
-define(ADMIN_USER_PASSWORD, <<"Admin1Password">>).
-define(REG_USER_NAME, <<"user1">>).
-define(REG_USER_PASSWORD, <<"User1Password">>).

all() ->
    ?ALL([
        post_should_return_unauthorized_error,
        post_should_return_forbidden_error,
        post_should_create_session,
        get_should_return_session,
        get_should_return_not_found_error,
        delete_should_remove_session,
        delete_should_return_not_found_error,
        session_should_authenticate_user,
        session_should_not_authenticate_user,
        session_should_expire
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

post_should_return_unauthorized_error(Config) ->
    ?assertMatch({ok, 401, _, _}, onepanel_test_rest:noauth_request(
        Config, <<"/session">>, post
    )),
    lists:foreach(fun(Username) ->
        ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/session">>, post,
            {Username, <<"somePassword">>}
        ))
    end, [<<"someUser">>, ?ADMIN_USER_NAME, ?REG_USER_NAME]).


post_should_return_forbidden_error(Config) ->
    ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
        Config, <<"/session">>, post,
        {?REG_USER_NAME, ?REG_USER_PASSWORD}
    )).


post_should_create_session(Config) ->
    {ok, 200, Headers, JsonBody} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/session">>, post,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
        )
    ),
    onepanel_test_utils:assert_fields(Headers, [<<"set-cookie">>]),
    onepanel_test_rest:assert_body_fields(JsonBody, [
        <<"sessionId">>, <<"username">>
    ]).


get_should_return_session(Config) ->
    {ok, SessionId} = ?assertMatch({ok, _},
        ?call(Config, onepanel_session, create, [?ADMIN_USER_NAME])),
    {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/session">>, get, {cookie, SessionId}
        )
    ),
    onepanel_test_rest:assert_body_fields(JsonBody,
        [<<"sessionId">>, <<"username">>]).


get_should_return_not_found_error(Config) ->
    ?assertMatch({ok, 404, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/session">>, get,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
        )
    ).


delete_should_remove_session(Config) ->
    {ok, SessionId} = ?assertMatch({ok, _},
        ?call(Config, onepanel_session, create, [?ADMIN_USER_NAME])),
    ?assertMatch({ok, 204, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/session">>, delete, {cookie, SessionId}
        )
    ),
    ?assertMatch(#error{reason = ?ERR_NOT_FOUND},
        ?call(Config, onepanel_session, get, [SessionId])).


delete_should_return_not_found_error(Config) ->
    ?assertMatch({ok, 404, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/session">>, delete,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
        )
    ).


session_should_authenticate_user(Config) ->
    {ok, SessionId} = ?assertMatch({ok, _},
        ?call(Config, onepanel_session, create, [?ADMIN_USER_NAME])),
    ?assertMatch({ok, #onepanel_user{username = ?ADMIN_USER_NAME}},
        ?call(Config, onepanel_user, authenticate, [SessionId])),
    ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/users/", ?ADMIN_USER_NAME/binary>>, get,
            {cookie, SessionId}
        )
    ).


session_should_not_authenticate_user(Config) ->
    ?assertMatch(#error{},
        ?call(Config, onepanel_user, authenticate, [<<"someSessionId">>])).


session_should_expire(Config) ->
    onepanel_test_utils:set_test_envs(?config(all_nodes, Config), [
        {session_ttl, 1000}
    ]),
    {ok, SessionId} = ?assertMatch({ok, _},
        ?call(Config, onepanel_session, create, [?ADMIN_USER_NAME])),
    ?assertMatch({ok, #onepanel_user{username = ?ADMIN_USER_NAME}},
        ?call(Config, onepanel_user, authenticate, [SessionId])),
    ?assertMatch(#error{reason = ?ERR_NOT_FOUND},
        ?call(Config, onepanel_user, authenticate, [SessionId]), 3).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) -> onepanel_test_utils:init(NewConfig) end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(_Case, Config) ->
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD, admin])),
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?REG_USER_NAME, ?REG_USER_PASSWORD, regular])),
    Config.


end_per_testcase(_Case, Config) ->
    ?call(Config, model, clear, [onepanel_user]),
    ?call(Config, model, clear, [onepanel_session]).

end_per_suite(_Config) ->
    ok.