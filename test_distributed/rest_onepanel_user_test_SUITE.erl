%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of 'rest_onepanel_user' module.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_onepanel_user_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    end_per_suite/1]).

%% tests
-export([
    method_should_return_not_found_error/1,
    method_should_return_unauthorized_error/1,
    get_as_admin_should_return_any_account/1,
    get_as_regular_should_return_only_own_account/1,
    patch_as_admin_should_change_any_account_password/1,
    patch_as_regular_should_change_only_own_account_password/1,
    patch_as_regular_should_check_current_password/1,
    post_noauth_should_create_account_when_admin_missing/1,
    post_as_regular_should_create_account_when_admin_missing/1,
    post_noauth_should_not_create_account_when_admin_present/1,
    post_as_regular_should_not_create_account_when_admin_present/1,
    post_as_admin_should_create_account/1,
    delete_as_regular_should_remove_only_own_account/1,
    delete_as_admin_should_remove_any_account/1,
    current_user_is_resolved/1
]).

-define(ADMIN_USER1_NAME, <<"admin1">>).
-define(ADMIN_USER1_PASSWORD, <<"Admin1Password">>).
-define(ADMIN_USER2_NAME, <<"admin2">>).
-define(ADMIN_USER2_PASSWORD, <<"Admin2Password">>).
-define(REG_USER1_NAME, <<"user1">>).
-define(REG_USER1_PASSWORD, <<"User1Password">>).
-define(REG_USER2_NAME, <<"user2">>).
-define(REG_USER2_PASSWORD, <<"User2Password">>).

all() ->
    ?ALL([
        method_should_return_not_found_error,
        method_should_return_unauthorized_error,
        get_as_admin_should_return_any_account,
        get_as_regular_should_return_only_own_account,
        patch_as_admin_should_change_any_account_password,
        patch_as_regular_should_change_only_own_account_password,
        patch_as_regular_should_check_current_password,
        post_noauth_should_create_account_when_admin_missing,
        post_as_regular_should_create_account_when_admin_missing,
        post_noauth_should_not_create_account_when_admin_present,
        post_as_regular_should_not_create_account_when_admin_present,
        post_as_admin_should_create_account,
        delete_as_regular_should_remove_only_own_account,
        delete_as_admin_should_remove_any_account,
        user_should_alias_users
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_not_found_error(Config) ->
    lists:foreach(fun({Method, Body}) ->
        ?assertMatch({ok, 404, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/someUser">>, Method, {?ADMIN_USER1_NAME,
                ?ADMIN_USER1_PASSWORD}, Body
        ))
    end, [{get, []}, {patch, #{password => <<"SomePassword1">>}}, {delete, #{}}]).


method_should_return_unauthorized_error(Config) ->
    lists:foreach(fun({Method, Body}) ->
        ?assertMatch({ok, 401, _, _}, onepanel_test_rest:noauth_request(
            Config, <<"/users/", ?REG_USER1_NAME/binary>>, Method, Body
        )),
        ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", ?REG_USER1_NAME/binary>>, Method,
            {<<"someUser">>, <<"somePassword">>}, Body
        ))
    end, [{get, []}, {patch, #{password => <<"SomePassword1">>}}]).


get_as_admin_should_return_any_account(Config) ->
    lists:foreach(fun(Username) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Config, <<"/users/", Username/binary>>, get,
                {?ADMIN_USER1_NAME, ?ADMIN_USER1_PASSWORD}
            )
        ),
        onepanel_test_rest:assert_body_fields(JsonBody,
            [<<"userId">>, <<"userRole">>])
    end, [
        ?ADMIN_USER1_NAME, ?ADMIN_USER2_NAME,
        ?REG_USER1_NAME, ?REG_USER2_NAME
    ]).


get_as_regular_should_return_only_own_account(Config) ->
    {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/users/", ?REG_USER1_NAME/binary>>,
            get, {?REG_USER1_NAME, ?REG_USER1_PASSWORD}
        )
    ),
    onepanel_test_rest:assert_body_fields(JsonBody,
        [<<"userId">>, <<"userRole">>]),


    lists:foreach(fun(Username) ->
        ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>,
            get, {?REG_USER1_NAME, ?REG_USER1_PASSWORD}
        ))
    end, [?ADMIN_USER1_NAME, ?ADMIN_USER2_NAME, ?REG_USER2_NAME]).


patch_as_admin_should_change_any_account_password(Config) ->
    lists:foreach(fun({Username, Password}) ->
        NewPassword = <<Password/binary, "New">>,
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, patch, {?ADMIN_USER1_NAME,
                ?ADMIN_USER1_PASSWORD}, #{
                currentPassword => ?ADMIN_USER1_PASSWORD,
                newPassword => NewPassword
            }
        )),
        ?assertMatch({ok, 200, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, get, {Username, NewPassword}
        )),
        ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, get, {Username, Password}
        ))
    end, [
        {?ADMIN_USER2_NAME, ?ADMIN_USER2_PASSWORD},
        {?REG_USER1_NAME, ?REG_USER1_PASSWORD},
        {?REG_USER2_NAME, ?REG_USER2_PASSWORD}
    ]),
    NewAdmin1Password = <<(?ADMIN_USER1_PASSWORD)/binary, "New">>,

    ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
        Config, <<"/user/">>, patch, {?ADMIN_USER1_NAME,
            ?ADMIN_USER1_PASSWORD}, #{
            currentPassword => ?ADMIN_USER1_PASSWORD,
            newPassword => NewAdmin1Password
        }
    )),
    ?assertMatch({ok, 200, _, _}, onepanel_test_rest:auth_request(
        Config, <<"/users/", ?ADMIN_USER1_NAME/binary>>, get,
        {?ADMIN_USER1_NAME, NewAdmin1Password}
    )),
    ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
        Config, <<"/users/", ?ADMIN_USER1_NAME/binary>>, get,
        {?ADMIN_USER1_NAME, ?ADMIN_USER1_PASSWORD}
    )).


patch_as_regular_should_change_only_own_account_password(Config) ->
    lists:foreach(fun(Username) ->
        ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, patch, {?REG_USER1_NAME,
                ?REG_USER1_PASSWORD}, #{
                currentPassword => ?REG_USER1_PASSWORD,
                newPassword => <<"SomePassword1">>
            }
        ))
    end, [?ADMIN_USER1_NAME, ?ADMIN_USER2_NAME, ?REG_USER2_NAME]),

    lists:foreach(fun({Username, Password}) ->
        NewPassword = <<Password/binary, "New">>,
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, patch, {Username,
                Password}, #{
                currentPassword => Password,
                newPassword => NewPassword
            }
        )),
        ?assertMatch({ok, 200, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, get, {Username, NewPassword}
        )),
        ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, get, {Username, Password}
        ))
    end, [
        {?REG_USER1_NAME, ?REG_USER1_PASSWORD},
        {?REG_USER2_NAME, ?REG_USER2_PASSWORD}
    ]).


patch_as_regular_should_check_current_password(Config) ->
    lists:foreach(fun({Username, Password}) ->
        NewPassword = <<Password/binary, "New">>,
        ?assertMatch({ok, 400, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, patch, {Username,
                Password}, #{
                currentPassword => NewPassword,
                newPassword => NewPassword
            }
        ))
    end, [
        {?REG_USER1_NAME, ?REG_USER1_PASSWORD},
        {?REG_USER2_NAME, ?REG_USER2_PASSWORD}
    ]).


post_noauth_should_create_account_when_admin_missing(Config) ->
    ?assertMatch({ok, 204, _, _}, onepanel_test_rest:noauth_request(
        Config, <<"/users">>, post, #{
            username => ?REG_USER1_NAME, password => ?REG_USER1_PASSWORD,
            userRole => regular
        }
    )).


post_as_regular_should_create_account_when_admin_missing(Config) ->
    ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
        Config, <<"/users">>, post, {?REG_USER1_NAME, ?REG_USER1_PASSWORD}, #{
            username => ?REG_USER2_NAME, password => ?REG_USER2_PASSWORD,
            userRole => regular
        }
    )).


post_noauth_should_not_create_account_when_admin_present(Config) ->
    ?assertMatch({ok, 403, _, _}, onepanel_test_rest:noauth_request(
        Config, <<"/users">>, post, #{
            username => ?REG_USER1_NAME, password => ?REG_USER1_PASSWORD,
            userRole => regular
        }
    )).


post_as_regular_should_not_create_account_when_admin_present(Config) ->
    ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
        Config, <<"/users">>, post, {?REG_USER1_NAME, ?REG_USER1_PASSWORD}, #{
            username => ?REG_USER1_NAME, password => ?REG_USER1_PASSWORD,
            userRole => regular
        }
    )).


post_as_admin_should_create_account(Config) ->
    lists:foreach(fun(Body) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users">>, post, {?ADMIN_USER1_NAME, ?ADMIN_USER1_PASSWORD},
            Body
        ))
    end, [
        #{username => ?REG_USER1_NAME, password => ?REG_USER1_PASSWORD,
            userRole => regular},
        #{username => ?REG_USER2_NAME, password => ?REG_USER2_PASSWORD,
            userRole => regular},
        #{username => ?ADMIN_USER2_NAME, password => ?ADMIN_USER2_PASSWORD,
            userRole => admin}
    ]).


delete_as_regular_should_remove_only_own_account(Config) ->
    lists:foreach(fun(Username) ->
        ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, delete, {?REG_USER1_NAME,
                ?REG_USER1_PASSWORD}
        ))
    end, [?ADMIN_USER1_NAME, ?ADMIN_USER2_NAME, ?REG_USER2_NAME]),

    lists:foreach(fun({Username, Password}) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, delete, {Username,
                Password}
        )),
        ?assertMatch({ok, 404, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, get, {?ADMIN_USER1_NAME,
                ?ADMIN_USER1_PASSWORD}
        ))
    end, [
        {?REG_USER1_NAME, ?REG_USER1_PASSWORD},
        {?REG_USER2_NAME, ?REG_USER2_PASSWORD}
    ]).


delete_as_admin_should_remove_any_account(Config) ->
    lists:foreach(fun(Username) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, delete, {?ADMIN_USER1_NAME,
                ?ADMIN_USER1_PASSWORD}
        )),
        ?assertMatch({ok, 404, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/users/", Username/binary>>, get, {?ADMIN_USER1_NAME,
                ?ADMIN_USER1_PASSWORD}
        ))
    end, [?ADMIN_USER2_NAME, ?REG_USER1_NAME, ?REG_USER2_NAME]),

    ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
        Config, <<"/users/", ?ADMIN_USER1_NAME/binary>>, delete,
        {?ADMIN_USER1_NAME, ?ADMIN_USER1_PASSWORD}
    )),
    ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
        Config, <<"/users/", ?ADMIN_USER1_NAME/binary>>, get,
        {?ADMIN_USER1_NAME, ?ADMIN_USER1_PASSWORD}
    )).

current_user_is_resolved(Config) ->
    {_, _, _, JsonBodyExplicitName} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/users/", ?REG_USER1_NAME/binary>>,
            get, {?REG_USER1_NAME, ?REG_USER1_PASSWORD}
        )
    ),
    {_, _, _, JsonBodyCurrentUser} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/user">>,
            get, {?REG_USER1_NAME, ?REG_USER1_PASSWORD}
        )
    ),
    ?assertEqual(JsonBodyExplicitName, JsonBodyCurrentUser),
    ok.

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) -> onepanel_test_utils:init(NewConfig) end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].

init_per_testcase(post_noauth_should_create_account_when_admin_missing, Config) ->
    Config;

init_per_testcase(post_as_regular_should_create_account_when_admin_missing, Config) ->
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?REG_USER1_NAME, ?REG_USER1_PASSWORD, regular])),
    Config;

init_per_testcase(post_noauth_should_not_create_account_when_admin_present, Config) ->
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?ADMIN_USER1_NAME, ?ADMIN_USER1_PASSWORD, admin])),
    Config;

init_per_testcase(post_as_regular_should_not_create_account_when_admin_present, Config) ->
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?REG_USER1_NAME, ?REG_USER1_PASSWORD, regular])),
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?ADMIN_USER1_NAME, ?ADMIN_USER1_PASSWORD, admin])),
    Config;

init_per_testcase(post_as_admin_should_create_account, Config) ->
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?ADMIN_USER1_NAME, ?ADMIN_USER1_PASSWORD, admin])),
    Config;

init_per_testcase(_Case, Config) ->
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?ADMIN_USER1_NAME, ?ADMIN_USER1_PASSWORD, admin])),
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?ADMIN_USER2_NAME, ?ADMIN_USER2_PASSWORD, admin])),
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?REG_USER1_NAME, ?REG_USER1_PASSWORD, regular])),
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?REG_USER2_NAME, ?REG_USER2_PASSWORD, regular])),
    Config.


end_per_testcase(_Case, Config) ->
    ?call(Config, model, clear, [onepanel_user]).

end_per_suite(_Config) ->
    ok.
