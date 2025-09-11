%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of user management endpoints.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_onepanel_user_test_SUITE).
-author("Krzysztof Trzepla").

-include("names.hrl").
-include("modules/errors.hrl").
-include("onepanel_test_utils.hrl").
-include("onepanel_test_rest.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").
-include_lib("ctool/include/http/codes.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    end_per_suite/1]).

%% tests
-export([
    method_should_return_forbidden_error/1,
    get_current_user_as_oz_user_should_return_privileges/1,
    get_current_user_as_root_should_fail/1,
    get_should_list_oz_users/1
]).

all() ->
    ?ALL([
        method_should_return_forbidden_error,
        get_current_user_as_oz_user_should_return_privileges,
        get_current_user_as_root_should_fail,
        get_should_list_oz_users
    ]).

-define(USER_ID1, <<"joeId">>).
-define(USER_ID2, <<"kyleId">>).


%%%===================================================================
%%% Test functions
%%%===================================================================


method_should_return_forbidden_error(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        lists:foreach(fun(Auth) ->
            ?assertMatch({ok, ?HTTP_403_FORBIDDEN, _, _}, onepanel_test_rest:auth_request(
                Host, Endpoint, Method, Auth
            ))
        end, ?PEER_AUTHS(Host))
    end, [{<<"/user">>, get}]).


get_current_user_as_oz_user_should_return_privileges(Config) ->
    lists:foreach(fun(PrivilegeSet) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Config, <<"/user">>, get,
                ?OZ_AUTHS(Config, PrivilegeSet)
            )
        ),
        onepanel_test_rest:assert_body_fields(JsonBody,
            [<<"username">>, <<"userId">>, <<"clusterPrivileges">>]),

        Expected = lists:sort([atom_to_binary(P, utf8) || P <- PrivilegeSet]),
        #{<<"clusterPrivileges">> := ReturnedPrivileges} = json_utils:decode(JsonBody),
        ?assertEqual(Expected, lists:sort(ReturnedPrivileges))
    end, [
        [],
        [?CLUSTER_VIEW],
        [?CLUSTER_UPDATE, ?CLUSTER_VIEW],
        privileges:cluster_admin()
    ]).


get_current_user_as_root_should_fail(Config) ->
    ?assertMatch({ok, ?HTTP_404_NOT_FOUND, _, _}, onepanel_test_rest:auth_request(
        Config, <<"/user">>, get, ?ROOT_AUTHS(Config)
    )).


get_should_list_oz_users(Config) ->
    Expected = lists:sort(?config(oz_user_ids, Config)),

    {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/zone/users">>, get,
            ?OZ_OR_ROOT_AUTHS(Config, [])
        )
    ),
    onepanel_test_rest:assert_body_fields(JsonBody, [<<"ids">>]),
    #{<<"ids">> := ReturnedIds} = json_utils:decode(JsonBody),
    ?assertEqual(Expected, lists:sort(ReturnedIds)).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    Posthook = fun(NewConfig) -> onepanel_test_utils:init(NewConfig) end,
    [{?LOAD_MODULES, [onepanel_test_rest]}, {?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(get_should_list_oz_users, Config) ->
    Config2 = init_per_testcase(default, Config),
    Nodes = ?config(onepanel_nodes, Config),
    UserIds = [?USER_ID1, ?USER_ID2],
    test_utils:mock_new(Nodes, [onezone_users]),
    test_utils:mock_expect(Nodes, rpc, call, fun
        (_, rpc_api, apply, [list_users, [?ROOT]]) -> {ok, UserIds};
        (Node, M, F, A) -> meck:passthrough([Node, M, F, A])
    end),
    [{oz_user_ids, UserIds} | Config2];

init_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    onepanel_test_rest:set_default_passphrase(Config),
    onepanel_test_rest:mock_token_authentication(Config),
    test_utils:mock_new(Nodes, [service, service_onezone, service_oz_worker, rpc],
        [passthrough, unstick]),
    test_utils:mock_expect(Nodes, service, is_healthy, fun(_) -> true end),
    test_utils:mock_expect(Nodes, service, all_healthy_ignoring_ones3, fun() -> true end),
    test_utils:mock_expect(Nodes, service_onezone, get_hosts,
        fun() -> ?config(onepanel_hosts, Config) end),
    test_utils:mock_expect(Nodes, service_oz_worker, get_hosts,
        fun() -> ?config(onepanel_hosts, Config) end),
    Config.


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes),
    ?call(Config, model, clear, [onepanel_user]).


end_per_suite(_Config) ->
    ok.
