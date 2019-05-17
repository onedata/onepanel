%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of onezone service.
%%% @end
%%%--------------------------------------------------------------------
-module(service_onezone_test_SUITE).
-author("Krzysztof Trzepla").

-include("names.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_testcase/2, end_per_testcase/2,
    init_per_suite/1, end_per_suite/1]).

%% tests
-export([
    users_should_be_migrated_and_removed/1
]).

all() ->
    ?ALL([
        users_should_be_migrated_and_removed
    ]).

-define(USERNAME1, <<"joe">>).
-define(USERNAME2, <<"kyle">>).
-define(PASSWORD1, <<"onePassword">>).
-define(PASSWORD2, <<"otherpassword">>).
-define(ROLE1, admin).
-define(ROLE2, regular).

%%%===================================================================
%%% Test functions
%%%===================================================================


users_should_be_migrated_and_removed(Config) ->
    Users = ?config(users, Config),
    [Node | _] = ?config(all_nodes, Config),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [?SERVICE_OZ, migrate_users, #{}]
    )),

    lists:foreach(fun
        (#onepanel_user{username = Name, password_hash = PassHash, role = Role}) ->
            test_utils:mock_assert_num_calls(
                ?config(all_nodes, Config), rpc, call,
                ['_', basic_auth, migrate_onepanel_user_to_onezone, [Name, PassHash, Role]],
                1)
    end, Users),
    ?assertEqual([], ?call(Config, onepanel_user, list, [])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    Posthook = fun onepanel_test_utils:init/1,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].

init_per_testcase(users_should_be_migrated_and_removed, Config) ->
    Users = [
        add_user(Config, ?USERNAME1, ?PASSWORD1, ?ROLE1),
        add_user(Config, ?USERNAME2, ?PASSWORD2, ?ROLE2)
    ],
    init_per_testcase(default, [{users, Users} | Config]);

init_per_testcase(_Case, Config) ->
    Hosts = ?config(onezone_hosts, Config),
    [Node | _] = Nodes = ?config(onezone_nodes, Config),


    ?assertEqual(ok,
        rpc:call(Node, service, save, [#service{name = ?SERVICE_OZW, hosts = Hosts}])),
    ?assertEqual(ok,
        rpc:call(Node, service, save, [#service{name = ?SERVICE_OZ, hosts = []}])),

    test_utils:mock_new(Nodes, [rpc], [passthrough, unstick]),
    test_utils:mock_expect(Nodes, rpc, call, fun
        (_Node, basic_auth, migrate_onepanel_user_to_onezone,
            [_Username, _PassHash, _Role]) ->
            {ok, <<"userId">>};
        (Node, Module, Function, Args) ->
            meck:passthrough([Node, Module, Function, Args])
    end),
    Config.


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes).

end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Helper functions
%%%===================================================================

-spec add_user(Config :: proplists:proplist(), Username :: binary(),
    Password :: binary(), Role :: admin | regular) ->
    #onepanel_user{}.
add_user(Config, Username, Password, Role) ->
    Record = #onepanel_user{
        username = Username, role = Role, uuid = onepanel_utils:gen_uuid(),
        password_hash = onedata_passwords:create_hash(Password)
    },
    ?assertEqual(ok, ?call(Config, onepanel_user, save, [Record])),
    Record.
