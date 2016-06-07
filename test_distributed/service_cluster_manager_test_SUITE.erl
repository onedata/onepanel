%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(service_cluster_manager_test_SUITE).
-author("Krzysztof Trzepla").

-include("db/models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% tests
-export([
    deploy_should_create_cluster/1,
    stop_should_deactivate_service/1,
    start_should_activate_service/1,
    restart_should_reactivate_service/1
]).

all() ->
    ?ALL([
        deploy_should_create_cluster,
        stop_should_deactivate_service,
        start_should_activate_service,
        restart_should_reactivate_service
    ]).

-define(SERVICE, cluster_manager).
-define(ATTEMPTS, 30).
-define(TIMEOUT, timer:seconds(10)).

%%%===================================================================
%%% Test functions
%%%===================================================================

deploy_should_create_cluster(Config) ->
    [Node | _] = Nodes = ?config(onepanel_nodes, Config),
    Hosts = onepanel_utils:nodes_to_hosts(Nodes),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [?SERVICE, deploy, #{hosts => Hosts, worker_num => 0}])),
    {ok, #service{hosts = ServiceHosts, params = Params}} =
        ?assertMatch({ok, _}, rpc:call(Node, service, get, [?SERVICE])),
    ?assertEqual(lists:usort(Hosts), lists:usort(ServiceHosts)),
    ?assertEqual({ok, hd(Hosts)}, maps:find(main_host, Params)).


stop_should_deactivate_service(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),

    ?assertEqual(ok, rpc:call(Node, service, apply, [?SERVICE, stop, #{}])),
    ?assertMatch({error, {service_cluster_manager, status, {errors, _}}},
        rpc:call(Node, service, apply, [?SERVICE, status, #{}])).


start_should_activate_service(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),

    ?assertEqual(ok, rpc:call(Node, service, apply, [?SERVICE, start, #{}])),
    ?assertEqual(ok, rpc:call(Node, service, apply, [?SERVICE, status, #{}]),
        ?ATTEMPTS).


restart_should_reactivate_service(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),

    ?assertEqual(ok, rpc:call(Node, service, apply, [?SERVICE, restart, #{}])),
    ?assertEqual(ok, rpc:call(Node, service, apply, [?SERVICE, status, #{}]),
        ?ATTEMPTS).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    onepanel_test_utils:ensure_initailized(NewConfig).


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).