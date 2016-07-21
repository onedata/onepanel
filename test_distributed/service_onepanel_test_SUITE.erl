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
-module(service_onepanel_test_SUITE).
-author("Krzysztof Trzepla").

-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% tests
-export([
    nodes_should_discover_each_other/1,
    deploy_should_create_cluster/1,
    join_should_add_node/1,
    join_should_work_after_leave/1,
    sequential_join_should_create_cluster/1,
    leave_should_remove_node/1
]).

all() ->
    ?ALL([
        nodes_should_discover_each_other,
        deploy_should_create_cluster,
        join_should_add_node,
        join_should_work_after_leave,
        sequential_join_should_create_cluster,
        leave_should_remove_node
    ]).

-define(SERVICE, service_onepanel:name()).

%%%===================================================================
%%% Test functions
%%%===================================================================

nodes_should_discover_each_other(Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    ExpectedNodes = lists:sort(Nodes),

    lists:foreach(fun(Node) ->
        ?assertEqual(ExpectedNodes,
            lists:sort(rpc:call(Node, onepanel_discovery, get_nodes, [])), 30)
    end, Nodes).


deploy_should_create_cluster(Config) ->
    [Node | _] = Nodes = ?config(onepanel_nodes, Config),
    Hosts = lists:sort(onepanel_cluster:nodes_to_hosts(Nodes)),

    ?assertEqual(ok, rpc:call(Node, service, apply,
        [?SERVICE, deploy, #{hosts => Hosts}])),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node, service_onepanel, get_hosts, []))).


join_should_add_node(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1, Host2] = Hosts = lists:sort(onepanel_cluster:nodes_to_hosts(Nodes)),

    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, deploy, #{hosts => [Host1]}])),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, #{cluster_hosts => [Host1], hosts => [Host2]}])),

    ?assertEqual(Hosts, lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))).


join_should_work_after_leave(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1, Host2] = lists:sort(onepanel_cluster:nodes_to_hosts(Nodes)),

    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, deploy, #{hosts => [Host1]}])),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, #{cluster_hosts => [Host1], hosts => [Host2]}])),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, leave_cluster, #{hosts => [Host2]}])),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, #{cluster_hosts => [Host1], hosts => [Host2]}])).


sequential_join_should_create_cluster(Config) ->
    [Node1, Node2, Node3, Node4, Node5 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2, Node3, Node4, Node5],
    [Host1, Host2, Host3, Host4, Host5] = Hosts =
        lists:sort(onepanel_cluster:nodes_to_hosts(Nodes)),

    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, deploy, #{hosts => [Host1]}])),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, #{cluster_hosts => [Host1], hosts => [Host2]}])),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, #{cluster_hosts => [Host2], hosts => [Host3]}])),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, #{cluster_hosts => [Host3], hosts => [Host4]}])),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, #{cluster_hosts => [Host4], hosts => [Host5]}])),

    lists:foreach(fun(Node) ->
        ?assertEqual(Hosts, lists:sort(rpc:call(Node, service_onepanel,
            get_hosts, [])))
    end, Nodes).


leave_should_remove_node(Config) ->
    [Node1, Node2, Node3 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2, Node3],
    [Host1, Host2, Host3] = lists:sort(onepanel_cluster:nodes_to_hosts(Nodes)),
    Hosts = [Host2, Host3],

    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, deploy, #{hosts => [Host1 | Hosts]}])),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, leave_cluster, #{hosts => [Host1]}])),

    ?assertEqual([Host1], lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node3, service_onepanel, get_hosts, []))).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    onepanel_test_utils:init(NewConfig).


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).