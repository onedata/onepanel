%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of onepanel service.
%%% @end
%%%--------------------------------------------------------------------
-module(service_onepanel_test_SUITE).
-author("Krzysztof Trzepla").

-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

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
-define(COOKIE, test_cookie).

%%%===================================================================
%%% Test functions
%%%===================================================================

nodes_should_discover_each_other(Config) ->
    Nodes = ?config(onepanel_nodes, Config),

    lists:foreach(fun(Node) ->
        ?assertMatch({true, _}, lists:foldl(fun
            (_ExpectedNode, {false, ActualNodes}) ->
                {false, ActualNodes};
            (ExpectedNode, {true, ActualNodes}) ->
                {lists:member(ExpectedNode, ActualNodes), ActualNodes}
        end, {true, rpc:call(Node, onepanel_discovery, get_nodes, [])}, Nodes), 30)
    end, Nodes).


deploy_should_create_cluster(Config) ->
    [Node | _] = Nodes = ?config(onepanel_nodes, Config),
    Hosts = lists:sort(onepanel_cluster:nodes_to_hosts(Nodes)),

    ?assertEqual(ok, rpc:call(Node, service, apply,
        [?SERVICE, deploy, #{cookie => ?COOKIE, hosts => Hosts}]
    )),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node, service_onepanel, get_hosts, []))).


join_should_add_node(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1, Host2] = Hosts = lists:sort(onepanel_cluster:nodes_to_hosts(Nodes)),

    Ctx = #{cookie => ?COOKIE},
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, deploy, Ctx#{hosts => [Host1]}]
    )),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, Ctx#{cluster_host => Host1, hosts => [Host2]}]
    )),

    ?assertEqual(Hosts, lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))).


join_should_work_after_leave(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1, Host2] = lists:sort(onepanel_cluster:nodes_to_hosts(Nodes)),

    Ctx = #{cookie => ?COOKIE},
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, deploy, Ctx#{hosts => [Host1]}]
    )),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, Ctx#{cluster_host => Host1, hosts => [Host2]}]
    )),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, leave_cluster, #{hosts => [Host2]}]
    )),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, Ctx#{cluster_host => Host1, hosts => [Host2]}]
    )).


sequential_join_should_create_cluster(Config) ->
    [Node1, Node2, Node3, Node4, Node5 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2, Node3, Node4, Node5],
    [Host1, Host2, Host3, Host4, Host5] = Hosts =
        lists:sort(onepanel_cluster:nodes_to_hosts(Nodes)),

    Ctx = #{cookie => ?COOKIE},
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, deploy, Ctx#{hosts => [Host1]}])),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, Ctx#{cluster_host => Host1, hosts => [Host2]}]
    )),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, Ctx#{cluster_host => Host2, hosts => [Host3]}]
    )),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, Ctx#{cluster_host => Host3, hosts => [Host4]}]
    )),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, join_cluster, Ctx#{cluster_host => Host4, hosts => [Host5]}]
    )),

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
        [?SERVICE, deploy, #{cookie => ?COOKIE, hosts => [Host1 | Hosts]}]
    )),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE, leave_cluster, #{hosts => [Host1]}]
    )),

    ?assertEqual([], lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node3, service_onepanel, get_hosts, []))).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_testcase(_Case, Config) ->
    onepanel_test_utils:ensure_started(
        ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json"))).


end_per_testcase(_Case, Config) ->
    test_node_starter:clean_environment(Config).
