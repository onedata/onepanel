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
-module(onepanel_cluster_test_SUITE).
-author("Krzysztof Trzepla").

-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% tests
-export([
    nodes_should_discover_each_other/1,
    init_should_create_cluster/1,
    join_should_add_node/1,
    join_should_work_after_leave/1,
    sequential_join_should_create_cluster/1,
    leave_should_remove_node/1,
    clear_should_remove_node/1
]).

all() ->
    ?ALL([
        nodes_should_discover_each_other,
        init_should_create_cluster,
        join_should_add_node,
        join_should_work_after_leave,
        sequential_join_should_create_cluster,
        leave_should_remove_node,
        clear_should_remove_node
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

nodes_should_discover_each_other(Config) ->
    Nodes = ?config(onepanel_nodes, Config),

    lists:foreach(fun(Node) ->
        ExpectedNodes = lists:delete(Node, Nodes),
        ?assertEqual(lists:sort(ExpectedNodes),
            lists:sort(rpc:call(Node, onepanel_discovery, get_nodes, [])), 30)
    end, Nodes).


init_should_create_cluster(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),

    ?assertEqual(ok, rpc:call(Node, onepanel_cluster, init, [])),
    ?assertEqual([Node], rpc:call(Node, onepanel_cluster, get_nodes, [])).


join_should_add_node(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = lists:sort([Node1, Node2]),

    ?assertEqual(ok, rpc:call(Node1, onepanel_cluster, init, [])),
    ?assertEqual(ok, rpc:call(Node2, onepanel_cluster, join, [Node1])),

    ?assertEqual(Nodes,
        lists:sort(rpc:call(Node1, onepanel_cluster, get_nodes, []))),
    ?assertEqual(Nodes,
        lists:sort(rpc:call(Node2, onepanel_cluster, get_nodes, []))).


join_should_work_after_leave(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),

    ?assertEqual(ok, rpc:call(Node1, onepanel_cluster, init, [])),
    ?assertEqual(ok, rpc:call(Node2, onepanel_cluster, join, [Node1])),
    ?assertEqual(ok, rpc:call(Node2, onepanel_cluster, leave, [Node1])),
    ?assertEqual(ok, rpc:call(Node2, onepanel_cluster, join, [Node1])).


sequential_join_should_create_cluster(Config) ->
    [Node1, Node2, Node3, Node4, Node5 | _] = ?config(onepanel_nodes, Config),
    Nodes = lists:sort([Node1, Node2, Node3, Node4, Node5]),

    ?assertEqual(ok, rpc:call(Node1, onepanel_cluster, init, [])),
    ?assertEqual(ok, rpc:call(Node2, onepanel_cluster, join, [Node1])),
    ?assertEqual(ok, rpc:call(Node3, onepanel_cluster, join, [Node2])),
    ?assertEqual(ok, rpc:call(Node4, onepanel_cluster, join, [Node3])),
    ?assertEqual(ok, rpc:call(Node5, onepanel_cluster, join, [Node4])),

    lists:foreach(fun(Node) ->
        ?assertEqual(Nodes,
            lists:sort(rpc:call(Node, onepanel_cluster, get_nodes, [])))
    end, Nodes).


leave_should_remove_node(Config) ->
    [Node1, Node2, Node3 | _] = ?config(onepanel_nodes, Config),
    Nodes = lists:sort([Node2, Node3]),

    rpc:call(Node1, onepanel_cluster, init, []),
    rpc:call(Node2, onepanel_cluster, join, [Node1]),
    rpc:call(Node3, onepanel_cluster, join, [Node1]),

    ?assertEqual(ok, rpc:call(Node1, onepanel_cluster, leave, [Node3])),
    ?assertEqual([Node1], rpc:call(Node1, onepanel_cluster, get_nodes, [])),
    ?assertEqual(Nodes,
        lists:sort(rpc:call(Node2, onepanel_cluster, get_nodes, []))),
    ?assertEqual(Nodes,
        lists:sort(rpc:call(Node3, onepanel_cluster, get_nodes, []))).


clear_should_remove_node(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),

    rpc:call(Node, onepanel_cluster, init, []),
    ?assertEqual(ok, rpc:call(Node, onepanel_cluster, clear_node, [])).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).


end_per_suite(Config) ->
    Config.%test_node_starter:clean_environment(Config).