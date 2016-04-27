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
-module(nodes_connection_test_SUITE).
-author("Krzysztof Trzepla").

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% tests
-export([
    onepanel_nodes_should_detect_each_other_using_multicast/1
]).

all() ->
    ?ALL([
        onepanel_nodes_should_detect_each_other_using_multicast
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

onepanel_nodes_should_detect_each_other_using_multicast(Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    lists:foreach(fun(Node) ->
        ExpectedNodes = lists:sort(lists:delete(Node, Nodes)),
        ActualNodes = lists:sort(rpc:call(Node, erlang, nodes, [])),
        ?assertEqual(ExpectedNodes, ActualNodes, 10)
    end, Nodes).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
