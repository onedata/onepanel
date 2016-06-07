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
-module(onepanel_test_utils).
-author("Krzysztof Trzepla").

-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([ensure_initailized/1, mock_start/1]).

-type config() :: proplists:proplist().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec ensure_initailized(Config :: config()) -> Config :: config().
ensure_initailized(Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    OpNodes = filter_nodes(oneprovider, Nodes),
    OzNodes = filter_nodes(onezone, Nodes),
    health_check(OpNodes),
    health_check(OzNodes),
    health_check((Nodes -- OpNodes) -- OzNodes),
    [{op_nodes, OpNodes}, {oz_nodes, OzNodes} | Config].


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec mock_start(Config :: config()) -> Config :: config().
mock_start(Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    lists:foreach(fun(App) ->
        {Results, []} = ?assertMatch({_, []},
            rpc:multicall(Nodes, application, start, [App])),
        ?assert(lists:all(fun(Result) -> Result =:= ok end, Results))
    end, [tools, meck]),
    Config.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec filter_nodes(Type :: atom(), Nodes :: [node()]) -> FilteredNodes :: [node()].
filter_nodes(Type, Nodes) ->
    lists:filter(fun(Node) ->
        [_, Domain] = string:tokens(erlang:atom_to_list(Node), "@"),
        lists:member(erlang:atom_to_list(Type), string:tokens(Domain, "."))
    end, Nodes).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec health_check(Nodes :: [node()]) -> ok | no_return().
health_check([]) ->
    ok;

health_check([Node | _] = Nodes) ->
    ?assertEqual(ok, rpc:call(Node, onepanel, health_check, [Nodes]), 120).