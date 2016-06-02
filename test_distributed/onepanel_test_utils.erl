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

%%%===================================================================
%%% API functions
%%%===================================================================

ensure_initailized(Config) ->
    [Node | _] = Nodes = ?config(onepanel_nodes, Config),
    ?assertEqual(ok, rpc:call(Node, onepanel, health_check, [Nodes]), 120),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec mock_start(Config :: proplists:proplist()) ->
    Config :: proplists:proplist().
mock_start(Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    lists:foreach(fun(App) ->
        {Results, []} = ?assertMatch({_, []},
            rpc:multicall(Nodes, application, start, [App])),
        ?assert(lists:all(fun(Result) -> Result =:= ok end, Results))
    end, [tools, meck]),
    Config.

%%%===================================================================
%%% Internal functions
%%%===================================================================
