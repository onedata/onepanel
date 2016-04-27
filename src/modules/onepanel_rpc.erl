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
-module(onepanel_rpc).
-author("Krzysztof Trzepla").

-include_lib("ctool/include/logging.hrl").

%% API
-export([apply/3, call/3, call/4, call/5]).

-type result() :: {Node :: node(), Result :: term()}.
-type results() :: [result()].

-export_type([result/0, results/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec apply(Module :: module(), Function :: atom(), Args :: [term()]) ->
    {Node :: node(), Result :: term()}.
apply(Module, Function, Args) ->
    try
        {node(), erlang:apply(Module, Function, Args)}
    catch
        _:Reason -> {node(), {error, Reason}}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @equiv call(onepanel:nodes(), Module, Function, Args)
%% @end
%%--------------------------------------------------------------------
-spec call(Module :: module(), Function :: atom(), Args :: [term()]) ->
    [{Node :: node(), Result :: term()}].
call(Module, Function, Args) ->
    call(onepanel:nodes(), Module, Function, Args).


%%--------------------------------------------------------------------
%% @doc
%% @equiv call(Nodes, Module, Function, Args, infinity)
%% @end
%%--------------------------------------------------------------------
-spec call(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()]) -> [{Node :: node(), Result :: term()}].
call(Nodes, Module, Function, Args) ->
    call(Nodes, Module, Function, Args, infinity).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec call(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()], Timeout :: timeout()) ->
    [{Node :: node(), Result :: term()}].
call(Nodes, Module, Function, Args, Timeout) ->
    {Values, BadNodes} = rpc:multicall(
        Nodes, ?MODULE, apply, [Module, Function, Args], Timeout
    ),
    Results = lists:foldl(fun(BadNode, Acc) ->
        [{BadNode, {error, badnode}} | Acc]
    end, Values, BadNodes),
    ?debug("Call ~p:~p(~p) on nodes ~p with timeout ~p returned ~p",
        [Module, Function, Args, Nodes, Timeout, Results]),
    Results.

%%%===================================================================
%%% Internal functions
%%%===================================================================
