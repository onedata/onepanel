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

-include("modules/errors.hrl").
-include("modules/logger.hrl").

%% API
-export([apply/3]).
-export([call/3, call/4, call/5]).
-export([check_call/3, check_call/4, check_call/5]).

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
        _:Reason -> {node(), ?error(Reason)}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @equiv call(onepanel_cluster:get_nodes(), Module, Function, Args)
%% @end
%%--------------------------------------------------------------------
-spec call(Module :: module(), Function :: atom(), Args :: [term()]) ->
    Results :: results().
call(Module, Function, Args) ->
    call(onepanel_cluster:get_nodes(), Module, Function, Args).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec call(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()]) -> Results :: results().
call(Nodes, Module, Function, Args) ->
    Timeout = onepanel_env:get(rpc_timeout),
    call(Nodes, Module, Function, Args, Timeout).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec call(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()], Timeout :: timeout()) -> Results :: results().
call(Nodes, Module, Function, Args, Timeout) when is_list(Nodes) ->
    {Values, BadNodes} = rpc:multicall(
        Nodes, ?MODULE, apply, [Module, Function, Args], Timeout
    ),
    Results = lists:foldl(fun(BadNode, Acc) ->
        [{BadNode, ?error(?ERR_BAD_NODE)} | Acc]
    end, Values, BadNodes),
    ?log_debug("Call ~p:~p(~p) on nodes ~p with timeout ~p returned ~p",
        [Module, Function, Args, Nodes, Timeout, Results]),
    Results.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec check_call(Module :: module(), Function :: atom(), Args :: [term()]) ->
    Results :: results() | no_return().
check_call(Module, Function, Args) ->
    check(call(Module, Function, Args)).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec check_call(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()]) -> Results :: results() | no_return().
check_call(Nodes, Module, Function, Args) ->
    check(call(Nodes, Module, Function, Args)).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec check_call(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()], Timeout :: timeout()) -> Results :: results() | no.
check_call(Nodes, Module, Function, Args, Timeout) ->
    check(call(Nodes, Module, Function, Args, Timeout)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec check(Results :: results()) -> Results :: results() | no_return().
check(Results) ->
    BadResults = lists:filter(fun
        ({_, #error{}}) -> true;
        ({_, _}) -> false
    end, Results),
    case BadResults of
        [] -> Results;
        _ -> ?throw(BadResults)
    end.
