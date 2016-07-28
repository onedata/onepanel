%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides an extension of rpc module functionality.
%%%--------------------------------------------------------------------
-module(onepanel_rpc).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/logger.hrl").

%% API
-export([apply/3]).
-export([call/3, call/4, call/5]).
-export([call_all/3, call_all/4, call_all/5]).
-export([call_any/3, call_any/4, call_any/5]).

-type result() :: {Node :: node(), Result :: term()}.
-type results() :: [result()].

-export_type([result/0, results/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Evaluates a function from a given module with provided arguments.
%% Returns a tuple consisting of node where execution took place and a result.
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
%% @doc @equiv call(service_onepanel:get_nodes(), Module, Function, Args)
%%--------------------------------------------------------------------
-spec call(Module :: module(), Function :: atom(), Args :: [term()]) ->
    Results :: results().
call(Module, Function, Args) ->
    call(service_onepanel:get_nodes(), Module, Function, Args).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/5} with a default timeout.
%%--------------------------------------------------------------------
-spec call(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()]) -> Results :: results().
call(Nodes, Module, Function, Args) ->
    Timeout = onepanel_env:get(rpc_timeout),
    call(Nodes, Module, Function, Args, Timeout).


%%--------------------------------------------------------------------
%% @doc Evaluates in parallel ```apply(Module, Function, Args)''' on the
%% specified nodes and collects the answers with timeout.
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
%% @doc Evaluates {@link call/3} and verifies that there is no errors.
%% In case of an error throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_all(Module :: module(), Function :: atom(), Args :: [term()]) ->
    Results :: results() | no_return().
call_all(Module, Function, Args) ->
    all(call(Module, Function, Args)).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/4} and verifies that there is no errors.
%% In case of an error throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_all(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()]) -> Results :: results() | no_return().
call_all(Nodes, Module, Function, Args) ->
    all(call(Nodes, Module, Function, Args)).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/5} and verifies that there is no errors.
%% In case of an error throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_all(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()], Timeout :: timeout()) -> Results :: results() | no_return().
call_all(Nodes, Module, Function, Args, Timeout) ->
    all(call(Nodes, Module, Function, Args, Timeout)).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/3} and verifies that there is at least one
%% successful result. In case of errors on all nodes throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_any(Module :: module(), Function :: atom(), Args :: [term()]) ->
    Value :: term() | no_return().
call_any(Module, Function, Args) ->
    any(call(Module, Function, Args)).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/4} and verifies that there is at least one
%% successful result. In case of errors on all nodes throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_any(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()]) -> Value :: term() | no_return().
call_any(Nodes, Module, Function, Args) ->
    any(call(Nodes, Module, Function, Args)).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/5} and verifies that there is at least one
%% successful result. In case of errors on all nodes throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_any(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()], Timeout :: timeout()) -> Value :: term() | no_return().
call_any(Nodes, Module, Function, Args, Timeout) ->
    any(call(Nodes, Module, Function, Args, Timeout)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Verifies that there is no errors.
%%--------------------------------------------------------------------
-spec all(Results :: results()) -> Results :: results() | no_return().
all(Results) ->
    BadResults = lists:filtermap(fun
        ({_, #error{}}) -> true;
        ({_, {error, Reason}}) -> {true, ?error(Reason)};
        ({_, _}) -> false
    end, Results),
    case BadResults of
        [] -> Results;
        _ -> ?throw(BadResults)
    end.


%%--------------------------------------------------------------------
%% @private @doc Verifies that there is at least one successful result.
%%--------------------------------------------------------------------
-spec any(Results :: results()) -> Value :: term() | no_return().
any([]) ->
    ok;

any(Results) ->
    GoodResults = lists:filter(fun
        ({_, #error{}}) -> false;
        ({_, {error, _}}) -> false;
        ({_, _}) -> true
    end, Results),
    case GoodResults of
        [] -> ?throw(?ERR_FAILURE_ON_ALL_NODES);
        [{_, Value} | _] -> Value
    end.
