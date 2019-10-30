%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides an extension of rpc module functionality.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_rpc).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").

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
        throw:Reason ->
            {node(), ?make_stacktrace(Reason)};
        Type:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            ?error("Unexpected error executing ~tp:~tp/~B~nError: ~tp:~tp~nStacktrace: ~tp",
            [Module, Function, erlang:length(Args), Type, Reason, Stacktrace]),
            {node(), ?make_stacktrace(Reason, undefined, Stacktrace)}
    end.


%%--------------------------------------------------------------------
%% @doc @equiv call(service_onepanel:get_nodes(), Module, Function, Args)
%% @end
%%--------------------------------------------------------------------
-spec call(Module :: module(), Function :: atom(), Args :: [term()]) ->
    Results :: results().
call(Module, Function, Args) ->
    call(service_onepanel:get_nodes(), Module, Function, Args).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/5} with a default timeout.
%% @end
%%--------------------------------------------------------------------
-spec call(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()]) -> Results :: results().
call(Nodes, Module, Function, Args) ->
    Timeout = onepanel_env:get(rpc_timeout),
    call(Nodes, Module, Function, Args, Timeout).


%%--------------------------------------------------------------------
%% @doc Evaluates in parallel ```apply(Module, Function, Args)''' on the
%% specified nodes and collects the answers with timeout.
%% Nodes must by a list of onepanel nodes.
%% @end
%%--------------------------------------------------------------------
-spec call(Nodes :: [node()], Module :: module(), Function :: atom(),
    Args :: [term()], Timeout :: timeout()) -> Results :: results().
call(Nodes, Module, Function, Args, Timeout) when is_list(Nodes) ->
    {Values, _} = rpc:multicall(
        Nodes, ?MODULE, apply, [Module, Function, Args], Timeout
    ),

    % Calling non-onepanel nodes causes badrpc error since
    % onepanel_rpc:apply is not available on them.
    Results = lists:filter(fun
        ({badrpc, _}) -> false;
        ({_Node, _Result}) -> true
    end, Values),

    BadNodes = onepanel_lists:subtract(Nodes, proplists:get_keys(Results)),
    AllResults = Results ++
        [{BadNode, ?make_error(?ERR_BAD_NODE)} || BadNode <- BadNodes],

    ?debug("Call ~p:~p(~p) on nodes ~p with timeout ~p returned ~p",
        [Module, Function, Args, Nodes, Timeout, AllResults]),
    AllResults.


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/3} and verifies that there are no errors.
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
%% @private @doc Verifies that there are no errors.
%%--------------------------------------------------------------------
-spec all(Results :: results()) -> Results :: results() | no_return().
all(Results) ->
    BadResults = lists:filtermap(fun
        ({_, #error{}}) -> true;
        ({_, {error, Reason}}) -> {true, ?make_error(Reason)};
        ({_, _}) -> false
    end, Results),
    case BadResults of
        [] -> Results;
        _ -> ?throw_error(BadResults)
    end.


%%--------------------------------------------------------------------
%% @private @doc Returns first successful result,
%% throws if all results are errors.
%%--------------------------------------------------------------------
-spec any(Results :: results()) -> Value :: term() | no_return().
any([]) ->
    ok;

any(Results) ->
    GoodResults = lists:filtermap(fun
        ({_, #error{}}) -> false;
        ({_, {error, _}}) -> false;
        ({_, Value}) -> {true, Value}
    end, Results),
    case GoodResults of
        [] -> ?throw_error(?ERR_FAILURE_ON_ALL_NODES);
        [Value | _] -> Value
    end.
