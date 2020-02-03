%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides an extension of rpc module functionality.
%%% Used mainly in the service actions flow.
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
%% @doc
%% Exported for rpc access.
%% Evaluates a function from a given module with provided arguments.
%%
%% rpc-calling this function rather than the target function directly
%% adds node information to the result (otherwise rpc:multicall
%% looses information about which nodes returned which result).
%% In case of error logs it and returns an error-tuple or #exception.
%% @end
%%--------------------------------------------------------------------
-spec apply(module(), Function :: atom(), Args :: [term()]) ->
    {Node :: node(), Result :: term()}.
apply(Module, Function, Args) ->
    try
        {node(), erlang:apply(Module, Function, Args)}
    catch
        throw:{error, _} = Reason ->
            {node(), Reason};
        Type:Reason ->
            ?error_stacktrace("Unexpected error executing ~tp:~tp/~B~nError: ~tp:~tp",
                [Module, Function, erlang:length(Args), Type, Reason]),
            {node(), {error, #exception{
                % do not store stacktrace not to pollute logs since it's logged here
                type = Type, value = Reason, stacktrace = []}}}
    end.


%%--------------------------------------------------------------------
%% @doc @equiv call(service_onepanel:get_nodes(), Module, Function, Args)
%% @end
%%--------------------------------------------------------------------
-spec call(module(), Function :: atom(), Args :: [term()]) ->
    Results :: results().
call(Module, Function, Args) ->
    call(service_onepanel:get_nodes(), Module, Function, Args).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/5} with a default timeout.
%% @end
%%--------------------------------------------------------------------
-spec call(Nodes :: [node()] | node(), module(), Function :: atom(),
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
-spec call([node()] | node(), module(), Function :: atom(),
    Args :: [term()], timeout()) -> results().
call(NodeOrNodes, Module, Function, Args, Timeout) ->
    Nodes = utils:ensure_list(NodeOrNodes),
    {Values, _} = rpc:multicall(
        Nodes, ?MODULE, apply, [Module, Function, Args], Timeout
    ),

    % Calling non-onepanel nodes causes badrpc error since
    % onepanel_rpc:apply is not available on them.
    Results = lists:filter(fun
        ({badrpc, _}) -> false;
        ({_Node, _Result}) -> true
    end, Values),

    BadNodes = lists_utils:subtract(Nodes, proplists:get_keys(Results)),
    AllResults = Results ++
        [{BadNode, ?ERROR_NO_CONNECTION_TO_CLUSTER_NODE} || BadNode <- BadNodes],

    ?debug("Call ~p:~p(~p) on nodes ~p with timeout ~p returned ~p",
        [Module, Function, Args, Nodes, Timeout, AllResults]),
    AllResults.


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/3} and verifies that there are no errors.
%% In case of an error throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_all(module(), Function :: atom(), Args :: [term()]) ->
    Results :: results() | no_return().
call_all(Module, Function, Args) ->
    all(call(Module, Function, Args)).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/4} and verifies that there is no errors.
%% In case of an error throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_all(Nodes :: [node()] | node(), module(), Function :: atom(),
    Args :: [term()]) -> Results :: results() | no_return().
call_all(Nodes, Module, Function, Args) ->
    all(call(Nodes, Module, Function, Args)).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/5} and verifies that there is no errors.
%% In case of an error throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_all(Nodes :: [node()] | node(), module(), Function :: atom(),
    Args :: [term()], timeout()) -> Results :: results() | no_return().
call_all(Nodes, Module, Function, Args, Timeout) ->
    all(call(Nodes, Module, Function, Args, Timeout)).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/3} and verifies that there is at least one
%% successful result. In case of errors on all nodes throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_any(module(), Function :: atom(), Args :: [term()]) ->
    Value :: term() | no_return().
call_any(Module, Function, Args) ->
    any(call(Module, Function, Args)).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/4} and verifies that there is at least one
%% successful result. In case of errors on all nodes throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_any(Nodes :: [node()] | node(), module(), Function :: atom(),
    Args :: [term()]) -> Value :: term() | no_return().
call_any(Nodes, Module, Function, Args) ->
    any(call(Nodes, Module, Function, Args)).


%%--------------------------------------------------------------------
%% @doc Evaluates {@link call/5} and verifies that there is at least one
%% successful result. In case of errors on all nodes throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec call_any(Nodes :: [node()] | node(), module(), Function :: atom(),
    Args :: [term()], timeout()) -> Value :: term() | no_return().
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
        ({_, {error, _} = Error}) -> {true, Error};
        ({_, _}) -> false
    end, Results),
    case BadResults of
        [] -> Results;
        [{error, Reason} | _] -> reraise(Reason)
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
        ({_, {error, _}}) -> false;
        ({_, Value}) -> {true, Value}
    end, Results),
    case {GoodResults, Results} of
        {[Value | _], _} -> Value;
        {[], [{_Node, {error, Reason}} | _]} -> reraise(Reason)
    end.


-spec reraise(#exception{} | term()) -> no_return().
reraise(#exception{type = Type, value = Value, stacktrace = []}) ->
    erlang:Type(Value);

reraise(#exception{type = Type, value = Value, stacktrace = Stacktrace}) ->
    erlang:raise(Type, Value, Stacktrace);

reraise(Value) ->
    throw(Value).
