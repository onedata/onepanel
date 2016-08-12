%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains service utility functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_utils).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include("modules/logger.hrl").

%% API
-export([get_steps/3, format_steps/2, notify/2, partition_results/1,
    throw_on_error/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a list of steps for the service action.
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Service :: service:name(), Action :: service:action(),
    Ctx :: service:ctx()) -> Steps :: [#step{}].
get_steps(Service, Action, Ctx) ->
    Module = service:get_module(Service),
    Steps = Module:get_steps(Action, Ctx),
    lists:flatten(lists:map(fun
        (#step{} = Step) ->
            get_step(Service, Step, Ctx);
        (#steps{} = NestedSteps) ->
            get_nested_steps(Service, NestedSteps, Ctx)
    end, Steps)).


%%--------------------------------------------------------------------
%% @doc Formats the service steps into a human-readable format.
%% @end
%%--------------------------------------------------------------------
-spec format_steps(Steps :: [#step{}], Acc :: string()) -> Log :: string().
format_steps([], Log) ->
    Log;
format_steps([#step{hosts = Hosts, module = Module, function = Function,
    args = Args} | Steps], Log) ->
    Step = io_lib:format("Hosts: ~p~nFunction: ~p:~p~nArgs: ~p~n~n",
        [Hosts, Module, Function, Args]),
    format_steps(Steps, Log ++ Step).


%%--------------------------------------------------------------------
%% @doc Notifies about the service action progress.
%% @end
%%--------------------------------------------------------------------
-spec notify(Msg :: {Stage :: service:stage(), Details},
    Notify :: service:notify()) -> ok when
    Details :: {Module, Function} | {Module, Function, Result},
    Module :: module(),
    Function :: atom(),
    Result :: term().
notify(Msg, Notify) when is_pid(Notify) ->
    log(Msg),
    Notify ! Msg,
    ok;

notify(Msg, _Notify) ->
    log(Msg),
    ok.


%%--------------------------------------------------------------------
%% @doc Divides the results into two groups. The first group represents
%% the results of step that completed successfully and the second group that
%% completed with an error.
%% @end
%%--------------------------------------------------------------------
-spec partition_results(Results :: onepanel_rpc:results()) ->
    {GoodResults :: onepanel_rpc:results(), BadResults :: onepanel_rpc:results()}.
partition_results(Results) ->
    lists:partition(fun
        ({_, #error{}}) -> false;
        (_) -> true
    end, Results).


%%--------------------------------------------------------------------
%% @doc Throws an exception if an error occurred during service action execution.
%% @end
%%--------------------------------------------------------------------
-spec throw_on_error(Results :: #error{} | list()) -> Steps :: list() | no_return().
throw_on_error(#error{} = Error) ->
    ?throw(Error);

throw_on_error(Results) ->
    case lists:reverse(Results) of
        [{task_finished, {_, _, #error{} = Error}}] ->
            ?throw(Error);
        [{task_finished, {Service, Action, #error{}}} | Steps] ->
            ?throw(#service_error{
                service = Service, action = Action, steps = lists:reverse(Steps)
            });
        [{task_finished, {_, _, ok}} | Steps] ->
            lists:reverse(Steps)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Substitutes default step values.
%%--------------------------------------------------------------------
-spec get_step(Service :: service:name(), Step :: #step{}, Ctx :: service:ctx()) ->
    Step :: [] | #step{}.
get_step(Service, #step{service = undefined} = Step, Ctx) ->
    get_step(Service, Step#step{service = Service}, Ctx);

get_step(Service, #step{service = StepService, module = undefined} = Step, Ctx) ->
    Module = service:get_module(StepService),
    get_step(Service, Step#step{module = Module}, Ctx);

get_step(Service, #step{ctx = undefined} = Step, Ctx) ->
    get_step(Service, Step#step{ctx = Ctx}, Ctx);

get_step(Service, #step{ctx = StepCtx, args = undefined} = Step, Ctx) ->
    get_step(Service, Step#step{args = [StepCtx]}, Ctx);

get_step(Service, #step{hosts = undefined, ctx = #{hosts := Hosts}} = Step, Ctx) ->
    get_step(Service, Step#step{hosts = Hosts}, Ctx);

get_step(Service, #step{hosts = undefined, service = StepService} = Step, Ctx) ->
    StepHosts = case service:get(StepService) of
        {ok, #service{hosts = Hosts}} -> Hosts;
        #error{reason = ?ERR_NOT_FOUND} -> onepanel_cluster:nodes_to_hosts()
    end,
    get_step(Service, Step#step{hosts = StepHosts}, Ctx);

get_step(_Service, #step{hosts = []}, _Ctx) ->
    [];

get_step(Service, #step{hosts = Hosts, verify_hosts = true} = Step, Ctx) ->
    ClusterHosts = service_onepanel:get_hosts(),
    lists:foreach(fun(Host) ->
        case lists:member(Host, ClusterHosts) of
            true -> ok;
            false -> ?throw({?ERR_HOST_NOT_FOUND, Host})
        end
    end, Hosts),
    get_step(Service, Step#step{verify_hosts = false}, Ctx);

get_step(Service, #step{hosts = Hosts, ctx = StepCtx, selection = any} = Step, Ctx) ->
    Host = utils:random_element(Hosts),
    get_step(Service, Step#step{hosts = [Host], selection = all,
        ctx = StepCtx#{rest => lists:delete(Host, Hosts)}
    }, Ctx);

get_step(Service, #step{hosts = Hosts, ctx = StepCtx, selection = first} = Step, Ctx) ->
    get_step(Service, Step#step{hosts = [hd(Hosts)],
        ctx = StepCtx#{rest => tl(Hosts)}, selection = all
    }, Ctx);

get_step(Service, #step{hosts = Hosts, ctx = StepCtx, selection = rest} = Step, Ctx) ->
    get_step(Service, Step#step{hosts = tl(Hosts),
        ctx = StepCtx#{first => hd(Hosts)}, selection = all
    }, Ctx);

get_step(_Service, #step{condition = StepCondition, ctx = StepCtx} =
    Step, _Ctx) ->
    case StepCondition(StepCtx) of
        true -> Step;
        false -> []
    end.


%%--------------------------------------------------------------------
%% @private @doc Returns a list of steps for the nested service action.
%%--------------------------------------------------------------------
-spec get_nested_steps(Service :: service:name(), Steps :: #steps{},
    Ctx :: service:ctx()) -> Steps :: [#step{}].
get_nested_steps(Service, #steps{service = undefined} = Steps, Ctx) ->
    get_nested_steps(Service, Steps#steps{service = Service}, Ctx);

get_nested_steps(Service, #steps{ctx = undefined} = Steps, Ctx) ->
    get_nested_steps(Service, Steps#steps{ctx = Ctx}, Ctx);

get_nested_steps(_Service, #steps{service = StepsService, action = StepsAction,
    ctx = StepsCtx, condition = StepsCondition}, _Ctx) ->
    case StepsCondition(StepsCtx) of
        true -> get_steps(StepsService, StepsAction, StepsCtx);
        false -> []
    end.


%%--------------------------------------------------------------------
%% @private @doc Logs the service action progress.
%%--------------------------------------------------------------------
-spec log(Msg :: {Stage :: service:stage(), Details}) -> ok when
    Details :: {Module, Function} | {Module, Function, Result},
    Module :: module(),
    Function :: atom(),
    Result :: term().
log({action_begin, {Module, Function}}) ->
    ?log_info("Executing action ~p:~p", [Module, Function]);
log({action_end, {Module, Function, ok}}) ->
    ?log_info("Action ~p:~p completed successfully", [Module, Function]);
log({action_end, {Module, Function, #error{reason = Reason, stacktrace = []}}}) ->
    ?log_error("Action ~p:~p failed due to: ~p", [Module, Function, Reason]);
log({action_end, {Module, Function, #error{reason = Reason,
    stacktrace = Stacktrace}}}) ->
    ?log_error("Action ~p:~p failed due to: ~p~nStacktrace: ~p",
        [Module, Function, Reason, Stacktrace]);
log({step_begin, {Module, Function}}) ->
    ?log_info("Executing step ~p:~p", [Module, Function]);
log({step_end, {Module, Function, {_, []}}}) ->
    ?log_info("Step ~p:~p completed successfully", [Module, Function]);
log({step_end, {Module, Function, {_, Errors}}}) ->
    ?log_error("Step ~p:~p failed~n~s", [Module, Function,
        format_errors(Errors, "")]).


%%--------------------------------------------------------------------
%% @private @doc Formats the service errors into a human-readable format.
%%--------------------------------------------------------------------
-spec format_errors(Errors :: [{Node, {error, Reason}} |{Node, {error, Reason,
    Stacktrace}}], Acc :: string()) -> Log :: string() when
    Node :: node(),
    Reason :: term(),
    Stacktrace :: term().
format_errors([], Log) ->
    Log;
format_errors([{Node, #error{reason = Reason, stacktrace = []}} | Errors], Log) ->
    Error = io_lib:format("Node: ~p~nReason: ~p~n", [Node, Reason]),
    format_errors(Errors, Log ++ Error);
format_errors([{Node, #error{reason = Reason, stacktrace = Stacktrace}} | Errors], Log) ->
    Error = io_lib:format("Node: ~p~nReason: ~p~nStacktrace: ~p~n",
        [Node, Reason, Stacktrace]),
    format_errors(Errors, Log ++ Error).