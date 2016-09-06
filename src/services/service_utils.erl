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
-include_lib("ctool/include/logging.hrl").
-include("modules/models.hrl").
-include("service.hrl").

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
    get_steps(#steps{service = Service, action = Action, ctx = Ctx,
        verify_hosts = true}).

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
    ?throw_error(Error);

throw_on_error(Results) ->
    case lists:reverse(Results) of
        [{task_finished, {_, _, #error{} = Error}}] ->
            ?throw_error(Error);
        [{task_finished, {Service, Action, #error{}}}, Step | _] ->
            {Module, Function, {_, BadResults}} = Step,
            ?throw_error(#service_error{
                service = Service, action = Action, module = Module,
                function = Function, bad_results = BadResults
            });
        [{task_finished, {_, _, ok}} | Steps] ->
            lists:reverse(Steps)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Substitutes default steps values.
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Steps :: #steps{}) -> [#step{}].
get_steps(#steps{service = Service, action = Action, ctx = Ctx, verify_hosts = Verify}) ->
    Module = service:get_module(Service),
    Steps = Module:get_steps(Action, Ctx),
    lists:flatten(lists:map(fun
        (#step{service = Service2, ctx = Ctx2, verify_hosts = Verify2} = Step) ->
            get_step(Step#step{
                service = utils:ensure_defined(Service2, undefined, Service),
                ctx = utils:ensure_defined(Ctx2, undefined, Ctx),
                verify_hosts = utils:ensure_defined(Verify2, undefined, Verify)
            });
        (#steps{service = Service2, ctx = Ctx2, verify_hosts = Verify2} = NestedSteps) ->
            get_nested_steps(NestedSteps#steps{
                service = utils:ensure_defined(Service2, undefined, Service),
                ctx = utils:ensure_defined(Ctx2, undefined, Ctx),
                verify_hosts = utils:ensure_defined(Verify2, undefined, Verify)
            })
    end, Steps)).


%%--------------------------------------------------------------------
%% @private @doc Substitutes default step values.
%% @end
%%--------------------------------------------------------------------
-spec get_step(Step :: #step{}) -> Step :: [] | #step{}.
get_step(#step{service = Service, module = undefined} = Step) ->
    get_step(Step#step{module = service:get_module(Service)});

get_step(#step{ctx = Ctx, args = undefined} = Step) ->
    get_step(Step#step{args = [Ctx]});

get_step(#step{hosts = undefined, ctx = #{hosts := Hosts}} = Step) ->
    get_step(Step#step{hosts = Hosts});

get_step(#step{hosts = undefined, service = Service} = Step) ->
    Hosts2 = case service:get(Service) of
        {ok, #service{hosts = Hosts}} -> Hosts;
        #error{reason = ?ERR_NOT_FOUND} -> onepanel_cluster:nodes_to_hosts()
    end,
    get_step(Step#step{hosts = Hosts2});

get_step(#step{hosts = []}) ->
    [];

get_step(#step{hosts = Hosts, verify_hosts = true} = Step) ->
    ClusterHosts = service_onepanel:get_hosts(),
    lists:foreach(fun(Host) ->
        case lists:member(Host, ClusterHosts) of
            true -> ok;
            false -> ?throw_error({?ERR_HOST_NOT_FOUND, Host})
        end
    end, Hosts),
    get_step(Step#step{verify_hosts = false});

get_step(#step{hosts = Hosts, ctx = Ctx, selection = any} = Step) ->
    Host = utils:random_element(Hosts),
    get_step(Step#step{hosts = [Host], selection = all,
        ctx = Ctx#{rest => lists:delete(Host, Hosts)}});

get_step(#step{hosts = Hosts, ctx = Ctx, selection = first} = Step) ->
    get_step(Step#step{hosts = [hd(Hosts)],
        ctx = Ctx#{rest => tl(Hosts)}, selection = all});

get_step(#step{hosts = Hosts, ctx = StepCtx, selection = rest} = Step) ->
    get_step(Step#step{hosts = tl(Hosts), ctx = StepCtx#{first => hd(Hosts)},
        selection = all});

get_step(#step{condition = Condition, ctx = Ctx} = Step) ->
    case Condition(Ctx) of
        true -> Step;
        false -> []
    end.


%%--------------------------------------------------------------------
%% @private @doc Returns a list of steps for the nested service action.
%% @end
%%--------------------------------------------------------------------
-spec get_nested_steps(Steps :: #steps{}) -> Steps :: [#step{}].
get_nested_steps(#steps{ctx = Ctx, condition = Condition} = Steps) ->
    case Condition(Ctx) of
        true -> get_steps(Steps);
        false -> []
    end.


%%--------------------------------------------------------------------
%% @private @doc Logs the service action progress.
%% @end
%%--------------------------------------------------------------------
-spec log(Msg :: {Stage :: service:stage(), Details}) -> ok when
    Details :: {Module, Function} | {Module, Function, Result},
    Module :: module(),
    Function :: atom(),
    Result :: term().
log({action_begin, {Module, Function}}) ->
    ?info("Executing action ~p:~p", [Module, Function]);
log({action_end, {Module, Function, ok}}) ->
    ?info("Action ~p:~p completed successfully", [Module, Function]);
log({action_end, {Module, Function, #error{reason = Reason, stacktrace = []}}}) ->
    ?error("Action ~p:~p failed due to: ~p", [Module, Function, Reason]);
log({action_end, {Module, Function, #error{reason = Reason,
    stacktrace = Stacktrace}}}) ->
    ?error("Action ~p:~p failed due to: ~p~nStacktrace: ~p",
        [Module, Function, Reason, Stacktrace]);
log({step_begin, {Module, Function}}) ->
    ?info("Executing step ~p:~p", [Module, Function]);
log({step_end, {Module, Function, {_, []}}}) ->
    ?info("Step ~p:~p completed successfully", [Module, Function]);
log({step_end, {Module, Function, {_, Errors}}}) ->
    ?error("Step ~p:~p failed~n~s", [Module, Function,
        format_errors(Errors, "")]).


%%--------------------------------------------------------------------
%% @private @doc Formats the service errors into a human-readable format.
%% @end
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