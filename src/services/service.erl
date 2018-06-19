%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains service management functions.
%%% It implements {@link model_behaviour} behaviour.
%%% @end
%%%--------------------------------------------------------------------
-module(service).
-author("Krzysztof Trzepla").

-behaviour(model_behaviour).

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("service.hrl").

%% Model behaviour callbacks
-export([get_fields/0, seed/0, create/1, save/1, update/2, get/1, exists/1,
    delete/1, list/0]).

%% API
-export([start/3, stop/2, status/2, apply/3, apply/4, apply_async/3,
    apply_sync/3, apply_sync/4, get_results/1, get_results/2, abort_task/1,
    exists_task/1]).
-export([get_module/1, get_hosts/1, get_nodes/1, is_member/2, add_host/2]).

-type name() :: atom().
-type action() :: atom().
-type ctx() :: maps:map().
-type notify() :: pid() | undefined.
-type host() :: string().
-type step() :: #step{} | #steps{}.
-type condition() :: fun((ctx()) -> boolean()).
-type stage() :: action_begin | action_end | step_begin | step_end.
-type record() :: #service{}.

-export_type([name/0, action/0, ctx/0, notify/0, host/0, step/0, condition/0,
    stage/0]).

%%%===================================================================
%%% Model behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get_fields/0}
%% @end
%%--------------------------------------------------------------------
-spec get_fields() -> list(atom()).
get_fields() ->
    record_info(fields, ?MODULE).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:seed/0}
%% @end
%%--------------------------------------------------------------------
-spec seed() -> any().
seed() ->
    ok.


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:create/1}
%% @end
%%--------------------------------------------------------------------
-spec create(Record :: record()) ->
    {ok, name()} | #error{} | no_return().
create(Record) ->
    model:create(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:save/1}
%% @end
%%--------------------------------------------------------------------
-spec save(Record :: record()) -> ok | no_return().
save(Record) ->
    model:save(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:update/2}
%% @end
%%--------------------------------------------------------------------
-spec update(Key :: model_behaviour:key(), Diff :: model_behaviour:diff()) ->
    ok | no_return().
update(Key, Diff) ->
    model:update(?MODULE, Key, Diff).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get/1}
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: model_behaviour:key()) ->
    {ok, Record :: record()} | #error{} | no_return().
get(Key) ->
    model:get(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:exists/1}
%% @end
%%--------------------------------------------------------------------
-spec exists(Key :: model_behaviour:key()) ->
    boolean() | no_return().
exists(Key) ->
    model:exists(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:delete/1}
%% @end
%%--------------------------------------------------------------------
-spec delete(Key :: model_behaviour:key()) -> ok | no_return().
delete(Key) ->
    model:delete(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:list/0}
%% @end
%%--------------------------------------------------------------------
-spec list() -> Records :: [model_behaviour:record()] | no_return().
list() ->
    model:list(?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Sets the system limits and starts the service using an init script
%% in a shell.
%% @end
%%--------------------------------------------------------------------
-spec start(string(), maps:map(), atom()) -> ok | no_return().
start(InitScript, SystemLimits, CustomCmdEnv) ->
    Tokens = case onepanel_env:find(CustomCmdEnv) of
        {ok, Cmd} -> [Cmd];
        _ -> ["service", InitScript, "start"]
    end,
    Tokens2 = maps:fold(fun
        (open_files, Value, Acc) -> ["ulimit", "-n", Value, ";" | Acc];
        (_, _, Acc) -> Acc
    end, Tokens, SystemLimits),
    onepanel_shell:check_output(Tokens2),
    ok.


%%--------------------------------------------------------------------
%% @doc Stops the service using an init script in a shell.
%% @end
%%--------------------------------------------------------------------
-spec stop(string(), atom()) -> ok | no_return().
stop(InitScript, CustomCmdEnv) ->
    Tokens = case onepanel_env:find(CustomCmdEnv) of
        {ok, Cmd} -> [Cmd];
        _ -> ["service", InitScript, "stop"]
    end,
    try
        onepanel_shell:check_call(Tokens)
    catch
        _:_ ->
            ?warning("Failed to stop service '~s'", [InitScript]),
            ok
    end.


%%--------------------------------------------------------------------
%% @doc Returns the service status using an init script in a shell.
%% @end
%%--------------------------------------------------------------------
-spec status(string(), atom()) -> running | stopped | missing.
status(InitScript, CustomCmdEnv) ->
    Tokens = case onepanel_env:find(CustomCmdEnv) of
        {ok, Cmd} -> [Cmd];
        _ -> ["service", InitScript, "status"]
    end,
    case onepanel_shell:call(Tokens) of
        0 -> running;
        127 -> missing;
        _ -> stopped
    end.


%%--------------------------------------------------------------------
%% @doc @equiv apply(Service, Action, Ctx, undefined)
%% @end
%%--------------------------------------------------------------------
-spec apply(Service :: name(), Action :: action(), Ctx :: ctx()) ->
    ok | {error, Reason :: term()}.
apply(Service, Action, Ctx) ->
    apply(Service, Action, Ctx, undefined).


%%--------------------------------------------------------------------
%% @doc Executes the service action and notifies about the process.
%% @end
%%--------------------------------------------------------------------
-spec apply(Service :: name(), Action :: action(), Ctx :: ctx(), Notify :: notify()) ->
    ok | {error, Reason :: term()}.
apply([], _Action, _Ctx, _Notify) ->
    ok;

apply(Service, Action, Ctx, Notify) ->
    TaskDelay = maps:get(task_delay, Ctx, 0),
    ?debug("Delaying task ~p:~p by ~p ms", [Service, Action, TaskDelay]),
    timer:sleep(TaskDelay),
    service_utils:notify({action_begin, {Service, Action}}, Notify),
    Result = try
        Steps = service_utils:get_steps(Service, Action, Ctx),
        ?debug("Execution of ~p:~p requires following steps:~n~s",
            [Service, Action, service_utils:format_steps(Steps, "")]),
        apply_steps(Steps, Notify)
    catch
        _:Reason -> ?make_stacktrace(Reason)
    end,
    service_utils:notify({action_end, {Service, Action, Result}}, Notify),
    Result.


%%--------------------------------------------------------------------
%% @doc Schedules the asynchronous service action.
%% @end
%%--------------------------------------------------------------------
-spec apply_async(Service :: service:name(), Action :: service:action(),
    Ctx :: service:ctx()) -> TaskId :: service_executor:task_id().
apply_async(Service, Action, Ctx) ->
    gen_server:call(?SERVICE_EXECUTOR_NAME, {apply, Service, Action, Ctx}).


%%--------------------------------------------------------------------
%% @doc Evaluates the service action synchronously and returns the results.
%% @end
%%--------------------------------------------------------------------
-spec apply_sync(Service :: service:name(), Action :: service:action(),
    Ctx :: service:ctx()) -> Results :: service_executor:results() | #error{}.
apply_sync(Service, Action, Ctx) ->
    apply_sync(Service, Action, Ctx, infinity).


%%--------------------------------------------------------------------
%% @doc Evaluates the service action synchronously with a timeout and returns
%% the results.
%% @end
%%--------------------------------------------------------------------
-spec apply_sync(Service :: service:name(), Action :: service:action(),
    Ctx :: service:ctx(), Timeout :: timeout()) ->
    Results :: service_executor:results() | #error{}.
apply_sync(Service, Action, Ctx, Timeout) ->
    TaskId = apply_async(Service, Action, Ctx),
    Result = service_executor:receive_results(TaskId, Timeout),
    abort_task(TaskId),
    Result.


%%--------------------------------------------------------------------
%% @doc @equiv get_results(TaskId, infinity)
%% @end
%%--------------------------------------------------------------------
-spec get_results(TaskId :: service_executor:task_id()) ->
    Results :: service_executor:results() | #error{}.
get_results(TaskId) ->
    get_results(TaskId, infinity).


%%--------------------------------------------------------------------
%% @doc Returns the asynchronous operation results.
%% @end
%%--------------------------------------------------------------------
-spec get_results(TaskId :: service_executor:task_id(), Timeout :: timeout()) ->
    Results :: service_executor:results() | #error{}.
get_results(TaskId, Timeout) ->
    case gen_server:call(?SERVICE_EXECUTOR_NAME, {get_results, TaskId}) of
        ok -> service_executor:receive_results(TaskId, Timeout);
        #error{} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Aborts the asynchronous operation.
%% @end
%%--------------------------------------------------------------------
-spec abort_task(TaskId :: binary()) -> ok | #error{}.
abort_task(TaskId) ->
    gen_server:call(?SERVICE_EXECUTOR_NAME, {abort_task, TaskId}).


%%--------------------------------------------------------------------
%% @doc Checks whether the asynchronous operation associated with the provided
%% ID exists.
%%--------------------------------------------------------------------
-spec exists_task(TaskId :: binary()) -> boolean().
exists_task(TaskId) ->
    gen_server:call(?SERVICE_EXECUTOR_NAME, {exists_task, TaskId}).


%%--------------------------------------------------------------------
%% @doc Returns service module for service name.
%% @end
%%--------------------------------------------------------------------
-spec get_module(Service :: name()) -> Module :: module().
get_module(Service) ->
    erlang:list_to_atom("service_" ++ erlang:atom_to_list(Service)).


%%--------------------------------------------------------------------
%% @doc Returns lists of hosts where provided service is deployed.
%% @end
%%--------------------------------------------------------------------
-spec get_hosts(Service :: name()) -> Hosts :: [host()].
get_hosts(Service) ->
    {ok, #service{hosts = Hosts}} = service:get(Service),
    Hosts.


%%--------------------------------------------------------------------
%% @doc Returns lists of onepanel nodes for hosts where provided service is
%% deployed.
%% @end
%%--------------------------------------------------------------------
-spec get_nodes(Service :: name()) -> Nodes :: [node()].
get_nodes(Service) ->
    onepanel_cluster:hosts_to_nodes(Service, get_hosts(Service)).


%%--------------------------------------------------------------------
%% @doc Checks whether service is deployed on provided host.
%% @end
%%--------------------------------------------------------------------
-spec is_member(Service :: name(), Host :: host()) ->
    boolean() | {error, Reason :: term()}.
is_member(Service, Host) ->
    model:transaction(fun() ->
        case ?MODULE:get(Service) of
            {ok, #service{hosts = Hosts}} -> lists:member(Host, Hosts);
            _ -> false
        end
    end).


%%--------------------------------------------------------------------
%% @doc Adds host to a list of hosts where provided service is deployed.
%% @end
%%--------------------------------------------------------------------
-spec add_host(Service :: name(), Host :: host()) -> ok.
add_host(Service, Host) ->
    ?MODULE:update(Service, fun(#service{hosts = Hosts} = S) ->
        S#service{hosts = [Host | lists:delete(Host, Hosts)]}
    end).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Applies the service steps associated with an action.
%%--------------------------------------------------------------------
-spec apply_steps(Steps :: [step()], Notify :: notify()) -> ok | #error{}.
apply_steps([], _Notify) ->
    ok;

apply_steps([#step{hosts = Hosts, module = Module, function = Function,
    args = Args, attempts = Attempts, retry_delay = Delay} = Step | Steps], Notify) ->

    Nodes = onepanel_cluster:hosts_to_nodes(Hosts),
    service_utils:notify({step_begin, {Module, Function}}, Notify),

    Results = onepanel_rpc:call(Nodes, Module, Function, Args),
    Status = service_utils:partition_results(Results),

    service_utils:notify({step_end, {Module, Function, Status}}, Notify),

    case {Status, Attempts} of
        {{_, []}, _} -> apply_steps(Steps, Notify);
        {{_, _}, 1} -> ?make_error({Module, Function, Status});
        {{_, _}, _} ->
            timer:sleep(Delay),
            apply_steps([Step#step{attempts = Attempts - 1} | Steps], Notify)
    end.
