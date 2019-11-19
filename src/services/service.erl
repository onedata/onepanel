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
-include_lib("ctool/include/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("service.hrl").

-define(DEFAULT_STATUS, healthy).

%% Model behaviour callbacks
-export([get_fields/0, get_record_version/0, seed/0, upgrade/2, create/1,
    save/1, update/2, get/1, exists/1, delete/1, list/0]).

%% API
-export([apply/3, apply/4]).
-export([apply_async/2, apply_async/3]).
-export([apply_sync/2, apply_sync/3, apply_sync/4]).
-export([get_results/1, get_results/2, abort_task/1,
    exists_task/1]).
-export([register_healthcheck/2]).
-export([update_status/2, update_status/3, all_healthy/0, healthy/1]).
-export([get_module/1, get_hosts/1, add_host/2]).
-export([get_ctx/1, update_ctx/2, store_in_ctx/3]).

% @formatter:off
-type name() :: ?SERVICE_OZ | ?SERVICE_OP |
    ?SERVICE_OPW | ?SERVICE_OZW | ?SERVICE_CW |
    ?SERVICE_CM | ?SERVICE_CB | ?SERVICE_PANEL |
    ?SERVICE_LE | ?SERVICE_CEPH |
    ?SERVICE_CEPH_OSD | ?SERVICE_CEPH_MON | ?SERVICE_CEPH_MGR.
-type action() :: atom().
-type notify() :: pid() | undefined.
-type host() :: string().
-type step() :: #step{} | #steps{}.
-type condition() :: boolean() | fun((ctx()) -> boolean()).
-type event() :: action_begin | action_steps_count | action_end |
                 step_begin | step_end.
-type record() :: #service{}.

-type status() :: healthy | unhealthy | stopped | missing.
% @formatter:on


%% ctx/0 is used as:
%% - data field in #service{} record, for storing persistent service information
%% - argument for get_steps functions and, by default, each step function invoked
%%
%% Common keys used in the step ctx:
%% 'hosts' - list of hosts on which step should be performed, used when
%%           #step.hosts is not set explicitely
%% 'rest' - filled by service_utils:get_step/1 when executing on 'first' host,
%%          contains remainder of the hosts list
%% 'first' - filled by service_utils:get_step/1 when executing on 'rest' hosts,
%%           contains the first host (excluded from step execution)
%% 'all' - filled by service_utils:get_step/1 when selecting
%%         'first' or 'rest' hosts, contains the original hosts list
%%
%% 'task_delay' - when present in ctx passed to service:apply, causes delay
%%                before the start of task execution
-type ctx() :: map().


-export_type([name/0, action/0, status/0, ctx/0, notify/0, host/0, step/0, condition/0,
    event/0]).

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
%% @doc {@link model_behaviour:get_record_version/0}
%% @end
%%--------------------------------------------------------------------
-spec get_record_version() -> model_behaviour:version().
get_record_version() ->
    1.


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:seed/0}
%% @end
%%--------------------------------------------------------------------
-spec seed() -> any().
seed() ->
    ok.


-spec upgrade(PreviousVsn :: model_behaviour:version(), PreviousRecord :: tuple()) ->
    no_return().
upgrade(_PreviousVsn, _PreviousRecord) ->
    error(?ERROR_NOT_SUPPORTED).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:create/1}
%% @end
%%--------------------------------------------------------------------
-spec create(Record :: record()) ->
    {ok, name()} | {error, _} | no_return().
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
    {ok, Record :: record()} | {error, _} | no_return().
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
%% @doc @equiv update_status(Service, hosts:self(), Status)
%% @end
%%--------------------------------------------------------------------
-spec update_status(name(), status()) -> status().
update_status(Service, Status) ->
    update_status(Service, hosts:self(), Status).

%%--------------------------------------------------------------------
%% @doc
%% Updates service status cache value for given service and host.
%% @end
%%--------------------------------------------------------------------
-spec update_status(name(), host(), status()) -> status().
update_status(Service, Host, Status) ->
    update_ctx(Service, fun(Ctx) ->
        kv_utils:put([status, Host], Status, Ctx)
    end),
    Status.


%%--------------------------------------------------------------------
%% @doc
%% Checks if all deployed services have reported healthy status
%% on last check.
%% Note that Ceph services never update their stored status (so it always
%% remains healthy) in order not to block REST requests regarding
%% Oneprovider from execution because of Ceph health warnings.
%% @TODO VFS-5661 Track the status and decide if Ceph status is relevant
%% to return 503 on endpoint-by-endpoint basis
%% @end
%%--------------------------------------------------------------------
-spec all_healthy() -> boolean().
all_healthy() ->
    lists:all(fun(#service{ctx = Ctx}) ->
        lists:all(fun(Status) ->
            healthy == Status
        end, maps:values(maps:get(status, Ctx, #{})))
    end, service:list()).


%%--------------------------------------------------------------------
%% @doc
%% Checks if all nodes of given service have reported healthy status
%% on last check.
%% @end
%%--------------------------------------------------------------------
-spec healthy(name()) -> boolean().
healthy(Service) ->
    case ?MODULE:get(Service) of
        {ok, #service{ctx = Ctx}} ->
            lists:all(fun({_Host, Status}) ->
                healthy == Status
            end, maps:to_list(maps:get(status, Ctx, #{})));
        _Error -> false
    end.


%%--------------------------------------------------------------------
%% @doc @equiv apply(Service, Action, Ctx, undefined)
%% @end
%%--------------------------------------------------------------------
-spec apply(Service :: name(), Action :: action(), Ctx :: ctx()) ->
    ok | {error, _}.
apply(Service, Action, Ctx) ->
    apply(Service, Action, Ctx, undefined).


%%--------------------------------------------------------------------
%% @doc Executes the service action and notifies about the process.
%% @end
%%--------------------------------------------------------------------
-spec apply(Service :: name(), Action :: action(), Ctx :: ctx(), Notify :: notify()) ->
    ok | {error, _}.
apply([], _Action, _Ctx, _Notify) ->
    ok;

apply(Service, Action, Ctx, Notify) ->
    TaskDelay = maps:get(task_delay, Ctx, 0),
    ?debug("Delaying task ~tp:~tp by ~tp ms", [Service, Action, TaskDelay]),
    timer:sleep(TaskDelay),
    service_utils:notify({action_begin, {Service, Action}}, Notify),
    Result = try
        Steps = service_utils:get_steps(Service, Action, Ctx),

        service_utils:notify({action_steps_count,
            {Service, Action, length(Steps)}}, Notify),

        ?debug("Execution of ~tp:~tp requires following steps:~n~ts",
            [Service, Action, service_utils:format_steps(Steps, "")]),
        apply_steps(Steps, Notify)
    catch
        throw:{error, _} = Error -> Error;
        Type:Error ->
            ?error("Error executing action ~p:~p: ~p:~p", [Service, Action, Type, Error]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end,
    service_utils:notify({action_end, {Service, Action, Result}}, Notify),
    Result.


%%--------------------------------------------------------------------
%% @doc @equiv apply_async(Service, Action, #{})
%% @end
%%--------------------------------------------------------------------
-spec apply_async(Service :: service:name(), Action :: service:action()) ->
    TaskId :: service_executor:task_id().
apply_async(Service, Action) ->
    apply_async(Service, Action, #{}).


%%--------------------------------------------------------------------
%% @doc Schedules the asynchronous service action.
%% @end
%%--------------------------------------------------------------------
-spec apply_async(Service :: service:name(), Action :: service:action(),
    Ctx :: service:ctx()) -> TaskId :: service_executor:task_id().
apply_async(Service, Action, Ctx) ->
    gen_server:call(?SERVICE_EXECUTOR_NAME, {apply, Service, Action, Ctx}).


%%--------------------------------------------------------------------
%% @doc @equiv apply_sync(Service, Action, #{})
%% @end
%%--------------------------------------------------------------------
-spec apply_sync(Service :: service:name(), Action :: service:action()) ->
    Results :: service_executor:results() | {error, _}.
apply_sync(Service, Action) ->
    apply_sync(Service, Action, #{}).


%%--------------------------------------------------------------------
%% @doc Evaluates the service action synchronously and returns the results.
%% @end
%%--------------------------------------------------------------------
-spec apply_sync(Service :: service:name(), Action :: service:action(),
    Ctx :: service:ctx()) -> Results :: service_executor:results() | {error, _}.
apply_sync(Service, Action, Ctx) ->
    apply_sync(Service, Action, Ctx, infinity).


%%--------------------------------------------------------------------
%% @doc Evaluates the service action synchronously with a timeout and returns
%% the results.
%% @end
%%--------------------------------------------------------------------
-spec apply_sync(Service :: service:name(), Action :: service:action(),
    Ctx :: service:ctx(), Timeout :: timeout()) ->
    Results :: service_executor:results() | {error, _}.
apply_sync(Service, Action, Ctx, Timeout) ->
    TaskId = apply_async(Service, Action, Ctx),
    Result = service_executor:receive_results(TaskId, Timeout),
    abort_task(TaskId),
    Result.


%%--------------------------------------------------------------------
%% @doc @equiv get_results(TaskId, infinity)
%% @end
%%--------------------------------------------------------------------
-spec get_results(service_executor:task_id()) -> {Results, StepsCount} when
    Results :: service_executor:results() | {error, _},
    StepsCount :: non_neg_integer() | {error, _}.
get_results(TaskId) ->
    get_results(TaskId, infinity).


%%--------------------------------------------------------------------
%% @doc Returns the asynchronous operation results.
%% @end
%%--------------------------------------------------------------------
-spec get_results(service_executor:task_id(), timeout()) -> {Results, StepsCount} when
    Results :: service_executor:results() | {error, _},
    StepsCount :: non_neg_integer() | {error, _}.
get_results(TaskId, Timeout) ->
    Results = case gen_server:call(?SERVICE_EXECUTOR_NAME, {get_results, TaskId}) of
        ok -> service_executor:receive_results(TaskId, Timeout);
        {error, _} = Error -> Error
    end,
    StepsCount = case gen_server:call(?SERVICE_EXECUTOR_NAME, {get_count, TaskId}) of
        ok -> service_executor:receive_count(TaskId, Timeout);
        {error, _} = Error2 -> {Results, Error2}
    end,
    {Results, StepsCount}.


%%--------------------------------------------------------------------
%% @doc Aborts the asynchronous operation.
%% @end
%%--------------------------------------------------------------------
-spec abort_task(TaskId :: binary()) -> ok | {error, _}.
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
    case service:get(Service) of
        {ok, #service{hosts = Hosts}} -> Hosts;
        {error, _} -> []
    end.


%%--------------------------------------------------------------------
%% @doc Adds host to a list of hosts where provided service is deployed.
%% @end
%%--------------------------------------------------------------------
-spec add_host(Service :: name(), Host :: host()) -> ok.
add_host(Service, Host) ->
    ?MODULE:update(Service, fun(#service{hosts = Hosts, ctx = Ctx} = S) ->
        OldStatus = maps:get(status, Ctx, #{}),
        NewStatus = OldStatus#{Host => maps:get(Host, OldStatus, ?DEFAULT_STATUS)},
        S#service{
            hosts = lists:usort([Host | Hosts]),
            % ensure statuses map contains all hosts
            ctx = Ctx#{status => NewStatus}
        }
    end).


-spec register_healthcheck(Service :: name(), Ctx :: ctx()) -> ok.
register_healthcheck(Service, Ctx) ->
    Period = onepanel_env:get(services_check_period),
    Module = service:get_module(Service),
    Name = case Ctx of
        #{id := Id} -> str_utils:format("~tp (id ~tp)", [Service, Id]);
        _ -> str_utils:format("~tp", [Service])
    end,

    Condition = fun() ->
        case (catch Module:status(Ctx)) of
            healthy -> false;
            unhealthy -> false;
            _ -> true
        end
    end,

    Action = fun() ->
        ?critical("Service ~ts is not running. Restarting...", [Name]),
        Results = service:apply_sync(Service, resume, Ctx),
        case service_utils:results_contain_error(Results) of
            {true, Error} ->
                ?critical("Failed to restart service ~ts due to:~n~ts",
                    [Name, onepanel_errors:format_error(Error)]);
            false -> ok
        end
    end,

    onepanel_cron:add_job(Name, Action, Period, Condition).


%%--------------------------------------------------------------------
%% @doc Returns the "ctx" field of a service model.
%% @end
%%--------------------------------------------------------------------
-spec get_ctx(name()) -> ctx() | {error, _} | no_return().
get_ctx(Service) ->
    case ?MODULE:get(Service) of
        {ok, #service{ctx = Ctx}} -> Ctx;
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Updates the "ctx" field of a service model.
%% @end
%%--------------------------------------------------------------------
-spec update_ctx(Service :: service:name(), Diff) -> ok | no_return()
    when Diff :: map() | fun((service:ctx()) -> service:ctx()).
update_ctx(Service, Diff) when is_map(Diff) ->
    update_ctx(Service, fun(Ctx) ->
        maps:merge(Ctx, Diff)
    end);

update_ctx(Service, Diff) when is_function(Diff, 1) ->
    service:update(Service, fun(#service{ctx = Ctx} = S) ->
        S#service{ctx = Diff(Ctx)}
    end).


-spec store_in_ctx(Service :: name(), Keys :: onepanel_maps:keys(),
    Value :: term()) -> ok | no_return().
store_in_ctx(Service, Keys, Value) ->
    update_ctx(Service, fun(Ctx) ->
        kv_utils:put(Keys, Value, Ctx)
    end).

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private @doc Applies the service steps associated with an action.
%%--------------------------------------------------------------------
-spec apply_steps(Steps :: [step()], Notify :: notify()) -> ok | {error, _}.
apply_steps([], _Notify) ->
    ok;

apply_steps([#step{hosts = Hosts, module = Module, function = Function,
    args = Args, attempts = Attempts, retry_delay = Delay} = Step | Steps], Notify) ->

    Nodes = nodes:service_to_nodes(?APP_NAME, Hosts),
    service_utils:notify({step_begin, {Module, Function}}, Notify),

    Results = onepanel_rpc:call(Nodes, Module, Function, Args),
    Status = service_utils:partition_results(Results),

    service_utils:notify({step_end, {Module, Function, Status}}, Notify),

    case {Status, Attempts} of
        {{_, []}, _} -> apply_steps(Steps, Notify);
        {{_, _}, 1} -> {error, {Module, Function, Status}};
        {{_, _}, _} ->
            timer:sleep(Delay),
            apply_steps([Step#step{attempts = Attempts - 1} | Steps], Notify)
    end.
