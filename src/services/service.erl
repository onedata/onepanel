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
-export([register_healthcheck/2, deregister_healthcheck/2]).
-export([update_status/2, update_status/3, all_healthy/0, is_healthy/1]).
-export([get_module/1, get_hosts/1, has_host/2, add_host/2]).
-export([get_ctx/1, update_ctx/2, store_in_ctx/3]).

% @formatter:off
-type name() :: ?SERVICE_OZ | ?SERVICE_OP
    | ?SERVICE_OPW | ?SERVICE_OZW | ?SERVICE_CW
    | ?SERVICE_CM | ?SERVICE_CB | ?SERVICE_PANEL
    | ?SERVICE_LE | ?SERVICE_CEPH
    | ?SERVICE_CEPH_OSD | ?SERVICE_CEPH_MON | ?SERVICE_CEPH_MGR.
-type action() :: atom().
-type notify() :: pid() | undefined.
-type host() :: string().
-type step() :: #step{} | #steps{}.
-type condition() :: boolean() | fun((step_ctx()) -> boolean()).
-type event() :: action_begin | action_steps_count | action_end |
                 step_begin | step_end.
-type status() :: healthy | unhealthy | stopped | missing.


%% record field used for arbitrary information about the service
-type model_ctx() :: service_op_worker:model_ctx() | service_oz_worker:model_ctx()
| service_oneprovider:model_ctx() | service_onezone:model_ctx()
| service_cluster_manager:model_ctx() | service_letsencrypt:model_ctx()
| service_couchbase:model_ctx() | service_ceph:model_ctx()
| service_ceph_mon:model_ctx() | service_ceph_mgr:model_ctx()
| service_ceph_osd:model_ctx().

-type record() :: #service{}.


%% A map stored in #step and #steps records and by default provided
%% as argument of the invoked functions.
%% This type spec lists common values used in the step ctx.
%% Note the keys are optional (=>).
-type step_ctx() :: #{
    %% 'hosts' - list of hosts on which step should be performed, used when
    %%           #step.hosts is not set explicitly
    hosts => [service:host()],
    %% 'rest' - filled by service:resolve_hosts/1 when executing on 'first' host,
    %%          contains the remainder of the hosts list
    rest => [service:host()],
    %% 'first' - filled by service:resolve_hosts/1 when executing on 'rest' hosts,
    %%           contains the first host (excluded from step execution)
    first => service:host(),
    %% 'all' - filled by service:resolve_hosts/1 when selecting
    %%         'first' or 'rest' hosts, contains the original hosts list
    all => [service:host()],

    %% function-specific arguments
    term() => term()
}.
% @formatter:on


-export_type([name/0, action/0, status/0, model_ctx/0, step_ctx/0, notify/0, host/0,
    step/0, condition/0, event/0]).

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
    ok = model:update(?MODULE, Key, Diff).


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
    lists:all(fun(#service{hosts = Hosts, ctx = Ctx}) ->
        lists:all(fun(Status) ->
            healthy == Status
        end, maps:values(maps:with(Hosts, maps:get(status, Ctx, #{}))))
    end, service:list()).


%%--------------------------------------------------------------------
%% @doc
%% Checks if all nodes of given service have reported healthy status
%% on last check.
%% @end
%%--------------------------------------------------------------------
-spec is_healthy(name()) -> boolean().
is_healthy(Service) ->
    case ?MODULE:get(Service) of
        {ok, #service{hosts = Hosts, ctx = Ctx}} ->
            lists:all(fun(Status) ->
                healthy == Status
            end, maps:values(maps:with(Hosts, maps:get(status, Ctx, #{}))));
        _Error -> false
    end.


%%--------------------------------------------------------------------
%% @doc @equiv apply(Service, Action, Ctx, undefined)
%% @end
%%--------------------------------------------------------------------
-spec apply(Service :: name(), Action :: action(), Ctx :: step_ctx()) ->
    ok | {error, _}.
apply(Service, Action, Ctx) ->
    apply(Service, Action, Ctx, undefined).


%%--------------------------------------------------------------------
%% @doc Executes the service action and notifies about the process.
%% @end
%%--------------------------------------------------------------------
-spec apply(Service :: name(), Action :: action(), Ctx :: step_ctx(), Notify :: notify()) ->
    ok | {error, _}.
apply(Service, Action, Ctx, Notify) ->
    service_utils:notify(#action_begin{service = Service, action = Action}, Notify),
    Result = try
        Steps = service_utils:get_steps(Service, Action, Ctx),

        service_utils:notify(#action_steps_count{
            service = Service, action = Action, count = length(Steps)
        }, Notify),

        ?debug("Execution of ~tp:~tp requires following steps:~n~ts",
            [Service, Action, service_utils:format_steps(Steps, "")]),
        apply_steps(Steps, Notify)
    catch
        throw:{error, _} = Error -> Error;
        Type:Error ->
            ?error_stacktrace("Error executing action ~p:~p: ~p:~p",
                [Service, Action, Type, Error]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end,
    % If one of the steps failed, the action Result is {error, {Module, Function, Status}.
    % Result might of different format if steps resolution itself failed.
    service_utils:notify(#action_end{
        service = Service, action = Action, result = Result
    }, Notify),
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
    Ctx :: service:step_ctx()) -> TaskId :: service_executor:task_id().
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
    Ctx :: service:step_ctx()) -> Results :: service_executor:results() | {error, _}.
apply_sync(Service, Action, Ctx) ->
    apply_sync(Service, Action, Ctx, infinity).


%%--------------------------------------------------------------------
%% @doc Evaluates the service action synchronously with a timeout and returns
%% the results.
%% @end
%%--------------------------------------------------------------------
-spec apply_sync(Service :: service:name(), Action :: service:action(),
    Ctx :: service:step_ctx(), timeout()) ->
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
        {error, _} = Error2 -> Error2
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
%% @doc Returns whether a service is deployed on given host.
%% @end
%%--------------------------------------------------------------------
-spec has_host(name(), host()) -> boolean().
has_host(Service, Host) ->
    lists:member(Host, get_hosts(Service)).


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


-spec register_healthcheck(Service :: name(), Ctx :: step_ctx()) -> ok.
register_healthcheck(Service, Ctx) ->
    Period = onepanel_env:get(services_check_period),
    Module = service:get_module(Service),
    Name = healthcheck_name(Service, Ctx),

    Condition = fun() ->
        case (catch Module:status(Ctx)) of
            healthy -> false;
            unhealthy -> false;
            _ -> true
        end
    end,

    Action = fun() ->
        ?critical("Service ~ts is not running. Restarting...", [Name]),
        Results = service:apply_sync(Service, resume, Ctx#{hosts => [hosts:self()]}),
        case service_utils:results_contain_error(Results) of
            {true, Error} ->
                ?critical("Failed to restart service ~ts due to:~n~tp",
                    [Name, Error]);
            false -> ok
        end
    end,

    onepanel_cron:add_job(Name, Action, Period, Condition).


-spec deregister_healthcheck(service:name(), #{id => ceph:id(), _ => _}) -> ok.
deregister_healthcheck(Service, Ctx) ->
    onepanel_cron:remove_job(healthcheck_name(Service, Ctx)).


%% @private
-spec healthcheck_name(service:name(), #{id => ceph:id(), _ => _}) -> binary().
healthcheck_name(Service, #{id := Id}) ->
    str_utils:format_bin("~tp (id ~tp)", [Service, Id]);
healthcheck_name(Service, _) ->
    str_utils:format_bin("~tp", [Service]).


%%--------------------------------------------------------------------
%% @doc Returns the "ctx" field of a service model.
%% Verbose typespec to make up for the lack of different models/records
%% in the service model.
%% @end
%%--------------------------------------------------------------------
-spec get_ctx
    (?SERVICE_OPW) -> service_op_worker:model_ctx() | {error, _};
    (?SERVICE_OZW) -> service_oz_worker:model_ctx() | {error, _};
    (?SERVICE_OP) -> service_oneprovider:model_ctx() | {error, _};
    (?SERVICE_OZ) -> service_onezone:model_ctx() | {error, _};
    (?SERVICE_CM) -> service_cluster_manager:model_ctx() | {error, _};
    (?SERVICE_LE) -> service_letsencrypt:model_ctx() | {error, _};
    (?SERVICE_CB) -> service_couchbase:model_ctx() | {error, _};
    (?SERVICE_CEPH) -> service_ceph:model_ctx() | {error, _};
    (?SERVICE_CEPH_MON) -> service_ceph_mon:model_ctx() | {error, _};
    (?SERVICE_CEPH_MGR) -> service_ceph_mgr:model_ctx() | {error, _};
    (?SERVICE_CEPH_OSD) -> service_ceph_osd:model_ctx() | {error, _};
    % #service model is not created for service_onepanel module
    (?SERVICE_PANEL) -> ?ERR_DOC_NOT_FOUND | {error, _}.
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
    when Diff :: map() | fun((service:model_ctx()) -> service:model_ctx()).
update_ctx(Service, Diff) when is_map(Diff) ->
    update_ctx(Service, fun(Ctx) ->
        maps:merge(Ctx, Diff)
    end);

update_ctx(Service, Diff) when is_function(Diff, 1) ->
    service:update(Service, fun(#service{ctx = Ctx} = S) ->
        S#service{ctx = Diff(Ctx)}
    end).


-spec store_in_ctx(Service :: name(), Keys :: kv_utils:path(), Value :: term()) ->
    ok | no_return().
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
-spec apply_steps(Steps :: [step()], Notify :: notify()) ->
    ok | {error, {module(), Function :: atom(), service_executor:hosts_results()}}.
apply_steps([], _Notify) ->
    ok;

apply_steps([#step{} = Step | StepsTail], Notify) ->
    case resolve_hosts(Step) of
        #step{hosts = []} ->
            apply_steps(StepsTail, Notify);
        #step{
            module = Module, function = Function, hosts = Hosts,
            args = Args, attempts = Attempts, retry_delay = Delay
        } = StepWithHosts ->
            Nodes = nodes:service_to_nodes(?APP_NAME, Hosts),
            service_utils:notify(#step_begin{module = Module, function = Function}, Notify),

            Results = onepanel_rpc:call(Nodes, Module, Function, Args),
            Status = service_utils:partition_results(Results),

            service_utils:notify(#step_end{
                module = Module, function = Function, good_bad_results = Status
            }, Notify),

            case {Status, Attempts} of
                {{_, []}, _} -> apply_steps(StepsTail, Notify);
                {{_, _}, 1} -> {error, {Module, Function, Status}};
                {{_, _}, _} ->
                    timer:sleep(Delay),
                    apply_steps([StepWithHosts#step{attempts = Attempts - 1} | StepsTail], Notify)
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Calculates host te be used for executing a step.
%% First available (non-undefined) source is used:
%% - #step.hosts field
%% - #step.ctx hosts key
%% - hosts:all(Service)
%%
%% After obtaining the steps list, the selection mode is applied.
%%--------------------------------------------------------------------
-spec resolve_hosts(#step{}) -> #step{}.
resolve_hosts(#step{hosts = undefined, ctx = #{hosts := Hosts}} = Step) ->
    resolve_hosts(Step#step{hosts = Hosts});

resolve_hosts(#step{hosts = undefined, service = Service} = Step) ->
    case hosts:all(Service) of
        [] ->
            % do not silently skip steps because of empty list in service model,
            % unless it is explicitly given in step ctx or hosts field.
            throw(?ERROR_NO_SERVICE_NODES(Service));
        Hosts ->
            resolve_hosts(Step#step{hosts = Hosts})
    end;

resolve_hosts(#step{hosts = []} = Step) ->
    Step;

resolve_hosts(#step{hosts = Hosts, verify_hosts = true} = Step) ->
    ok = onepanel_utils:ensure_known_hosts(Hosts),
    resolve_hosts(Step#step{verify_hosts = false});

resolve_hosts(#step{hosts = Hosts, ctx = Ctx, selection = any} = Step) ->
    Host = lists_utils:random_element(Hosts),
    resolve_hosts(Step#step{hosts = [Host], selection = all,
        ctx = Ctx#{rest => lists:delete(Host, Hosts), all => Hosts}});

resolve_hosts(#step{hosts = Hosts, ctx = Ctx, selection = first} = Step) ->
    resolve_hosts(Step#step{hosts = [hd(Hosts)],
        ctx = Ctx#{rest => tl(Hosts), all => Hosts}, selection = all});

resolve_hosts(#step{hosts = Hosts, ctx = Ctx, selection = rest} = Step) ->
    resolve_hosts(Step#step{hosts = tl(Hosts),
        ctx = Ctx#{first => hd(Hosts), all => Hosts}, selection = all});

resolve_hosts(#step{} = Step) ->
    Step.
