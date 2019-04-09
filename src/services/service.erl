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

-define(DEFAULT_STATUS, stopped).

%% Model behaviour callbacks
-export([get_fields/0, get_record_version/0, seed/0, upgrade/2, create/1,
    save/1, update/2, get/1, exists/1, delete/1, list/0]).

%% API
-export([apply/3, apply/4, apply_async/3,
    apply_sync/3, apply_sync/4, get_results/1, get_results/2, abort_task/1,
    exists_task/1]).
-export([register_healthcheck/1]).
-export([get_status/2, update_status/2, update_status/3, all_healthy/0,
    healthy/1]).
-export([get_module/1, get_hosts/1, add_host/2]).
-export([get_ctx/1, update_ctx/2]).

% @formatter:off
-type name() :: ?SERVICE_OZ | ?SERVICE_OP |
    ?SERVICE_OPW | ?SERVICE_OZW | ?SERVICE_CW |
    ?SERVICE_CM | ?SERVICE_CB | ?SERVICE_PANEL |
    ?SERVICE_LE.
-type action() :: atom().
-type notify() :: pid() | undefined.
-type host() :: string().
-type step() :: #step{} | #steps{}.
-type condition() :: fun((ctx()) -> boolean()).
-type stage() :: action_begin | action_end | step_begin | step_end.
-type record() :: #service{}.

-type status() :: healthy | unhealthy | stopped | missing.
% @formatter:on


%% ctx/0 is used as:
%% - data field in #service{} record, for storing persistent service information
%% - argument for get_steps functions and, by default, each step function invoked
%%
%% Common keys used in the step arguments:
%% 'hosts' - list of hosts on which step should be performed, unless specified
%%           explicitly in #step.hosts
-type ctx() :: map().


-export_type([name/0, action/0, status/0, ctx/0, notify/0, host/0, step/0, condition/0,
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
    ?throw_error(?ERR_NOT_SUPPORTED).


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
        onepanel_maps:store([status, Host], Status, Ctx)
    end),
    Status.


%%--------------------------------------------------------------------
%% @doc
%% Returns cached service status.
%% This cache is updated on each start/stop operation
%% and by onepanel_cron periodically invoking ServiceModule:status/1.
%% @end
%%--------------------------------------------------------------------
-spec get_status(name(), host()) -> status() | #error{}.
get_status(Service, Host) ->
    case get_ctx(Service) of
        Ctx when is_map(Ctx) -> onepanel_maps:get([status, Host], Ctx);
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Checks if all deployed services have reported healthy status
%% on last check.
%% @end
%%--------------------------------------------------------------------
-spec all_healthy() -> boolean().
all_healthy() ->
    lists:all(fun(#service{ctx = Ctx}) ->
        lists:all(fun({_Host, Status}) ->
            healthy == Status
        end, maps:to_list(maps:get(status, Ctx, #{})))
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
    case service:get(Service) of
        {ok, #service{hosts = Hosts}} -> Hosts;
        #error{} -> []
    end.


%%--------------------------------------------------------------------
%% @doc Adds host to a list of hosts where provided service is deployed.
%% @end
%%--------------------------------------------------------------------
-spec add_host(Service :: name(), Host :: host()) -> ok.
add_host(Service, Host) ->
    ?MODULE:update(Service, fun(#service{hosts = Hosts, ctx = Ctx} = S) ->
        NewStatus = case maps:get(status, Ctx, #{}) of
            #{Host := _} = Status -> Status;
            Status -> Status#{Host => ?DEFAULT_STATUS}
        end,
        S#service{
            hosts = lists:usort([Host | Hosts]),
            % ensure statuses map contains all hosts
            ctx = Ctx#{status => NewStatus}
        }
    end).


-spec register_healthcheck(Service :: name()) -> ok.
register_healthcheck(Service) ->
    Module = get_module(Service),
    Condition = fun() ->
        case (catch Module:status(#{})) of
            healthy -> false;
            unhealthy -> false;
            _ -> true
        end
    end,

    Action = fun() ->
        ?critical("Service ~p is not running. Restarting...", [Service]),
        Results = apply_sync(Service, resume,
            #{hosts => [hosts:self()]}),
        case service_utils:results_contain_error(Results) of
            {true, Error} ->
                % @fixme improve error formatting
                ?critical("Failed to restart service ~p due to:~n~p",
                    [Service, rest_replier:format_error(error, Error)]);
            false -> ok
        end
    end,

    Period = onepanel_env:get(services_check_period),

    onepanel_cron:add_job(Service, Action, Period, Condition).


%%--------------------------------------------------------------------
%% @doc Returns the "ctx" field of a service model.
%% @end
%%--------------------------------------------------------------------
-spec get_ctx(name()) -> ctx() | #error{} | no_return().
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

    Nodes = nodes:service_to_nodes(?APP_NAME, Hosts),
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
