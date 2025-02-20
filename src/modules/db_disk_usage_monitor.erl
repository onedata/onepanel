%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module handles monitoring of DB disk usage across the cluster.
%%% @end
%%%--------------------------------------------------------------------
-module(db_disk_usage_monitor).
-author("Bartosz Walkowicz").

-include("names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/onedata.hrl").

-export([
    restart_periodic_check/0,
    run_periodic_check/0,
    check_usage_on_host/1
]).

-ifdef(TEST).
%% Export for unit testing
-export([
    parse_du_cmd_output/1,
    parse_df_cmd_output/1
]).
-endif.

-record(usage_info, {
    %% status is first because of sorting — DO NOT CHANGE!
    status :: status(),
    host :: service:host(),
    db_root_dir_size = undefined :: non_neg_integer() | undefined,
    available_disk_size = undefined :: non_neg_integer() | undefined,
    usage = undefined :: float() | undefined %% [0..1]
}).
-type usage_info() :: #usage_info{}.
-type circuit_breaker_state() :: open | closed.


-define(ROOT_DIR, application:get_env(?APP_NAME, db_root_dir, "/opt/couchbase")).

-define(CRON_JOB_NAME, ?MODULE).
-define(PERIODIC_CHECK_INTERVAL, timer:seconds(application:get_env(
    ?APP_NAME, db_disk_usage_check_interval_seconds, 600  %% 10 minutes
))).

-define(WARNING_THRESHOLD, application:get_env(?APP_NAME, db_disk_usage_warning_threshold, 0.45)).
-define(ALERT_THRESHOLD, application:get_env(?APP_NAME, db_disk_usage_alert_threshold, 0.75)).
-define(CIRCUIT_BREAKER_ACTIVATION_THRESHOLD, application:get_env(
    ?APP_NAME, db_disk_usage_circuit_breaker_activation_threshold, 0.9
)).

-define(CMD_OUTPUT_TRIM_THRESHOLD, 997).

-define(STATUS_DISK_CRITICALLY_LOW, 0).
-define(STATUS_ALERT, 1).
-define(STATUS_WARNING, 2).
-define(STATUS_OK, 3).
-define(STATUS_RPC_FAILED, 4).

% represents the severity of disk space availability status, 0 being the most critical one
-type status() :: 0 | 1 | 2 | 3 | 4.

%%%===================================================================
%%% API
%%%===================================================================


-spec restart_periodic_check() -> ok | no_return().
restart_periodic_check() ->
    true = service_onepanel:run_on_master_node(fun() ->
        % ensure the previous periodic sync job is aborted
        abort_periodic_check(),

        ?info("Scheduling periodic cluster-wide DB disk usage check"),
        ok = onepanel_cron:add_job(
            ?CRON_JOB_NAME,
            fun() -> run_periodic_check() end,
            ?PERIODIC_CHECK_INTERVAL
        ),
        true
    end),
    ok.


-spec check_usage_on_host(service:host()) -> usage_info() | no_return().
check_usage_on_host(Host) ->
    DBRootDirSize = get_db_root_dir_size(),
    AvailableDiskSize = get_available_disk_size(),
    Usage = DBRootDirSize / (DBRootDirSize + AvailableDiskSize),

    [{Status, Threshold} | _]  = lists:dropwhile(fun({_, ThresholdValue}) -> Usage < ThresholdValue end, [
        {?STATUS_DISK_CRITICALLY_LOW, ?CIRCUIT_BREAKER_ACTIVATION_THRESHOLD},
        {?STATUS_ALERT, ?ALERT_THRESHOLD},
        {?STATUS_WARNING, ?WARNING_THRESHOLD},
        {?STATUS_OK, 0.0}
    ]),
    application:get_env(?APP_NAME, db_disk_monitor_verbose_logs, false) andalso ?info(
        "Disk usage check on ~ts:"
        "~n> Usage percent ~.2f"
        "~n> Threshold percent ~.2f"
        "~n> Status ~ts"
        "~n> STATUS_DISK_CRITICALLY_LOW ~.2f"
        "~n> STATUS_ALERT ~.2f"
        "~n> STATUS_WARNING ~.2f"
        "~n> STATUS_OK ~.2f",
        [
            Host,
            100 * Usage,
            100 * Threshold,
            status_to_label(Status),
            ?CIRCUIT_BREAKER_ACTIVATION_THRESHOLD,
            ?ALERT_THRESHOLD,
            ?WARNING_THRESHOLD,
            0.0
        ]
    ),
    #usage_info{
        status = Status,
        host = Host,
        db_root_dir_size = DBRootDirSize,
        available_disk_size = AvailableDiskSize,
        usage = Usage
    }.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec abort_periodic_check() -> ok.
abort_periodic_check() ->
    % remove any previous periodic check jobs across the cluster
    case utils:rpc_multicall(service_onepanel:get_nodes(), onepanel_cron, remove_job, [?CRON_JOB_NAME]) of
        {_, []} ->
            ok;
        {_, BadNodes} ->
            ?error(?autoformat_with_msg("Failed to remove ~tp cron job on nodes", [?CRON_JOB_NAME], BadNodes))
    end.


%% @private
-spec run_periodic_check() -> boolean().
run_periodic_check() ->
    try
        Hosts = service_couchbase:get_hosts(),
        ?debug("Running periodic db disk usage check for hosts: ~tp", [Hosts]),

        Results = lists:map(fun(Host) ->
            Node = nodes:service_to_node(?APP_NAME, Host),
            case rpc:call(Node, db_disk_usage_monitor, check_usage_on_host, [Host]) of
                #usage_info{} = UsageInfo ->
                    UsageInfo;
                {badrpc, ErrorReason} ->
                    ?error(?autoformat_with_msg(
                        "Failed to check db usage:", [Host, ErrorReason]
                    )),
                    #usage_info{
                        status = ?STATUS_RPC_FAILED,
                        host = Host
                    }
            end
        end, Hosts),

        CircuitBreakerState = get_service_circuit_breaker_state(),
        NewCircuitBreakerState = handle_state_transition(CircuitBreakerState, Results),
        set_service_circuit_breaker_state(NewCircuitBreakerState),
        true
    catch Class:Reason:Stacktrace ->
        ?error_exception(Class, Reason, Stacktrace),
        false
    end.


%% @private
-spec get_db_root_dir_size() -> non_neg_integer() | no_return().
get_db_root_dir_size() ->
    DuCmd = ["du", "--bytes", "--summarize", ?ROOT_DIR],
    parse_du_cmd_output(shell_utils:get_success_output(DuCmd)).


%% @private
-spec parse_du_cmd_output(binary()) -> non_neg_integer() | no_return().
parse_du_cmd_output(DuOutput) ->
    {match, [SizeStr]} = re:run(DuOutput, "^(?P<size>\\d+)\t.*$", [{capture, [size], list}]),
    Size = list_to_integer(SizeStr),
    Size =< 0 andalso ?warning(?autoformat_with_msg("Got an unexpected result from the du command", [DuOutput])),
    Size.


%% @private
-spec get_available_disk_size() -> non_neg_integer() | no_return().
get_available_disk_size() ->
    DfCmd = ["df", "--block-size", "1", "--output=avail", ?ROOT_DIR],
    parse_df_cmd_output(shell_utils:get_success_output(DfCmd)).


%% @private
-spec parse_df_cmd_output(binary()) -> non_neg_integer() | no_return().
parse_df_cmd_output(DfOutput) ->
    {match, [SizeStr]} = re:run(DfOutput, "^\s*Avail\n(?P<size>\\d+)$", [{capture, [size], list}]),
    Size = list_to_integer(SizeStr),
    Size < 0 andalso ?warning(?autoformat_with_msg("Got an unexpected result from the df command", [DfOutput])),
    Size.


%% @private
-spec handle_state_transition(circuit_breaker_state(), [usage_info()]) ->
    circuit_breaker_state().
handle_state_transition(CurrentState, UsageInfos) ->
    [#usage_info{status=WorstStatus} | _] = SortedUsageInfos = lists:sort(UsageInfos),
    handle_state_transition(CurrentState, WorstStatus, SortedUsageInfos).


%% @private
-spec handle_state_transition(circuit_breaker_state(), status(), [usage_info()]) -> circuit_breaker_state().
handle_state_transition(closed, ?STATUS_DISK_CRITICALLY_LOW, SortedUsageInfos) ->
    handle_status_logging(?STATUS_DISK_CRITICALLY_LOW, fun() ->
        ?emergency(
            "DB disk space is nearly exhausted! "
            "All services will now stop processing requests until the problem is resolved.~ts~n",
            [format_usage_info(SortedUsageInfos)]
        )
    end),
    open;
handle_state_transition(closed, ?STATUS_ALERT, SortedUsageInfos) ->
    handle_status_logging(?STATUS_ALERT, fun() ->
        ?alert(
            "DB disk usage is very high. Provide more space for the DB as soon as possible. When the usage "
            "reaches ~.2f%, all services will stop processing requests to prevent database corruption.~ts~n",
            [?CIRCUIT_BREAKER_ACTIVATION_THRESHOLD * 100, format_usage_info(SortedUsageInfos)]
        )
    end),
    closed;
handle_state_transition(closed, ?STATUS_WARNING, SortedUsageInfos) ->
    handle_status_logging(?STATUS_WARNING, fun() ->
        ?warning(
            "DB disk usage exceeded safe thresholds. "
            "Provide more space for the DB to ensure uninterrupted services.~ts~n",
            [format_usage_info(SortedUsageInfos)]
        )
    end),
    closed;
handle_state_transition(closed, ?STATUS_OK, SortedUsageInfos) ->
    handle_status_logging(?STATUS_OK, fun() ->
        ?info(
            "DB disk usage is within safe thresholds.~ts~n", [format_usage_info(SortedUsageInfos)]
        ) end
    ),
    closed;

handle_state_transition(open, ?STATUS_DISK_CRITICALLY_LOW, SortedUsageInfos) ->
    handle_status_logging(?STATUS_DISK_CRITICALLY_LOW, fun() ->
        ?emergency(
            "DB disk space is still critically low. All services remain stopped until the issue is resolved.~ts~n",
            [format_usage_info(SortedUsageInfos)]
        )
    end),
    open;
handle_state_transition(open, NewStatus, SortedUsageInfos) ->
    handle_status_logging(NewStatus, fun() ->
        ?notice(
            "DB disk usage has returned to acceptable levels. Services have resumed normal functionality.~ts~n",
            [format_usage_info(SortedUsageInfos)]
        )
    end),
    closed.


%% @private
-spec handle_status_logging(status(), fun(() -> term())) -> ok.
handle_status_logging(NewStatus, LogFun) ->
    case node_cache:get(worst_status, unknown) of
        NewStatus ->
            case application:get_env(?APP_NAME, db_disk_monitor_verbose_logs, false) of
                true -> LogFun();
                false -> utils:throttle(status_to_log_throttling_interval(NewStatus), LogFun)
            end;
        _ ->
            LogFun(),
            node_cache:put(worst_status, NewStatus)
    end.


%% @private
-spec status_to_log_throttling_interval(status()) -> non_neg_integer().
status_to_log_throttling_interval(?STATUS_DISK_CRITICALLY_LOW) -> timer:hours(1);
status_to_log_throttling_interval(?STATUS_ALERT) -> timer:hours(6);
status_to_log_throttling_interval(?STATUS_WARNING) -> timer:hours(12);
status_to_log_throttling_interval(?STATUS_OK) -> timer:hours(24);
status_to_log_throttling_interval(?STATUS_RPC_FAILED) -> timer:hours(24).


%% @private
-spec format_usage_info([usage_info()]) -> binary().
format_usage_info(SortedUsageInfos) ->
    str_utils:join_binary(lists:map(fun
        (#usage_info{
            status = Status,
            host = Host,
            db_root_dir_size = undefined,
            available_disk_size = undefined,
            usage =  undefined
        }) ->
        str_utils:format_bin(
            "~n> Host: ~ts"
            "~n> Status: ~ts"
            "~n> DB root directory path: ~ts"
            "~n> DB root directory size: ???"
            "~n> Available disk size: ???"
            "~n> Usage percent: ???",
            [
                Host,
                status_to_label(Status),
                ?ROOT_DIR
            ]
        );
        (#usage_info{
            status = Status,
            host = Host,
            db_root_dir_size = DBRootDirSize,
            available_disk_size = AvailableDiskSize,
            usage =  Usage
        }) ->
            str_utils:format_bin(
                "~n> Host: ~ts"
                "~n> Status: ~ts"
                "~n> DB root directory path: ~ts"
                "~n> DB root directory size: ~ts"
                "~n> Available disk size: ~ts"
                "~n> Usage percent: ~.2f%",
                [
                    Host,
                    status_to_label(Status),
                    ?ROOT_DIR,
                    str_utils:format_byte_size(DBRootDirSize),
                    str_utils:format_byte_size(AvailableDiskSize),
                    100 * Usage
                ]
            )
    end, SortedUsageInfos), <<"\n---------------">>).


%% @private
-spec status_to_label(status()) -> string().
status_to_label(?STATUS_DISK_CRITICALLY_LOW) -> "DISK SPACE CRITICALLY LOW";
status_to_label(?STATUS_ALERT) -> "ALERT";
status_to_label(?STATUS_WARNING) -> "WARNING";
status_to_label(?STATUS_OK) -> "OK";
status_to_label(?STATUS_RPC_FAILED) -> "RPC_FAILED".


%% @private
-spec set_service_circuit_breaker_state(circuit_breaker_state()) -> ok.
set_service_circuit_breaker_state(State) ->
    PanelNodes = nodes:all(?SERVICE_PANEL),
    ?catch_exceptions(onepanel_env:set(PanelNodes, service_circuit_breaker_state, State, ?APP_NAME)),
    ClusterType = onepanel_env:get_cluster_type(),
    case ClusterType of
        ?ONEZONE ->
            lists:foreach(fun(Node) ->
                ?catch_exceptions(oz_worker_rpc:circuit_breaker_toggle(Node, State))
            end, service_oz_worker:get_nodes());
        ?ONEPROVIDER ->
            lists:foreach(fun(Node) ->
                ?catch_exceptions(op_worker_rpc:circuit_breaker_toggle(Node, State))
            end, service_op_worker:get_nodes())
    end.


%% @private
-spec get_service_circuit_breaker_state() -> circuit_breaker_state().
get_service_circuit_breaker_state() ->
    onepanel_env:get(service_circuit_breaker_state, ?APP_NAME, closed).
