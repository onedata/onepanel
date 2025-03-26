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


-define(ROOT_DIR, onepanel:get_env(db_root_dir, "/opt/couchbase")).

-define(CRON_JOB_NAME, ?MODULE).

-define(PERIODIC_CHECK_INTERVAL, timer:seconds(onepanel:get_env(db_disk_usage_check_interval_seconds, 600))).  % 10 min

-define(WARNING_THRESHOLD, onepanel:get_env(db_disk_usage_warning_threshold, 0.45)).
-define(ALERT_THRESHOLD, onepanel:get_env(db_disk_usage_alert_threshold, 0.75)).
-define(CIRCUIT_BREAKER_ACTIVATION_THRESHOLD, onepanel:get_env(db_disk_usage_circuit_breaker_activation_threshold, 0.9)).

-define(VERBOSE_LOGS_ENABLED, onepanel:get_env(db_disk_monitor_verbose_logs, false)).

-define(CMD_OUTPUT_TRIM_THRESHOLD, 997).

% represents the severity of disk space availability status, 0 being the most critical one
-type status() :: 0 | 1 | 2 | 3 | 4.

-define(STATUS_DISK_CRITICALLY_LOW, 0).
-define(STATUS_ALERT, 1).
-define(STATUS_WARNING, 2).
-define(STATUS_OK, 3).
-define(STATUS_RPC_FAILED, 4).

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

    [{Status, Threshold} | _] = lists:dropwhile(fun({_, ThresholdValue}) -> Usage < ThresholdValue end, [
        {?STATUS_DISK_CRITICALLY_LOW, ?CIRCUIT_BREAKER_ACTIVATION_THRESHOLD},
        {?STATUS_ALERT, ?ALERT_THRESHOLD},
        {?STATUS_WARNING, ?WARNING_THRESHOLD},
        {?STATUS_OK, 0.0}
    ]),
    ?VERBOSE_LOGS_ENABLED andalso ?info(
        "Disk usage check on ~ts:"
        "~n> Usage percent: ~.2f%"
        "~n> Status: ~ts (>= ~.2f%)"
        "~n> Threshold settings: ~.2f%, ~.2f%, ~.2f%",
        [
            Host,
            100 * Usage,
            status_to_label(Status),
            100 * Threshold,
            100 * ?CIRCUIT_BREAKER_ACTIVATION_THRESHOLD,
            100 * ?ALERT_THRESHOLD,
            100 * ?WARNING_THRESHOLD
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

        PreviousCbState = get_service_circuit_breaker_state(),
        NewCircuitBreakerState = handle_state_transition(PreviousCbState, Results),
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
handle_state_transition(PreviousCbState, UsageInfos) ->
    [#usage_info{status = WorstStatus} | _] = SortedUsageInfos = lists:sort(UsageInfos),
    handle_state_transition(PreviousCbState, WorstStatus, SortedUsageInfos).


%% @private
-spec handle_state_transition(circuit_breaker_state(), status(), [usage_info()]) -> circuit_breaker_state().
handle_state_transition(closed, ?STATUS_DISK_CRITICALLY_LOW, SortedUsageInfos) ->
    log_summary_with_throttling(?STATUS_DISK_CRITICALLY_LOW, fun() ->
        ?emergency(
            "DB disk space is nearly exhausted! "
            "All services will now stop processing requests until the problem is resolved.~ts",
            [format_summary(SortedUsageInfos)]
        )
    end),
    open;
handle_state_transition(closed, ?STATUS_ALERT, SortedUsageInfos) ->
    log_summary_with_throttling(?STATUS_ALERT, fun() ->
        ?alert(
            "DB disk usage is very high. Provide more space for the DB as soon as possible. When the usage "
            "reaches ~.2f%, all services will stop processing requests to prevent database corruption.~ts",
            [?CIRCUIT_BREAKER_ACTIVATION_THRESHOLD * 100, format_summary(SortedUsageInfos)]
        )
    end),
    closed;
handle_state_transition(closed, ?STATUS_WARNING, SortedUsageInfos) ->
    log_summary_with_throttling(?STATUS_WARNING, fun() ->
        ?warning(
            "DB disk usage exceeded safe thresholds. "
            "Provide more space for the DB to ensure uninterrupted services.~ts",
            [format_summary(SortedUsageInfos)]
        )
    end),
    closed;
handle_state_transition(closed, ?STATUS_OK, SortedUsageInfos) ->
    log_summary_with_throttling(?STATUS_OK, fun() ->
        ?info(
            "DB disk usage is within safe thresholds.~ts",
            [format_summary(SortedUsageInfos)]
        )
    end),
    closed;

handle_state_transition(open, ?STATUS_DISK_CRITICALLY_LOW, SortedUsageInfos) ->
    log_summary_with_throttling(?STATUS_DISK_CRITICALLY_LOW, fun() ->
        ?emergency(
            "DB disk space is still critically low. All services remain suspended until the issue is resolved.~ts",
            [format_summary(SortedUsageInfos)]
        )
    end),
    open;
handle_state_transition(open, CurrentWorstStatus, SortedUsageInfos) ->
    log_summary_with_throttling(CurrentWorstStatus, fun() ->
        ?notice(
            "DB disk usage has returned to acceptable levels. Services have resumed normal operation.~ts",
            [format_summary(SortedUsageInfos)]
        )
    end),
    closed.


%% @private
-spec log_summary_with_throttling(status(), fun(() -> term())) -> ok.
log_summary_with_throttling(CurrentWorstStatus, LogFun) ->
    % always log on status change
    CurrentWorstStatus /= node_cache:get(prev_worst_status, unknown) andalso utils:reset_throttle_interval(?MODULE),
    utils:throttle(?MODULE, status_to_summary_log_throttling_interval(CurrentWorstStatus), LogFun),
    node_cache:put(prev_worst_status, CurrentWorstStatus).


%% @private
-spec status_to_summary_log_throttling_interval(status()) -> non_neg_integer().
status_to_summary_log_throttling_interval(Status) ->
    VerboseLogsEnabled = ?VERBOSE_LOGS_ENABLED,
    case Status of
        _ when VerboseLogsEnabled -> 0;
        ?STATUS_DISK_CRITICALLY_LOW -> timer:hours(1);
        ?STATUS_ALERT -> timer:hours(6);
        ?STATUS_WARNING -> timer:hours(12);
        ?STATUS_OK -> timer:hours(24);
        % each failed RPC triggers an independent error log, so there is no need to log it in the summary often
        ?STATUS_RPC_FAILED -> timer:hours(24)
    end.


%% @private
-spec format_summary([usage_info()]) -> binary().
format_summary(SortedUsageInfos) ->
    FormattedUsageInfos = lists:map(fun(#usage_info{
        status = Status,
        host = Host,
        db_root_dir_size = DBRootDirSize,
        available_disk_size = AvailableDiskSize,
        usage = Usage
    }) ->
        str_utils:format_bin(
            "~n> Host: ~ts"
            "~n> Status: ~ts"
            "~n> DB root directory path: ~ts"
            "~n> DB root directory size: ~ts"
            "~n> Available disk size: ~ts"
            "~n> Usage percent: ~ts",
            [
                Host,
                status_to_label(Status),
                ?ROOT_DIR,
                utils:convert_defined(DBRootDirSize, fun str_utils:format_byte_size/1),
                utils:convert_defined(AvailableDiskSize, fun str_utils:format_byte_size/1),
                utils:convert_defined(Usage, fun(UsageRate) -> str_utils:format("~.2f%", [UsageRate * 100]) end)
            ]
        )
    end, SortedUsageInfos),
    % include the delimiter at the beginning and at the end
    str_utils:join_binary([<<"">>] ++ FormattedUsageInfos ++ [<<"">>], <<"\n-----------------------">>).


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
    ?log_all_exceptions(onepanel_env:set(PanelNodes, service_circuit_breaker_state, State, ?APP_NAME)),
    ClusterType = onepanel_env:get_cluster_type(),
    case ClusterType of
        ?ONEZONE ->
            lists:foreach(fun(Node) ->
                ?log_all_exceptions(oz_worker_rpc:circuit_breaker_toggle(Node, State))
            end, service_oz_worker:get_nodes());
        ?ONEPROVIDER ->
            lists:foreach(fun(Node) ->
                ?log_all_exceptions(op_worker_rpc:circuit_breaker_toggle(Node, State))
            end, service_op_worker:get_nodes())
    end.


%% @private
-spec get_service_circuit_breaker_state() -> circuit_breaker_state().
get_service_circuit_breaker_state() ->
    onepanel:get_env(service_circuit_breaker_state, closed).
