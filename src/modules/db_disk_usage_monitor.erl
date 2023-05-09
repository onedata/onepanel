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
-feature(maybe_expr, enable).
-module(db_disk_usage_monitor).
-author("Bartosz Walkowicz").

-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    restart_periodic_check/0,
    run_on_master/1,
    run_periodic_check/0
]).

-record(usage_info, {
    db_root_dir_size :: non_neg_integer(),
    available_disk_size :: non_neg_integer(),
    usage :: float()  %% [0..1]
}).
-type usage_info() :: #usage_info{}.


-define(ROOT_DIR, application:get_env(?APP_NAME, db_root_dir, "/opt/couchbase")).

-define(CRON_JOB_NAME, ?MODULE).
-define(PERIODIC_CHECK_INTERVAL, timer:seconds(application:get_env(
    ?APP_NAME, db_disk_usage_check_interval_seconds, 600  %% 10 minutes
))).

-define(WARNING_THRESHOLD, application:get_env(?APP_NAME, db_disk_usage_warning_threshold, 0.45)).
-define(ALERT_THRESHOLD, application:get_env(?APP_NAME, db_disk_usage_alert_threshold, 0.75)).
-define(CIRCUIT_BREAKER_ACTIVATION_THRESHOLD_THRESHOLD, application:get_env(
    ?APP_NAME, db_disk_usage_circuit_breaker_activation_threshold, 0.9
)).

-define(CMD_OUTPUT_TRIM_THRESHOLD, 997).


%%%===================================================================
%%% API
%%%===================================================================


-spec restart_periodic_check() -> ok | no_return().
restart_periodic_check() ->
    true = run_on_master(fun() ->
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


-spec run_on_master(fun(() -> boolean())) -> boolean() | no_return().
run_on_master(Fun) ->
    case get_master_node() of
        Self when node() =:= Self ->
            try
                Fun()
            catch Class:Reason:Stacktrace ->
                ?error_exception(Class, Reason, Stacktrace),
                false
            end;
        MasterNode ->
            case rpc:call(MasterNode, ?MODULE, ?FUNCTION_NAME, [Fun]) of
                Result when is_boolean(Result) -> Result
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec get_master_node() -> node().
get_master_node() ->
    % @TODO VFS-6085 rework the master choice when onepanel cluster is resizeable
    hd(lists:sort(service_onepanel:get_nodes())).


%% @private
-spec abort_periodic_check() -> ok.
abort_periodic_check() ->
    % remove any previous periodic check jobs across the cluster
    utils:rpc_multicall(service_onepanel:get_nodes(), onepanel_cron, remove_job, [?CRON_JOB_NAME]),
    ok.


%% @private
-spec run_periodic_check() -> boolean().
run_periodic_check() ->
    try
        Hosts = service_couchbase:get_hosts(),
        ?debug("Running periodic db disk usage check for hosts: ~p", [Hosts]),

        Nodes = nodes:service_to_nodes(?APP_NAME, Hosts),
        Results = erpc:multicall(Nodes, fun check_usage_on_host/0),
        CircuitBreakerState = onepanel_env:get(service_circuit_breaker_state, ?APP_NAME, closed),
        handle_offenders(CircuitBreakerState, group_offenders(lists:zip(Hosts, Results))),

        true
    catch Class:Reason:Stacktrace ->
        ?error_exception(Class, Reason, Stacktrace),
        false
    end.


%% @private
-spec check_usage_on_host() -> usage_info() | no_return().
check_usage_on_host() ->
    DBRootDirSize = get_db_root_dir_size(),
    AvailableDiskSize = get_available_disk_size(),

    #usage_info{
        db_root_dir_size = DBRootDirSize,
        available_disk_size = AvailableDiskSize,
        usage = DBRootDirSize / (DBRootDirSize + AvailableDiskSize)
    }.


%% @private
-spec get_db_root_dir_size() -> non_neg_integer() | no_return().
get_db_root_dir_size() ->
    DuCmd = ["du", "--bytes", "--summarize", ?ROOT_DIR],
    parse_du_cmd_output(shell_utils:get_success_output(DuCmd)).


%% @private
-spec parse_du_cmd_output(binary()) -> non_neg_integer() | no_return().
parse_du_cmd_output(DuOutput) ->
    {match, [SizeStr]} = re:run(DuOutput, "^(?P<size>\\d+)\t.*$", [{capture, [size], list}]),
    list_to_integer(SizeStr).


%% @private
-spec get_available_disk_size() -> non_neg_integer() | no_return().
get_available_disk_size() ->
    DfCmd = ["df", "--block-size", "1", "--output=avail", ?ROOT_DIR],
    parse_df_cmd_output(shell_utils:get_success_output(DfCmd)).


%% @private
-spec parse_df_cmd_output(binary()) -> non_neg_integer() | no_return().
parse_df_cmd_output(DfOutput) ->
    {match, [SizeStr]} = re:run(DfOutput, "^\s*Avail\n(?P<size>\\d+)$", [{capture, [size], list}]),
    list_to_integer(SizeStr).


%% @private
-spec group_offenders([{service:host(), {ok, usage_info()} | term()}]) ->
    #{atom() => [{service:host(), usage_info()}]}.
group_offenders(ResultPerHost) ->
    ThresholdsByPriority = [
        {circuit_breaker_activation_threshold, ?CIRCUIT_BREAKER_ACTIVATION_THRESHOLD_THRESHOLD},
        {alert_threshold, ?ALERT_THRESHOLD},
        {warning_threshold, ?WARNING_THRESHOLD}
    ],

    lists:foldl(fun
        ({Host, {ok, UsageInfo = #usage_info{usage = Usage}}}, Acc) ->
            case find_first_exceeded_threshold(Usage, ThresholdsByPriority) of
                {ok, ThresholdKey} ->
                    Offender = {Host, UsageInfo},

                    maps:update_with(ThresholdKey, fun(Offenders) ->
                        [Offender | Offenders]
                    end, [Offender], Acc);
                error ->
                    Acc
            end;

        ({Host, ErrorReason}, Acc) ->
            ?error("Failed to check db usage:~s", [?autoformat([Host, ErrorReason])]),
            Acc
    end, #{}, ResultPerHost).


%% @private
-spec find_first_exceeded_threshold(float(), [{atom(), float()}]) -> {ok, atom()} | error.
find_first_exceeded_threshold(_Usage, []) ->
    error;
find_first_exceeded_threshold(Usage, [{ThresholdKey, ThresholdValue} | _]) when Usage >= ThresholdValue ->
    {ok, ThresholdKey};
find_first_exceeded_threshold(Usage, [_ | ThresholdsByPriority]) ->
    find_first_exceeded_threshold(Usage, ThresholdsByPriority).


%% @private
-spec handle_offenders(open | closed, #{atom() => [{service:host(), usage_info()}]}) ->
    ok.
handle_offenders(closed, OffendersPerThreshold) when map_size(OffendersPerThreshold) == 0 ->
    ok;

handle_offenders(CircuitBreakerState = closed, OffendersPerThreshold = #{warning_threshold := Offenders}) ->
    ?warning("DB disk usage exceeded safe thresholds. Provide more space for the DB to ensure uninterrupted services.~s", [
        format_offenders(Offenders)
    ]),
    handle_offenders(CircuitBreakerState, maps:remove(warning_threshold, OffendersPerThreshold));

handle_offenders(CircuitBreakerState = closed, OffendersPerThreshold = #{alert_threshold := Offenders}) ->
    ?alert(
        "DB disk usage is very high. Provide more space for the DB as soon as possible. "
        "When the usage reaches ~p%, all services will stop processing requests to prevent database corruption.~s",
        [?CIRCUIT_BREAKER_ACTIVATION_THRESHOLD_THRESHOLD * 100, format_offenders(Offenders)]
    ),
    handle_offenders(CircuitBreakerState, maps:remove(alert_threshold, OffendersPerThreshold));

handle_offenders(closed, #{circuit_breaker_activation_threshold := Offenders}) ->
    ?emergency(
        "DB disk space is nearly exhausted! All services will now stop processing requests until the problem is resolved.~s",
        [format_offenders(Offenders)]
    ),
    set_service_circuit_breaker_state(open);

handle_offenders(open, #{circuit_breaker_activation_threshold := _Offenders}) ->
    % service_circuit_breaker must have been opened on previous check
    ok;

handle_offenders(open, OffendersPerThreshold) ->
    ?notice("DB disk space is no longer near exhaustion. All services will now resume processing requests."),

    set_service_circuit_breaker_state(closed),
    handle_offenders(closed, OffendersPerThreshold).


%% @private
-spec format_offenders([{service:host(), usage_info()}]) -> binary().
format_offenders(Offenders) ->
    str_utils:join_binary(lists:map(fun({Host, UsageInfo}) ->
        str_utils:format(
            "~n~n> Host: ~s~n> DB root directory size: ~s~n> Available disk size: ~s~n> Usage percent: ~p%",
            [
                Host,
                str_utils:format_byte_size(UsageInfo#usage_info.db_root_dir_size),
                str_utils:format_byte_size(UsageInfo#usage_info.available_disk_size),
                100 * UsageInfo#usage_info.usage
            ]
        )
    end, Offenders)).


%% @private
-spec set_service_circuit_breaker_state(open | closed) -> ok.
set_service_circuit_breaker_state(State) ->
    PanelNodes = nodes:all(?SERVICE_PANEL),
    onepanel_env:set(PanelNodes, service_circuit_breaker_state, State, ?APP_NAME),
    ok.
