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

-type usage_info() :: #{
    db_root_dir_size := non_neg_integer(),
    available_disk_size := non_neg_integer(),
    usage := float()  %% [0..1]
}.


-define(ROOT_DIR, application:get_env(?APP_NAME, db_root_dir, "/opt/couchbase")).

-define(CRON_JOB_NAME, ?MODULE).
-define(PERIODIC_CHECK_INTERVAL, timer:seconds(application:get_env(
    ?APP_NAME, db_disk_usage_check_interval_seconds, 600  %% 10 minutes
))).

-define(WARNING_THRESHOLD, application:get_env(?APP_NAME, db_disk_usage_warning_threshold, 0.45)).
-define(ALERT_THRESHOLD, application:get_env(?APP_NAME, db_disk_usage_alert_threshold, 0.75)).
-define(EMERGENCY_THRESHOLD, application:get_env(?APP_NAME, db_disk_usage_emergency_threshold, 0.9)).

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
                % all internal functions in this module return a boolean -
                % crash in case of any problems with RPC
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
        handle_offenders(group_offenders(lists:zip(Hosts, Results))),

        true
    catch Class:Reason:Stacktrace ->
        ?error_exception(Class, Reason, Stacktrace),
        false
    end.


%% @private
-spec check_usage_on_host() -> {ok, usage_info()} | {error, term()}.
check_usage_on_host() ->
    maybe
        {ok, DBRootDirSize} ?= get_db_root_dir_size(),
        {ok, AvailableDiskSize} ?= get_available_disk_size(),

        UsageInfo = #{
            db_root_dir_size => DBRootDirSize,
            available_disk_size => AvailableDiskSize,
            usage => DBRootDirSize / (DBRootDirSize + AvailableDiskSize)
        },
        {ok, UsageInfo}
    end.


%% @private
-spec get_db_root_dir_size() -> {ok, non_neg_integer()} | {error, cmd_du_failed}.
get_db_root_dir_size() ->
    DuCmd = "du --bytes --summarize " ++ ?ROOT_DIR,
    DuOutput = os:cmd(DuCmd),

    case re:run(DuOutput, "^(?P<size>\\d+)\t.*$", [{capture, [size], list}]) of
        {match, [SizeStr]} ->
            {ok, list_to_integer(SizeStr)};
        nomatch ->
            log_cmd_failure(DuCmd, DuOutput),
            {error, cmd_du_failed}
    end.


%% @private
-spec get_available_disk_size() -> {ok, non_neg_integer()} | {error, cmd_df_failed}.
get_available_disk_size() ->
    DfCmd = "df --block-size 1 --output=avail " ++ ?ROOT_DIR,
    DfOutput = os:cmd(DfCmd),

    case re:run(DfOutput, "^\s*Avail\n(?P<size>\\d+)\n$", [{capture, [size], list}]) of
        {match, [SizeStr]} ->
            {ok, list_to_integer(SizeStr)};
        nomatch ->
            log_cmd_failure(DfCmd, DfOutput),
            {error, cmd_df_failed}
    end.


%% @private
-spec log_cmd_failure(string(), string()) -> ok.
log_cmd_failure(Cmd, CmdOutput) ->
    ?warning("Calling '~s' failed with:~n~s", [
        Cmd, string:slice(CmdOutput, 0, ?CMD_OUTPUT_TRIM_THRESHOLD)
    ]).


%% @private
-spec group_offenders([{service:host(), {ok, {ok, usage_info()} | {error, term()}} | term()}]) ->
    #{atom() => [{service:host(), usage_info()}]}.
group_offenders(ResultPerHost) ->
    Thresholds = [
        {emergency_threshold, ?EMERGENCY_THRESHOLD},
        {alert_threshold, ?ALERT_THRESHOLD},
        {warning_threshold, ?WARNING_THRESHOLD}
    ],

    lists:foldl(fun
        ({Host, {ok, {ok, UsageInfo = #{usage := Usage}}}}, Acc) ->
            case find_exceeded_threshold(Usage, Thresholds) of
                {ok, ThresholdKey} ->
                    Offender = {Host, UsageInfo},

                    maps:update_with(ThresholdKey, fun(Offenders) ->
                        [Offender | Offenders]
                    end, [Offender], Acc);
                error ->
                    Acc
            end;

        ({Host, {ok, ErrorReason}}, Acc) ->
            ?warning("Failed to check db usage:~s", [?autoformat([Host, ErrorReason])]),
            Acc;

        ({Host, ErrorReason}, Acc) ->
            ?warning("Failed to check db usage:~s", [?autoformat([Host, ErrorReason])]),
            Acc
    end, #{}, ResultPerHost).


%% @private
-spec find_exceeded_threshold(float(), [{atom(), float()}]) -> {ok, atom()} | error.
find_exceeded_threshold(_Usage, []) ->
    error;
find_exceeded_threshold(Usage, [{ThresholdKey, ThresholdValue} | _]) when Usage >= ThresholdValue ->
    {ok, ThresholdKey};
find_exceeded_threshold(Usage, [_ | Thresholds]) ->
    find_exceeded_threshold(Usage, Thresholds).


%% @private
-spec handle_offenders(#{atom() => [{service:host(), usage_info()}]}) -> ok.
handle_offenders(OffendersPerThreshold) ->
    CircuitBreakerStatus = onepanel_env:get(service_circuit_breaker, ?APP_NAME, disabled),

    case {maps:is_key(emergency_threshold, OffendersPerThreshold), CircuitBreakerStatus} of
        {true, disabled} ->
            ?emergency("Services will stop processing requests due to exhaused db disk space!~s", [
                format_offenders(maps:get(emergency_threshold, OffendersPerThreshold))
            ]),
            set_service_circuit_breaker_status(enabled);
        {true, enabled} ->
            % service_circuit_breaker must have been enabled on previous check
            ok;
        {false, enabled} ->
            ?info("Services will start processing requests again"),
            set_service_circuit_breaker_status(disabled),
            issue_eventual_warnings_or_alerts(OffendersPerThreshold);
        {false, disabled} ->
            issue_eventual_warnings_or_alerts(OffendersPerThreshold)
    end.


%% @private
-spec issue_eventual_warnings_or_alerts(#{atom() => [{service:host(), usage_info()}]}) -> ok.
issue_eventual_warnings_or_alerts(OffendersPerThreshold = #{warning_threshold := Offenders}) ->
    ?warning("DB disk usage exceeded safe thresholds.~s", [format_offenders(Offenders)]),
    issue_eventual_warnings_or_alerts(maps:remove(warning_threshold, OffendersPerThreshold));

issue_eventual_warnings_or_alerts(OffendersPerThreshold = #{alert_threshold := Offenders}) ->
    ?alert("Services may stop processing requests soon due to high levels of DB disk usage!~s", [
        format_offenders(Offenders)
    ]),
    issue_eventual_warnings_or_alerts(maps:remove(alert_threshold, OffendersPerThreshold));

issue_eventual_warnings_or_alerts(_) ->
    ok.


%% @private
-spec format_offenders([{service:host(), usage_info()}]) -> binary().
format_offenders(Offenders) ->
    str_utils:join_binary(lists:map(fun({Host, UsageInfo}) ->
        ?autoformat([Host, UsageInfo])
    end, Offenders)).


%% @private
-spec set_service_circuit_breaker_status(enabled | disabled) -> ok.
set_service_circuit_breaker_status(Status) ->
    PanelNodes = nodes:all(?SERVICE_PANEL),
    onepanel_env:set(PanelNodes, service_circuit_breaker, Status, ?APP_NAME),
    ok.
