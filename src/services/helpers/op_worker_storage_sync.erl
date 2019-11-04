%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%%--------------------------------------------------------------------
%%% @doc This module contains helper functions used during op_worker service
%%% storage sync configuration.
%%% TODO VFS-5717 get rid of space_strategies boilerplate in this module
%%% @end
%%%-------------------------------------------------------------------
-module(op_worker_storage_sync).
-author("Jakub Kudzia").

-include("names.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/errors.hrl").

-type id() :: binary().
-type args() :: map().
-type strategy_name() :: atom().

%% API
-export([maybe_configure_storage_import/3, maybe_configure_storage_update/3,
    get_storage_import_details/3, get_storage_update_details/3, get_stats/4]).

%%-------------------------------------------------------------------
%% @doc This function modifies storage_import configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec maybe_configure_storage_import(Node :: node(), SpaceId :: id(), Args :: args()) -> ok.
maybe_configure_storage_import(_Node, _SpaceId, Args0) when map_size(Args0) == 0 ->
    ok;
maybe_configure_storage_import(Node, SpaceId, Args) ->
    StrategyName = onepanel_utils:typed_get(strategy, Args, atom),
    case current_import_strategy(Node, SpaceId) of
        StrategyName ->
            ok;  % ignore if NewStrategyName is the same as current
        no_import ->
            configure_storage_import(Node, SpaceId, Args, StrategyName);
        _ ->
            ?throw_error(?ERR_STORAGE_SYNC_IMPORT_STARTED)
    end.


%%-------------------------------------------------------------------
%% @doc This function modifies storage_update configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec maybe_configure_storage_update(Node :: node(), SpaceId :: id(), Args :: args()) -> ok.
maybe_configure_storage_update(_Node, _SpaceId, Args) when map_size(Args) == 0 ->
    ok;
maybe_configure_storage_update(Node, SpaceId, Args) ->
    % todo VFS-5717
    StrategyName = onepanel_utils:typed_get(strategy, Args, atom),
    configure_storage_update(Node, SpaceId, Args, StrategyName).


%%-------------------------------------------------------------------
%% @doc Returns storage_import details from given node.
%% @end
%%-------------------------------------------------------------------
-spec get_storage_import_details(Node :: node(), SpaceId :: id(),
    StorageId :: id()) -> proplists:proplist().
get_storage_import_details(Node, SpaceId, StorageId) ->
    % todo VFS-5717
    {ImportEnabled, Args} = op_worker_rpc:get_storage_import_details(Node, SpaceId, StorageId),
    case ImportEnabled of
        false ->
            [{strategy, no_import}];
        true ->
            [
                {strategy, simple_scan},
                {maxDepth, maps:get(max_depth, Args)},
                {syncAcl, maps:get(sync_acl, Args)}
            ]
    end.


%%-------------------------------------------------------------------
%% @doc Returns storage_update details from given node.
%% @end
%%-------------------------------------------------------------------
-spec get_storage_update_details(Node :: node(), SpaceId :: id(), StorageId :: id()) -> proplists:proplist().
get_storage_update_details(Node, SpaceId, StorageId) ->
    % todo VFS-5717
    {UpdateEnabled, Args} = op_worker_rpc:get_storage_update_details(
        Node, SpaceId, StorageId),
    case UpdateEnabled of
        false ->
            [{strategy, no_update}];
        true ->
            [
                {strategy, simple_scan},
                {maxDepth, maps:get(max_depth, Args)},
                {scanInterval, maps:get(scan_interval, Args)},
                {writeOnce, maps:get(write_once, Args)},
                {deleteEnable, maps:get(delete_enable, Args)},
                {syncAcl, maps:get(sync_acl, Args)}
            ]
    end.

%%-------------------------------------------------------------------
%% @doc Returns storage_sync_stats for given SpaceId.
%% @end
%%-------------------------------------------------------------------
-spec get_stats(Node :: node(), SpaceId :: id(), Period :: binary(),
    Metrics :: [binary()]) -> proplists:proplist().
get_stats(Node, SpaceId, _Period, [<<"">>]) ->
    get_status(Node, SpaceId);
get_stats(Node, SpaceId, Period, Metrics) ->
    [
        {stats, get_all_metrics(Node, SpaceId, Period, Metrics)}
        | get_status(Node, SpaceId)
    ].


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc This function modifies storage_import configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec configure_storage_import(Node :: node(), SpaceId :: id(), Args :: args(),
    NewStrategyName :: strategy_name()) -> ok.
configure_storage_import(Node, SpaceId, Args0, StrategyName) ->
    Enabled = case StrategyName of
    % todo VFS-5717 get rid of space_strategies boilerplate
        simple_scan -> true;
        no_import -> false
    end,
    Args = onepanel_maps:remove_undefined(#{
        max_depth => onepanel_utils:typed_get(max_depth, Args0, integer, undefined),
        sync_acl => onepanel_utils:typed_get(sync_acl, Args0, boolean, undefined)
    }),
    ok = op_worker_rpc:configure_storage_import(Node, SpaceId, Enabled, Args).

%%-------------------------------------------------------------------
%% @private
%% @doc This function modifies storage_update configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec configure_storage_update(Node :: node(), SpaceId :: id(), Args :: args(),
    NewStrategyName :: strategy_name()) -> ok.
configure_storage_update(Node, SpaceId, _Args0, no_update) ->
    % todo VFS-5717
    ok = op_worker_rpc:configure_storage_update(Node, SpaceId, false, #{});
configure_storage_update(Node, SpaceId, Args0, simple_scan) ->
    Args = onepanel_maps:remove_undefined(#{
        max_depth => onepanel_utils:typed_get(max_depth, Args0, integer, undefined),
        scan_interval => onepanel_utils:typed_get(scan_interval, Args0, integer, undefined),
        write_once => onepanel_utils:typed_get(write_once, Args0, boolean, undefined),
        delete_enable => onepanel_utils:typed_get(delete_enable, Args0, boolean, undefined),
        sync_acl => onepanel_utils:typed_get(sync_acl, Args0, boolean, undefined)
    }),
    ok = op_worker_rpc:configure_storage_update(Node, SpaceId, true, Args).

%%-------------------------------------------------------------------
%% @private
%% @doc Returns current import strategy set in provider.
%% @end
%%-------------------------------------------------------------------
-spec current_import_strategy(Node :: node(), SpaceId :: id()) -> strategy_name().
current_import_strategy(Node, SpaceId) ->
    StorageId = op_worker_storage:get_supporting_storage(Node, SpaceId),
    ImportDetails = op_worker_storage_sync:get_storage_import_details(Node,
        SpaceId, StorageId),
    proplists:get_value(strategy, ImportDetails).

%%-------------------------------------------------------------------
%% @private
%% @doc Returns storage_sync metrics.
%% @end
%%-------------------------------------------------------------------
-spec get_all_metrics(Node :: node(), SpaceId :: id(), Period :: binary(),
    Metrics :: [binary()]) -> proplists:proplist().
get_all_metrics(Node, SpaceId, Period, Metrics) ->
    lists:foldl(fun(Metric, AccIn) ->
        MetricResult = get_metric(Node, SpaceId, Period, Metric),
        [{Metric, MetricResult} | AccIn]
    end, [], Metrics).

%%-------------------------------------------------------------------
%% @private
%% @doc Returns storage_sync metrics.
%% @end
%%-------------------------------------------------------------------
-spec get_metric(Node :: node(), SpaceId :: id(), Period :: binary(),
    Metric :: binary()) -> proplists:proplist().
get_metric(Node, SpaceId, Period, Metric) ->
    Type = map_metric_name_to_type(Metric),
    Results = op_worker_rpc:storage_sync_monitoring_get_metric(
        Node, SpaceId, Type, binary_to_atom(Period, utf8)),
    LastValueTimestamp = proplists:get_value(timestamp, Results),
    Values = proplists:get_value(values, Results),
    [
        {name, Metric},
        {lastValueDate, time_utils:epoch_to_iso8601(LastValueTimestamp)},
        {values, Values}
    ].

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Maps type of metric.
%% @end
%%-------------------------------------------------------------------
-spec map_metric_name_to_type(binary()) -> atom().
map_metric_name_to_type(<<"queueLength">>) -> queue_length;
map_metric_name_to_type(<<"insertCount">>) -> imported_files;
map_metric_name_to_type(<<"updateCount">>) -> updated_files;
map_metric_name_to_type(<<"deleteCount">>) -> deleted_files.



%%-------------------------------------------------------------------
%% @private
%% @doc Returns current storage_sync status.
%% @end
%%-------------------------------------------------------------------
-spec get_status(Node :: node(), SpaceId :: id()) ->
    proplists:proplist().
get_status(Node, SpaceId) ->
    [
        {importStatus, get_import_status(Node, SpaceId)},
        {updateStatus, get_update_status(Node, SpaceId)}
    ].

%%-------------------------------------------------------------------
%% @private
%% @doc Returns current storage_import status
%% @end
%%-------------------------------------------------------------------
-spec get_import_status(Node :: node(), SpaceId :: id()) -> binary().
get_import_status(Node, SpaceId) ->
    case op_worker_rpc:storage_sync_monitoring_get_import_status(Node, SpaceId) of
        finished ->
            <<"done">>;
        _ ->
            % "not_started" or "in_progress"
            <<"inProgress">>
    end.

%%-------------------------------------------------------------------
%% @private
%% @doc Returns current storage_update status
%% @end
%%-------------------------------------------------------------------
-spec get_update_status(Node :: node(), SpaceId :: id()) -> binary().
get_update_status(Node, SpaceId) ->
    case op_worker_rpc:storage_sync_monitoring_get_update_status(Node, SpaceId) of
        in_progress ->
            <<"inProgress">>;
        _ ->
            % "not_started" or "finished" (and waiting for next scan)
            <<"waiting">>
    end.

