%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%%--------------------------------------------------------------------
%%% @doc This module contains helper functions used during configuration
%%% of storage import in op_worker service.
%%% @end
%%%-------------------------------------------------------------------
-module(op_worker_storage_import).
-author("Jakub Kudzia").

-include("names.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/errors.hrl").

-type id() :: binary().
-type args() :: map().

%% API
-export([start_scan/2, stop_scan/2]).
-export([maybe_configure_storage_import/4, get_storage_import_details/3, get_stats/4]).


%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_scan(node(), id()) -> ok.
start_scan(Node, SpaceId) ->
    {ok, StorageIds} = op_worker_storage:get_supporting_storages(Node, SpaceId),
    StorageId = hd(StorageIds),
    op_worker_rpc:storage_import_start_scan(Node, SpaceId, StorageId).

-spec stop_scan(node(), id()) -> ok.
stop_scan(Node, SpaceId) ->
    {ok, StorageIds} = op_worker_storage:get_supporting_storages(Node, SpaceId),
    StorageId = hd(StorageIds),
    op_worker_rpc:storage_import_stop_scan(Node, SpaceId, StorageId).

%%-------------------------------------------------------------------
%% @doc This function modifies storage_import configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec maybe_configure_storage_import(Node :: node(), SpaceId :: id(), StorageId :: id(),
    StorageImportArgs :: args()) -> ok.
maybe_configure_storage_import(_Node, _SpaceId, _StorageId, StorageImportConfig)
    when map_size(StorageImportConfig) == 0 ->
    ok;
maybe_configure_storage_import(Node, SpaceId, StorageId, StorageImportConfig) ->
    case configure_storage_import(Node, SpaceId, StorageId, StorageImportConfig) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end.


-spec get_storage_import_details(node(), SpaceId :: id(), StorageId :: id()) ->
    #{atom() => json_utils:json_term()}.
get_storage_import_details(Node, SpaceId, StorageId) ->
    {ok, StorageImportConfig} = op_worker_rpc:storage_import_get_configuration(Node, SpaceId, StorageId),
    kv_utils:copy_found([
        {[storage_import, mode], [storageImport, mode]},
        {[storage_import, scan_config, max_depth], [storageImport, scanConfig, maxDepth]},
        {[storage_import, scan_config, sync_acl], [storageImport, scanConfig, syncAcl]},
        {[storage_import, scan_config, continuous_scan], [storageImport, scanConfig, continuousScan]},
        {[storage_import, scan_config, scan_interval], [storageImport, scanConfig, scanInterval]},
        {[storage_import, scan_config, detect_modifications], [storageImport, scanConfig, detectModifications]},
        {[storage_import, scan_config, detect_deletions], [storageImport, scanConfig, detectDeletions]}
    ], StorageImportConfig).

%%-------------------------------------------------------------------
%% @doc Returns statistics of auto storage import for given SpaceId.
%% @end
%%-------------------------------------------------------------------
-spec get_stats(Node :: node(), SpaceId :: id(), Period :: binary(),
    Metrics :: [binary()]) -> #{atom() => json_utils:json_term()}.
get_stats(Node, SpaceId, _Period, [<<"">>]) ->
    #{
        status => get_status(Node, SpaceId)
    };
get_stats(Node, SpaceId, Period, Metrics) ->
    #{
        status => get_status(Node, SpaceId),
        stats => get_all_metrics(Node, SpaceId, Period, Metrics)
    }.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc This function modifies storage_import configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec configure_storage_import(Node :: node(), SpaceId :: id(), StorageId :: id(),
    StorageImportConfig :: args()) -> ok | {error, term()}.
configure_storage_import(Node, SpaceId, StorageId, StorageImportConfig) ->
    case onepanel_utils:get_converted(mode, StorageImportConfig, binary) of
        <<"auto">> ->
            ScanConfig = maps:get(scan_config, StorageImportConfig, #{}),
            ScanConfig2 = maps_utils:remove_undefined(#{
                max_depth => onepanel_utils:get_converted(max_depth, ScanConfig, integer, undefined),
                scan_interval => onepanel_utils:get_converted(scan_interval, ScanConfig, integer, undefined),
                continuous_scan => onepanel_utils:get_converted(continuous_scan, ScanConfig, boolean, undefined),
                detect_modifications => onepanel_utils:get_converted(detect_modifications, ScanConfig, boolean, undefined),
                detect_deletions => onepanel_utils:get_converted(detect_deletions, ScanConfig, boolean, undefined),
                sync_acl => onepanel_utils:get_converted(sync_acl, ScanConfig, boolean, undefined)
            }),
            op_worker_rpc:storage_import_configure_auto_import(Node, SpaceId, StorageId, ScanConfig2);
        <<"manual">> ->
            op_worker_rpc:storage_import_enable_manual_import(Node, SpaceId, StorageId)
    end.


%%-------------------------------------------------------------------
%% @private
%% @doc Returns all auto storage import metrics.
%% @end
%%-------------------------------------------------------------------
-spec get_all_metrics(OpNode :: node(), SpaceId :: id(), Period :: binary(),
    Metrics :: [Metric]) -> #{Metric => map()}
    when Metric :: binary().
get_all_metrics(OpNode, SpaceId, Period, Metrics) ->
    lists:foldl(fun(Metric, Acc) ->
        Acc#{Metric => get_metric(OpNode, SpaceId, Period, Metric)}
    end, #{}, Metrics).

%%-------------------------------------------------------------------
%% @private
%% @doc Returns auto storage import metric for given period.
%% @end
%%-------------------------------------------------------------------
-spec get_metric(Node :: node(), SpaceId :: id(), Period :: binary(),
    Metric :: binary()) -> map().
get_metric(Node, SpaceId, Period, Metric) ->
    Type = map_metric_name_to_type(Metric),
    Results = op_worker_rpc:storage_import_monitoring_get_metric(
        Node, SpaceId, Type, binary_to_atom(Period, utf8)),
    LastValueTimestamp = proplists:get_value(timestamp, Results),
    Values = proplists:get_value(values, Results),
    #{
        name => Metric,
        lastValueDate => time_utils:epoch_to_iso8601(LastValueTimestamp),
        values => Values
    }.

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
%% @doc Returns current status of auto storage import mechanism
%% @end
%%-------------------------------------------------------------------
-spec get_status(Node :: node(), SpaceId :: id()) -> atom().
get_status(Node, SpaceId) ->
    op_worker_rpc:storage_import_monitoring_get_status(Node, SpaceId).


