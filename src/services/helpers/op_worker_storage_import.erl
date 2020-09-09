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
-type metric_type() :: binary(). % <<"queueLength">>, <<"importCount">>, <<"updateCount">>, <<"deleteCount">>

%% API
-export([start_scan/2, stop_scan/2]).
-export([maybe_configure_storage_import/3, maybe_reconfigure_storage_import/3]).
-export([get_storage_import_details/2, get_stats/4, get_info/2]).

-export_type([metric_type/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_scan(node(), id()) -> ok.
start_scan(Node, SpaceId) ->
    case op_worker_rpc:storage_import_start_scan(Node, SpaceId) of
        ok -> ok;
        {error, already_started} -> throw(?ERROR_ALREADY_EXISTS)
    end.

-spec stop_scan(node(), id()) -> ok.
stop_scan(Node, SpaceId) ->
    case op_worker_rpc:storage_import_stop_scan(Node, SpaceId) of
        ok -> ok;
        {error, not_found} -> throw(?ERROR_NOT_FOUND)
    end.


-spec maybe_configure_storage_import(Node :: node(), SpaceId :: id(), StorageImportArgs :: args()) -> ok.
maybe_configure_storage_import(_Node, _SpaceId, StorageImportConfig)
    when map_size(StorageImportConfig) == 0 ->
    ok;
maybe_configure_storage_import(Node, SpaceId, StorageImportConfig) ->
    case configure_storage_import(Node, SpaceId, StorageImportConfig) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end.


-spec maybe_reconfigure_storage_import(Node :: node(), SpaceId :: id(), StorageImportArgs :: args()) -> ok.
maybe_reconfigure_storage_import(_Node, _SpaceId, StorageImportConfig)
    when map_size(StorageImportConfig) == 0 ->
    ok;
maybe_reconfigure_storage_import(Node, SpaceId, StorageImportConfig) ->
    case reconfigure_storage_import(Node, SpaceId, StorageImportConfig) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end.


-spec get_storage_import_details(node(), SpaceId :: id()) ->
    #{atom() => json_utils:json_term()}.
get_storage_import_details(Node, SpaceId) ->
    {ok, StorageImportConfig} = op_worker_rpc:storage_import_get_configuration(Node, SpaceId),
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
-spec get_info(Node :: node(), SpaceId :: id()) -> json_utils:json_term().
get_info(Node, SpaceId) ->
    {ok, Info} = op_worker_rpc:storage_import_get_info(Node, SpaceId),
    Info.


%%-------------------------------------------------------------------
%% @doc Returns statistics of auto storage import for given SpaceId.
%% @end
%%-------------------------------------------------------------------
-spec get_stats(Node :: node(), SpaceId :: id(), Period :: binary(),
    Metrics :: [binary()]) -> json_utils:json_term().
get_stats(Node, SpaceId, Period, Metrics) ->
    assert_metrics(Metrics),
    {ok, Results} = op_worker_rpc:storage_import_get_stats(
        Node, SpaceId, Metrics, binary_to_atom(Period, utf8)),
    Results.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc This function modifies storage_import configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec configure_storage_import(Node :: node(), SpaceId :: id(), StorageImportConfig :: args()) ->
    ok | {error, term()}.
configure_storage_import(Node, SpaceId, StorageImportConfig) ->
    case onepanel_utils:get_converted(mode, StorageImportConfig, binary) of
        <<"auto">> ->
            ScanConfig = maps:get(scan_config, StorageImportConfig, #{}),
            configure_auto_storage_import(Node, SpaceId, ScanConfig);
        <<"manual">> ->
            op_worker_rpc:storage_import_set_manual_import(Node, SpaceId)
    end.


-spec reconfigure_storage_import(Node :: node(), SpaceId :: id(), StorageImportConfig :: args()) ->
    ok | {error, term()}.
reconfigure_storage_import(Node, SpaceId, StorageImportConfig) ->
    {ok, CurrentStorageImportConfig} = op_worker_rpc:storage_import_get_configuration(Node, SpaceId),
    CurrentMode = onepanel_utils:get_converted(mode, CurrentStorageImportConfig, binary),
    % storage import mode should not be changed
    NewMode = onepanel_utils:get_converted(mode, StorageImportConfig, binary, CurrentMode),
    case CurrentMode =/= NewMode of
        true ->
            throw(?ERROR_STORAGE_IMPORT_MODE_CANNOT_BE_CHANGED(SpaceId));
        false ->
            case CurrentMode of
                <<"auto">> ->
                    ScanConfig = maps:get(scan_config, StorageImportConfig, #{}),
                    configure_auto_storage_import(Node, SpaceId, ScanConfig);
                <<"manual">> ->
                    % there is nothing to reconfigure in manual mode
                    ok
            end
    end.


-spec configure_auto_storage_import(Node :: node(), SpaceId :: id(), StorageImportConfig :: args()) ->
    ok | {error, term()}.
configure_auto_storage_import(Node, SpaceId, ScanConfig) ->
    ScanConfig2 = maps_utils:remove_undefined(#{
        max_depth => onepanel_utils:get_converted(max_depth, ScanConfig, integer, undefined),
        scan_interval => onepanel_utils:get_converted(scan_interval, ScanConfig, integer, undefined),
        continuous_scan => onepanel_utils:get_converted(continuous_scan, ScanConfig, boolean, undefined),
        detect_modifications => onepanel_utils:get_converted(detect_modifications, ScanConfig, boolean, undefined),
        detect_deletions => onepanel_utils:get_converted(detect_deletions, ScanConfig, boolean, undefined),
        sync_acl => onepanel_utils:get_converted(sync_acl, ScanConfig, boolean, undefined)
    }),
    op_worker_rpc:storage_import_configure_auto_import(Node, SpaceId, ScanConfig2).


-spec assert_metrics([binary()]) -> ok.
assert_metrics(Metrics) ->
    lists:foreach(fun(Metric) ->
        case is_supported_metric(Metric) of
            true -> ok;
            false -> throw(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Metric, supported_metrics()))
        end
    end, Metrics).


-spec is_supported_metric(binary()) -> boolean().
is_supported_metric(Metric) ->
    lists:member(Metric, supported_metrics()).


-spec supported_metrics() -> [metric_type()].
supported_metrics() ->
    [<<"queueLength">>, <<"importCount">>, <<"updateCount">>, <<"deleteCount">>].