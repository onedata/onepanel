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

%% API
-export([start_scan/2, stop_scan/2]).
-export([maybe_configure_storage_import/5, get_initial_scan_details/3, get_auto_scan_details/3, get_stats/4]).


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
-spec maybe_configure_storage_import(Node :: node(), SpaceId :: id(), StorageId :: id(), InitialScanArgs :: args(),
    AutoScanArgs :: args()) -> ok.
maybe_configure_storage_import(_Node, _SpaceId, StorageId, InitialScanArgs, AutoScanArgs)
    when map_size(InitialScanArgs) == 0 andalso map_size(AutoScanArgs) == 0 ->
    ok;
maybe_configure_storage_import(Node, SpaceId, StorageId, InitialScanArgs, AutoScanArgs) ->
    case configure_storage_import(Node, SpaceId, StorageId, InitialScanArgs, AutoScanArgs) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end.



%%-------------------------------------------------------------------
%% @doc Returns storage_import details from given node.
%% @end
%%-------------------------------------------------------------------
-spec get_initial_scan_details(Node :: node(), SpaceId :: id(),
    StorageId :: id()) -> #{atom() => json_utils:json_term()}.
get_initial_scan_details(Node, SpaceId, StorageId) ->
    % todo VFS-5717
    {ok, Details} = op_worker_rpc:storage_import_get_initial_scan_config(Node, SpaceId, StorageId),
    case maps:get(enabled, Details) of
        false ->
            #{strategy => no_import};
        true ->
            #{
                strategy => simple_scan,
                maxDepth => maps:get(max_depth, Details),
                syncAcl => maps:get(sync_acl, Details)
            }
    end.


%%-------------------------------------------------------------------
%% @doc Returns storage_update details from given node.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_scan_details(node(), SpaceId :: id(), StorageId :: id()) ->
    #{atom() => json_utils:json_term()}.
get_auto_scan_details(Node, SpaceId, StorageId) ->
    % todo VFS-5717
    {ok, Details} = op_worker_rpc:storage_import_get_auto_scan_config(Node, SpaceId, StorageId),
    case maps:get(enabled, Details) of
        false ->
            #{strategy => no_update};
        true ->
            #{
                strategy => simple_scan,
                maxDepth => maps:get(max_depth, Details),
                scanInterval => maps:get(scan_interval, Details),
                writeOnce => maps:get(write_once, Details),
                deleteEnable => maps:get(delete_enable, Details),
                syncAcl => maps:get(sync_acl, Details)
            }
    end.

%%-------------------------------------------------------------------
%% @doc Returns storage_sync_stats for given SpaceId.
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
    InitialScanArgs0 :: args(), AutoScanArgs0 :: args()) -> ok.
configure_storage_import(Node, SpaceId, StorageId, InitialScanArgs0, AutoScanArgs0) ->
    % todo VFS-5717 get rid of space_strategies boilerplate
    ImportStrategyName = onepanel_utils:get_converted(strategy, InitialScanArgs0, atom, undefined),
    UpdateStrategyName = onepanel_utils:get_converted(strategy, AutoScanArgs0, atom, undefined),
    ImportEnabled = case ImportStrategyName of
        simple_scan -> true;
        no_import -> false;
        undefined -> undefined
    end,
    AutoScanEnabled = case UpdateStrategyName of
        simple_scan -> true;
        no_update -> false;
        undefined -> undefined
    end,

    % if any of ImportEnabled or AutoScanEnabled is undefined, this setting will be filtered from args and
    % therefore it won't be changed

    InitialScanArgs = maps_utils:remove_undefined(#{
        enabled => ImportEnabled,
        max_depth => onepanel_utils:get_converted(max_depth, InitialScanArgs0, integer, undefined),
        sync_acl => onepanel_utils:get_converted(sync_acl, InitialScanArgs0, boolean, undefined)
    }),
    AutoScanArgs = maps_utils:remove_undefined(#{
        enabled => AutoScanEnabled,
        max_depth => onepanel_utils:get_converted(max_depth, AutoScanArgs0, integer, undefined),
        scan_interval => onepanel_utils:get_converted(scan_interval, AutoScanArgs0, integer, undefined),
        write_once => onepanel_utils:get_converted(write_once, AutoScanArgs0, boolean, undefined),
        delete_enable => onepanel_utils:get_converted(delete_enable, AutoScanArgs0, boolean, undefined),
        sync_acl => onepanel_utils:get_converted(sync_acl, AutoScanArgs0, boolean, undefined)
    }),
    op_worker_rpc:storage_import_configure(Node, SpaceId, StorageId, InitialScanArgs, AutoScanArgs).


%%-------------------------------------------------------------------
%% @private
%% @doc Returns storage_sync metrics.
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
%% @doc Returns storage_sync metrics.
%% @end
%%-------------------------------------------------------------------
-spec get_metric(Node :: node(), SpaceId :: id(), Period :: binary(),
    Metric :: binary()) -> map().
get_metric(Node, SpaceId, Period, Metric) ->
    Type = map_metric_name_to_type(Metric),
    Results = op_worker_rpc:storage_sync_monitoring_get_metric(
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
%% @doc Returns current storage_sync status.
%% @end
%%-------------------------------------------------------------------
-spec get_status(Node :: node(), SpaceId :: id()) -> atom().
get_status(Node, SpaceId) ->
    op_worker_rpc:storage_sync_monitoring_get_status(Node, SpaceId).


