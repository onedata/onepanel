%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%%--------------------------------------------------------------------
%%% @doc This module contains helper function used during op_worker service
%%% storage sync configuration.
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
-export([maybe_modify_storage_import/3, maybe_modify_storage_update/3,
    get_storage_import_details/3, get_storage_update_details/3, get_stats/4]).

%%-------------------------------------------------------------------
%% @doc This function modifies storage_import configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec maybe_modify_storage_import(Node :: node(), SpaceId :: id(), Args :: args())
        -> {ok, id()}.
maybe_modify_storage_import(_Node, SpaceId, Args0) when map_size(Args0) == 0 ->
    {ok, SpaceId};
maybe_modify_storage_import(Node, SpaceId, Args) ->
    StrategyName = onepanel_utils:typed_get(strategy, Args, atom),
    case current_import_strategy(Node, SpaceId) of
        StrategyName ->
            {ok, SpaceId};  % ignore if NewStrategyName is the same as current
        no_import ->
            modify_storage_import(Node, SpaceId, Args, StrategyName);
        _ ->
            ?throw_error(?ERR_STORAGE_SYNC_IMPORT_STARTED)
    end.


%%-------------------------------------------------------------------
%% @doc This function modifies storage_update configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec maybe_modify_storage_update(Node :: node(), SpaceId :: id(), Args :: args())
        ->{ok, id()}.
maybe_modify_storage_update(_Node, SpaceId, Args) when map_size(Args) == 0 ->
    {ok, SpaceId};
maybe_modify_storage_update(Node, SpaceId, Args) ->
    StrategyName = onepanel_utils:typed_get(strategy, Args, atom),
    modify_storage_update(Node, SpaceId, Args, StrategyName).


%%-------------------------------------------------------------------
%% @doc Returns storage_import details from given node.
%% @end
%%-------------------------------------------------------------------
-spec get_storage_import_details(Node :: node(), SpaceId :: id(),
    StorageId :: id()) -> proplists:proplist().
get_storage_import_details(Node, SpaceId, StorageId) ->
    {StrategyName, Args} = rpc:call(Node, space_strategies,
        get_storage_import_details, [SpaceId, StorageId]),
    Details = [{strategy, StrategyName}],
    case StrategyName of
        no_import ->
            Details;
        simple_scan ->
            [
                {maxDepth, maps:get(max_depth, Args)},
                {syncAcl, maps:get(sync_acl, Args)}
                | Details
            ]
    end.


%%-------------------------------------------------------------------
%% @doc Returns storage_update details from given node.
%% @end
%%-------------------------------------------------------------------
-spec get_storage_update_details(Node :: node(), SpaceId :: id(),
    StorageId :: id()) -> proplists:proplist().
get_storage_update_details(Node, SpaceId, StorageId) ->
    {StrategyName, Args} = rpc:call(Node, space_strategies,
        get_storage_update_details, [SpaceId, StorageId]),
    Details = [{strategy, StrategyName}],
    case StrategyName of
        no_update ->
            Details;
        simple_scan ->
            [
                {maxDepth, maps:get(max_depth, Args)},
                {scanInterval, maps:get(scan_interval, Args)},
                {writeOnce, maps:get(write_once, Args)},
                {deleteEnable, maps:get(delete_enable, Args)},
                {syncAcl, maps:get(sync_acl, Args)}
                | Details
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
-spec modify_storage_import(Node :: node(), SpaceId :: id(), Args :: args(),
    NewStrategyName :: strategy_name()) -> {ok, id()}.
modify_storage_import(_Node, SpaceId, _Args0, no_import) ->
    {ok, SpaceId};
modify_storage_import(Node, SpaceId, Args0, NewStrategyName) ->
    Args = #{
        max_depth => onepanel_utils:typed_get(max_depth, Args0, integer,
            get_default(max_depth)),
        sync_acl => onepanel_utils:typed_get(sync_acl, Args0, boolean,
            get_default(sync_acl))
    },
    {ok, _} = rpc:call(Node, storage_sync, modify_storage_import, [
            SpaceId, NewStrategyName, Args
    ]).

%%-------------------------------------------------------------------
%% @private
%% @doc This function modifies storage_update configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec modify_storage_update(Node :: node(), SpaceId :: id(), Args :: args(),
    NewStrategyName :: strategy_name()) -> {ok, id()}.
modify_storage_update(Node, SpaceId, _Args0, no_update) ->
    {ok, _} = rpc:call(Node, storage_sync, modify_storage_update, [
        SpaceId, no_update, #{}]);
modify_storage_update(Node, SpaceId, Args0, NewStrategyName) ->
    Args = #{
        max_depth => onepanel_utils:typed_get(max_depth, Args0, integer,
            get_default(max_depth)),
        scan_interval => onepanel_utils:typed_get(scan_interval, Args0, integer,
            get_default(scan_interval)),
        write_once => onepanel_utils:typed_get(write_once, Args0, boolean,
            get_default(write_once)),
        delete_enable => onepanel_utils:typed_get(delete_enable, Args0, boolean,
            get_default(delete_enable)),
        sync_acl => onepanel_utils:typed_get(sync_acl, Args0, boolean,
            get_default(sync_acl))
    },

    {ok, _} = rpc:call(Node, storage_sync, modify_storage_update, [
        SpaceId, NewStrategyName, Args
    ]).


%%-------------------------------------------------------------------
%% @private
%% @doc Util function that maps given argument name to key for its
%% default value in app.config.
%% @end
%%-------------------------------------------------------------------
-spec map_key_to_default_key(atom()) -> atom().
map_key_to_default_key(max_depth) -> oneprovider_sync_max_depth;
map_key_to_default_key(scan_interval) -> oneprovider_sync_update_scan_interval;
map_key_to_default_key(write_once) -> oneprovider_sync_update_delete_enable;
map_key_to_default_key(delete_enable) -> oneprovider_sync_update_write_once;
map_key_to_default_key(sync_acl) -> oneprovider_sync_acl.


%%-------------------------------------------------------------------
%% @private
%% @doc Returns default value defined in app.config
%% @end
%%-------------------------------------------------------------------
-spec get_default(atom()) -> term().
get_default(Key) ->
    {ok, Value} = application:get_env(?APP_NAME, map_key_to_default_key(Key)),
    Value.

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
    Metric :: binary()) -> proplists:proplist() | atom().
get_metric(Node, SpaceId, Period, Metric) ->
    Type = map_metric_name_to_type(Metric),
    Results = rpc:call(Node, storage_sync_monitoring,
        get_metric, [SpaceId, Type, binary_to_atom(Period, latin1)]),
    case Results of
        undefined ->
            null;
        Results ->
            LastValueTimestamp = proplists:get_value(timestamp, Results),
            Values = proplists:get_value(values, Results),
            [
                {name, Metric},
                {lastValueDate, time_utils:datetime_to_datestamp(LastValueTimestamp)},
                {values, Values}
            ]
    end.

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
    ImportState = rpc:call(Node, storage_sync_monitoring, get_import_state, [SpaceId]),
    case ImportState of
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
    UpdateState = rpc:call(Node, storage_sync_monitoring,
        get_update_state, [SpaceId]),
    case UpdateState of
        in_progress ->
            <<"inProgress">>;
        _ ->
            % "not_started" or "finished" (and waiting for next scan)
            <<"waiting">>
    end.

