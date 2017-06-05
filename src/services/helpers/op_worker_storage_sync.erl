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

-type id() :: binary().
-type args() :: map().

%% API
-export([modify_storage_import/3, modify_storage_update/3,
    get_storage_import_details/3, get_storage_update_details/3]).

%%-------------------------------------------------------------------
%% @doc This function modifies storage_import configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec modify_storage_import(Node :: node(), SpaceId :: id(), Args :: args())
        -> {ok, id()}.
modify_storage_import(Node, SpaceId, Args0) ->
    StrategyName = onepanel_utils:typed_get(strategy, Args0, atom),
    Args = case StrategyName of
        no_import ->
            #{};
        _ ->
            MaxDepth = onepanel_utils:typed_get(max_depth, Args0, integer,
                get_default(max_depth)),
            #{max_depth => MaxDepth}
    end,
    {ok, _} = rpc:call(Node, storage_sync, modify_storage_import, [
        SpaceId, StrategyName, Args
    ]).


%%-------------------------------------------------------------------
%% @doc This function modifies storage_update configuration on given Node.
%% @end
%%-------------------------------------------------------------------
-spec modify_storage_update(Node :: node(), SpaceId :: id(), Args :: args())
        ->{ok, id()}.
modify_storage_update(Node, SpaceId, Args0) ->
    StrategyName = onepanel_utils:typed_get(strategy, Args0, atom),
    Args = case StrategyName of
        no_update ->
            #{};
        _ ->
            MaxDepth = onepanel_utils:typed_get(max_depth, Args0, integer,
                get_default(max_depth)),
            ScanInterval = onepanel_utils:typed_get(scan_interval, Args0,
                integer, get_default(scan_interval)),
            WriteOnce = onepanel_utils:typed_get(write_once, Args0, boolean,
                get_default(write_once)),
            DeleteEnable = onepanel_utils:typed_get(delete_enable, Args0, boolean,
                get_default(delete_enable)),

            #{
                max_depth => MaxDepth,
                scan_interval => ScanInterval,
                write_once => WriteOnce,
                delete_enable => DeleteEnable
            }
    end,

    {ok, _} = rpc:call(Node, storage_sync, modify_storage_update, [
        SpaceId, StrategyName, Args
    ]).



%%%===================================================================
%%% Internal functions
%%%===================================================================

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
map_key_to_default_key(delete_enable) -> oneprovider_sync_update_write_once.


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
            [{maxDepth, maps:get(max_depth, Args)} | Details]
    end.


%%-------------------------------------------------------------------
%% @private
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
                {deleteEnable, maps:get(delete_enable, Args)}
                | Details]
    end.


