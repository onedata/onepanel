%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for building NFS storage specifications
%%% from REST API maps and converting NFS storage descriptions back to maps.
%%% It translates between the REST API format (camelCase keys, binaries)
%%% and ctool storage records (snake_case atoms, records).
%%% @end
%%%--------------------------------------------------------------------
-module(nfs_storage_spec_builder).
-author("Bartosz Walkowicz").

-include_lib("op_panel_contracts/include/storage/nfs.hrl").

%% API
-export([
    build_credentials/1,
    build_credentials_diff/1,
    build_configuration/1,
    build_configuration_diff/1,
    credentials_to_map/1,
    configuration_to_map/1
]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec build_credentials(map()) -> onedata_storage:nfs_credentials().
build_credentials(Params) ->
    #nfs_credentials{
        uid = maps:get(uid, Params, 0),
        gid = maps:get(gid, Params, 0)
    }.


-spec build_credentials_diff(map()) -> onedata_storage:nfs_credentials_diff().
build_credentials_diff(Params) ->
    #nfs_credentials_diff{
        uid = maps:get(uid, Params, undefined),
        gid = maps:get(gid, Params, undefined)
    }.


-spec build_configuration(map()) -> onedata_storage:nfs_configuration().
build_configuration(Params) ->
    #nfs_configuration{
        version = maps:get(version, Params, undefined),
        host = maps:get(host, Params),
        volume = maps:get(volume, Params),
        read_ahead = maps:get(readAhead, Params, undefined),
        dir_cache = maps:get(dirCache, Params, undefined),
        auto_reconnect = maps:get(autoReconnect, Params, undefined),
        connection_pool_size = maps:get(connectionPoolSize, Params, undefined),
        storage_path_type = storage_spec_builder_utils:binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


-spec build_configuration_diff(map()) -> onedata_storage:nfs_configuration_diff().
build_configuration_diff(Params) ->
    #nfs_configuration_diff{
        version = maps:get(version, Params, undefined),
        host = maps:get(host, Params, undefined),
        volume = maps:get(volume, Params, undefined),
        read_ahead = maps:get(readAhead, Params, undefined),
        dir_cache = maps:get(dirCache, Params, undefined),
        auto_reconnect = maps:get(autoReconnect, Params, undefined),
        connection_pool_size = maps:get(connectionPoolSize, Params, undefined)
    }.


-spec credentials_to_map(onedata_storage:nfs_credentials()) -> map().
credentials_to_map(#nfs_credentials{uid = Uid, gid = Gid}) ->
    Base = #{uid => Uid},
    maps_utils:put_if_defined(Base, gid, Gid).


-spec configuration_to_map(onedata_storage:nfs_configuration()) -> map().
configuration_to_map(#nfs_configuration{
    version = Version,
    host = Host,
    volume = Volume,
    read_ahead = ReadAhead,
    dir_cache = DirCache,
    auto_reconnect = AutoReconnect,
    connection_pool_size = PoolSize,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        host => Host,
        volume => Volume,
        storagePathType => storage_spec_builder_utils:storage_path_type_to_binary(StoragePathType)
    },
    Base1 = maps_utils:put_if_defined(Base, version, Version),
    Base2 = maps_utils:put_if_defined(Base1, readAhead, ReadAhead),
    Base3 = maps_utils:put_if_defined(Base2, dirCache, DirCache),
    Base4 = maps_utils:put_if_defined(Base3, autoReconnect, AutoReconnect),
    maps_utils:put_if_defined(Base4, connectionPoolSize, PoolSize).
