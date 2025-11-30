%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for building Ceph storage specifications
%%% from REST API maps and converting Ceph storage descriptions back to maps.
%%% It translates between the REST API format (camelCase keys, binaries)
%%% and ctool storage records (snake_case atoms, records).
%%% @end
%%%--------------------------------------------------------------------
-module(ceph_storage_spec_builder).
-author("Bartosz Walkowicz").

-include_lib("op_panel_contracts/include/storage/ceph.hrl").

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


-spec build_credentials(map()) -> onedata_storage:ceph_credentials().
build_credentials(Params) ->
    #ceph_credentials{
        username = maps:get(username, Params),
        key = maps:get(key, Params)
    }.


-spec build_credentials_diff(map()) -> onedata_storage:ceph_credentials_diff().
build_credentials_diff(Params) ->
    #ceph_credentials_diff{
        username = maps:get(username, Params, undefined),
        key = maps:get(key, Params, undefined)
    }.


-spec build_configuration(map()) -> onedata_storage:ceph_configuration().
build_configuration(Params) ->
    #ceph_configuration{
        monitor_hostname = maps:get(monitorHostname, Params),
        cluster_name = maps:get(clusterName, Params),
        pool_name = maps:get(poolName, Params),
        storage_path_type = storage_spec_builder_utils:binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"flat">>)
        )
    }.


-spec build_configuration_diff(map()) -> onedata_storage:ceph_configuration_diff().
build_configuration_diff(Params) ->
    #ceph_configuration_diff{
        monitor_hostname = maps:get(monitorHostname, Params, undefined),
        cluster_name = maps:get(clusterName, Params, undefined),
        pool_name = maps:get(poolName, Params, undefined)
    }.


-spec credentials_to_map(onedata_storage:ceph_credentials()) -> map().
credentials_to_map(#ceph_credentials{username = Username, key = Key}) ->
    #{username => Username, key => Key}.


-spec configuration_to_map(onedata_storage:ceph_configuration()) -> map().
configuration_to_map(#ceph_configuration{
    monitor_hostname = MonitorHostname,
    cluster_name = ClusterName,
    pool_name = PoolName,
    storage_path_type = StoragePathType
}) ->
    #{
        monitorHostname => MonitorHostname,
        clusterName => ClusterName,
        poolName => PoolName,
        storagePathType => storage_spec_builder_utils:storage_path_type_to_binary(StoragePathType)
    }.
