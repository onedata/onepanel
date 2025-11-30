%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for building NullDevice storage specifications
%%% from REST API maps and converting NullDevice storage descriptions back to maps.
%%% It translates between the REST API format (camelCase keys, binaries)
%%% and ctool storage records (snake_case atoms, records).
%%% @end
%%%--------------------------------------------------------------------
-module(nulldevice_storage_spec_builder).
-author("Bartosz Walkowicz").

-include_lib("op_panel_contracts/include/storage/nulldevice.hrl").

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


-spec build_credentials(map()) -> onedata_storage:nulldevice_credentials().
build_credentials(Params) ->
    #nulldevice_credentials{
        uid = maps:get(uid, Params, 0),
        gid = maps:get(gid, Params, 0)
    }.


-spec build_credentials_diff(map()) -> onedata_storage:nulldevice_credentials_diff().
build_credentials_diff(Params) ->
    #nulldevice_credentials_diff{
        uid = maps:get(uid, Params, undefined),
        gid = maps:get(gid, Params, undefined)
    }.


-spec build_configuration(map()) -> onedata_storage:nulldevice_configuration().
build_configuration(Params) ->
    #nulldevice_configuration{
        latency_min = maps:get(latencyMin, Params, undefined),
        latency_max = maps:get(latencyMax, Params, undefined),
        timeout_probability = maps:get(timeoutProbability, Params, undefined),
        filter = maps:get(filter, Params, undefined),
        simulated_filesystem_parameters = maps:get(simulatedFilesystemParameters, Params, undefined),
        simulated_filesystem_grow_speed = maps:get(simulatedFilesystemGrowSpeed, Params, undefined),
        enable_data_verification = maps:get(enableDataVerification, Params, undefined),
        storage_path_type = storage_spec_builder_utils:binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


-spec build_configuration_diff(map()) -> onedata_storage:nulldevice_configuration_diff().
build_configuration_diff(Params) ->
    #nulldevice_configuration_diff{
        latency_min = maps:get(latencyMin, Params, undefined),
        latency_max = maps:get(latencyMax, Params, undefined),
        timeout_probability = maps:get(timeoutProbability, Params, undefined),
        filter = maps:get(filter, Params, undefined),
        simulated_filesystem_parameters = maps:get(simulatedFilesystemParameters, Params, undefined),
        simulated_filesystem_grow_speed = maps:get(simulatedFilesystemGrowSpeed, Params, undefined),
        enable_data_verification = maps:get(enableDataVerification, Params, undefined)
    }.


-spec credentials_to_map(onedata_storage:nulldevice_credentials()) -> map().
credentials_to_map(#nulldevice_credentials{uid = Uid, gid = Gid}) ->
    Base = #{uid => Uid},
    maps_utils:put_if_defined(Base, gid, Gid).


-spec configuration_to_map(onedata_storage:nulldevice_configuration()) -> map().
configuration_to_map(#nulldevice_configuration{
    latency_min = LatencyMin,
    latency_max = LatencyMax,
    timeout_probability = TimeoutProb,
    filter = Filter,
    simulated_filesystem_parameters = SimFsParams,
    simulated_filesystem_grow_speed = SimFsGrowSpeed,
    enable_data_verification = EnableDataVerif,
    storage_path_type = StoragePathType
}) ->
    Base = #{storagePathType => storage_spec_builder_utils:storage_path_type_to_binary(StoragePathType)},
    Base1 = maps_utils:put_if_defined(Base, latencyMin, LatencyMin),
    Base2 = maps_utils:put_if_defined(Base1, latencyMax, LatencyMax),
    Base3 = maps_utils:put_if_defined(Base2, timeoutProbability, TimeoutProb),
    Base4 = maps_utils:put_if_defined(Base3, filter, Filter),
    Base5 = maps_utils:put_if_defined(Base4, simulatedFilesystemParameters, SimFsParams),
    Base6 = maps_utils:put_if_defined(Base5, simulatedFilesystemGrowSpeed, SimFsGrowSpeed),
    maps_utils:put_if_defined(Base6, enableDataVerification, EnableDataVerif).
