%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for building POSIX storage specifications
%%% from REST API maps and converting POSIX storage descriptions back to maps.
%%% It translates between the REST API format (camelCase keys, binaries)
%%% and ctool storage records (snake_case atoms, records).
%%% @end
%%%--------------------------------------------------------------------
-module(posix_storage_spec_builder).
-author("Bartosz Walkowicz").

-include_lib("op_panel_contracts/include/storage/posix.hrl").

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


-spec build_credentials(map()) -> onedata_storage:posix_credentials().
build_credentials(Params) ->
    #posix_credentials{
        uid = maps:get(rootUid, Params, 0),
        gid = maps:get(rootGid, Params, 0)
    }.


-spec build_credentials_diff(map()) -> onedata_storage:posix_credentials_diff().
build_credentials_diff(Params) ->
    #posix_credentials_diff{
        uid = maps:get(rootUid, Params, undefined),
        gid = maps:get(rootGid, Params, undefined)
    }.


-spec build_configuration(map()) -> onedata_storage:posix_configuration().
build_configuration(Params) ->
    #posix_configuration{
        mount_point = maps:get(mountPoint, Params),
        storage_path_type = storage_spec_builder_utils:binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


-spec build_configuration_diff(map()) -> onedata_storage:posix_configuration_diff().
build_configuration_diff(Params) ->
    #posix_configuration_diff{
        mount_point = maps:get(mountPoint, Params, undefined)
    }.


-spec credentials_to_map(onedata_storage:posix_credentials()) -> map().
credentials_to_map(#posix_credentials{uid = Uid, gid = Gid}) ->
    Base = #{rootUid => Uid},
    maps_utils:put_if_defined(Base, rootGid, Gid).


-spec configuration_to_map(onedata_storage:posix_configuration()) -> map().
configuration_to_map(#posix_configuration{
    mount_point = MountPoint,
    storage_path_type = StoragePathType
}) ->
    #{
        mountPoint => MountPoint,
        storagePathType => storage_spec_builder_utils:storage_path_type_to_binary(StoragePathType)
    }.
