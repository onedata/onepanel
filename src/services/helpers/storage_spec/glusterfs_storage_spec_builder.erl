%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for building GlusterFS storage specifications
%%% from REST API maps and converting GlusterFS storage descriptions back to maps.
%%% It translates between the REST API format (camelCase keys, binaries)
%%% and ctool storage records (snake_case atoms, records).
%%% @end
%%%--------------------------------------------------------------------
-module(glusterfs_storage_spec_builder).
-author("Bartosz Walkowicz").

-include_lib("ctool/include/storage/glusterfs.hrl").

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


-spec build_credentials(map()) -> onedata_storage:glusterfs_credentials().
build_credentials(Params) ->
    #glusterfs_credentials{
        uid = maps:get(uid, Params, 0),
        gid = maps:get(gid, Params, 0)
    }.


-spec build_credentials_diff(map()) -> onedata_storage:glusterfs_credentials_diff().
build_credentials_diff(Params) ->
    #glusterfs_credentials_diff{
        uid = maps:get(uid, Params, undefined),
        gid = maps:get(gid, Params, undefined)
    }.


-spec build_configuration(map()) -> onedata_storage:glusterfs_configuration().
build_configuration(Params) ->
    #glusterfs_configuration{
        volume = maps:get(volume, Params),
        hostname = maps:get(hostname, Params),
        port = maps:get(port, Params, undefined),
        transport = case maps:get(transport, Params, undefined) of
            undefined -> undefined;
            Transport -> binary_to_transport(Transport)
        end,
        mount_point = maps:get(mountPoint, Params, undefined),
        xlator_options = maps:get(xlatorOptions, Params, undefined),
        storage_path_type = storage_spec_builder_utils:binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


-spec build_configuration_diff(map()) -> onedata_storage:glusterfs_configuration_diff().
build_configuration_diff(Params) ->
    #glusterfs_configuration_diff{
        volume = maps:get(volume, Params, undefined),
        hostname = maps:get(hostname, Params, undefined),
        port = maps:get(port, Params, undefined),
        transport = case maps:get(transport, Params, undefined) of
            undefined -> undefined;
            Transport -> binary_to_transport(Transport)
        end,
        mount_point = maps:get(mountPoint, Params, undefined),
        xlator_options = maps:get(xlatorOptions, Params, undefined)
    }.


-spec credentials_to_map(onedata_storage:glusterfs_credentials()) -> map().
credentials_to_map(#glusterfs_credentials{uid = Uid, gid = Gid}) ->
    Base = #{uid => Uid},
    maps_utils:put_if_defined(Base, gid, Gid).


-spec configuration_to_map(onedata_storage:glusterfs_configuration()) -> map().
configuration_to_map(#glusterfs_configuration{
    volume = Volume,
    hostname = Hostname,
    port = Port,
    transport = Transport,
    mount_point = MountPoint,
    xlator_options = XlatorOptions,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        volume => Volume,
        hostname => Hostname,
        storagePathType => storage_spec_builder_utils:storage_path_type_to_binary(StoragePathType)
    },
    Base1 = maps_utils:put_if_defined(Base, port, Port),
    Base2 = maps_utils:put_if_defined(Base1, transport, case Transport of
        undefined -> undefined;
        _ -> transport_to_binary(Transport)
    end),
    Base3 = maps_utils:put_if_defined(Base2, mountPoint, MountPoint),
    maps_utils:put_if_defined(Base3, xlatorOptions, XlatorOptions).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec binary_to_transport(binary()) -> tcp | rdma | socket.
binary_to_transport(<<"tcp">>) -> tcp;
binary_to_transport(<<"rdma">>) -> rdma;
binary_to_transport(<<"socket">>) -> socket.


%% @private
-spec transport_to_binary(tcp | rdma | socket) -> binary().
transport_to_binary(tcp) -> <<"tcp">>;
transport_to_binary(rdma) -> <<"rdma">>;
transport_to_binary(socket) -> <<"socket">>.
