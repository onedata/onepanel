%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(op_worker_luma).
-author("Jakub Kudzia").

%% API
-export([
    get_luma_configuration/1,
    clear_luma_db/1,
    get_user_mapping/2,
    get_default_posix_credentials/2,
    get_display_credentials/2,
    get_uid_to_onedata_user_mapping/2,
    get_acl_user_to_onedata_user_mapping/2,
    get_acl_group_to_onedata_group_mapping/2,
    add_user_mapping/2,
    add_default_posix_credentials/3,
    add_display_credentials/3,
    add_uid_to_onedata_user_mapping/3,
    add_acl_user_to_onedata_user_mapping/3,
    add_acl_group_to_onedata_group_mapping/3,
    update_user_mapping/3,
    remove_user_mapping/2,
    remove_default_posix_credentials/2,
    remove_display_credentials/2,
    remove_uid_to_onedata_user_mapping/2,
    remove_acl_user_to_onedata_user_mapping/2,
    remove_acl_group_to_onedata_group_mapping/2
]).

-type storage_id() :: binary().
-type space_id() :: binary().
-type onedata_user_id() :: binary().
-type uid() :: integer().
-type acl_principal() :: integer().
-type storage() :: term(). % todo
-type mapping() :: term(). % todo
-type credentials() :: term(). % todo

%%%===================================================================
%%% API functions
%%%===================================================================

-spec get_luma_configuration(storage()) -> ok.
get_luma_configuration(Storage) ->
    ok = op_worker_rpc:get_luma_configuration(Storage).

-spec clear_luma_db(storage_id()) -> ok.
clear_luma_db(StorageId) ->
    ok = op_worker_rpc:luma_clear_db(StorageId).

-spec get_user_mapping(storage_id(), onedata_user_id()) -> ok.
get_user_mapping(StorageId, OnedataUserId) ->
    op_worker_rpc:get_user_mapping(StorageId, OnedataUserId).


-spec get_default_posix_credentials(storage_id(), space_id()) -> ok.
get_default_posix_credentials(StorageId, SpaceId) ->
    op_worker_rpc:get_default_posix_credentials(StorageId, SpaceId).


-spec get_display_credentials(storage_id(), space_id()) -> ok.
get_display_credentials(StorageId, SpaceId) ->
    op_worker_rpc:get_display_credentials(StorageId, SpaceId).


-spec get_uid_to_onedata_user_mapping(storage_id(), uid()) -> ok.
get_uid_to_onedata_user_mapping(StorageId, Uid) ->
    op_worker_rpc:get_uid_to_onedata_user_mapping(StorageId, Uid).


-spec get_acl_user_to_onedata_user_mapping(storage_id(), acl_principal()) -> ok.
get_acl_user_to_onedata_user_mapping(StorageId, AclUser) ->
    op_worker_rpc:get_acl_user_to_onedata_user_mapping(StorageId, AclUser).

-spec get_acl_group_to_onedata_group_mapping(storage_id(), acl_principal()) -> ok.
get_acl_group_to_onedata_group_mapping(StorageId, AclGroup) ->
    op_worker_rpc:get_acl_group_to_onedata_group_mapping(StorageId, AclGroup).

-spec add_user_mapping(storage_id(), mapping()) -> ok.
add_user_mapping(StorageId, Mapping) ->
    op_worker_rpc:add_user_mapping(StorageId, Mapping).

-spec add_default_posix_credentials(storage_id(), space_id(), credentials()) -> ok.
add_default_posix_credentials(StorageId, SpaceId, Credentials) ->
    op_worker_rpc:add_default_posix_credentials(StorageId, SpaceId, Credentials).

-spec add_display_credentials(storage_id(), space_id(), credentials()) -> ok.
add_display_credentials(StorageId, SpaceId, Credentials) ->
    op_worker_rpc:add_display_credentials(StorageId, SpaceId, Credentials).

-spec add_uid_to_onedata_user_mapping(storage_id(), uid(), mapping()) -> ok.
add_uid_to_onedata_user_mapping(StorageId, Uid, Mapping) ->
    op_worker_rpc:add_uid_to_onedata_user_mapping(StorageId, Uid, Mapping).

-spec add_acl_user_to_onedata_user_mapping(storage_id(), acl_principal(), mapping()) -> ok.
add_acl_user_to_onedata_user_mapping(StorageId, AclUser, Mapping) ->
    op_worker_rpc:add_acl_user_to_onedata_user_mapping(StorageId, AclUser, Mapping).

-spec add_acl_group_to_onedata_group_mapping(storage_id(), acl_principal(), mapping()) -> ok.
add_acl_group_to_onedata_group_mapping(StorageId, AclGroup, Mapping) ->
    op_worker_rpc:add_acl_group_to_onedata_group_mapping(StorageId, AclGroup, Mapping).

-spec update_user_mapping(storage_id(), onedata_user_id(), mapping()) -> ok.
update_user_mapping(StorageId, OnedataUserId, Mapping) ->
    op_worker_rpc:update_user_mapping(StorageId, OnedataUserId, Mapping).

-spec remove_user_mapping(storage_id(), onedata_user_id()) -> ok.
remove_user_mapping(StorageId, OnedataUserId) ->
    op_worker_rpc:remove_user_mapping(StorageId, OnedataUserId).

-spec remove_default_posix_credentials(storage_id(), space_id()) -> ok.
remove_default_posix_credentials(StorageId, SpaceId) ->
    op_worker_rpc:remove_default_posix_credentials(StorageId, SpaceId).

-spec remove_display_credentials(storage_id(), space_id()) -> ok.
remove_display_credentials(StorageId, SpaceId) ->
    op_worker_rpc:remove_display_credentials(StorageId, SpaceId).

-spec remove_uid_to_onedata_user_mapping(storage_id(), uid()) -> ok.
remove_uid_to_onedata_user_mapping(StorageId, Uid) ->
    op_worker_rpc:remove_uid_to_onedata_user_mapping(StorageId, Uid).

-spec remove_acl_user_to_onedata_user_mapping(storage_id(), acl_principal()) -> ok.
remove_acl_user_to_onedata_user_mapping(StorageId, AclUser) ->
    op_worker_rpc:remove_acl_user_to_onedata_user_mapping(StorageId, AclUser).

-spec remove_acl_group_to_onedata_group_mapping(storage_id(), acl_principal()) -> ok.
remove_acl_group_to_onedata_group_mapping(StorageId, AclGroup) ->
    op_worker_rpc:remove_acl_group_to_onedata_group_mapping(StorageId, AclGroup).


%%%===================================================================
%%% Internal functions
%%%===================================================================

