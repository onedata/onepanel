%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module mirrors functions from op_worker's 'rpc_api' module,
%%% providing rpc access to them. Most functions come in two variants:
%%% one defaulting to any op_worker node and other with explicit Node argument.
%%% Unless noted otherwise in type spec, functions in this module throw
%%% an error upon receiving badrpc.
%%% @end
%%%-------------------------------------------------------------------
-module(op_worker_rpc).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include("names.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

% @formatter:off
-define(NO_EXCEPTION_CALL(Node, Args),
    rpc:call(Node, rpc_api, apply, [?FUNCTION_NAME, Args])).

-define(NO_EXCEPTION_CALL(Args),
    ?NO_EXCEPTION_CALL(element(2, {ok, _} = nodes:any(?SERVICE_OPW)), Args)).

-define(CALL(Node, Args),
    case ?NO_EXCEPTION_CALL(Node, Args) of
        {badrpc, nodedown} -> throw(?ERROR_SERVICE_UNAVAILABLE);
        {badrpc, _} = __Error -> error(__Error);
        __Result -> __Result
    end).

-define(CALL(Args),
    ?CALL(element(2, {ok, _} = nodes:any(?SERVICE_OPW)), Args)).
% @formatter:on

-opaque helper() :: tuple().
-opaque luma_config() :: tuple().
-opaque storage_data() :: tuple().
-type autocleaning_run_id() :: binary().
-type autocleaning_run_links_list_limit() :: integer() | all.
-type autocleaning_run_links_offset() :: integer().
-type file_popularity_config_id() :: od_space_id().
-type helper_args() :: #{binary() => binary()}.
-type helper_name() :: binary().
-type helper_user_ctx() :: #{binary() => binary()}.
-type luma_feed() :: atom().
-type luma_config_api_key() :: undefined | binary().
-type luma_config_url() :: binary().
-type luma_details() :: json_utils:json_map().
-type luma_uid() :: non_neg_integer().
-type luma_acl_who() :: binary().
-type luma_storage_user() :: json_utils:json_map().
-type luma_posix_credentials() :: #{binary() => non_neg_integer()}.
-type luma_onedata_user() :: #{binary() => binary()}.
-type luma_onedata_group() :: #{binary() => binary()}.
-type od_space_id() :: binary().
-type od_user_id() :: binary().
-type space_quota_id() :: od_space_id().
-type storage_import_config() :: map().
-type auto_storage_import_config() :: map().
-type storage_id() :: binary().
-type storage_name() :: binary().
-type storage_qos_parameters() :: #{binary() => binary()}.
-type storage_import_monitoring_plot_counter_type() :: op_worker_storage_import:metric_type().
-type storage_import_monitoring_window() :: day | hour | minute.

-export_type([storage_data/0, luma_config/0, helper/0,
    helper_args/0, helper_user_ctx/0, od_space_id/0, luma_feed/0, luma_details/0]).

-export([storage_create/6, storage_create/7]).
-export([storage_safe_remove/1, storage_safe_remove/2]).
-export([storage_supports_any_space/1, storage_supports_any_space/2]).
-export([storage_list_ids/0, storage_list_ids/1]).
-export([storage_get_helper/1, storage_get_helper/2]).
-export([storage_update_admin_ctx/2, storage_update_admin_ctx/3]).
-export([storage_update_helper_args/2, storage_update_helper_args/3]).
-export([storage_set_imported_storage/2, storage_set_imported_storage/3]).
-export([storage_set_readonly/2, storage_set_readonly/3]).
-export([storage_set_qos_parameters/2, storage_set_qos_parameters/3]).
-export([storage_update_luma_config/2, storage_update_luma_config/3]).
-export([storage_update_name/2, storage_update_name/3]).
-export([storage_exists/1, storage_exists/2]).
-export([storage_describe/1, storage_describe/2]).
-export([storage_is_imported_storage/1, storage_is_imported_storage/2]).
-export([storage_get_luma_feed/1, storage_get_luma_feed/2]).
-export([luma_clear_db/1, luma_clear_db/2]).
-export([luma_storage_users_get_and_describe/2, luma_storage_users_get_and_describe/3]).
-export([luma_storage_users_store/3, luma_storage_users_store/4]).
-export([luma_storage_users_update/3, luma_storage_users_update/4]).
-export([luma_storage_users_delete/2, luma_storage_users_delete/3]).
-export([luma_spaces_display_defaults_get_and_describe/2, luma_spaces_display_defaults_get_and_describe/3]).
-export([luma_spaces_display_defaults_store/3, luma_spaces_display_defaults_store/4]).
-export([luma_spaces_display_defaults_delete/2, luma_spaces_display_defaults_delete/3]).
-export([luma_spaces_posix_storage_defaults_get_and_describe/2, luma_spaces_posix_storage_defaults_get_and_describe/3]).
-export([luma_spaces_posix_storage_defaults_store/3, luma_spaces_posix_storage_defaults_store/4]).
-export([luma_spaces_posix_storage_defaults_delete/2, luma_spaces_posix_storage_defaults_delete/3]).
-export([luma_onedata_users_get_by_uid_and_describe/2, luma_onedata_users_get_by_uid_and_describe/3]).
-export([luma_onedata_users_store_by_uid/3, luma_onedata_users_store_by_uid/4]).
-export([luma_onedata_users_delete_uid_mapping/2, luma_onedata_users_delete_uid_mapping/3]).
-export([luma_onedata_users_get_by_acl_user_and_describe/2, luma_onedata_users_get_by_acl_user_and_describe/3]).
-export([luma_onedata_users_store_by_acl_user/3, luma_onedata_users_store_by_acl_user/4]).
-export([luma_onedata_users_delete_acl_user_mapping/2, luma_onedata_users_delete_acl_user_mapping/3]).
-export([luma_onedata_groups_get_and_describe/2, luma_onedata_groups_get_and_describe/3]).
-export([luma_onedata_groups_store/3, luma_onedata_groups_store/4]).
-export([luma_onedata_groups_delete/2, luma_onedata_groups_delete/3]).
-export([new_helper/3, new_helper/4]).
-export([new_luma_config/1, new_luma_config/2]).
-export([new_luma_config_with_external_feed/2, new_luma_config_with_external_feed/3]).
-export([verify_storage_on_all_nodes/2, verify_storage_on_all_nodes/3]).
-export([prepare_helper_args/2, prepare_helper_args/3]).
-export([prepare_user_ctx_params/2, prepare_user_ctx_params/3]).
-export([space_logic_get_storage_ids/1, space_logic_get_storage_ids/2]).
-export([file_popularity_api_configure/2, file_popularity_api_configure/3]).
-export([file_popularity_api_get_configuration/1, file_popularity_api_get_configuration/2]).
-export([autocleaning_configure/2, autocleaning_configure/3]).
-export([autocleaning_get_configuration/1, autocleaning_get_configuration/2]).
-export([autocleaning_list_reports/4, autocleaning_list_reports/5]).
-export([autocleaning_get_run_report/1, autocleaning_get_run_report/2]).
-export([autocleaning_status/1, autocleaning_status/2]).
-export([autocleaning_force_run/1, autocleaning_force_run/2]).
-export([autocleaning_cancel_run/1, autocleaning_cancel_run/2]).
-export([get_provider_id/0, get_provider_id/1]).
-export([get_access_token/0, get_access_token/1]).
-export([get_identity_token/0, get_identity_token/1]).
-export([is_connected_to_oz/0, is_connected_to_oz/1]).
-export([is_registered/0, is_registered/1]).
-export([on_deregister/0, on_deregister/1]).
-export([get_op_worker_version/0, get_op_worker_version/1]).
-export([provider_logic_update/1, provider_logic_update/2]).
-export([support_space/3, support_space/4]).
-export([revoke_space_support/1, revoke_space_support/2]).
-export([get_spaces/0, get_spaces/1]).
-export([supports_space/1, supports_space/2]).
-export([get_space_details/1, get_space_details/2]).
-export([get_provider_details/0, get_provider_details/1]).
-export([is_subdomain_delegated/0, is_subdomain_delegated/1]).
-export([set_delegated_subdomain/1, set_delegated_subdomain/2]).
-export([set_domain/1, set_domain/2]).
-export([space_quota_current_size/1, space_quota_current_size/2]).
-export([update_space_support_size/2, update_space_support_size/3]).
-export([update_subdomain_delegation_ips/0, update_subdomain_delegation_ips/1]).
-export([force_oz_connection_start/0, force_oz_connection_start/1]).
-export([provider_auth_save/2, provider_auth_save/3]).
-export([get_root_token_file_path/0, get_root_token_file_path/1]).
-export([storage_import_get_configuration/1, storage_import_get_configuration/2]).
-export([storage_import_set_manual_import/1, storage_import_set_manual_import/2]).
-export([storage_import_configure_auto_import/2, storage_import_configure_auto_import/3]).
-export([storage_import_start_scan/1, storage_import_start_scan/2]).
-export([storage_import_stop_scan/1, storage_import_stop_scan/2]).
-export([storage_import_get_stats/3, storage_import_get_stats/4]).
-export([storage_import_get_info/1, storage_import_get_info/2]).
-export([storage_import_get_manual_example/1, storage_import_get_manual_example/2]).
-export([restart_rtransfer_link/0, restart_rtransfer_link/1]).
-export([set_txt_record/3, set_txt_record/4]).
-export([remove_txt_record/1, remove_txt_record/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec storage_create(storage_name(), helper(),
    luma_config(), Imported :: boolean(), Readonly :: boolean(), storage_qos_parameters()) ->
    {ok, storage_id()} | {error, term()}.
storage_create(Name, Helpers, LumaConfig, ImportedStorage, Readonly, QosParameters) ->
    ?CALL([Name, Helpers, LumaConfig, ImportedStorage, Readonly, QosParameters]).

-spec storage_create(node(), storage_name(), helper(),
    luma_config(), Imported :: boolean(), Readonly :: boolean(), storage_qos_parameters()) ->
    {ok, storage_id()} | {error, term()}.
storage_create(Node, Name, Helpers, LumaConfig, ImportedStorage, Readonly, QosParameters) ->
    ?CALL(Node, [Name, Helpers, LumaConfig, ImportedStorage, Readonly, QosParameters]).


-spec storage_safe_remove(op_worker_storage:id()) -> ok | {error, storage_in_use | term()}.
storage_safe_remove(StorageId) ->
    ?CALL([StorageId]).

-spec storage_safe_remove(node(), op_worker_storage:id()) -> ok | {error, storage_in_use | term()}.
storage_safe_remove(Node, StorageId) ->
    ?CALL(Node, [StorageId]).


-spec storage_supports_any_space(storage_id()) -> boolean().
storage_supports_any_space(StorageId) ->
    ?CALL([StorageId]).

-spec storage_supports_any_space(node(), storage_id()) -> boolean().
storage_supports_any_space(Node, StorageId) ->
    ?CALL(Node, [StorageId]).


-spec storage_list_ids() -> {ok, [storage_id()]} | {error, term()}.
storage_list_ids() ->
    ?CALL([]).

-spec storage_list_ids(node()) -> {ok, [storage_id()]} | {error, term()}.
storage_list_ids(Node) ->
    ?CALL(Node, []).


-spec storage_get_helper(storage_id()) -> {ok, helper()} | {error, Reason :: term()}.
storage_get_helper(StorageId) ->
    ?CALL([StorageId]).

-spec storage_get_helper(node(), storage_id()) -> {ok, helper()} | {error, Reason :: term()}.
storage_get_helper(Node, StorageId) ->
    ?CALL(Node, [StorageId]).



-spec storage_update_admin_ctx(storage_id(), helper_user_ctx()) ->
    ok | {error, term()}.
storage_update_admin_ctx(StorageId, Changes) ->
    ?CALL([StorageId, Changes]).

-spec storage_update_admin_ctx(node(), storage_id(), helper_user_ctx()) ->
    ok | {error, term()}.
storage_update_admin_ctx(Node, StorageId, Changes) ->
    ?CALL(Node, [StorageId, Changes]).


-spec storage_update_helper_args(storage_id(), helper_args()) ->
    ok | {error, term()}.
storage_update_helper_args(StorageId, Changes) ->
    ?CALL([StorageId, Changes]).

-spec storage_update_helper_args(node(), storage_id(), helper_args()) ->
    ok | {error, term()}.
storage_update_helper_args(Node, StorageId, Changes) ->
    ?CALL(Node, [StorageId, Changes]).


-spec storage_set_imported_storage(storage_id(), boolean()) ->
    ok | {error, term()}.
storage_set_imported_storage(StorageId, Value) ->
    ?CALL([StorageId, Value]).

-spec storage_set_imported_storage(node(), storage_id(), boolean()) ->
    ok | {error, term()}.
storage_set_imported_storage(Node, StorageId, Value) ->
    ?CALL(Node, [StorageId, Value]).


-spec storage_set_readonly(storage_id(), boolean()) ->
    ok | {error, term()}.
storage_set_readonly(StorageId, Value) ->
    ?CALL([StorageId, Value]).

-spec storage_set_readonly(node(), storage_id(), boolean()) ->
    ok | {error, term()}.
storage_set_readonly(Node, StorageId, Value) ->
    ?CALL(Node, [StorageId, Value]).


-spec storage_update_luma_config(storage_id(), Diff) -> ok | {error, term()}
    when Diff :: #{mode => luma_feed(), url => luma_config_url(), api_key => luma_config_api_key()}.
storage_update_luma_config(StorageId, Changes) ->
    ?CALL([StorageId, Changes]).

-spec storage_update_luma_config(node(), storage_id(), Diff) -> ok | {error, term()}
    when Diff :: #{mode => luma_feed(), url => luma_config_url(), api_key => luma_config_api_key()}.
storage_update_luma_config(Node, StorageId, Changes) ->
    ?CALL(Node, [StorageId, Changes]).


-spec storage_set_qos_parameters(storage_id(), op_worker_storage:qos_parameters()) ->
    ok | {error, term()}.
storage_set_qos_parameters(StorageId, QosParameters) ->
    ?CALL([StorageId, QosParameters]).

-spec storage_set_qos_parameters(node(), storage_id(), op_worker_storage:qos_parameters()) ->
    ok | {error, term()}.
storage_set_qos_parameters(Node, StorageId, QosParameters) ->
    ?CALL(Node, [StorageId, QosParameters]).


-spec storage_update_name(storage_id(), storage_name()) ->
    ok.
storage_update_name(StorageId, NewName) ->
    ?CALL([StorageId, NewName]).

-spec storage_update_name(node(), storage_id(), storage_name()) ->
    ok.
storage_update_name(Node, StorageId, NewName) ->
    ?CALL(Node, [StorageId, NewName]).


-spec storage_exists(storage_id()) -> boolean().
storage_exists(StorageId) ->
    ?CALL([StorageId]).

-spec storage_exists(node(), storage_id()) -> boolean().
storage_exists(Node, StorageId) ->
    ?CALL(Node, [StorageId]).


-spec storage_describe(storage_id()) ->
    {ok, #{binary() := binary() | boolean() | undefined}} | {error, term()}.
storage_describe(StorageId) ->
    ?CALL([StorageId]).

-spec storage_describe(node(), storage_id()) ->
    {ok, #{binary() := binary() | boolean() | undefined}} | {error, term()}.
storage_describe(Node, StorageId) ->
    ?CALL(Node, [StorageId]).


-spec storage_is_imported_storage(storage_id()) ->
    boolean() | {error, term()}.
storage_is_imported_storage(StorageId) ->
    ?CALL([StorageId]).

-spec storage_is_imported_storage(node(), storage_id()) ->
    boolean() | {error, term()}.
storage_is_imported_storage(Node, StorageId) ->
    ?CALL(Node, [StorageId]).


-spec storage_get_luma_feed(storage_id() | storage_data()) -> luma_feed().
storage_get_luma_feed(Storage) ->
    ?CALL([Storage]).

-spec storage_get_luma_feed(node(), storage_data()) -> luma_feed().
storage_get_luma_feed(Node, Storage) ->
    ?CALL(Node, [Storage]).


-spec luma_clear_db(storage_id()) -> ok.
luma_clear_db(StorageId) ->
    ?CALL([StorageId]).

-spec luma_clear_db(node(), storage_id()) -> ok.
luma_clear_db(Node, StorageId) ->
    ?CALL(Node, [StorageId]).


-spec luma_storage_users_get_and_describe(storage_id(), od_user_id()) -> {ok, luma_storage_user()}.
luma_storage_users_get_and_describe(Storage, UserId) ->
    ?CALL([Storage, UserId]).

-spec luma_storage_users_get_and_describe(node(), storage_id(), od_user_id()) -> {ok, luma_storage_user()}.
luma_storage_users_get_and_describe(Node, Storage, UserId) ->
    ?CALL(Node, [Storage, UserId]).


-spec luma_storage_users_store(storage_id(), luma_onedata_user(), luma_storage_user()) ->
    {ok, od_user_id()}.
luma_storage_users_store(Storage, OnedataUser, StorageUser) ->
    ?CALL([Storage, OnedataUser, StorageUser]).

-spec luma_storage_users_store(node(), storage_id(), luma_onedata_user(), luma_storage_user()) ->
    {ok, od_user_id()}.
luma_storage_users_store(Node, Storage, OnedataUser, StorageUser) ->
    ?CALL(Node, [Storage, OnedataUser, StorageUser]).


-spec luma_storage_users_update(storage_id(), od_user_id(), luma_storage_user()) ->
    {ok, od_user_id()}.
luma_storage_users_update(Storage, UserId, StorageUser) ->
    ?CALL([Storage, UserId, StorageUser]).

-spec luma_storage_users_update(node(), storage_id(), od_user_id(), luma_storage_user()) ->
    {ok, od_user_id()}.
luma_storage_users_update(Node, Storage, UserId, StorageUser) ->
    ?CALL(Node, [Storage, UserId, StorageUser]).


-spec luma_storage_users_delete(storage_id(), od_user_id()) -> ok.
luma_storage_users_delete(Storage, UserId) ->
    ?CALL([Storage, UserId]).

-spec luma_storage_users_delete(node(), storage_id(), od_user_id()) -> ok.
luma_storage_users_delete(Node, Storage, UserId) ->
    ?CALL(Node, [Storage, UserId]).


-spec luma_spaces_display_defaults_get_and_describe(storage_id(), od_space_id()) -> ok.
luma_spaces_display_defaults_get_and_describe(Storage, SpaceId) ->
    ?CALL([Storage, SpaceId]).

-spec luma_spaces_display_defaults_get_and_describe(node(), storage_id(), od_space_id()) -> ok.
luma_spaces_display_defaults_get_and_describe(Node, Storage, SpaceId) ->
    ?CALL(Node, [Storage, SpaceId]).


-spec luma_spaces_display_defaults_store(storage_id(), od_space_id(), luma_posix_credentials()) -> ok.
luma_spaces_display_defaults_store(Storage, SpaceId, DisplayDefaults) ->
    ?CALL([Storage, SpaceId, DisplayDefaults]).

-spec luma_spaces_display_defaults_store(node(), storage_id(), od_space_id(), luma_posix_credentials()) -> ok.
luma_spaces_display_defaults_store(Node, Storage, SpaceId, DisplayDefaults) ->
    ?CALL(Node, [Storage, SpaceId, DisplayDefaults]).


-spec luma_spaces_display_defaults_delete(storage_id(), od_space_id()) -> ok.
luma_spaces_display_defaults_delete(Storage, SpaceId) ->
    ?CALL([Storage, SpaceId]).

-spec luma_spaces_display_defaults_delete(node(), storage_id(), od_space_id()) -> ok.
luma_spaces_display_defaults_delete(Node, Storage, SpaceId) ->
    ?CALL(Node, [Storage, SpaceId]).


-spec luma_spaces_posix_storage_defaults_get_and_describe(storage_id(), od_space_id()) -> ok.
luma_spaces_posix_storage_defaults_get_and_describe(Storage, SpaceId) ->
    ?CALL([Storage, SpaceId]).

-spec luma_spaces_posix_storage_defaults_get_and_describe(node(), storage_id(), od_space_id()) -> ok.
luma_spaces_posix_storage_defaults_get_and_describe(Node, Storage, SpaceId) ->
    ?CALL(Node, [Storage, SpaceId]).


-spec luma_spaces_posix_storage_defaults_store(storage_id(), od_space_id(), luma_posix_credentials()) -> ok.
luma_spaces_posix_storage_defaults_store(Storage, SpaceId, PosixDefaults) ->
    ?CALL([Storage, SpaceId, PosixDefaults]).

-spec luma_spaces_posix_storage_defaults_store(node(), storage_id(), od_space_id(), luma_posix_credentials()) -> ok.
luma_spaces_posix_storage_defaults_store(Node, Storage, SpaceId, PosixDefaults) ->
    ?CALL(Node, [Storage, SpaceId, PosixDefaults]).


-spec luma_spaces_posix_storage_defaults_delete(storage_id(), od_space_id()) -> ok.
luma_spaces_posix_storage_defaults_delete(Storage, SpaceId) ->
    ?CALL([Storage, SpaceId]).

-spec luma_spaces_posix_storage_defaults_delete(node(), storage_id(), od_space_id()) -> ok.
luma_spaces_posix_storage_defaults_delete(Node, Storage, SpaceId) ->
    ?CALL(Node, [Storage, SpaceId]).


-spec luma_onedata_users_get_by_uid_and_describe(storage_id(), luma_uid()) -> ok.
luma_onedata_users_get_by_uid_and_describe(Storage, Uid) ->
    ?CALL([Storage, Uid]).

-spec luma_onedata_users_get_by_uid_and_describe(node(), storage_id(), luma_uid()) -> ok.
luma_onedata_users_get_by_uid_and_describe(Node, Storage, Uid) ->
    ?CALL(Node, [Storage, Uid]).


-spec luma_onedata_users_store_by_uid(storage_id(), luma_uid(), luma_onedata_user()) -> ok.
luma_onedata_users_store_by_uid(Storage, Uid, OnedataUser) ->
    ?CALL([Storage, Uid, OnedataUser]).

-spec luma_onedata_users_store_by_uid(node(), storage_id(), luma_uid(), luma_posix_credentials()) -> ok.
luma_onedata_users_store_by_uid(Node, Storage, Uid, OnedataUser) ->
    ?CALL(Node, [Storage, Uid, OnedataUser]).


-spec luma_onedata_users_delete_uid_mapping(storage_id(), luma_uid()) -> ok.
luma_onedata_users_delete_uid_mapping(Storage, Uid) ->
    ?CALL([Storage, Uid]).

-spec luma_onedata_users_delete_uid_mapping(node(), storage_id(), luma_uid()) -> ok.
luma_onedata_users_delete_uid_mapping(Node, Storage, Uid) ->
    ?CALL(Node, [Storage, Uid]).


-spec luma_onedata_users_get_by_acl_user_and_describe(storage_id(), luma_acl_who()) -> ok.
luma_onedata_users_get_by_acl_user_and_describe(Storage, AclUser) ->
    ?CALL([Storage, AclUser]).

-spec luma_onedata_users_get_by_acl_user_and_describe(node(), storage_id(), luma_acl_who()) -> ok.
luma_onedata_users_get_by_acl_user_and_describe(Node, Storage, AclUser) ->
    ?CALL(Node, [Storage, AclUser]).


-spec luma_onedata_users_store_by_acl_user(storage_id(), luma_acl_who(), luma_onedata_user()) -> ok.
luma_onedata_users_store_by_acl_user(Storage, AclUser, OnedataUser) ->
    ?CALL([Storage, AclUser, OnedataUser]).

-spec luma_onedata_users_store_by_acl_user(node(), storage_id(), luma_acl_who(), luma_onedata_user()) -> ok.
luma_onedata_users_store_by_acl_user(Node, Storage, AclUser, OnedataUser) ->
    ?CALL(Node, [Storage, AclUser, OnedataUser]).


-spec luma_onedata_users_delete_acl_user_mapping(storage_id(), luma_acl_who()) -> ok.
luma_onedata_users_delete_acl_user_mapping(Storage, AclUser) ->
    ?CALL([Storage, AclUser]).

-spec luma_onedata_users_delete_acl_user_mapping(node(), storage_id(), luma_acl_who()) -> ok.
luma_onedata_users_delete_acl_user_mapping(Node, Storage, AclUser) ->
    ?CALL(Node, [Storage, AclUser]).


-spec luma_onedata_groups_get_and_describe(storage_id(), luma_acl_who()) -> ok.
luma_onedata_groups_get_and_describe(Storage, AclGroup) ->
    ?CALL([Storage, AclGroup]).

-spec luma_onedata_groups_get_and_describe(node(), storage_id(), luma_acl_who()) -> ok.
luma_onedata_groups_get_and_describe(Node, Storage, AclGroup) ->
    ?CALL(Node, [Storage, AclGroup]).


-spec luma_onedata_groups_store(storage_id(), od_space_id(), luma_onedata_group()) -> ok.
luma_onedata_groups_store(Storage, SpaceId, OnedataGroup) ->
    ?CALL([Storage, SpaceId, OnedataGroup]).

-spec luma_onedata_groups_store(node(), storage_id(), od_space_id(), luma_onedata_group()) -> ok.
luma_onedata_groups_store(Node, Storage, SpaceId, OnedataGroup) ->
    ?CALL(Node, [Storage, SpaceId, OnedataGroup]).


-spec luma_onedata_groups_delete(storage_id(), luma_acl_who()) -> ok.
luma_onedata_groups_delete(Storage, AclGroup) ->
    ?CALL([Storage, AclGroup]).

-spec luma_onedata_groups_delete(node(), storage_id(), luma_acl_who()) -> ok.
luma_onedata_groups_delete(Node, Storage, AclGroup) ->
    ?CALL(Node, [Storage, AclGroup]).


-spec new_helper(helper_name(), helper_args(), helper_user_ctx()) ->
    {ok, helper()}.
new_helper(HelperName, Args, AdminCtx) ->
    ?CALL([HelperName, Args, AdminCtx]).

-spec new_helper(node(), helper_name(), helper_args(), helper_user_ctx()) ->
    {ok, helper()}.
new_helper(Node, HelperName, Args, AdminCtx) ->
    ?CALL(Node, [HelperName, Args, AdminCtx]).


-spec new_luma_config(Mode :: luma_feed()) ->
    luma_config().
new_luma_config(Mode) ->
    ?CALL([Mode]).

-spec new_luma_config(node(), Mode :: luma_feed()) ->
    luma_config().
new_luma_config(Node, Mode) ->
    ?CALL(Node, [Mode]).


-spec new_luma_config_with_external_feed(URL :: binary(), ApiKey :: binary() | undefined) ->
    luma_config().
new_luma_config_with_external_feed(URL, ApiKey) ->
    ?CALL([URL, ApiKey]).

-spec new_luma_config_with_external_feed(node(), URL :: binary(), ApiKey :: binary() | undefined) ->
    luma_config().
new_luma_config_with_external_feed(Node, URL, ApiKey) ->
    ?CALL(Node, [URL, ApiKey]).


-spec verify_storage_on_all_nodes(helper(), luma_feed()) -> ok | errors:error().
verify_storage_on_all_nodes(Helper, LumaFeed) ->
    ?CALL([Helper, LumaFeed]).

-spec verify_storage_on_all_nodes(node(), helper(), luma_feed()) -> ok | errors:error().
verify_storage_on_all_nodes(Node, Helper, LumaFeed) ->
    ?CALL(Node, [Helper, LumaFeed]).


-spec prepare_helper_args(helper_name(), helper_args()) -> helper_args().
prepare_helper_args(HelperName, Params) ->
    ?CALL([HelperName, Params]).

-spec prepare_helper_args(node(), helper_name(), helper_args()) -> helper_args().
prepare_helper_args(Node, HelperName, Params) ->
    ?CALL(Node, [HelperName, Params]).


-spec prepare_user_ctx_params(helper_name(), helper_user_ctx()) -> helper_user_ctx().
prepare_user_ctx_params(HelperName, Params) ->
    ?CALL([HelperName, Params]).

-spec prepare_user_ctx_params(node(), helper_name(), helper_user_ctx()) -> helper_user_ctx().
prepare_user_ctx_params(Node, HelperName, Params) ->
    ?CALL(Node, [HelperName, Params]).

-spec space_logic_get_storage_ids(od_space_id()) -> {ok, [op_worker_storage:id()]}.
space_logic_get_storage_ids(SpaceId) ->
    ?CALL([SpaceId]).

-spec space_logic_get_storage_ids(node(), od_space_id()) -> {ok, [op_worker_storage:id()]}.
space_logic_get_storage_ids(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec file_popularity_api_configure(file_popularity_config_id(), map()) ->
    ok | errors:error() | {error, term()}.
file_popularity_api_configure(SpaceId, NewConfiguration) ->
    ?CALL([SpaceId, NewConfiguration]).

-spec file_popularity_api_configure(node(), file_popularity_config_id(), map()) ->
    ok | errors:error() | {error, term()}.
file_popularity_api_configure(Node, SpaceId, NewConfiguration) ->
    ?CALL(Node, [SpaceId, NewConfiguration]).


-spec file_popularity_api_get_configuration(file_popularity_config_id()) ->
    {ok, map()} | {error, term()}.
file_popularity_api_get_configuration(SpaceId) ->
    ?CALL([SpaceId]).

-spec file_popularity_api_get_configuration(node(), file_popularity_config_id()) ->
    {ok, map()} | {error, term()}.
file_popularity_api_get_configuration(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec autocleaning_configure(od_space_id(), map()) ->
    ok | errors:error() | {error, term()}.
autocleaning_configure(SpaceId, Configuration) ->
    ?CALL([SpaceId, Configuration]).

-spec autocleaning_configure(node(), od_space_id(), map()) ->
    ok | errors:error() | {error, term()}.
autocleaning_configure(Node, SpaceId, Configuration) ->
    ?CALL(Node, [SpaceId, Configuration]).


-spec autocleaning_get_configuration(od_space_id()) -> map().
autocleaning_get_configuration(SpaceId) ->
    ?CALL([SpaceId]).

-spec autocleaning_get_configuration(node(), od_space_id()) -> map().
autocleaning_get_configuration(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec get_provider_id() -> {ok, service_oneprovider:id()} | {error, term()}.
get_provider_id() ->
    ?CALL([]).

-spec get_provider_id(node()) -> {ok, service_oneprovider:id()} | {error, term()}.
get_provider_id(Node) ->
    ?CALL(Node, []).


-spec get_access_token() -> {ok, tokens:serialized()} | {error, term()}.
get_access_token() ->
    ?CALL([]).

-spec get_access_token(node()) -> {ok, tokens:serialized()} | {error, term()}.
get_access_token(Node) ->
    ?CALL(Node, []).


-spec get_identity_token() -> {ok, tokens:serialized()} | {error, term()}.
get_identity_token() ->
    ?CALL([]).

-spec get_identity_token(node()) -> {ok, tokens:serialized()} | {error, term()}.
get_identity_token(Node) ->
    ?CALL(Node, []).


-spec is_connected_to_oz() -> boolean().
is_connected_to_oz() ->
    ?CALL([]).

-spec is_connected_to_oz(node()) -> boolean().
is_connected_to_oz(Node) ->
    ?CALL(Node, []).


-spec is_registered() -> boolean() | {badrpc, term()}.
is_registered() ->
    ?NO_EXCEPTION_CALL([]).

-spec is_registered(node()) -> boolean() | {badrpc, term()}.
is_registered(Node) ->
    ?NO_EXCEPTION_CALL(Node, []).


-spec on_deregister() -> ok.
on_deregister() ->
    ?CALL([]).

-spec on_deregister(node()) -> ok.
on_deregister(Node) ->
    ?CALL(Node, []).


-spec get_op_worker_version() -> binary() | {badrpc, term()}.
get_op_worker_version() ->
    ?NO_EXCEPTION_CALL([]).

-spec get_op_worker_version(node()) -> binary() | {badrpc, term()}.
get_op_worker_version(Node) ->
    ?NO_EXCEPTION_CALL(Node, []).


-spec provider_logic_update(Data :: #{binary() => term()}) ->
    ok | {error, term()}.
provider_logic_update(Data) ->
   ?CALL([Data]).

-spec provider_logic_update(node(), Data :: #{binary() => term()}) ->
    ok | {error, term()}.
provider_logic_update(Node, Data) ->
    ?CALL(Node, [Data]).


-spec support_space(storage_id(), tokens:serialized(), SupportSize :: integer()) ->
    {ok, od_space_id()} | errors:error().
support_space(StorageId, Token, SupportSize) ->
    ?CALL([StorageId, Token, SupportSize]).

-spec support_space(node(), storage_id(), tokens:serialized(), SupportSize :: integer()) ->
    {ok, od_space_id()} | errors:error().
support_space(Node, StorageId, Token, SupportSize) ->
    ?CALL(Node, [StorageId, Token, SupportSize]).


-spec revoke_space_support(od_space_id()) -> ok | {error, term()}.
revoke_space_support(SpaceId) ->
    ?CALL([SpaceId]).

-spec revoke_space_support(node(), od_space_id()) -> ok | {error, term()}.
revoke_space_support(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec get_spaces() -> {ok, [od_space_id()]} | {error, term()}.
get_spaces() ->
    ?CALL([]).

-spec get_spaces(node()) -> {ok, [od_space_id()]} | {error, term()}.
get_spaces(Node) ->
    ?CALL(Node, []).


-spec supports_space(od_space_id()) -> boolean().
supports_space(SpaceId) ->
    ?CALL([SpaceId]).

-spec supports_space(node(), od_space_id()) -> boolean().
supports_space(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec get_space_details(od_space_id()) ->
    {ok, #{atom() := term()}} | {error, term()}.
get_space_details(SpaceId) ->
    ?CALL([SpaceId]).

-spec get_space_details(node(), od_space_id()) ->
    {ok, #{atom() := term()}} | {error, term()}.
get_space_details(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


%%--------------------------------------------------------------------
%% @doc
%% Returns current provider's data in a map.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_details() -> {ok, #{atom() := term()}} | {error, term()}.
get_provider_details() ->
    ?CALL([]).

-spec get_provider_details(node()) ->
    {ok, #{atom() := term()}} | {error, term()}.
get_provider_details(Node) ->
    ?CALL(Node, []).


-spec is_subdomain_delegated() ->
    {true, Subdomain :: binary()} | false | {error, term()}.
is_subdomain_delegated() ->
    ?CALL([]).

-spec is_subdomain_delegated(node()) ->
    {true, Subdomain :: binary()} | false | {error, term()}.
is_subdomain_delegated(Node) ->
    ?CALL(Node, []).


-spec set_delegated_subdomain(binary()) ->
    ok | {error, subdomain_exists} | {error, term()}.
set_delegated_subdomain(Subdomain) ->
    ?CALL([Subdomain]).

-spec set_delegated_subdomain(node(), binary()) ->
    ok | {error, subdomain_exists} | {error, term()}.
set_delegated_subdomain(Node, Subdomain) ->
    ?CALL(Node, [Subdomain]).


-spec set_domain(binary()) -> ok | {error, term()}.
set_domain(Domain) ->
    ?CALL([Domain]).

-spec set_domain(node(), binary()) -> ok | {error, term()}.
set_domain(Node, Domain) ->
    ?CALL(Node, [Domain]).


-spec space_quota_current_size(space_quota_id()) -> non_neg_integer().
space_quota_current_size(SpaceId) ->
    ?CALL([SpaceId]).

-spec space_quota_current_size(node(), space_quota_id()) -> non_neg_integer().
space_quota_current_size(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec update_space_support_size(od_space_id(), NewSupportSize :: integer()) ->
    ok | errors:error().
update_space_support_size(SpaceId, NewSupportSize) ->
    ?CALL([SpaceId, NewSupportSize]).

-spec update_space_support_size(node(), od_space_id(), NewSupportSize :: integer()) ->
    ok | errors:error().
update_space_support_size(Node, SpaceId, NewSupportSize) ->
    ?CALL(Node, [SpaceId, NewSupportSize]).


-spec update_subdomain_delegation_ips() -> ok | error.
update_subdomain_delegation_ips() ->
    ?CALL([]).

-spec update_subdomain_delegation_ips(node()) -> ok | error.
update_subdomain_delegation_ips(Node) ->
    ?CALL(Node, []).


-spec autocleaning_list_reports(od_space_id(),
    autocleaning_run_id() | undefined, autocleaning_run_links_offset(),
    autocleaning_run_links_list_limit()) ->
    {ok, [autocleaning_run_id()]}.
autocleaning_list_reports(SpaceId, Index, Offset, Limit) ->
    ?CALL([SpaceId, Index, Offset, Limit]).

-spec autocleaning_list_reports(node(), od_space_id(),
    autocleaning_run_id() | undefined, autocleaning_run_links_offset(),
    autocleaning_run_links_list_limit()) ->
    {ok, [autocleaning_run_id()]}.
autocleaning_list_reports(Node, SpaceId, Index, Offset, Limit) ->
    ?CALL(Node, [SpaceId, Index, Offset, Limit]).


-spec autocleaning_get_run_report(autocleaning_run_id()) ->
    {ok, map()} | {error, term()}.
autocleaning_get_run_report(RunId) ->
    ?CALL([RunId]).

-spec autocleaning_get_run_report(node(), autocleaning_run_id()) ->
    {ok, map()} | {error, term()}.
autocleaning_get_run_report(Node, RunId) ->
    ?CALL(Node, [RunId]).


-spec autocleaning_status(od_space_id()) -> map().
autocleaning_status(SpaceId) ->
    ?CALL([SpaceId]).

-spec autocleaning_status(node(), od_space_id()) -> map().
autocleaning_status(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec autocleaning_force_run(od_space_id()) ->
    {ok, autocleaning_run_id()} | {error, term()}.
autocleaning_force_run(SpaceId) ->
    ?CALL([SpaceId]).

-spec autocleaning_force_run(node(), od_space_id()) ->
    {ok, autocleaning_run_id()} | {error, term()}.
autocleaning_force_run(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec autocleaning_cancel_run(od_space_id()) -> ok | {error, term()}.
autocleaning_cancel_run(SpaceId) ->
    ?CALL([SpaceId]).

-spec autocleaning_cancel_run(node(), od_space_id()) -> ok | {error, term()}.
autocleaning_cancel_run(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec force_oz_connection_start() -> boolean().
force_oz_connection_start() ->
    ?CALL([]).

-spec force_oz_connection_start(node()) -> boolean().
force_oz_connection_start(Node) ->
    ?CALL(Node, []).



-spec provider_auth_save(service_oneprovider:id(), tokens:serialized()) -> ok.
provider_auth_save(ProviderId, RootToken) ->
    ?CALL([ProviderId, RootToken]).

-spec provider_auth_save(node(), service_oneprovider:id(), tokens:serialized()) ->
    ok.
provider_auth_save(Node, ProviderId, RootToken) ->
    ?CALL(Node, [ProviderId, RootToken]).


-spec get_root_token_file_path() -> string().
get_root_token_file_path() ->
    ?CALL([]).

-spec get_root_token_file_path(node()) -> string().
get_root_token_file_path(Node) ->
    ?CALL(Node, []).


-spec storage_import_get_configuration(od_space_id()) ->
    {ok, storage_import_config()} | {error, term()}.
storage_import_get_configuration(SpaceId) ->
    ?CALL([SpaceId]).

-spec storage_import_get_configuration(node(), od_space_id()) ->
    {ok, storage_import_config()} | {error, term()}.
storage_import_get_configuration(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec storage_import_set_manual_import(od_space_id()) -> ok | {error, term()}.
storage_import_set_manual_import(SpaceId) ->
    ?CALL([SpaceId]).

-spec storage_import_set_manual_import(node(), od_space_id()) -> ok | {error, term()}.
storage_import_set_manual_import(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec storage_import_configure_auto_import(od_space_id(), auto_storage_import_config()) ->
    ok | {error, term()}.
storage_import_configure_auto_import(SpaceId, AutoStorageImportConfig) ->
    ?CALL([SpaceId, AutoStorageImportConfig]).

-spec storage_import_configure_auto_import(node(), od_space_id(), auto_storage_import_config()) ->
    ok | {error, term()}.
storage_import_configure_auto_import(Node, SpaceId, AutoStorageImportConfig) ->
    ?CALL(Node, [SpaceId, AutoStorageImportConfig]).


-spec storage_import_start_scan(od_space_id()) ->  ok | {error, term()}.
storage_import_start_scan(SpaceId) ->
    ?CALL([SpaceId]).

-spec storage_import_start_scan(node(), od_space_id()) -> ok | {error, term()}.
storage_import_start_scan(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec storage_import_stop_scan(od_space_id()) ->  ok | {error, term()}.
storage_import_stop_scan(SpaceId) ->
    ?CALL([SpaceId]).

-spec storage_import_stop_scan(node(), od_space_id()) ->  ok | {error, term()}.
storage_import_stop_scan(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec storage_import_get_stats(od_space_id(),
    [storage_import_monitoring_plot_counter_type()],
    storage_import_monitoring_window()) -> {ok, json_utils:json_term()}.
storage_import_get_stats(SpaceId, Types, Window) ->
    ?CALL([SpaceId, Types, Window]).

-spec storage_import_get_stats(node(), od_space_id(),
    [storage_import_monitoring_plot_counter_type()],
    storage_import_monitoring_window()) -> {ok, json_utils:json_term()}.
storage_import_get_stats(Node, SpaceId, Types, Window) ->
    ?CALL(Node, [SpaceId, Types, Window]).


-spec storage_import_get_info(od_space_id()) ->
    {ok, json_utils:json_term()}.
storage_import_get_info(SpaceId) ->
    ?CALL([SpaceId]).

-spec storage_import_get_info(node(), od_space_id()) ->
    {ok, json_utils:json_term()}.
storage_import_get_info(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec storage_import_get_manual_example(od_space_id()) ->
    {ok, json_utils:json_term()}.
storage_import_get_manual_example(SpaceId) ->
    ?CALL([SpaceId]).

-spec storage_import_get_manual_example(node(), od_space_id()) ->
    {ok, json_utils:json_term()}.
storage_import_get_manual_example(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec restart_rtransfer_link() -> ok | {error, not_running}.
restart_rtransfer_link() ->
    ?CALL([]).

-spec restart_rtransfer_link(node()) -> ok | {error, not_running}.
restart_rtransfer_link(Node) ->
    ?CALL(Node, []).


-spec set_txt_record(Name :: binary(), Content :: binary(),
    TTL :: non_neg_integer() | undefined) -> ok | no_return().
set_txt_record(Name, Content, TTL) ->
    ?CALL([Name, Content, TTL]).

-spec set_txt_record(node(), Name :: binary(), Content :: binary(),
    TTL :: non_neg_integer() | undefined) -> ok | no_return().
set_txt_record(Node, Name, Content, TTL) ->
    ?CALL(Node, [Name, Content, TTL]).


-spec remove_txt_record(Name :: binary()) -> ok | no_return().
remove_txt_record(Name) ->
    ?CALL([Name]).

-spec remove_txt_record(node(), Name :: binary()) -> ok | no_return().
remove_txt_record(Node, Name) ->
    ?CALL(Node, [Name]).
