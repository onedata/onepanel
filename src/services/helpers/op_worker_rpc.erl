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
-opaque storage_doc() :: tuple().
-type autocleaning_run_id() :: binary().
-type autocleaning_run_links_list_limit() :: integer() | all.
-type autocleaning_run_links_offset() :: integer().
-type file_popularity_config_id() :: od_space_id().
-type helper_args() :: #{binary() => binary()}.
-type helper_name() :: binary().
-type helper_user_ctx() :: #{binary() => binary()}.
-type luma_config_api_key() :: undefined | binary().
-type luma_config_url() :: binary().
-type od_space_id() :: binary().
-type space_quota_id() :: od_space_id().
-type space_strategy_arguments() :: map().
-type storage_id() :: binary().
-type storage_import_status() :: atom().
-type storage_name() :: binary().
-type storage_qos_parameters() :: #{binary() => binary()}.
-type storage_update_status() :: atom().
-type sync_monitoring_plot_counter_type() :: imported_files | updated_files |
    deleted_files | queue_length.
-type sync_monitoring_window() :: day | hour | minute.

-export_type([storage_doc/0, luma_config/0, helper/0,
    helper_args/0, helper_user_ctx/0, od_space_id/0]).

-export([storage_create/6, storage_create/7]).
-export([storage_safe_remove/1, storage_safe_remove/2]).
-export([storage_supports_any_space/1, storage_supports_any_space/2]).
-export([storage_list_ids/0, storage_list_ids/1]).
-export([storage_get_helper/1, storage_get_helper/2]).
-export([storage_update_admin_ctx/2, storage_update_admin_ctx/3]).
-export([storage_update_helper_args/2, storage_update_helper_args/3]).
-export([storage_set_insecure/2, storage_set_insecure/3]).
-export([storage_set_readonly/2, storage_set_readonly/3]).
-export([storage_set_imported_storage/2, storage_set_imported_storage/3]).
-export([storage_set_qos_parameters/2, storage_set_qos_parameters/3]).
-export([storage_update_luma_config/2, storage_update_luma_config/3]).
-export([storage_update_name/2, storage_update_name/3]).
-export([storage_exists/1, storage_exists/2]).
-export([storage_describe/1, storage_describe/2]).
-export([storage_is_imported_storage/1, storage_is_imported_storage/2]).
-export([invalidate_luma_cache/1, invalidate_luma_cache/2]).
-export([new_helper/5, new_helper/6]).
-export([new_luma_config/2, new_luma_config/3]).
-export([verify_storage_on_all_nodes/1, verify_storage_on_all_nodes/2]).
-export([prepare_helper_args/2, prepare_helper_args/3]).
-export([prepare_user_ctx_params/2, prepare_user_ctx_params/3]).
-export([space_logic_get_storage_ids/1, space_logic_get_storage_ids/2]).
-export([file_popularity_api_configure/2, file_popularity_api_configure/3]).
-export([file_popularity_api_get_configuration/1,
    file_popularity_api_get_configuration/2]).
-export([autocleaning_configure/2, autocleaning_configure/3]).
-export([autocleaning_get_configuration/1, autocleaning_get_configuration/2]).
-export([autocleaning_list_reports/4, autocleaning_list_reports/5]).
-export([autocleaning_get_run_report/1, autocleaning_get_run_report/2]).
-export([autocleaning_status/1, autocleaning_status/2]).
-export([autocleaning_force_start/1, autocleaning_force_start/2]).
-export([get_provider_id/0, get_provider_id/1]).
-export([get_access_token/0, get_access_token/1]).
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
-export([get_storage_import_details/2, get_storage_import_details/3]).
-export([get_storage_update_details/2, get_storage_update_details/3]).
-export([configure_storage_import/3, configure_storage_import/4]).
-export([configure_storage_update/3, configure_storage_update/4]).
-export([storage_sync_monitoring_get_metric/3,
    storage_sync_monitoring_get_metric/4]).
-export([storage_sync_monitoring_get_import_status/1,
    storage_sync_monitoring_get_import_status/2]).
-export([storage_sync_monitoring_get_update_status/1,
    storage_sync_monitoring_get_update_status/2]).
-export([restart_rtransfer_link/0, restart_rtransfer_link/1]).
-export([set_txt_record/3, set_txt_record/4]).
-export([remove_txt_record/1, remove_txt_record/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec storage_create(storage_name(), helper(), boolean(),
    undefined | luma_config(), boolean(), storage_qos_parameters()) ->
    {ok, storage_id()} | {error, term()}.
storage_create(Name, Helpers, ReadOnly, LumaConfig, ImportedStorage, QosParameters) ->
    ?CALL([Name, Helpers, ReadOnly, LumaConfig, ImportedStorage, QosParameters]).

-spec storage_create(node(), storage_name(), helper(), boolean(),
    undefined | luma_config(), boolean(), storage_qos_parameters()) ->
    {ok, storage_id()} | {error, term()}.
storage_create(Node, Name, Helpers, ReadOnly, LumaConfig, ImportedStorage, QosParameters) ->
    ?CALL(Node, [Name, Helpers, ReadOnly, LumaConfig, ImportedStorage, QosParameters]).


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


-spec storage_set_insecure(storage_id(), Insecure :: boolean()) ->
    ok | {error, term()}.
storage_set_insecure(StorageId, Insecure) ->
    ?CALL([StorageId, Insecure]).

-spec storage_set_insecure(node(), storage_id(), Insecure :: boolean()) ->
    ok | {error, term()}.
storage_set_insecure(Node, StorageId, Insecure) ->
    ?CALL(Node, [StorageId, Insecure]).


-spec storage_set_readonly(storage_id(), Readonly :: boolean()) ->
    ok | {error, term()}.
storage_set_readonly(StorageId, Readonly) ->
    ?CALL([StorageId, Readonly]).

-spec storage_set_readonly(node(), storage_id(), Readonly :: boolean()) ->
    ok | {error, term()}.
storage_set_readonly(Node, StorageId, Readonly) ->
    ?CALL(Node, [StorageId, Readonly]).


-spec storage_set_imported_storage(storage_id(), boolean()) ->
    ok | {error, term()}.
storage_set_imported_storage(StorageId, Value) ->
    ?CALL([StorageId, Value]).

-spec storage_set_imported_storage(node(), storage_id(), boolean()) ->
    ok | {error, term()}.
storage_set_imported_storage(Node, StorageId, Value) ->
    ?CALL(Node, [StorageId, Value]).


-spec storage_update_luma_config(storage_id(), DiffOrNewConfig) -> ok | {error, term()}
    when DiffOrNewConfig :: #{url => luma_config_url(), api_key => luma_config_api_key()} | luma_config().
storage_update_luma_config(StorageId, Changes) ->
    ?CALL([StorageId, Changes]).

-spec storage_update_luma_config(node(), storage_id(), DiffOrNewConfig) -> ok | {error, term()}
    when DiffOrNewConfig :: #{url => luma_config_url(), api_key => luma_config_api_key()} | luma_config().
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


-spec invalidate_luma_cache(storage_id()) -> ok.
invalidate_luma_cache(StorageId) ->
    ?CALL([StorageId]).

-spec invalidate_luma_cache(node(), storage_id()) -> ok.
invalidate_luma_cache(Node, StorageId) ->
    ?CALL(Node, [StorageId]).


-spec new_helper(helper_name(), helper_args(), helper_user_ctx(),
    Insecure :: boolean(), StoragePathType :: binary()) -> {ok, helper()}.
new_helper(HelperName, Args, AdminCtx, Insecure, StoragePathType) ->
    ?CALL([HelperName, Args, AdminCtx, Insecure, StoragePathType]).

-spec new_helper(node(), helper_name(), helper_args(), helper_user_ctx(),
    Insecure :: boolean(), StoragePathType :: binary()) -> {ok, helper()}.
new_helper(Node, HelperName, Args, AdminCtx, Insecure, StoragePathType) ->
    ?CALL(Node, [HelperName, Args, AdminCtx, Insecure, StoragePathType]).


-spec new_luma_config(URL :: binary(), ApiKey :: binary() | undefined) ->
    luma_config().
new_luma_config(URL, ApiKey) ->
    ?CALL([URL, ApiKey]).

-spec new_luma_config(node(), URL :: binary(), ApiKey :: binary() | undefined) ->
    luma_config().
new_luma_config(Node, URL, ApiKey) ->
    ?CALL(Node, [URL, ApiKey]).


-spec verify_storage_on_all_nodes(helper()) -> ok | errors:error().
verify_storage_on_all_nodes(Helper) ->
    ?CALL([Helper]).

-spec verify_storage_on_all_nodes(node(), helper()) -> ok | errors:error().
verify_storage_on_all_nodes(Node, Helper) ->
    ?CALL(Node, [Helper]).


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


-spec autocleaning_force_start(od_space_id()) ->
    {ok, autocleaning_run_id()} | {error, term()}.
autocleaning_force_start(SpaceId) ->
    ?CALL([SpaceId]).

-spec autocleaning_force_start(node(), od_space_id()) ->
    {ok, autocleaning_run_id()} | {error, term()}.
autocleaning_force_start(Node, SpaceId) ->
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


-spec get_storage_import_details(od_space_id(), storage_id()) ->
    {boolean(), space_strategy_arguments()}.
get_storage_import_details(SpaceId, StorageId) ->
    ?CALL([SpaceId, StorageId]).

-spec get_storage_import_details(node(), od_space_id(), storage_id()) ->
    {boolean(), space_strategy_arguments()}.
get_storage_import_details(Node, SpaceId, StorageId) ->
    ?CALL(Node, [SpaceId, StorageId]).


-spec get_storage_update_details(od_space_id(), storage_id()) ->
    {boolean(), space_strategy_arguments()}.
get_storage_update_details(SpaceId, StorageId) ->
    ?CALL([SpaceId, StorageId]).

-spec get_storage_update_details(node(), od_space_id(), storage_id()) ->
    {boolean(), space_strategy_arguments()}.
get_storage_update_details(Node, SpaceId, StorageId) ->
    ?CALL(Node, [SpaceId, StorageId]).


-spec configure_storage_import(od_space_id(), boolean(),
    space_strategy_arguments()) ->
    ok | {error, term()}.
configure_storage_import(SpaceId, Enabled, Args) ->
    ?CALL([SpaceId, Enabled, Args]).

-spec configure_storage_import(node(), od_space_id(), boolean(),
    space_strategy_arguments()) ->
    ok | {error, term()}.
configure_storage_import(Node, SpaceId, Enabled, Args) ->
    ?CALL(Node, [SpaceId, Enabled, Args]).


-spec configure_storage_update(od_space_id(), boolean(),
    space_strategy_arguments()) -> ok | {error, term()}.
configure_storage_update(SpaceId, Enabled, Args) ->
    ?CALL([SpaceId, Enabled, Args]).

-spec configure_storage_update(node(), od_space_id(), boolean(),
    space_strategy_arguments()) -> ok | {error, term()}.
configure_storage_update(Node, SpaceId, Enabled, Args) ->
    ?CALL(Node, [SpaceId, Enabled, Args]).


-spec storage_sync_monitoring_get_metric(od_space_id(),
    sync_monitoring_plot_counter_type(),
    sync_monitoring_window()) -> proplists:proplist().
storage_sync_monitoring_get_metric(SpaceId, Type, Window) ->
    ?CALL([SpaceId, Type, Window]).

-spec storage_sync_monitoring_get_metric(node(), od_space_id(),
    sync_monitoring_plot_counter_type(),
    sync_monitoring_window()) -> proplists:proplist().
storage_sync_monitoring_get_metric(Node, SpaceId, Type, Window) ->
    ?CALL(Node, [SpaceId, Type, Window]).


-spec storage_sync_monitoring_get_import_status(od_space_id()) ->
    storage_import_status().
storage_sync_monitoring_get_import_status(SpaceId) ->
    ?CALL([SpaceId]).

-spec storage_sync_monitoring_get_import_status(node(), od_space_id()) ->
    storage_import_status().
storage_sync_monitoring_get_import_status(Node, SpaceId) ->
    ?CALL(Node, [SpaceId]).


-spec storage_sync_monitoring_get_update_status(od_space_id()) ->
    storage_update_status().
storage_sync_monitoring_get_update_status(SpaceId) ->
    ?CALL([SpaceId]).

-spec storage_sync_monitoring_get_update_status(node(), od_space_id()) ->
    storage_update_status().
storage_sync_monitoring_get_update_status(Node, SpaceId) ->
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
