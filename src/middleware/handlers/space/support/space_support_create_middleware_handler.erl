%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Supports a space (op_panel only).
%%% TODO VFS-13069 test storageImport parameter
%%% @end
%%%-------------------------------------------------------------------
-module(space_support_create_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

% middleware_handler callbacks
-export([
    supported_interfaces/1,
    service_availability_requirements/1,
    preauthorize/1,
    validate/1,
    process/1,
    translate_output/2
]).

-type t() :: ?MODULE.
-type input() :: map().
-type state() :: #onp_req_state{input :: input()}.
-type output() :: binary().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> false | {true, [rest]}.
supported_interfaces(_) ->
    middleware_handler_utils:if_op_then([rest]).


-spec service_availability_requirements(middleware_handler:req_ctx()) ->
    {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    {true, [?SERVICE_OPW, all_healthy_ignoring_ones3]}.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok.
validate(_) ->
    ok.


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{input = Data}) ->
    Ctx = kv_utils:copy_found([
        {token, token},
        {size, size},
        {storageId, storage_id},
        {importedStorage, imported_storage},
        {accountingEnabled, accounting_enabled},
        {dirStatsServiceEnabled, dir_stats_service_enabled}
    ], Data),
    Ctx2 = get_storage_import_args(Data, Ctx),

    middleware_handler_utils:service_call(?SERVICE_OP, support_space, Ctx2).


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, SpaceId) ->
    {ok, ?CREATED_REPLY([<<"provider">>, <<"spaces">>, SpaceId], #{<<"id">> => SpaceId})}.


%%%===================================================================
%%% internal helpers
%%%===================================================================


%% @private
-spec get_storage_import_args(input(), service:step_ctx()) -> service:step_ctx().
get_storage_import_args(Data, Ctx) ->
    kv_utils:copy_found([
        {[storageImport, mode], [storage_import, mode]},
        {[storageImport, autoStorageImportConfig, maxDepth], [storage_import, auto_storage_import_config, max_depth]},
        {[storageImport, autoStorageImportConfig, syncAcl], [storage_import, auto_storage_import_config, sync_acl]},
        {[storageImport, autoStorageImportConfig, continuousScan], [storage_import, auto_storage_import_config, continuous_scan]},
        {[storageImport, autoStorageImportConfig, scanInterval], [storage_import, auto_storage_import_config, scan_interval]},
        {[storageImport, autoStorageImportConfig, detectModifications], [storage_import, auto_storage_import_config, detect_modifications]},
        {[storageImport, autoStorageImportConfig, detectDeletions], [storage_import, auto_storage_import_config, detect_deletions]}
    ], Data, Ctx).
