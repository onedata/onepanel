%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Modifies space support configuration (op_panel only).
%%% @end
%%%-------------------------------------------------------------------
-module(space_support_update_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

% middleware_handler callbacks
-export([
    supported_interfaces/0,
    service_availability_requirements/1,
    preauthorize/1,
    validate/1,
    process/1
]).

-type t() :: ?MODULE.
-type input() :: map().
-type state() :: #onp_req_state{ctx :: #onp_req_ctx{}, input :: input()}.
-type output() :: undefined.

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces() -> false | {true, [rest]}.
supported_interfaces() ->
    middleware_handler_utils:if_cluster_type_then(?ONEPROVIDER, [rest]).


-spec service_availability_requirements(middleware_handler:req_ctx()) ->
    {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    {true, [?SERVICE_OPW, all_healthy_ignoring_ones3]}.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok.
validate(_) -> ok.


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{id = SpaceId}}, input = Data}) ->
    Ctx1 = kv_utils:copy_found([
        {size, size},
        {accountingEnabled, accounting_enabled},
        {dirStatsServiceEnabled, dir_stats_service_enabled}
    ], Data),
    Ctx2 = get_auto_storage_import_args(Data, Ctx1#{space_id => SpaceId}),
    middleware_utils:execute_service_action(?SERVICE_OP, modify_space, Ctx2).


%%%===================================================================
%%% internal helpers
%%%===================================================================


%% @private
-spec get_auto_storage_import_args(input(), service:step_ctx()) ->
    service:step_ctx().
get_auto_storage_import_args(Data, Ctx) ->
    kv_utils:copy_found([
        {[autoStorageImportConfig, maxDepth], [auto_storage_import_config, max_depth]},
        {[autoStorageImportConfig, syncAcl], [auto_storage_import_config, sync_acl]},
        {[autoStorageImportConfig, continuousScan], [auto_storage_import_config, continuous_scan]},
        {[autoStorageImportConfig, scanInterval], [auto_storage_import_config, scan_interval]},
        {[autoStorageImportConfig, detectModifications], [auto_storage_import_config, detect_modifications]},
        {[autoStorageImportConfig, detectDeletions], [auto_storage_import_config, detect_deletions]}
    ], Data, Ctx).
