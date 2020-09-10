%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Middleware plugin for the onp_space type.
%%% All operations handled by this module are available only in op_panel.
%%% @end
%%%-------------------------------------------------------------------
-module(space_middleware).
-author("Wojciech Geisler").

-behaviour(middleware_plugin).

-include("authentication.hrl").
-include("http/rest.hrl").
-include("middleware/middleware.hrl").
-include("names.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([operation_supported/3, required_availability/3, fetch_entity/1,
    authorize/2, validate/2]).
-export([create/1, get/2, update/1, delete/1]).


%%%===================================================================
%%% middleware_plugin callbacks
%%%===================================================================

-spec operation_supported(middleware:operation(), gri:aspect(),
    middleware:scope()) -> boolean().
operation_supported(create, support, private) -> onepanel:is_op_panel();
operation_supported(create, start_auto_cleaning, private) -> onepanel:is_op_panel();
operation_supported(create, cancel_auto_cleaning, private) -> onepanel:is_op_panel();
operation_supported(create, start_auto_storage_import_scan, private) -> onepanel:is_op_panel();
operation_supported(create, stop_auto_storage_import_scan, private) -> onepanel:is_op_panel();

operation_supported(get, instance, private) -> onepanel:is_op_panel();
operation_supported(get, list, private) -> onepanel:is_op_panel();
operation_supported(get, auto_cleaning_configuration, private) -> onepanel:is_op_panel();
operation_supported(get, {auto_cleaning_report, _ReportId}, private) -> onepanel:is_op_panel();
operation_supported(get, auto_cleaning_reports_list, private) -> onepanel:is_op_panel();
operation_supported(get, auto_cleaning_status, private) -> onepanel:is_op_panel();
operation_supported(get, file_popularity_configuration, private) -> onepanel:is_op_panel();
operation_supported(get, {file_popularity_report, _ReportId}, private) -> onepanel:is_op_panel();
operation_supported(get, auto_storage_import_stats, private) -> onepanel:is_op_panel();
operation_supported(get, auto_storage_import_info, private) -> onepanel:is_op_panel();

operation_supported(update, support, private) -> onepanel:is_op_panel();
operation_supported(update, auto_cleaning_configuration, private) -> onepanel:is_op_panel();
operation_supported(update, file_popularity_configuration, private) -> onepanel:is_op_panel();

operation_supported(delete, support, private) -> onepanel:is_op_panel();

operation_supported(_, _, _) -> false.


-spec required_availability(middleware:operation(), gri:aspect(),
    middleware:scope()) -> [middleware:availability_level()].
required_availability(_, _, _) -> [?SERVICE_OPW, all_healthy].


-spec fetch_entity(middleware:req()) ->
    {ok, middleware:versioned_entity()} | undefined | errors:error().
fetch_entity(#onp_req{gri = #gri{id = SpaceId}}) ->
    SpaceDetails = middleware_utils:result_from_service_action(
        ?SERVICE_OP, get_space_details, #{id => SpaceId}
    ),
    {ok, {SpaceDetails, 1}}.


-spec authorize(middleware:req(), middleware:entity()) -> boolean().
authorize(#onp_req{operation = Op, client = Client, gri = #gri{aspect = As}}, _) when
    Op == create, As == support;
    Op == create, As == start_auto_cleaning;
    Op == create, As == cancel_auto_cleaning;
    Op == create, As == start_auto_storage_import_scan;
    Op == create, As == stop_auto_storage_import_scan;
    Op == update, As == support;
    Op == update, As == auto_cleaning_configuration;
    Op == update, As == file_popularity_configuration;
    Op == delete, As == support
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{
    operation = get, client = #client{role = member}, gri = #gri{aspect = {As, _}}
}, _) when
    As == auto_cleaning_report;
    As == file_popularity_report
->
    true;

authorize(#onp_req{
    operation = get, client = #client{role = member}, gri = #gri{aspect = As}
}, _) when
    As == instance;
    As == list;
    As == auto_cleaning_configuration;
    As == auto_cleaning_reports_list;
    As == auto_cleaning_status;
    As == file_popularity_configuration;
    As == auto_storage_import_stats;
    As == auto_storage_import_info
->
    true.


%% @TODO VFS-6025 Validate parameters here rather than during execution
-spec validate(middleware:req(), middleware:entity()) -> ok | no_return().
validate(#onp_req{operation = Op, gri = #gri{aspect = {As, _}}}, _) when
    Op == get, As == auto_cleaning_report;
    Op == get, As == file_popularity_report
->
    ok;

validate(#onp_req{operation = get, gri = #gri{aspect = auto_storage_import_stats}, data = Data}, _) ->
    op_worker_storage_import:validate_period(Data),
    op_worker_storage_import:validate_metrics(Data);
validate(#onp_req{operation = Op, gri = #gri{aspect = As}}, _) when
    Op == create, As == support;
    Op == create, As == start_auto_cleaning;
    Op == create, As == cancel_auto_cleaning;
    Op == create, As == start_auto_storage_import_scan;
    Op == create, As == stop_auto_storage_import_scan;

    Op == get, As == instance;
    Op == get, As == list;
    Op == get, As == auto_cleaning_configuration;
    Op == get, As == auto_cleaning_reports_list;
    Op == get, As == auto_cleaning_status;
    Op == get, As == file_popularity_configuration;
    Op == get, As == auto_storage_import_stats;
    Op == get, As == auto_storage_import_info;

    Op == update, As == support;
    Op == update, As == auto_cleaning_configuration;
    Op == update, As == file_popularity_configuration;

    Op == delete, As == support
->
    ok.


-spec create(middleware:req()) -> middleware:create_result().
create(#onp_req{gri = #gri{aspect = support} = GRI, data = Data}) ->
    Ctx = kv_utils:copy_found([
        {token, token},
        {size, size},
        {storageId, storage_id},
        {importedStorage, imported_storage}], Data),
    Ctx2 = get_storage_import_args(Data, Ctx),

    SpaceId = middleware_utils:result_from_service_action(
        ?SERVICE_OP, support_space, Ctx2),
    {ok, value, GRI#gri{id = SpaceId}, #{id => SpaceId}};

create(#onp_req{gri = #gri{id = SpaceId, aspect = start_auto_cleaning}}) ->
    case middleware_utils:result_from_service_action(
        ?SERVICE_OP, start_auto_cleaning, #{space_id => SpaceId}
    ) of
        {ok, ReportId} -> {ok, value, ReportId};
        no_need -> ok
    end;

create(#onp_req{gri = #gri{id = SpaceId, aspect = cancel_auto_cleaning}}) ->
    middleware_utils:result_from_service_action(
        ?SERVICE_OP, cancel_auto_cleaning, #{space_id => SpaceId}
    );

create(#onp_req{gri = #gri{id = SpaceId, aspect = start_auto_storage_import_scan}}) ->
    middleware_utils:execute_service_action(
        ?SERVICE_OP, start_auto_storage_import_scan, #{space_id => SpaceId}
    );

create(#onp_req{gri = #gri{id = SpaceId, aspect = stop_auto_storage_import_scan}}) ->
    middleware_utils:execute_service_action(
        ?SERVICE_OP, stop_auto_storage_import_scan, #{space_id => SpaceId}
    ).


-spec get(middleware:req(), middleware:entity()) -> middleware:get_result().
get(#onp_req{gri = #gri{aspect = instance}}, Space) ->
    {ok, Space};

get(#onp_req{gri = #gri{aspect = list}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(?SERVICE_OP, get_spaces)};

get(#onp_req{gri = #gri{id = Id, aspect = auto_cleaning_configuration}}, _Space) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OP, get_auto_cleaning_configuration, #{space_id => Id}
    )};

get(#onp_req{gri = #gri{id = Id, aspect = file_popularity_configuration}}, _Space) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OP, get_file_popularity_configuration, #{space_id => Id}
    )};

get(#onp_req{gri = #gri{id = SpaceId, aspect = {auto_cleaning_report, ReportId}}}, _Space) ->
    Ctx = #{space_id => SpaceId, report_id => ReportId},
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OP, get_auto_cleaning_report, Ctx
    )};

get(#onp_req{gri = #gri{id = SpaceId, aspect = auto_cleaning_reports_list}, data =  Data}, _Space) ->
    Ctx = maps:with([space_id, offset, limit, index], Data#{space_id => SpaceId}),
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OP, get_auto_cleaning_reports, Ctx
    )};

get(#onp_req{gri = #gri{id = SpaceId, aspect = auto_cleaning_status}}, _Space) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OP, get_auto_cleaning_status, #{space_id => SpaceId}
    )};

get(#onp_req{gri = #gri{id = SpaceId, aspect = auto_storage_import_stats}, data = Data}, _Space) ->
    Ctx = maps:with([space_id, period, metrics], Data#{space_id => SpaceId}),
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OP, get_auto_storage_import_stats, Ctx
    )};

get(#onp_req{gri = #gri{id = SpaceId, aspect = auto_storage_import_info}}, _Space) ->
    Ctx = #{space_id => SpaceId},
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OP, get_auto_storage_import_info, Ctx
    )}.

-spec update(middleware:req()) -> middleware:update_result().
update(#onp_req{gri = #gri{id = Id, aspect = support}, data = Data}) ->
    Ctx1 = maps:with([size, space_id] , Data#{space_id => Id}),
    Ctx2 = get_storage_import_args(Data, Ctx1),
    middleware_utils:execute_service_action(?SERVICE_OP, modify_space, Ctx2);

update(#onp_req{gri = #gri{id = Id, aspect = auto_cleaning_configuration}, data = Data}) ->
    Ctx = get_auto_cleaning_configuration(Data, #{space_id => Id}),
    middleware_utils:execute_service_action(
        ?SERVICE_OP, configure_auto_cleaning, Ctx);

update(#onp_req{gri = #gri{id = Id, aspect = file_popularity_configuration}, data = Data}) ->
    Ctx = kv_utils:copy_found([
        {enabled, enabled},
        {lastOpenHourWeight, last_open_hour_weight},
        {avgOpenCountPerDayWeight, avg_open_count_per_day_weight},
        {maxAvgOpenCountPerDay, max_avg_open_count_per_day}
    ], Data, #{space_id => Id}),
    {ok, value, _TaskId = service:apply_async(
        ?SERVICE_OP, configure_file_popularity, Ctx
    )}.


-spec delete(middleware:req()) -> middleware:delete_result().
delete(#onp_req{gri = #gri{id = Id, aspect = support}}) ->
    middleware_utils:execute_service_action(
        ?SERVICE_OP, revoke_space_support, #{id => Id}).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc Parse args for storage_import.
%% @end
%%-------------------------------------------------------------------
-spec get_storage_import_args(Data :: middleware:data(), Ctx :: service:step_ctx())
        -> service:step_ctx().
get_storage_import_args(Data, Ctx) ->
    kv_utils:copy_found([
        {[storageImport, mode], [storage_import, mode]},
        {[storageImport, scanConfig, maxDepth], [storage_import, scan_config, max_depth]},
        {[storageImport, scanConfig, syncAcl], [storage_import, scan_config, sync_acl]},
        {[storageImport, scanConfig, continuousScan], [storage_import, scan_config, continuous_scan]},
        {[storageImport, scanConfig, scanInterval], [storage_import, scan_config, scan_interval]},
        {[storageImport, scanConfig, detectModifications], [storage_import, scan_config, detect_modifications]},
        {[storageImport, scanConfig, detectDeletions], [storage_import, scan_config, detect_deletions]}
    ], Data, Ctx).


%%-------------------------------------------------------------------
%% @private
%% @doc Parse configuration for autocleaning
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_configuration(middleware:data(), Ctx :: service:step_ctx())
        -> service:step_ctx().
get_auto_cleaning_configuration(Data, Ctx) ->
    kv_utils:copy_found([
        {[enabled], [enabled]},
        {[target], [target]},
        {[threshold], [threshold]},
        {[rules, enabled], [rules, enabled]},
        {[rules, maxOpenCount], [rules, max_open_count]},
        {[rules, minHoursSinceLastOpen], [rules, min_hours_since_last_open]},
        {[rules, minFileSize], [rules, min_file_size]},
        {[rules, maxFileSize], [rules, max_file_size]},
        {[rules, maxHourlyMovingAverage], [rules, max_hourly_moving_average]},
        {[rules, maxDailyMovingAverage], [rules, max_daily_moving_average]},
        {[rules, maxMonthlyMovingAverage], [rules, max_monthly_moving_average]}
    ], Data, Ctx).
