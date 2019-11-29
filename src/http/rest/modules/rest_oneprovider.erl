%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /provider REST resources.
%%%-------------------------------------------------------------------
-module(rest_oneprovider).
-author("Krzysztof Trzepla").

-include("authentication.hrl").
-include("http/rest.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, is_conflict/4, is_available/3,
    accept_resource/4, provide_resource/2, delete_resource/2]).

-define(SERVICE, ?SERVICE_OP).
-define(WORKER, ?SERVICE_OPW).

%%%===================================================================
%%% REST behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_authorized/3}
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    State :: rest_handler:state()) ->
    {Authorized :: boolean(), Req :: cowboy_req:req()}.
is_authorized(Req, _Method, #rstate{client = #client{role = root}}) ->
    {true, Req};

is_authorized(Req, 'GET', #rstate{client = #client{role = member}}) ->
    {true, Req};

is_authorized(Req, 'DELETE', #rstate{resource = provider,
    client = #client{role = member} = Client}) ->
    {rest_utils:has_privileges(Client, ?CLUSTER_DELETE), Req};

is_authorized(Req, _Method, #rstate{client = #client{role = member} = Client}) ->
    {rest_utils:has_privileges(Client, ?CLUSTER_UPDATE), Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = storage, bindings = #{id := Id}}) ->
    case nodes:any(?WORKER) of
        {ok, Node} ->
            {op_worker_storage:exists(Node, Id), Req};
        _ ->
            {false, Req}
    end;

exists_resource(Req, #rstate{resource = storages}) ->
    {service:exists(?WORKER) and service_oneprovider:is_registered(), Req};

exists_resource(Req, #rstate{resource = space, bindings = #{id := Id}}) ->
    {service_oneprovider:is_space_supported(#{space_id => Id}), Req};

exists_resource(Req, #rstate{resource = onezone_info, params = #{token := _}}) ->
    {true, Req};

exists_resource(Req, #rstate{resource = transfers_mock}) ->
    {hosts:all(?WORKER) /= [], Req};

exists_resource(Req, _State) ->
    {service_oneprovider:is_registered(), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_conflict/4}
%% @end
%%--------------------------------------------------------------------
is_conflict(Req, 'DELETE', _Args,
    #rstate{resource = storage, bindings = #{id:=Id}}) ->
    case op_worker_storage:can_be_removed(Id) of
        false -> {true, rest_replier:set_error_body(Req, ?ERROR_STORAGE_IN_USE)};
        true -> {false, Req}
    end;

is_conflict(Req, 'POST', _Args, #rstate{resource = provider}) ->
    {service_oneprovider:is_registered(), Req};

is_conflict(Req, _Method, _Args, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_available/3}
%% @end
%%--------------------------------------------------------------------
is_available(Req, 'GET', #rstate{resource = cluster_ips}) -> {true, Req};
is_available(Req, 'GET', #rstate{resource = provider}) -> {true, Req};
is_available(Req, _Method, #rstate{resource = transfers_mock}) -> {true, Req};
is_available(Req, _Method, _State) ->
    {service:exists(?SERVICE_OPW) andalso service:all_healthy(), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'POST', Args, #rstate{resource = provider}) ->
    Ctx = kv_utils:copy_found([
        {token, oneprovider_token},
        {name, oneprovider_name},
        {subdomainDelegation, oneprovider_subdomain_delegation},
        {domain, oneprovider_domain},
        {subdomain, oneprovider_subdomain},
        {adminEmail, oneprovider_admin_email},
        {geoLatitude, oneprovider_geo_latitude},
        {geoLongitude, oneprovider_geo_longitude}
    ], Args),

    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, register, Ctx
    ))};

accept_resource(Req, 'PATCH', Args, #rstate{resource = provider}) ->
    Ctx = kv_utils:copy_found([
        {name, oneprovider_name},
        {subdomainDelegation, oneprovider_subdomain_delegation},
        {domain, oneprovider_domain},
        {subdomain, oneprovider_subdomain},
        {adminEmail, oneprovider_admin_email},
        {geoLatitude, oneprovider_geo_latitude},
        {geoLongitude, oneprovider_geo_longitude},
        {letsEncryptEnabled, letsencrypt_enabled}
    ], Args),

    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, modify_details, Ctx
    ))};

accept_resource(Req, 'POST', Args, #rstate{resource = spaces}) ->
    Ctx = kv_utils:copy_found([
        {token, token},
        {size, size},
        {storageId, storage_id},
        {importedStorage, imported_storage}], Args),
    Ctx2 = get_storage_import_args(Args, Ctx),
    Ctx3 = get_storage_update_args(Args, Ctx2),

    {true, rest_replier:handle_service_step(Req, service_oneprovider, support_space,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, support_space, Ctx3
        ))
    )};

accept_resource(Req, 'PATCH', Args, #rstate{resource = space, bindings = #{id := Id}}) ->
    Ctx1 = kv_utils:copy_found([
        {size, size}
    ], Args, #{space_id => Id}),
    Ctx2 = get_storage_update_args(Args, Ctx1),
    Ctx3 = get_storage_import_args(Args, Ctx2),

    {true, rest_replier:handle_service_step(Req, service_oneprovider, modify_space,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, modify_space, Ctx3
        ))
    )};

accept_resource(Req, 'POST', Args, #rstate{resource = storages}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?WORKER, add_storages, #{
            storages => Args, ignore_exists => false
        }
    ))};

accept_resource(Req, 'PATCH', Args, #rstate{resource = storage,
    bindings = #{id := Id}}) ->
    % only 1 storage should be modified at a time
    [{OldName, #{type := Type} = Params}] = maps:to_list(Args),

    % Type and name must be given due to limitations of onepanel
    % rest models specification. Ensures received values match the storage
    % identified by id.
    case service_op_worker:get_storages(#{id => Id}) of
        #{name := OldName, type := Type} -> ok;
        #{name := ActualName, type := _} when ActualName /= OldName ->
            throw(?ERROR_BAD_VALUE_NOT_ALLOWED(OldName, [ActualName]));
        #{name := OldName, type := ActualType} ->
            Key = str_utils:join_as_binaries([OldName, type], <<".">>),
            throw(?ERROR_BAD_VALUE_NOT_ALLOWED(Key, [ActualType]))
    end,

    {true, rest_replier:handle_service_step(Req, service_op_worker, update_storage,
        service_utils:throw_on_error(service:apply_sync(
            ?WORKER, update_storage, #{id => Id, storage => Params}
        ))
    )};

accept_resource(Req, 'PATCH', _Args, #rstate{
    resource = luma,
    bindings = #{id := Id}
}) ->
    Ctx = #{id => Id},
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?WORKER, invalidate_luma_cache, Ctx
    ))};

accept_resource(Req, 'PATCH', Args, #rstate{resource = cluster_ips}) ->
    ClusterIps = maps:get(hosts, Args),
    Ctx = #{cluster_ips => onepanel_utils:convert(ClusterIps, {keys, list})},

    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, set_cluster_ips, Ctx
    ))};

accept_resource(Req, 'PATCH', Args, #rstate{
    resource = file_popularity_configuration,
    bindings = #{id := Id},
    version = Version
}) ->
    Ctx = kv_utils:copy_found([
        {enabled, [enabled]},
        {lastOpenHourWeight, [last_open_hour_weight]},
        {avgOpenCountPerDayWeight, [avg_open_count_per_day_weight]},
        {maxAvgOpenCountPerDay, [max_avg_open_count_per_day]}
    ], Args, #{space_id => Id}),
    {true, rest_replier:handle_service_action_async(Req, service:apply_async(
        ?SERVICE, configure_file_popularity, Ctx), Version
    )};


accept_resource(Req, 'PATCH', Args, #rstate{
    resource = space_auto_cleaning_configuration,
    bindings = #{id := Id}
}) ->
    Ctx = #{space_id => Id},
    Ctx2 = get_auto_cleaning_configuration(Args, Ctx),
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
            ?SERVICE, configure_auto_cleaning, Ctx2
    ))};

accept_resource(Req, 'POST', _Args, #rstate{
    resource = space_auto_cleaning_start,
    bindings = #{id := Id}
}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, start_auto_cleaning, #{space_id => Id}
    ))};

accept_resource(Req, 'PATCH', #{transfersMock := Enabled}, #rstate{resource = transfers_mock}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?WORKER, set_transfers_mock, #{transfers_mock => Enabled}
    ))}.

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = provider}) ->
    {rest_replier:format_service_step(service_oneprovider, get_details,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_details, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = spaces}) ->
    {rest_replier:format_service_step(service_oneprovider, get_spaces,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_spaces, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = space, bindings = #{id := Id}}) ->
    {rest_replier:format_service_step(service_oneprovider, get_space_details,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_space_details, #{id => Id}
        ))
    ), Req};

provide_resource(Req, #rstate{
    resource = space_sync_stats,
    bindings = #{id := Id},
    params = Params
}) ->
    Ctx = maps:with([space_id, period, metrics], Params#{space_id => Id}),
    {rest_replier:format_service_step(service_oneprovider, get_sync_stats,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_sync_stats, Ctx
        ))
    ), Req};

provide_resource(Req, #rstate{
    resource = space_auto_cleaning_reports,
    bindings = #{id := Id},
    params = Params
}) ->
    Ctx = maps:with([space_id, offset, limit, index], Params#{space_id => Id}),
    {rest_replier:format_service_step(service_oneprovider, get_auto_cleaning_reports,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_auto_cleaning_reports, Ctx
        ))
    ), Req};

provide_resource(Req, #rstate{
    resource = space_auto_cleaning_report,
    bindings = #{id := Id, report_id := ReportId}
}) ->
    Ctx = #{space_id => Id, report_id => ReportId},

    {rest_replier:format_service_step(service_oneprovider, get_auto_cleaning_report,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_auto_cleaning_report, Ctx
        ))
    ), Req};

provide_resource(Req, #rstate{
    resource = space_auto_cleaning_status,
    bindings = #{id := Id}
}) ->
    Ctx = #{space_id => Id},
    {rest_replier:format_service_step(service_oneprovider, get_auto_cleaning_status,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_auto_cleaning_status, Ctx
        ))
    ), Req};

provide_resource(Req, #rstate{
    resource = space_auto_cleaning_configuration,
    bindings = #{id := Id}
}) ->
    Ctx = #{space_id => Id},
    {rest_replier:format_service_step(service_oneprovider, get_auto_cleaning_configuration,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_auto_cleaning_configuration, Ctx
        ))
    ), Req};

provide_resource(Req, #rstate{
    resource = file_popularity_configuration,
    bindings = #{id := Id}
}) ->
    Ctx = #{space_id => Id},
    {rest_replier:format_service_step(service_oneprovider, get_file_popularity_configuration,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_file_popularity_configuration, Ctx
        ))
    ), Req};

provide_resource(Req, #rstate{resource = storage, bindings = #{id := Id}}) ->
    {rest_replier:format_storage_details(
        service_utils:throw_on_error(service:apply_sync(
            ?WORKER, get_storages, #{id => Id}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = storages}) ->
    {rest_replier:format_service_step(service_op_worker, get_storages,
        service_utils:throw_on_error(service:apply_sync(
            ?WORKER, get_storages, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = cluster_ips}) ->
    {rest_replier:format_service_step(service_oneprovider, format_cluster_ips,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, format_cluster_ips, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = onezone_info, params = #{token := Token}}) ->
    Domain = onezone_tokens:read_domain(Token),
    {onezone_client:fetch_zone_info(Domain), Req};

provide_resource(Req, #rstate{resource = onezone_info}) ->
    % exists_resource ensures the Oneprovider is registered
    Domain = list_to_binary(service_oneprovider:get_oz_domain()),
    {onezone_client:fetch_zone_info(Domain), Req};

provide_resource(Req, #rstate{resource = transfers_mock}) ->
    {rest_replier:format_service_step(service_op_worker, get_transfers_mock,
        service_utils:throw_on_error(service:apply_sync(
            ?WORKER, get_transfers_mock, #{}
        ))
    ), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, #rstate{resource = provider}) ->
    {true, rest_replier:throw_on_service_error(Req,
        service:apply_sync(?SERVICE, unregister, #{})
    )};

delete_resource(Req, #rstate{resource = storage, bindings = #{id := Id}}) ->
    {true, rest_replier:throw_on_service_error(Req,
        service:apply_sync(?WORKER, remove_storage, #{id => Id})
    )};

delete_resource(Req, #rstate{resource = space, bindings = #{id := Id}}) ->
    {true, rest_replier:throw_on_service_error(Req,
        service:apply_sync(?SERVICE, revoke_space_support, #{id => Id})
    )}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc Parse args for storage_update.
%% @end
%%-------------------------------------------------------------------
-spec get_storage_update_args(Args :: rest_handler:args(), Ctx :: service:ctx())
        -> service:ctx().
get_storage_update_args(Args, Ctx) ->
    kv_utils:copy_found([
        {[storageUpdate, strategy], [storage_update, strategy]},
        {[storageUpdate, maxDepth], [storage_update, max_depth]},
        {[storageUpdate, writeOnce], [storage_update, write_once]},
        {[storageUpdate, deleteEnable], [storage_update, delete_enable]},
        {[storageUpdate, scanInterval], [storage_update, scan_interval]},
        {[storageUpdate, syncAcl], [storage_update, sync_acl]}
    ], Args, Ctx).

%%-------------------------------------------------------------------
%% @private
%% @doc Parse args for storage_import.
%% @end
%%-------------------------------------------------------------------
-spec get_storage_import_args(Args :: rest_handler:args(), Ctx :: service:ctx())
        -> service:ctx().
get_storage_import_args(Args, Ctx) ->
    kv_utils:copy_found([
        {[storageImport, strategy], [storage_import, strategy]},
        {[storageImport, maxDepth], [storage_import, max_depth]},
        {[storageImport, syncAcl], [storage_import, sync_acl]}
    ], Args, Ctx).


%%-------------------------------------------------------------------
%% @private
%% @doc Parse configuration for autocleaning
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_configuration(Args :: rest_handler:args(), Ctx :: service:ctx())
        -> service:ctx().
get_auto_cleaning_configuration(Args, Ctx) ->
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
    ], Args, Ctx).
