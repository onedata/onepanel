%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /provider REST resources.
%%%-------------------------------------------------------------------
-module(rest_oneprovider).
-author("Krzysztof Trzepla").

-include("http/rest.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/models.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, accept_possible/4, is_available/3,
    accept_resource/4, provide_resource/2, delete_resource/2]).

-define(SERVICE, service_oneprovider:name()).

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
is_authorized(Req, _Method, #rstate{client = #client{role = admin}}) ->
    {true, Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, _State) ->
    case service:get(?SERVICE) of
        {ok, #service{ctx = #{registered := true}}} -> {true, Req};
        {ok, #service{}} -> {false, Req};
        #error{reason = ?ERR_NOT_FOUND} -> {false, Req}
    end.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_possible/4}
%% @end
%%--------------------------------------------------------------------
accept_possible(Req, _Method, _Args, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_available/3}
%% @end
%%--------------------------------------------------------------------
is_available(Req, 'GET', #rstate{resource = cluster_ips}) ->
    {true, Req};

is_available(Req, _Method, _State) ->
    {service:all_healthy(), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'POST', Args, #rstate{resource = provider}) ->
    Ctx = onepanel_maps:get_store_multiple([
        {onezoneDomainName, onezone_domain},
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

accept_resource(Req, 'POST', Args, #rstate{resource = spaces}) ->
    Ctx = onepanel_maps:get_store_multiple([
        {token, token},
        {size, size},
        {storageId, storage_id},
        {mountInRoot, mount_in_root}], Args),
    Ctx2 = get_storage_import_args(Args, Ctx),
    Ctx3 = get_storage_update_args(Args, Ctx2),

    {true, rest_replier:handle_service_step(Req, service_oneprovider, support_space,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, support_space, Ctx3
        ))
    )};

accept_resource(Req, 'POST', _Args, #rstate{resource = start_cleaning, bindings = #{id := Id}}) ->
    {true, rest_replier:handle_service_step(Req, service_oneprovider, start_cleaning,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, start_cleaning, #{space_id => Id}
        ))
    )};

accept_resource(Req, 'PATCH', Args, #rstate{resource = provider}) ->
    Ctx = onepanel_maps:get_store_multiple([
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


accept_resource(Req, 'PATCH', Args, #rstate{resource = space, bindings = #{id := Id}}) ->
    Ctx2 = get_storage_update_args(Args),
    Ctx3 = get_storage_import_args(Args, Ctx2),
    Ctx4 = get_file_popularity_args(Args, Ctx3),
    Ctx5 = get_autocleaning_args(Args, Ctx4),
    Ctx6 = Ctx5#{space_id => Id},

    {true, rest_replier:handle_service_step(Req, service_oneprovider, modify_space,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, modify_space, Ctx6
        ))
    )};

accept_resource(Req, 'PATCH', Args, #rstate{resource = cluster_ips}) ->
    {ok, ClusterIps} = onepanel_maps:get(hosts, Args),
    Ctx = #{cluster_ips => onepanel_utils:convert(ClusterIps, {keys, list})},

    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, set_cluster_ips, Ctx
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
    Ctx = onepanel_maps:get_store(period, Params, period),
    Ctx2 = onepanel_maps:get_store(metrics, Params, metrics, Ctx),
    Ctx3 = Ctx2#{space_id => Id},

    {rest_replier:format_service_step(service_oneprovider, get_sync_stats,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_sync_stats, Ctx3
        ))
    ), Req};

provide_resource(Req, #rstate{
    resource = space_auto_cleaning_report_collection,
    bindings = #{id := Id},
    params = Params
}) ->
    Ctx = onepanel_maps:get_store(started_after, Params, started_after),
    Ctx2 = Ctx#{space_id => Id},

    {rest_replier:format_service_step(service_oneprovider, get_autocleaning_reports,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_autocleaning_reports, Ctx2
        ))
    ), Req};

provide_resource(Req, #rstate{
    resource = space_auto_cleaning_status,
    bindings = #{id := Id}
}) ->
    Ctx = #{space_id => Id},
    {rest_replier:format_service_step(service_oneprovider, get_autocleaning_status,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_autocleaning_status, Ctx
        ))
    ), Req};

provide_resource(Req, #rstate{resource = cluster_ips}) ->
    {rest_replier:format_service_step(service_oneprovider, format_cluster_ips,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, format_cluster_ips, #{}
        ))
    ), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, #rstate{resource = provider}) ->
    Response = {true, rest_replier:throw_on_service_error(
        Req, service:apply_sync(?SERVICE, unregister, #{})
    )},
    Response;

delete_resource(Req, #rstate{resource = space, bindings = #{id := Id}}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, revoke_space_support, #{id => Id}
    ))}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc @equiv get_storage_update_args(Args, #{}).
%% @end
%%-------------------------------------------------------------------
-spec get_storage_update_args(Args :: rest_handler:args()) -> service:ctx().
get_storage_update_args(Args) ->
    get_storage_update_args(Args, #{}).

%%-------------------------------------------------------------------
%% @private
%% @doc Parse args for storage_update.
%% @end
%%-------------------------------------------------------------------
-spec get_storage_update_args(Args :: rest_handler:args(), Ctx :: service:ctx())
        -> service:ctx().
get_storage_update_args(Args, Ctx) ->
    onepanel_maps:get_store_multiple([
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
    onepanel_maps:get_store_multiple([
        {[storageImport, strategy], [storage_import, strategy]},
        {[storageImport, maxDepth], [storage_import, max_depth]},
        {[storageImport, syncAcl], [storage_import, sync_acl]}
    ], Args, Ctx).


%%-------------------------------------------------------------------
%% @private
%% @doc Parse args for file_popularity
%% @end
%%-------------------------------------------------------------------
-spec get_file_popularity_args(Args :: rest_handler:args(), Ctx :: service:ctx())
        -> service:ctx().
get_file_popularity_args(Args, Ctx) ->
    onepanel_maps:get_store([filesPopularity, enabled], Args,
        [files_popularity, enabled], Ctx).

%%-------------------------------------------------------------------
%% @private
%% @doc Parse args for autocleaning
%% @end
%%-------------------------------------------------------------------
-spec get_autocleaning_args(Args :: rest_handler:args(), Ctx :: service:ctx())
        -> service:ctx().
get_autocleaning_args(Args, Ctx) ->
    onepanel_maps:get_store_multiple([
        {[autoCleaning, enabled],
            [auto_cleaning, enabled]},
        {[autoCleaning, settings, lowerFileSizeLimit],
            [auto_cleaning, settings, lower_file_size_limit]},
        {[autoCleaning, settings, upperFileSizeLimit],
            [auto_cleaning, settings, upper_file_size_limit]},
        {[autoCleaning, settings, maxFileNotOpenedHours],
            [auto_cleaning, settings, max_file_not_opened_hours]},
        {[autoCleaning, settings, target],
            [auto_cleaning, settings, target]},
        {[autoCleaning, settings, threshold],
            [auto_cleaning, settings, threshold]}
    ], Args, Ctx).
