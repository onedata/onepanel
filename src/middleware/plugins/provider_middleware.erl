%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Middleware plugin for the onp_provider type.
%%% Mostly handles operations which are specific to op_panel and configuring
%%% the deployed Oneprovider.
%%% An exception is the instance:protected aspect used to fetch information
%%% about other providers available to the currently authenticated user.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_middleware).
-author("Wojciech Geisler").

-behaviour(middleware_plugin).

-include("authentication.hrl").
-include("deployment_progress.hrl").
-include("http/rest.hrl").
-include("middleware/middleware.hrl").
-include("names.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([operation_supported/3, required_availability/3, fetch_entity/1,
    authorize/2, validate/2]).
-export([create/1, get/2, update/1, delete/1]).


%%%===================================================================
%%% middleware_plugin callbacks
%%%===================================================================

-spec operation_supported(middleware:operation(), gri:aspect(),
    middleware:scope()) -> boolean().
% register
operation_supported(create, instance, private) -> onepanel:is_op_panel();
operation_supported(create, cluster, private) -> true;

% this provider
operation_supported(get, instance, private) -> onepanel:is_op_panel();
% remote provider
operation_supported(get, remote_instance, private) -> true;
operation_supported(get, cluster, private) -> onepanel:is_op_panel();
operation_supported(get, transfers_mock, private) -> onepanel:is_op_panel();

operation_supported(update, transfers_mock, private) -> onepanel:is_op_panel();
operation_supported(update, instance, private) -> onepanel:is_op_panel();

operation_supported(delete, instance, private) -> onepanel:is_op_panel();

operation_supported(_, _, _) -> false.


-spec required_availability(middleware:operation(), gri:aspect(),
    middleware:scope()) -> [middleware:availability_level()].
required_availability(create, instance, private) -> [?SERVICE_OPW, all_healthy];
required_availability(create, cluster, private) -> [];

required_availability(get, instance, private) -> [];
required_availability(get, remote_instance, private) ->
    case onepanel_env:get_cluster_type() of
        ?ONEPROVIDER -> [];
        ?ONEZONE -> [all_healthy]
    end;
required_availability(get, cluster, private) -> [];
required_availability(get, transfers_mock, private) -> [];

required_availability(update, instance, private) -> [?SERVICE_OPW, all_healthy];
required_availability(update, transfers_mock, private) -> [?SERVICE_OPW, all_healthy];

required_availability(delete, instance, private) -> [?SERVICE_OPW, all_healthy].



-spec fetch_entity(middleware:req()) ->
    {ok, middleware:versioned_entity()} | undefined | errors:error().
fetch_entity(#onp_req{}) ->
    undefined.


-spec authorize(middleware:req(), middleware:entity()) -> boolean().
authorize(#onp_req{operation = create, gri = #gri{aspect = instance}}, _) ->
    % only root can register a provider
    false;

authorize(#onp_req{
    operation = create, client = Client, gri = #gri{aspect = cluster}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{
    operation = get, client = #client{role = member}, gri = #gri{aspect = As}
}, _) when
    As == instance;
    As == remote_instance;
    As == cluster;
    As == transfers_mock
->
    true;

authorize(#onp_req{
    operation = update, client = Client, gri = #gri{aspect = As}
}, _) when
    As == instance;
    As == transfers_mock
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{
    operation = delete, client = Client, gri = #gri{aspect = instance}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_DELETE).


-spec validate(middleware:req(), middleware:entity()) -> ok | no_return().
validate(#onp_req{operation = create, gri = #gri{aspect = instance}}, _) ->
    case service_oneprovider:is_registered() of
        true -> throw(?ERROR_ALREADY_EXISTS);
        false -> ok
    end;

validate(#onp_req{operation = create, gri = #gri{aspect = cluster}}, _) ->
    case onepanel_deployment:is_set(?PROGRESS_READY) of
        true -> throw(?ERROR_ALREADY_EXISTS);
        false -> ok
    end;

validate(#onp_req{
    operation = get, gri = #gri{aspect = instance, scope = private}
}, _) ->
    case service_oneprovider:is_registered() of
        true -> ok;
        false -> throw(?ERROR_UNREGISTERED_ONEPROVIDER)
    end;

validate(#onp_req{
    operation = get, gri = #gri{aspect = remote_instance},
    client = Client
}, _) ->
    case Client of
        #client{role = member} -> ok;
        #client{role = root} -> throw(?ERROR_NOT_FOUND)
    end;

validate(#onp_req{operation = get, gri = #gri{aspect = cluster}}, _) ->
    case onepanel_deployment:is_set(?PROGRESS_CLUSTER) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end;

validate(#onp_req{operation = Op, gri = #gri{aspect = transfers_mock}}, _) when
    Op == get;
    Op == update
->
    case service:get_hosts(?SERVICE_OPW) of
        [] -> throw(?ERROR_NO_SERVICE_NODES(?SERVICE_OPW));
        _ -> ok
    end;

validate(#onp_req{
    operation = update, gri = #gri{aspect = instance}
}, _) ->
    case service_oneprovider:is_registered() of
        true -> ok;
        false -> throw(?ERROR_UNREGISTERED_ONEPROVIDER)
    end;

validate(#onp_req{operation = delete, gri = #gri{aspect = instance}}, _) ->
    case service_oneprovider:is_registered() of
        true -> ok;
        false -> throw(?ERROR_UNREGISTERED_ONEPROVIDER)
    end.


-spec create(middleware:req()) -> middleware:create_result().
create(#onp_req{gri = #gri{aspect = instance}, data = Data}) ->
    Ctx = kv_utils:copy_found([
        {tokenProvisionMethod, oneprovider_token_provision_method},
        {token, oneprovider_token},
        {tokenFile, oneprovider_token_file},
        {name, oneprovider_name},
        {subdomainDelegation, oneprovider_subdomain_delegation},
        {domain, oneprovider_domain},
        {subdomain, oneprovider_subdomain},
        {adminEmail, oneprovider_admin_email},
        {geoLatitude, oneprovider_geo_latitude},
        {geoLongitude, oneprovider_geo_longitude}
    ], Data),
    middleware_utils:execute_service_action(?SERVICE_OP, register, Ctx);

create(#onp_req{gri = #gri{aspect = cluster}, data = Data}) ->
    ?notice("Received cluster configuration request with the following batch config:~n~p", [
        Data
    ]),
    DbHosts = middleware_utils:get_hosts([cluster, databases, nodes], Data),
    CmHosts = middleware_utils:get_hosts([cluster, managers, nodes], Data),
    [MainCmHost] = middleware_utils:get_hosts([cluster, managers, mainNode], Data),
    OpwHosts = middleware_utils:get_hosts([cluster, workers, nodes], Data),

    StorageCtx = kv_utils:copy_found([{[cluster, storages], storages}], Data),
    StorageCtx2 = StorageCtx#{hosts => OpwHosts},

    LetsencryptCtx = kv_utils:copy_found(
        [{[oneprovider, letsEncryptEnabled], letsencrypt_enabled}],
        Data),

    DbCtx = kv_utils:copy_found([
        {[cluster, databases, serverQuota], couchbase_server_quota},
        {[cluster, databases, bucketQuota], couchbase_bucket_quota}
    ], Data, #{hosts => DbHosts}),

    OpaHosts = lists:usort(DbHosts ++ CmHosts ++ OpwHosts),
    OpaCtx = kv_utils:copy_found([
        {[onepanel, interactiveDeployment], interactive_deployment, true},
        {[onepanel, guiDebugMode], gui_debug_mode}
    ], Data, maps:put(hosts, OpaHosts, maps:get(onepanel, Data, #{}))),
    ClusterIPs = middleware_utils:get_cluster_ips(Data),

    % In batch mode IPs do not need user approval
    % TODO VFS-4140 Use proper batch config enabling argument
    IPsConfigured = kv_utils:get([oneprovider, register], Data, false),

    ClusterCtx = #{
        ?SERVICE_PANEL => OpaCtx,
        ?SERVICE_CB => DbCtx,
        ?SERVICE_CM => #{main_host => MainCmHost,
            hosts => CmHosts, worker_num => length(OpwHosts)},
        ?SERVICE_OPW => #{hosts => OpwHosts, db_hosts => DbHosts,
            cm_hosts => CmHosts, main_cm_host => MainCmHost,
            mark_cluster_ips_configured => IPsConfigured
        },
        ?SERVICE_LE => LetsencryptCtx#{hosts => OpaHosts},
        storages => StorageCtx2
    },

    OpwCtx = kv_utils:copy_found([
        {[oneprovider, tokenProvisionMethod], oneprovider_token_provision_method},
        {[oneprovider, token], oneprovider_token},
        {[oneprovider, tokenFile], oneprovider_token_file},
        {[oneprovider, register], oneprovider_register},
        {[oneprovider, name], oneprovider_name},
        {[oneprovider, subdomainDelegation], oneprovider_subdomain_delegation},
        {[oneprovider, domain], oneprovider_domain},
        {[oneprovider, subdomain], oneprovider_subdomain},
        {[oneprovider, adminEmail], oneprovider_admin_email},
        {[oneprovider, geoLatitude], oneprovider_geo_latitude},
        {[oneprovider, geoLongitude], oneprovider_geo_longitude}
    ], Data, #{
        hosts => OpwHosts,
        cluster_ips => ClusterIPs
    }),

    CommonCtx = #{cluster => ClusterCtx, ?SERVICE_OP => OpwCtx},

    {ok, value, _TaskId = service:apply_async(?SERVICE_OP, deploy, CommonCtx)}.


-spec get(middleware:req(), middleware:entity()) -> middleware:get_result().
get(#onp_req{gri = #gri{aspect = instance, id = undefined, scope = private}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OP, get_details
    )};

get(#onp_req{client = #client{zone_credentials = Auth},
    gri = #gri{aspect = remote_instance, id = Id}
}, _) ->
    {ok, value, clusters:fetch_remote_provider_info(Auth, Id)};

get(#onp_req{gri = #gri{aspect = cluster}}, _) ->
    {ok, value, middleware_utils:format_service_configuration(service_oneprovider)};

get(#onp_req{gri = #gri{aspect = transfers_mock}}, _) ->
    {ok, value, service_op_worker:is_transfers_mock_enabled()}.


-spec update(middleware:req()) -> middleware:update_result().
update(#onp_req{gri = #gri{aspect = instance}, data = Data}) ->
    Ctx = kv_utils:copy_found([
        {name, oneprovider_name},
        {subdomainDelegation, oneprovider_subdomain_delegation},
        {domain, oneprovider_domain},
        {subdomain, oneprovider_subdomain},
        {adminEmail, oneprovider_admin_email},
        {geoLatitude, oneprovider_geo_latitude},
        {geoLongitude, oneprovider_geo_longitude},
        {letsEncryptEnabled, letsencrypt_enabled}
    ], Data),

    middleware_utils:execute_service_action(?SERVICE_OP, modify_details, Ctx);

update(#onp_req{gri = #gri{aspect = transfers_mock}, data = Data}) ->
    Enabled = maps:get(transfersMock, Data),
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, set_transfers_mock, #{transfers_mock => Enabled}).


-spec delete(middleware:req()) -> middleware:delete_result().
delete(#onp_req{gri = #gri{aspect = instance}}) ->
    middleware_utils:execute_service_action(?SERVICE_OP, unregister, #{}).

