%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Deploys Oneprovider cluster (batch configuration).
%%% @end
%%%-------------------------------------------------------------------
-module(provider_cluster_create_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("deployment_progress.hrl").
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
-type output() :: service_executor:task_id().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> {true, [rest]}.
supported_interfaces(_) ->
    {true, [rest]}.


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok | errors:error().
validate(_) ->
    case onepanel_deployment:is_set(?PROGRESS_READY) of
        true -> ?ERROR_ALREADY_EXISTS;
        false -> ok
    end.


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{input = Data}) ->
    ?notice("Received cluster configuration request with the following batch config:~n~tp", [
        Data
    ]),
    DbHosts = middleware_utils:get_hosts([cluster, databases, nodes], Data),
    CmHosts = middleware_utils:get_hosts([cluster, managers, nodes], Data),
    [MainCmHost] = middleware_utils:get_hosts([cluster, managers, mainNode], Data),
    OpwHosts = middleware_utils:get_hosts([cluster, workers, nodes], Data),

    OneS3NodesKey = [cluster, oneS3, nodes],
    OneS3Hosts = case kv_utils:is_key(OneS3NodesKey, Data) of
        true -> middleware_utils:get_hosts(OneS3NodesKey, Data);
        false -> []
    end,
    OneS3Ctx = kv_utils:copy_found(
        [{[cluster, oneS3, port], port}],
        Data,
        #{hosts => OneS3Hosts}
    ),

    StorageCtx = kv_utils:copy_found([{[cluster, storages], storages}], Data),
    StorageCtx2 = StorageCtx#{hosts => OpwHosts},

    LetsencryptCtx = kv_utils:copy_found(
        [{[oneprovider, letsEncryptEnabled], letsencrypt_enabled}],
        Data),

    DbCtx = kv_utils:copy_found([
        {[cluster, databases, serverQuota], couchbase_server_quota},
        {[cluster, databases, bucketQuota], couchbase_bucket_quota}
    ], Data, #{hosts => DbHosts}),

    OpaHosts = lists:usort(DbHosts ++ CmHosts ++ OpwHosts ++ OneS3Hosts),
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
        ?SERVICE_ONES3 => OneS3Ctx,
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
        cluster_ips => ClusterIPs,
        deploy_ones3 => OneS3Hosts /= []
    }),

    CommonCtx = #{cluster => ClusterCtx, ?SERVICE_OP => OpwCtx},

    {ok, service:apply_async(?SERVICE_OP, deploy, CommonCtx)}.


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, TaskId) ->
    {ok, ?ASYNC_TASK_REPLY(TaskId)}.
