%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /cluster REST resources.
%%%-------------------------------------------------------------------
-module(rest_service).
-author("Krzysztof Trzepla").

-include("authentication.hrl").
-include("deployment_progress.hrl").
-include("http/rest.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, accept_possible/4, is_available/3,
    accept_resource/4, provide_resource/2, delete_resource/2]).

-define(SERVICES, [service_couchbase, service_cluster_manager, service_op_worker,
    service_oz_worker, service_oneprovider, service_onezone]).

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
is_authorized(Req, _Method, #rstate{client = #client{role = Role}})
    when Role == root; Role == admin; Role == user ->
    {true, Req};

is_authorized(Req, 'GET', #rstate{resource = Resource}) when
    Resource =:= task orelse Resource =:= nagios ->
    {true, Req};

is_authorized(Req, Method, #rstate{resource = Resource}) when
    (Method =:= 'POST' orelse Method =:= 'GET') andalso
        (Resource =:= service_oneprovider orelse Resource =:= service_onezone) ->
    {onepanel_user:get_by_role(admin) == [], Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = nagios}) ->
    Worker = cluster_worker_name(),
    Module = service:get_module(Worker),
    {model:exists(service) andalso service:exists(Worker)
        andalso Module:get_hosts() /= [], Req};

exists_resource(Req, #rstate{resource = task, bindings = #{id := TaskId}}) ->
    {service:exists_task(TaskId), Req};

exists_resource(Req, #rstate{resource = SModule, bindings = #{host := Host}}) ->
    {lists:member(Host, hosts:all(SModule:name())), Req};

exists_resource(Req, #rstate{resource = dns_check_configuration}) ->
    {true, Req};

exists_resource(Req, #rstate{resource = Resource}) when
    Resource =:= service_onezone; Resource =:= service_oneprovider;
    Resource =:= dns_check ->
    % TODO VFS-4460 Short-circuit if workers are off
    {model:exists(onepanel_deployment) andalso
        onepanel_deployment:is_completed(?PROGRESS_CLUSTER), Req};

exists_resource(Req, #rstate{resource = SModule}) ->
    case lists:member(SModule, ?SERVICES) of
        true ->
            {model:exists(service) andalso service:exists(SModule:name()), Req};
        false -> {false, Req}
    end.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_possible/4}
%% @end
%%--------------------------------------------------------------------
accept_possible(Req, 'POST', _Args, #rstate{resource = SModule})
    when SModule == service_oneprovider orelse SModule == service_onezone ->
    {not onepanel_deployment:is_completed(?PROGRESS_READY), Req};

accept_possible(Req, _Method, _Args, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_available/3}
%% @end
%%--------------------------------------------------------------------
is_available(Req, _Method, #rstate{resource = Resource}) when
    Resource == luma;
    Resource == storages;
    Resource == storage ->
    {service:all_healthy(), Req};

is_available(Req, Method, #rstate{resource = Resource}) when
    Method == 'GET' andalso Resource == dns_check ->
    % @TODO When service_op_worker:get_domain will work when op_worker is offline
    %       this request should always be allowed.
    {onepanel_env:get_release_type() == onezone
        orelse service:all_healthy(), Req};

is_available(Req, _Method, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean() | stop, Req :: cowboy_req:req()}.
accept_resource(Req, 'POST', Args, #rstate{resource = service_couchbase, version = Version}) ->
    Ctx = #{hosts => onepanel_utils:typed_get(hosts, Args, {seq, list})},
    Ctx2 = onepanel_maps:get_store(serverQuota, Args, couchbase_server_quota, Ctx),
    Ctx3 = onepanel_maps:get_store(bucketQuota, Args, couchbase_bucket_quota, Ctx2),
    {true, rest_replier:handle_service_action_async(Req, service:apply_async(
        ?SERVICE_CB, deploy, Ctx3
    ), Version)};

accept_resource(Req, 'POST', Args, #rstate{resource = service_cluster_manager, version = Version}) ->
    Hosts = onepanel_utils:typed_get(hosts, Args, {seq, list}),
    MainHost = onepanel_utils:typed_get(mainHost, Args, list),
    {true, rest_replier:handle_service_action_async(Req, service:apply_async(
        ?SERVICE_CM, deploy, #{
            main_host => MainHost, hosts => Hosts
        }
    ), Version)};

accept_resource(Req, 'POST', Args, #rstate{resource = service_op_worker} = State) ->
    deploy_cluster_worker(Req, Args, State);


accept_resource(Req, 'POST', Args, #rstate{resource = service_oz_worker} = State) ->
    deploy_cluster_worker(Req, Args, State);

accept_resource(Req, 'POST', Args,
    #rstate{resource = service_oneprovider, version = Version}) ->

    DbHosts = rest_utils:get_hosts([cluster, databases, nodes], Args),
    CmHosts = rest_utils:get_hosts([cluster, managers, nodes], Args),
    [MainCmHost] = rest_utils:get_hosts([cluster, managers, mainNode], Args),
    OpwHosts = rest_utils:get_hosts([cluster, workers, nodes], Args),

    StorageCtx = onepanel_maps:get_store([cluster, storages], Args, storages),
    StorageCtx2 = StorageCtx#{hosts => OpwHosts, ignore_exists => true},

    LetsencryptCtx =
        onepanel_maps:get_store([oneprovider, letsEncryptEnabled], Args, letsencrypt_enabled),

    DbCtx = onepanel_maps:get_store_multiple([
        {[cluster, databases, serverQuota], couchbase_server_quota},
        {[cluster, databases, bucketQuota], couchbase_bucket_quota}
    ], Args, #{hosts => DbHosts}),

    Auth = cowboy_req:header(<<"authorization">>, Req),
    OpaCtx = maps:get(onepanel, Args, #{}),
    OpaHosts = lists:usort(DbHosts ++ CmHosts ++ OpwHosts),
    OpaCtx2 = OpaCtx#{
        hosts => OpaHosts,
        auth => Auth,
        api_version => Version
    },
    OpaCtx3 = onepanel_maps:get_store([onepanel, interactiveDeployment], Args,
        interactive_deployment, OpaCtx2, true),

    ClusterIPs = rest_utils:get_cluster_ips(Args),

    % In batch mode IPs do not need user approval
    % TODO VFS-4140 Use proper batch config enabling argument
    IPsConfigured = onepanel_maps:get([oneprovider, register], Args, false),

    ClusterCtx = #{
        ?SERVICE_PANEL => OpaCtx3,
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

    OpwCtx = onepanel_maps:get_store_multiple([
        {[oneprovider, token], oneprovider_token},
        {[oneprovider, register], oneprovider_register},
        {[oneprovider, name], oneprovider_name},
        {[oneprovider, subdomainDelegation], oneprovider_subdomain_delegation},
        {[oneprovider, domain], oneprovider_domain},
        {[oneprovider, subdomain], oneprovider_subdomain},
        {[oneprovider, adminEmail], oneprovider_admin_email},
        {[oneprovider, geoLatitude], oneprovider_geo_latitude},
        {[oneprovider, geoLongitude], oneprovider_geo_longitude}
    ], Args, #{hosts => OpwHosts, cluster_ips => ClusterIPs}),

    {true, rest_replier:handle_service_action_async(Req, service:apply_async(
        ?SERVICE_OP, deploy, #{
            cluster => ClusterCtx, ?SERVICE_OP => OpwCtx
        }
    ), Version)};

accept_resource(Req, 'POST', Args, #rstate{
    resource = service_onezone, version = Version}) ->

    DbHosts = rest_utils:get_hosts([cluster, databases, nodes], Args),
    CmHosts = rest_utils:get_hosts([cluster, managers, nodes], Args),
    [MainCmHost] = rest_utils:get_hosts([cluster, managers, mainNode], Args),
    OzwHosts = rest_utils:get_hosts([cluster, workers, nodes], Args),
    AllHosts = lists:usort(DbHosts ++ CmHosts ++ OzwHosts),
    ClusterIPs = rest_utils:get_cluster_ips(Args),

    DbCtx = onepanel_maps:get_store_multiple([
        {[cluster, databases, serverQuota], couchbase_server_quota},
        {[cluster, databases, bucketQuota], couchbase_bucket_quota}
    ], Args, #{hosts => DbHosts}),

    Auth = cowboy_req:header(<<"authorization">>, Req),
    OpaCtx = maps:get(onepanel, Args, #{}),
    OpaCtx2 = OpaCtx#{
        hosts => AllHosts,
        auth => Auth,
        api_version => Version
    },
    OpaCtx3 = onepanel_maps:get_store([onepanel, interactiveDeployment], Args,
        interactive_deployment, OpaCtx2, true),

    LeCtx = onepanel_maps:get_store_multiple([
        {[onezone, letsEncryptEnabled], letsencrypt_enabled}
    ], Args, #{hosts => AllHosts}),

    OzCtx = onepanel_maps:get_store_multiple([
        {[onezone, name], name},
        {[onezone, domainName], domain},
        {[onezone, builtInDnsServer], [dns_check_config, built_in_dns_server]}
    ], Args),

    OzwCtx = #{
        hosts => OzwHosts, db_hosts => DbHosts, cm_hosts => CmHosts,
        main_cm_host => MainCmHost,
        cluster_ips => ClusterIPs
    },

    OzwCtx2 = onepanel_maps:get_store_multiple([
        {[onezone, name], onezone_name},
        {[onezone, domainName], onezone_domain}
    ], Args, OzwCtx),

    OzwCtx3 = case Args of
        #{onezone := #{policies := Policies}} ->
            OzwCtx2#{policies => rest_onezone:make_policies_ctx(Policies)};
        _ -> OzwCtx2
    end,

    ClusterCtx = #{
        ?SERVICE_PANEL => OpaCtx3,
        ?SERVICE_CB => DbCtx,
        ?SERVICE_CM => #{main_host => MainCmHost,
            hosts => CmHosts, worker_num => length(OzwHosts)},
        ?SERVICE_OZW => OzwCtx3,
        ?SERVICE_LE => LeCtx
    },

    {true, rest_replier:handle_service_action_async(Req, service:apply_async(
        ?SERVICE_OZ, deploy, #{
            cluster => ClusterCtx, ?SERVICE_OZ => OzCtx
        }
    ), Version)};

accept_resource(Req, 'PATCH', Args, #rstate{resource = dns_check_configuration}) ->
    Ctx = case Args of
        #{dnsServers := IPs} ->
            #{dns_servers => parse_ip4_list([dnsServers], IPs)};
        _ -> #{}
    end,
    Ctx2 = onepanel_maps:get_store_multiple([
        {builtInDnsServer, built_in_dns_server},
        {dnsCheckAcknowledged, dns_check_acknowledged}
    ], Args, Ctx),

    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        cluster_worker_name(), configure_dns_check, Ctx2
    ))};

accept_resource(Req, 'PATCH', _Args, #rstate{resource = SModule,
    bindings = #{host := Host}, params = #{started := true}}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        SModule:name(), start, #{hosts => [Host]}
    ))};

accept_resource(Req, 'PATCH', _Args, #rstate{resource = SModule,
    bindings = #{host := Host}, params = #{started := false}}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        SModule:name(), stop, #{hosts => [Host]}
    ))};

accept_resource(Req, 'PATCH', _Args, #rstate{resource = SModule,
    params = #{started := true}}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        SModule:name(), start, #{}
    ))};

accept_resource(Req, 'PATCH', _Args, #rstate{resource = SModule,
    params = #{started := false}}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        SModule:name(), stop, #{}
    ))}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()} |
    {stop, Req :: cowboy_req:req(), State :: rest_handler:state()}.
provide_resource(Req, #rstate{resource = nagios} = State) ->
    Worker = cluster_worker_name(),

    Result = rest_replier:format_service_step(
        service:get_module(Worker), get_nagios_response,
        service:apply_sync(Worker, get_nagios_response, #{})
    ),
    case Result of
        {ok, Code, Headers, Body} ->
            Req2 = cowboy_req:reply(Code, Headers, Body, Req),
            {stop, Req2, State};
        {error, econnrefused} ->
            Req2 = cowboy_req:reply(503, #{}, Req),
            {stop, Req2, State};
        {error, timeout} ->
            Req2 = cowboy_req:reply(503, #{}, Req),
            {stop, Req2, State}
    end;

provide_resource(Req, #rstate{resource = task, bindings = #{id := TaskId}}) ->
    {rest_replier:format_service_task_results(service:get_results(TaskId)), Req};

provide_resource(Req, #rstate{resource = SModule}) when
    SModule =:= service_onezone; SModule =:= service_oneprovider ->
    {rest_replier:format_service_configuration(SModule), Req};

provide_resource(Req, #rstate{resource = dns_check, params = Params}) ->
    Ctx = #{force_check => onepanel_maps:get(forceCheck, Params, false)},

    {rest_replier:format_dns_check_result(
        service_utils:throw_on_error(service:apply_sync(
            cluster_worker_name(), dns_check, Ctx
        ))
    ), Req};

provide_resource(Req, #rstate{resource = dns_check_configuration}) ->
    {rest_replier:format_service_step(dns_check, get_configuration,
        service_utils:throw_on_error(service:apply_sync(
            cluster_worker_name(), get_dns_check_configuration, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = SModule, bindings = #{host := Host}}) ->
    {rest_replier:format_service_host_status(SModule,
        service_utils:throw_on_error(
            service:apply_sync(SModule:name(), status, #{hosts => [Host]})
        )
    ), Req};

provide_resource(Req, #rstate{resource = SModule}) ->
    {rest_replier:format_service_status(SModule,
        service_utils:throw_on_error(
            service:apply_sync(SModule:name(), status, #{})
        )
    ), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, _State) ->
    {false, Req}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Deploys cluster worker service.
%% @end
%%--------------------------------------------------------------------
-spec deploy_cluster_worker(Req :: cowboy_req:req(), Args :: rest_handler:args(),
    State :: rest_handler:state()) -> {Accepted :: boolean(), Req :: cowboy_req:req()}.
deploy_cluster_worker(Req, Args, #rstate{resource = SModule, version = Version}) ->
    Hosts = onepanel_utils:typed_get(hosts, Args, {seq, list}),
    {ok, #service{hosts = DbHosts}} = service:get(service_couchbase:name()),
    {ok, #service{hosts = CmHosts, ctx = #{main_host := MainCmHost}}} =
        service:get(service_cluster_manager:name()),
    {true, rest_replier:handle_service_action_async(Req, service:apply_async(
        SModule:name(), deploy, #{
            hosts => Hosts, db_hosts => DbHosts, cm_hosts => CmHosts,
            main_cm_host => MainCmHost
        }
    ), Version)}.


%%--------------------------------------------------------------------
%% @private @doc Returns name of cluster worker depending on the release type.
%% @end
%%--------------------------------------------------------------------
-spec cluster_worker_name() -> service:name().
cluster_worker_name() ->
    case onepanel_env:get_release_type() of
        oneprovider -> ?SERVICE_OPW;
        onezone -> ?SERVICE_OZW
    end.


%% @private
-spec parse_ip4_list(FieldName :: onepanel_parser:keys(), IpBinaries :: [binary()]) ->
    [inet:ip4_address()] | no_return().
parse_ip4_list(FieldName, IpBinaries) ->
    lists:map(fun(IpBinary) ->
        case onepanel_ip:parse_ip4(IpBinary) of
            {ok, IP} -> IP;
            _ -> ?throw_error({?ERR_INVALID_VALUE, FieldName, ['IPv4']})
        end
    end, IpBinaries).
