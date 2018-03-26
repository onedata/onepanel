%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /cluster REST resources.
%%%-------------------------------------------------------------------
-module(rest_service).
-author("Krzysztof Trzepla").

-include("http/rest.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/models.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, accept_resource/4,
    provide_resource/2, delete_resource/2]).

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
is_authorized(Req, _Method, #rstate{client = #client{role = admin}}) ->
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
    {model:exists(service) andalso (
        service:exists(service_oz_worker:name()) orelse
            service:exists(service_op_worker:name())
    ), Req};

exists_resource(Req, #rstate{resource = task, bindings = #{id := TaskId}}) ->
    {service:exists_task(TaskId), Req};

exists_resource(Req, #rstate{resource = storage, bindings = #{id := Id}}) ->
    Node = utils:random_element(service_op_worker:get_nodes()),
    {rpc:call(Node, storage, exists, [Id]), Req};

exists_resource(Req, #rstate{resource = storages}) ->
    {true, Req};

exists_resource(Req, #rstate{resource = luma}) ->
    {true, Req};

exists_resource(Req, #rstate{resource = SModule, bindings = #{host := Host}}) ->
    {service:is_member(SModule:name(), Host), Req};

exists_resource(Req, #rstate{resource = SModule}) ->
    case lists:member(SModule, ?SERVICES) of
        true ->
            {model:exists(service) andalso service:exists(SModule:name()), Req};
        false -> {false, Req}
    end.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'POST', Args, #rstate{resource = service_couchbase, version = Version}) ->
    Ctx = #{hosts => onepanel_utils:typed_get(hosts, Args, {seq, list})},
    Ctx2 = onepanel_maps:get_store(serverQuota, Args, couchbase_server_quota, Ctx),
    Ctx3 = onepanel_maps:get_store(bucketQuota, Args, couchbase_bucket_quota, Ctx2),
    {true, rest_replier:handle_service_action_async(Req, service:apply_async(
        service_couchbase:name(), deploy, Ctx3
    ), Version)};

accept_resource(Req, 'POST', Args, #rstate{resource = service_cluster_manager, version = Version}) ->
    Hosts = onepanel_utils:typed_get(hosts, Args, {seq, list}),
    MainHost = onepanel_utils:typed_get(mainHost, Args, list),
    {true, rest_replier:handle_service_action_async(Req, service:apply_async(
        service_cluster_manager:name(), deploy, #{
            main_host => MainHost, hosts => Hosts
        }
    ), Version)};

accept_resource(Req, 'POST', Args, #rstate{resource = service_op_worker} = State) ->
    deploy_cluster_worker(Req, Args, State);


accept_resource(Req, 'POST', Args, #rstate{resource = service_oz_worker} = State) ->
    deploy_cluster_worker(Req, Args, State);

accept_resource(Req, 'POST', Args, #rstate{resource = service_oneprovider, version = Version}) ->
    DbHosts = rest_utils:get_hosts([cluster, databases, nodes], Args),
    CmHosts = rest_utils:get_hosts([cluster, managers, nodes], Args),
    [MainCmHost] = rest_utils:get_hosts([cluster, managers, mainNode], Args),
    OpwHosts = rest_utils:get_hosts([cluster, workers, nodes], Args),

    StorageCtx = onepanel_maps:get_store([cluster, storages], Args, storages),
    StorageCtx2 = StorageCtx#{hosts => OpwHosts, ignore_exists => true},

    DbCtx = #{hosts => DbHosts},
    DbCtx2 = onepanel_maps:get_store([cluster, databases, serverQuota], Args,
        couchbase_server_quota, DbCtx),
    DbCtx3 = onepanel_maps:get_store([cluster, databases, bucketQuota], Args,
        couchbase_bucket_quota, DbCtx2),

    Auth = cowboy_req:header(<<"authorization">>, Req),
    OpaCtx = maps:get(onepanel, Args, #{}),
    OpaCtx2 = OpaCtx#{
        hosts => lists:usort(DbHosts ++ CmHosts ++ OpwHosts),
        auth => Auth,
        api_version => Version
    },

    ClusterIPs = rest_utils:get_cluster_ips(Args),

    % In batch mode IPs do not need user approval
    % TODO VFS-4140 Use proper batch config enabling argument
    IPsConfigured = onepanel_maps:get([oneprovider, register], Args, false),

    ClusterCtx = #{
        service_onepanel:name() => OpaCtx2,
        service_couchbase:name() => DbCtx3,
        service_cluster_manager:name() => #{main_host => MainCmHost,
            hosts => CmHosts, worker_num => length(OpwHosts)},
        service_op_worker:name() => #{hosts => OpwHosts, db_hosts => DbHosts,
            cm_hosts => CmHosts, main_cm_host => MainCmHost,
            mark_cluster_ips_configured => IPsConfigured
        },
        storages => StorageCtx2
    },


    OpwCtx = onepanel_maps:get_store([onezone, domainName], Args, onezone_domain),
    OpwCtx2 = onepanel_maps:get_store([oneprovider, register], Args, oneprovider_register, OpwCtx),
    OpwCtx3 = onepanel_maps:get_store([oneprovider, name], Args, oneprovider_name, OpwCtx2),
    OpwCtx4 = onepanel_maps:get_store([oneprovider, subdomainDelegation], Args, oneprovider_subdomain_delegation, OpwCtx3),
    OpwCtx5 = onepanel_maps:get_store([oneprovider, domain], Args, oneprovider_domain, OpwCtx4),
    OpwCtx6 = onepanel_maps:get_store([oneprovider, subdomain], Args, oneprovider_subdomain, OpwCtx5),
    OpwCtx7 = onepanel_maps:get_store([oneprovider, adminEmail], Args, oneprovider_admin_email, OpwCtx6),
    OpwCtx8 = onepanel_maps:get_store([oneprovider, geoLatitude], Args, oneprovider_geo_latitude, OpwCtx7),
    OpwCtx9 = onepanel_maps:get_store([oneprovider, geoLongitude], Args, oneprovider_geo_longitude, OpwCtx8),
    OpwCtx10 = onepanel_maps:get_store([oneprovider, letsEncryptEnabled], Args,
        oneprovider_letsencrypt_enabled, OpwCtx9, false),
    OpwCtx11 = OpwCtx10#{hosts => OpwHosts, cluster_ips => ClusterIPs},

    {true, rest_replier:handle_service_action_async(Req, service:apply_async(
        service_oneprovider:name(), deploy, #{
            cluster => ClusterCtx, service_oneprovider:name() => OpwCtx11
        }
    ), Version)};

accept_resource(Req, 'POST', Args, #rstate{resource = service_onezone, version = Version}) ->
    DbHosts = rest_utils:get_hosts([cluster, databases, nodes], Args),
    CmHosts = rest_utils:get_hosts([cluster, managers, nodes], Args),
    [MainCmHost] = rest_utils:get_hosts([cluster, managers, mainNode], Args),
    OzwHosts = rest_utils:get_hosts([cluster, workers, nodes], Args),
    ClusterIPs = rest_utils:get_cluster_ips(Args),

    DbCtx = #{hosts => DbHosts},
    DbCtx2 = onepanel_maps:get_store([cluster, databases, serverQuota], Args,
        couchbase_server_quota, DbCtx),
    DbCtx3 = onepanel_maps:get_store([cluster, databases, bucketQuota], Args,
        couchbase_bucket_quota, DbCtx2),

    Auth = cowboy_req:header(<<"authorization">>, Req),
    OpaCtx = maps:get(onepanel, Args, #{}),
    OpaCtx2 = OpaCtx#{
        hosts => lists:usort(DbHosts ++ CmHosts ++ OzwHosts),
        auth => Auth,
        api_version => Version
    },

    OzCtx = onepanel_maps:get_store([onezone, name], Args, name),
    OzCtx2 = onepanel_maps:get_store([onezone, domainName], Args, domain, OzCtx),

    OzwCtx = #{
        hosts => OzwHosts, db_hosts => DbHosts, cm_hosts => CmHosts,
        main_cm_host => MainCmHost,
        cluster_ips => ClusterIPs
    },
    OzwCtx2 = onepanel_maps:get_store([onezone, name], Args, onezone_name, OzwCtx),
    OzwCtx3 = onepanel_maps:get_store([onezone, domainName], Args, onezone_domain, OzwCtx2),

    ClusterCtx = #{
        service_onepanel:name() => OpaCtx2,
        service_couchbase:name() => DbCtx3,
        service_cluster_manager:name() => #{main_host => MainCmHost,
            hosts => CmHosts, worker_num => length(OzwHosts)},
        service_oz_worker:name() => OzwCtx3
    },

    {true, rest_replier:handle_service_action_async(Req, service:apply_async(
        service_onezone:name(), deploy, #{
            cluster => ClusterCtx, service_onezone:name() => OzCtx2
        }
    ), Version)};

accept_resource(Req, 'POST', Args, #rstate{resource = storages}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        service_op_worker:name(), add_storages, #{
            storages => Args, ignore_exists => false
        }
    ))};

accept_resource(Req, 'PATCH', _Args, #rstate{resource = luma,
    bindings = #{id := Id}}) ->
    Ctx = #{id => Id},
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        service_op_worker:name(), invalidate_luma_cache, Ctx
    ))};

accept_resource(Req, 'PATCH', Args, #rstate{resource = storage,
    bindings = #{id := Id}}) ->
    Ctx = #{id => Id},
    Ctx2 = onepanel_maps:get_store(timeout, Args, [args, timeout], Ctx),
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        service_op_worker:name(), update_storage, Ctx2
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
    SModule = case onepanel_env:get(release_type) of
        oneprovider -> service_op_worker;
        onezone -> service_oz_worker
    end,

    {ok, Code, Headers, Body} = rest_replier:format_service_step(
        SModule, get_nagios_response, service_utils:throw_on_error(
            service:apply_sync(SModule:name(), get_nagios_response, #{})
        )
    ),
    Req2 = cowboy_req:reply(Code, Headers, Body, Req),
    {stop, Req2, State};

provide_resource(Req, #rstate{resource = task, bindings = #{id := TaskId}}) ->
    {rest_replier:format_service_task_results(service:get_results(TaskId)), Req};

provide_resource(Req, #rstate{resource = storage, bindings = #{id := Id}}) ->
    {rest_replier:format_service_step(service_op_worker, get_storages,
        service_utils:throw_on_error(service:apply_sync(
            service_op_worker:name(), get_storages, #{id => Id}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = storages}) ->
    {rest_replier:format_service_step(service_op_worker, get_storages,
        service_utils:throw_on_error(service:apply_sync(
            service_op_worker:name(), get_storages, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = SModule}) when
    SModule =:= service_onezone; SModule =:= service_oneprovider ->
    {rest_replier:format_configuration(SModule), Req};

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