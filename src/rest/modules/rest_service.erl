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
-include("modules/logger.hrl").
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
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = task, bindings = #{id := TaskId}}) ->
    {service_executor:exists_task(TaskId), Req};

exists_resource(Req, #rstate{resource = storage, bindings = #{name := Name}}) ->
    Node = utils:random_element(service_op_worker:get_nodes()),
    case rpc:call(Node, storage, get_by_name, [Name]) of
        {ok, _} -> {true, Req};
        {error, {not_found, storage}} -> {false, Req}
    end;

exists_resource(Req, #rstate{resource = storages}) ->
    {true, Req};

exists_resource(Req, #rstate{resource = Service, bindings = #{host := Host}}) ->
    {service:is_member(Service:name(), Host), Req};

exists_resource(Req, #rstate{resource = Service}) ->
    case lists:member(Service, ?SERVICES) of
        true -> {service:exists(Service:name()), Req};
        false -> {false, Req}
    end.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'PUT', Args, #rstate{resource = service_couchbase, version = Version}) ->
    Hosts = onepanel_utils:convert(maps:get(hosts, Args), {seq, list}),
    {true, rest_utils:handle_service_action_async(Req, service_executor:apply_async(
        service_couchbase:name(), deploy, #{hosts => Hosts}
    ), Version)};

accept_resource(Req, 'PUT', Args, #rstate{resource = service_cluster_manager, version = Version}) ->
    Hosts = onepanel_utils:convert(maps:get(hosts, Args), {seq, list}),
    MainHost = onepanel_utils:convert(maps:get(mainHost, Args), list),
    {true, rest_utils:handle_service_action_async(Req, service_executor:apply_async(
        service_cluster_manager:name(), deploy, #{
            main_host => MainHost, hosts => Hosts
        }
    ), Version)};

accept_resource(Req, 'PUT', Args, #rstate{resource = service_op_worker} = State) ->
    deploy_cluster_worker(Req, Args, State);


accept_resource(Req, 'PUT', Args, #rstate{resource = service_oz_worker} = State) ->
    deploy_cluster_worker(Req, Args, State);

accept_resource(Req, 'PUT', Args, #rstate{resource = service_oneprovider, version = Version}) ->
    DbHosts = rest_utils:get_hosts([cluster, databases, nodes], Args),
    CmHosts = rest_utils:get_hosts([cluster, managers, nodes], Args),
    [MainCmHost] = rest_utils:get_hosts([cluster, managers, mainNode], Args),
    OpwHosts = rest_utils:get_hosts([cluster, workers, nodes], Args),

    StorageCtx = onepanel_maps:store(storages, [cluster, storages], Args),
    StorageCtx2 = StorageCtx#{hosts => OpwHosts},
    ClusterCtx = #{
        service_couchbase:name() => #{hosts => DbHosts},
        service_cluster_manager:name() => #{main_host => MainCmHost, hosts => CmHosts},
        service_op_worker:name() => #{hosts => OpwHosts, db_hosts => DbHosts,
            cm_hosts => CmHosts, main_cm_host => MainCmHost
        },
        storages => StorageCtx2
    },
    OpCtx = onepanel_maps:store(onezone_domain, [onezone, domainName], Args),
    OpCtx2 = onepanel_maps:store(oneprovider_register, [oneprovider, register], Args, OpCtx),
    OpCtx3 = onepanel_maps:store(oneprovider_name, [oneprovider, name], Args, OpCtx2),
    OpCtx4 = onepanel_maps:store(oneprovider_redirection_point, [oneprovider, redirectionPoint], Args, OpCtx3),
    OpCtx5 = onepanel_maps:store(oneprovider_geo_latitude, [oneprovider, geoLatitude], Args, OpCtx4),
    OpCtx6 = onepanel_maps:store(oneprovider_geo_longitude, [oneprovider, geoLongitude], Args, OpCtx5),
    OpCtx7 = OpCtx6#{hosts => OpwHosts},

    {true, rest_utils:handle_service_action_async(Req, service_executor:apply_async(
        service_oneprovider:name(), deploy, #{
            cluster => ClusterCtx, service_oneprovider:name() => OpCtx7
        }
    ), Version)};

accept_resource(Req, 'PUT', Args, #rstate{resource = service_onezone, version = Version}) ->
    DbHosts = rest_utils:get_hosts([cluster, databases, nodes], Args),
    CmHosts = rest_utils:get_hosts([cluster, managers, nodes], Args),
    [MainCmHost] = rest_utils:get_hosts([cluster, managers, mainNode], Args),
    OzwHosts = rest_utils:get_hosts([cluster, workers, nodes], Args),

    OzCtx = #{
        hosts => OzwHosts, db_hosts => DbHosts, cm_hosts => CmHosts, 
        main_cm_host => MainCmHost
    },
    OzCtx2 = onepanel_maps:store(onezone_name, [onezone, name], Args, OzCtx),
    OzCtx3 = onepanel_maps:store(onezone_domain, [onezone, domainName], Args, OzCtx2),
    ClusterCtx = #{
        service_couchbase:name() => #{hosts => DbHosts},
        service_cluster_manager:name() => #{main_host => MainCmHost, hosts => CmHosts},
        service_oz_worker:name() => OzCtx3
    },

    {true, rest_utils:handle_service_action_async(Req, service_executor:apply_async(
        service_onezone:name(), deploy, #{cluster => ClusterCtx}
    ), Version)};

accept_resource(Req, 'PUT', Args, #rstate{resource = storages}) ->
    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        service_op_worker:name(), add_storages, #{storages => Args}
    ))};

accept_resource(Req, 'PATCH', _Args, #rstate{resource = Service,
    bindings = #{host := Host}, params = #{started := true}}) ->
    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        Service:name(), start, #{hosts => [Host]}
    ))};

accept_resource(Req, 'PATCH', _Args, #rstate{resource = Service,
    bindings = #{host := Host}, params = #{started := false}}) ->
    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        Service:name(), stop, #{hosts => [Host]}
    ))};

accept_resource(Req, 'PATCH', _Args, #rstate{resource = Service,
    params = #{started := true}}) ->
    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        Service:name(), start, #{}
    ))};

accept_resource(Req, 'PATCH', _Args, #rstate{resource = Service,
    params = #{started := false}}) ->
    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        Service:name(), stop, #{}
    ))};

accept_resource(Req, _Method, _Args, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = task, bindings = #{id := TaskId}}) ->
    {rest_utils:format_service_steps(service_executor:get_results(TaskId)), Req};

provide_resource(Req, #rstate{resource = storage, bindings = #{name := Name}}) ->
    {rest_utils:format_service_step(service_op_worker, get_storages,
        service_executor:apply_sync(service_op_worker:name(), get_storages,
            #{name => Name})
    ), Req};

provide_resource(Req, #rstate{resource = storages}) ->
    {rest_utils:format_service_step(service_op_worker, get_storages,
        service_executor:apply_sync(service_op_worker:name(), get_storages, #{})
    ), Req};

provide_resource(Req, #rstate{resource = Service, bindings = #{host := Host}}) ->
    {rest_utils:format_service_step(Service, status,
        service_executor:apply_sync(Service:name(), status, #{hosts => [Host]})
    ), Req};

provide_resource(Req, #rstate{resource = Service}) ->
    {rest_utils:format_service_step(Service, status,
        service_executor:apply_sync(Service:name(), status, #{})
    ), Req};

provide_resource(Req, _State) ->
    {[], Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
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
%%--------------------------------------------------------------------
-spec deploy_cluster_worker(Req :: cowboy_req:req(), Args :: rest_handler:args(),
    State :: rest_handler:state()) -> {Accepted :: boolean(), Req :: cowboy_req:req()}.
deploy_cluster_worker(Req, Args, #rstate{resource = Service, version = Version}) ->
    Hosts = onepanel_utils:convert(maps:get(hosts, Args), {seq, list}),
    {ok, #service{hosts = DbHosts}} = service:get(service_couchbase:name()),
    {ok, #service{hosts = CmHosts, ctx = #{main_host := MainCmHost}}} =
        service:get(service_cluster_manager:name()),
    {true, rest_utils:handle_service_action_async(Req, service_executor:apply_async(
        Service:name(), deploy, #{
            hosts => Hosts, db_hosts => DbHosts, cm_hosts => CmHosts,
            main_cm_host => MainCmHost
        }
    ), Version)}.