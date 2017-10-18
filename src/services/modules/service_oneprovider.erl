%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains oneprovider services management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_oneprovider).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("service.hrl").
-include_lib("ctool/include/oz/oz_providers.hrl").
-include_lib("ctool/include/oz/oz_spaces.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, register/1, unregister/1, modify_details/1, get_details/1,
    support_space/1, revoke_space_support/1, get_spaces/1, get_space_details/1,
    modify_space/1, get_sync_stats/1, restart_listeners/1,
    restart_provider_listeners/1]).

-define(SERVICE_OPA, service_onepanel:name()).
-define(SERVICE_CB, service_couchbase:name()).
-define(SERVICE_CM, service_cluster_manager:name()).
-define(SERVICE_OPW, service_op_worker:name()).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    oneprovider.


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_hosts/0}
%% @end
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    lists:usort(lists:append([
        service:get_hosts(?SERVICE_CB),
        service:get_hosts(?SERVICE_CM),
        service:get_hosts(?SERVICE_OPW)
    ])).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_nodes/0}
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    lists:usort(lists:append([
        service:get_nodes(?SERVICE_CB),
        service:get_nodes(?SERVICE_CM),
        service:get_nodes(?SERVICE_OPW)
    ])).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, Ctx) ->
    {ok, OpaCtx} = onepanel_maps:get([cluster, ?SERVICE_OPA], Ctx),
    {ok, CbCtx} = onepanel_maps:get([cluster, ?SERVICE_CB], Ctx),
    {ok, CmCtx} = onepanel_maps:get([cluster, ?SERVICE_CM], Ctx),
    {ok, OpwCtx} = onepanel_maps:get([cluster, ?SERVICE_OPW], Ctx),
    StorageCtx = onepanel_maps:get([cluster, storages], Ctx, #{}),
    OpCtx = onepanel_maps:get(name(), Ctx, #{}),

    service:create(#service{name = name()}),
    Register = fun
        (#{oneprovider_register := true}) ->
            case service:get(name()) of
                {ok, #service{ctx = #{registered := Registered}}} ->
                    not Registered;
                _ -> true
            end;
        (_) -> false
    end,
    S = #step{verify_hosts = false},
    Ss = #steps{verify_hosts = false},
    [
        Ss#steps{service = ?SERVICE_OPA, action = deploy, ctx = OpaCtx},
        Ss#steps{service = ?SERVICE_CB, action = deploy, ctx = CbCtx},
        S#step{service = ?SERVICE_CB, function = status, ctx = CbCtx},
        Ss#steps{service = ?SERVICE_CM, action = deploy, ctx = CmCtx},
        S#step{service = ?SERVICE_CM, function = status, ctx = CmCtx},
        Ss#steps{service = ?SERVICE_OPW, action = deploy, ctx = OpwCtx},
        S#step{service = ?SERVICE_OPW, function = status, ctx = OpwCtx},
        Ss#steps{service = ?SERVICE_OPW, action = add_storages, ctx = StorageCtx},
        Ss#steps{action = register, ctx = OpCtx, condition = Register},
        Ss#steps{service = ?SERVICE_OPA, action = add_users, ctx = OpaCtx}
    ];

get_steps(start, _Ctx) ->
    [
        #steps{service = ?SERVICE_CB, action = start},
        #steps{service = ?SERVICE_CM, action = start},
        #steps{service = ?SERVICE_OPW, action = start}
    ];

get_steps(stop, _Ctx) ->
    [
        #steps{service = ?SERVICE_OPW, action = stop},
        #steps{service = ?SERVICE_CM, action = stop},
        #steps{service = ?SERVICE_CB, action = stop}
    ];

get_steps(restart, _Ctx) ->
    [
        #steps{action = stop},
        #steps{action = start}
    ];

get_steps(status, _Ctx) ->
    [
        #steps{service = ?SERVICE_CB, action = status},
        #steps{service = ?SERVICE_CM, action = status},
        #steps{service = ?SERVICE_OPW, action = status}
    ];

get_steps(register, #{hosts := Hosts} = Ctx) ->
    [
        #step{hosts = Hosts, function = configure, ctx = Ctx#{application => name()}},
        #step{hosts = Hosts, function = configure,
            ctx = Ctx#{application => ?APP_NAME}},
        #step{hosts = Hosts, function = register, selection = any,
            attempts = onepanel_env:get(oneprovider_register_attempts)}
    ];

get_steps(register, Ctx) ->
    get_steps(register, Ctx#{hosts => service_op_worker:get_hosts()});

get_steps(restart_listeners, #{hosts := Hosts}) ->
    [
        #step{hosts = Hosts, function = restart_provider_listeners},
        #step{hosts = Hosts, function = restart_listeners}
    ];

get_steps(restart_listeners, Ctx) ->
    get_steps(restart_listeners, Ctx#{hosts => service_op_worker:get_hosts()});

get_steps(unregister, #{hosts := Hosts} = Ctx) ->
    [
        #step{hosts = Hosts, function = unregister, selection = any, ctx = Ctx}
    ];

get_steps(unregister, Ctx) ->
    get_steps(unregister, Ctx#{hosts => service_op_worker:get_hosts()});

get_steps(Action, Ctx) when
    Action =:= get_details;
    Action =:= modify_details;
    Action =:= support_space;
    Action =:= revoke_space_support;
    Action =:= get_spaces;
    Action =:= get_space_details;
    Action =:= modify_space;
    Action =:= get_sync_stats ->
    case Ctx of
        #{hosts := Hosts} ->
            [#step{hosts = Hosts, function = Action, selection = any}];
        _ ->
            [#step{function = Action, selection = any,
                ctx = Ctx#{hosts => service_op_worker:get_hosts()}}]
    end.

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(#{application := ?APP_NAME} = Ctx) ->
    OzDomain = service_ctx:get(onezone_domain, Ctx),
    application:set_env(?APP_NAME, onezone_domain, OzDomain),
    onepanel_env:write([?APP_NAME, onezone_domain], OzDomain);

configure(Ctx) ->
    OzDomain = service_ctx:get(onezone_domain, Ctx),
    Name = service_op_worker:name(),
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(Name, Host),
    AppConfigFile = service_ctx:get(op_worker_app_config_file, Ctx),
    rpc:call(Node, application, set_env, [Name, oz_domain, OzDomain]),
    onepanel_env:write([Name, oz_domain], OzDomain, AppConfigFile).


%%--------------------------------------------------------------------
%% @doc Registers provider in the zone.
%% @end
%%--------------------------------------------------------------------
-spec register(Ctx :: service:ctx()) ->
    {ok, ProviderId :: binary()} | no_return().
register(Ctx) ->
    Length = service_ctx:get(oneprovider_key_length, Ctx),
    Subject = service_ctx:get(oneprovider_csr_subject, Ctx),
    Key = onepanel_ssl:gen_rsa(Length),
    Csr = onepanel_ssl:gen_csr(Subject, Key),

    Hosts = onepanel_cluster:nodes_to_hosts(service_op_worker:get_nodes()),
    Nodes = onepanel_cluster:hosts_to_nodes(Hosts),

    {ok, CaCert} = oz_providers:get_oz_cacert(provider),
    DefaultUrls = lists:map(fun({_, IpAddress}) ->
        IpAddress
    end, onepanel_rpc:call_all(Nodes, onepanel_utils, get_ip_address, [])),
    Params = [
        {<<"urls">>, maps:get(oneprovider_urls, Ctx, DefaultUrls)},
        {<<"csr">>, Csr},
        {<<"redirectionPoint">>,
            service_ctx:get(oneprovider_redirection_point, Ctx, binary)},
        {<<"name">>,
            service_ctx:get(oneprovider_name, Ctx, binary)},
        {<<"latitude">>,
            service_ctx:get(oneprovider_geo_latitude, Ctx, float, 0.0)},
        {<<"longitude">>,
            service_ctx:get(oneprovider_geo_longitude, Ctx, float, 0.0)}
    ],

    Validator = fun
        ({ok, ProviderId, Cert}) -> {ProviderId, Cert};
        ({error, Reason}) -> ?throw_error(Reason)
    end,
    {ProviderId, Cert} = onepanel_utils:wait_until(oz_providers, register,
        [provider, Params], {validator, Validator}, 10, timer:seconds(30)),

    lists:foreach(fun({File, Content}) ->
        onepanel_rpc:call_all(Nodes, onepanel_utils, save_file, [File, Content])
    end, [
        {oz_plugin:get_key_file(), Key},
        {oz_plugin:get_csr_file(), Csr},
        {oz_plugin:get_cert_file(), Cert},
        {filename:join(oz_plugin:get_cacerts_dir(), "ozp_cacert.pem"), CaCert},
        {filename:join(oz_plugin:get_provider_cacerts_dir(), "ozp_cacert.pem"), CaCert}
    ]),

    OpwNodes = onepanel_cluster:hosts_to_nodes(service_op_worker:name(), Hosts),
    onepanel_rpc:call_all(OpwNodes, application, set_env, [
        service_op_worker:name(), provider_id, ProviderId
    ]),

    rpc:multicall(Nodes, oz_endpoint, reset_oz_cacerts, []),
    rpc:multicall(OpwNodes, oz_endpoint, reset_oz_cacerts, []),

    service:update(name(), fun(#service{ctx = C} = S) ->
        S#service{ctx = C#{registered => true}}
    end),

    {ok, ProviderId}.


%%--------------------------------------------------------------------
%% @doc Unregisters provider in the zone.
%% @end
%%--------------------------------------------------------------------
-spec unregister(Ctx :: service:ctx()) -> ok | no_return().
unregister(#{hosts := Hosts}) ->
    Nodes = onepanel_cluster:hosts_to_nodes(Hosts),
    ok = oz_providers:unregister(provider),

    service:update(name(), fun(#service{ctx = C} = S) ->
        S#service{ctx = C#{registered => false}}
    end),

    lists:foreach(fun(File) ->
        onepanel_rpc:call_all(Nodes, file, delete, [File])
    end, [
        oz_plugin:get_key_file(),
        oz_plugin:get_csr_file(),
        oz_plugin:get_cert_file()
    ]).


%%--------------------------------------------------------------------
%% @doc Modifies configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Ctx :: service:ctx()) -> ok.
modify_details(Ctx) ->
    Params = onepanel_maps:get_store(oneprovider_name, Ctx, <<"name">>),
    Params2 = onepanel_maps:get_store(oneprovider_redirection_point, Ctx,
        <<"redirectionPoint">>, Params),
    Params3 = onepanel_maps:get_store(oneprovider_geo_latitude, Ctx,
        <<"latitude">>, Params2),
    Params4 = onepanel_maps:get_store(oneprovider_geo_longitude, Ctx,
        <<"longitude">>, Params3),
    ok = oz_providers:modify_details(provider, maps:to_list(Params4)).


%%--------------------------------------------------------------------
%% @doc Returns configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Ctx :: service:ctx()) -> list().
get_details(_Ctx) ->
    {ok, #provider_details{id = Id, name = Name, urls = Urls,
        redirection_point = RedirectionPoint, latitude = Latitude,
        longitude = Longitude}} = oz_providers:get_details(provider),
    [
        {id, Id}, {name, Name}, {redirectionPoint, RedirectionPoint},
        {urls, Urls}, {geoLatitude, onepanel_utils:convert(Latitude, float)},
        {geoLongitude, onepanel_utils:convert(Longitude, float)}
    ].


%%--------------------------------------------------------------------
%% @doc Creates or supports space with selected storage.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Ctx :: service:ctx()) -> list().
support_space(#{storage_id := StorageId, name := Name, node := Node} = Ctx) ->
    assert_storage_exists(Node, StorageId),
    {ok, SpaceId} = oz_providers:create_space(provider, [
        {<<"name">>, Name},
        {<<"size">>, onepanel_utils:typed_get(size, Ctx, binary)},
        {<<"token">>, onepanel_utils:typed_get(token, Ctx, binary)}
    ]),
    support_space(Ctx, SpaceId);

support_space(#{storage_id := StorageId, node := Node} = Ctx) ->
    assert_storage_exists(Node, StorageId),
    {ok, SpaceId} = oz_providers:support_space(provider, [
        {<<"size">>, onepanel_utils:typed_get(size, Ctx, binary)},
        {<<"token">>, onepanel_utils:typed_get(token, Ctx, binary)}
    ]),
    support_space(Ctx, SpaceId);

support_space(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    support_space(Ctx#{node => Node}).


%%--------------------------------------------------------------------
%% @doc Revokes support for the space given by ID.
%% @end
%%--------------------------------------------------------------------
-spec revoke_space_support(Ctx :: service:ctx()) -> ok.
revoke_space_support(#{id := SpaceId}) ->
    [Node | _] = service_op_worker:get_nodes(),
    ok = oz_providers:revoke_space_support(provider, SpaceId),
    ok = rpc:call(Node, space_storage, delete, [SpaceId]).


%%--------------------------------------------------------------------
%% @doc Returns list of spaces supported by the provider.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Ctx :: service:ctx()) -> list().
get_spaces(_Ctx) ->
    {ok, SpaceIds} = oz_providers:get_spaces(provider),
    [{ids, SpaceIds}].


%%--------------------------------------------------------------------
%% @doc Returns details of the space given by ID.
%% @end
%%--------------------------------------------------------------------
-spec get_space_details(Ctx :: service:ctx()) -> list().
get_space_details(#{id := SpaceId, node := Node}) ->
    {ok, #space_details{id = Id, name = Name, providers_supports = Providers}} =
        oz_providers:get_space_details(provider, SpaceId),
    StorageIds = op_worker_storage:get_supporting_storages(Node, SpaceId),
    StorageId = hd(StorageIds),
    MountInRoot = op_worker_storage:is_mounted_in_root(Node, SpaceId, StorageId),
    ImportDetails = op_worker_storage_sync:get_storage_import_details(
        Node, SpaceId, StorageId
    ),
    UpdateDetails = op_worker_storage_sync:get_storage_update_details(
        Node, SpaceId, StorageId
    ),
    [
        {id, Id},
        {name, Name},
        {supportingProviders, Providers},
        {storageId, StorageId},
        {localStorages, StorageIds},
        {mountInRoot, MountInRoot},
        {storageImport, ImportDetails},
        {storageUpdate, UpdateDetails}
    ];
get_space_details(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    get_space_details(Ctx#{node => Node}).


%%--------------------------------------------------------------------
%% @doc Modifies space details.
%% @end
%%--------------------------------------------------------------------
-spec modify_space(Ctx :: service:ctx()) -> list().
modify_space(#{space_id := SpaceId, node := Node} = Ctx) ->
    ImportArgs = maps:get(storage_import, Ctx, #{}),
    UpdateArgs = maps:get(storage_update, Ctx, #{}),
    op_worker_storage_sync:maybe_modify_storage_import(Node, SpaceId, ImportArgs),
    op_worker_storage_sync:maybe_modify_storage_update(Node, SpaceId, UpdateArgs),
    [{id, SpaceId}];
modify_space(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    modify_space(Ctx#{node => Node}).

%%--------------------------------------------------------------------
%% @doc Get storage_sync stats
%% @end
%%--------------------------------------------------------------------
-spec get_sync_stats(Ctx :: service:ctx()) -> list().
get_sync_stats(#{space_id := SpaceId, node := Node} = Ctx) ->
    Period = onepanel_utils:typed_get(period, Ctx, binary, undefined),
    MetricsJoined = onepanel_utils:typed_get(metrics, Ctx, binary, <<"">>),
    Metrics = binary:split(MetricsJoined, <<",">>, [global, trim]),
    op_worker_storage_sync:get_stats(Node, SpaceId, Period, Metrics);
get_sync_stats(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    get_sync_stats(Ctx#{node => Node}).

%%--------------------------------------------------------------------
%% @doc
%% Restarts secure listeners and SSL application.
%% @end
%%--------------------------------------------------------------------
-spec restart_listeners(Ctx :: service:ctx()) -> ok.
restart_listeners(_Ctx) ->
    Modules = [
        rest_listener,
        hackney,
        ssl
    ],
    lists:foreach(fun(Module) ->
        Module:stop()
    end, Modules),
    lists:foreach(fun(Module) ->
        Module:start()
    end, lists:reverse(Modules)).

%%--------------------------------------------------------------------
%% @doc
%% Restarts provider's secure listeners and SSL application.
%% @end
%%--------------------------------------------------------------------
-spec restart_provider_listeners(Ctx :: service:ctx()) -> ok.
restart_provider_listeners(_Ctx) ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    Modules = [
        gui_listener,
        rest_listener,
        provider_listener,
        protocol_listener,
        hackney,
        ssl
    ],
    lists:foreach(fun(Module) ->
        onepanel_rpc:call_all([Node], Module, stop, [])
    end, Modules),
    lists:foreach(fun(Module) ->
        onepanel_rpc:call_all([Node], Module, start, [])
    end, lists:reverse(Modules)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that storage with provided ID exists.
%% @end
%%--------------------------------------------------------------------
-spec assert_storage_exists(Node :: node(), StorageId :: binary()) ->
    ok | no_return().
assert_storage_exists(Node, StorageId) ->
    case rpc:call(Node, storage, exists, [StorageId]) of
        true -> ok;
        _ -> ?throw_error({?ERR_STORAGE_NOT_FOUND, StorageId})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Configures storage of a supported space.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Ctx :: service:ctx(), SpaceId :: binary()) -> list().
support_space(#{storage_id := StorageId, node := Node} = Ctx, SpaceId) ->
    MountInRoot = onepanel_utils:typed_get(mount_in_root, Ctx, boolean, false),
    ImportArgs = maps:get(storage_import, Ctx, #{}),
    UpdateArgs = maps:get(storage_update, Ctx, #{}),
    {ok, _} = rpc:call(Node, space_storage, add, [SpaceId, StorageId, MountInRoot]),
    op_worker_storage_sync:maybe_modify_storage_import(Node, SpaceId, ImportArgs),
    op_worker_storage_sync:maybe_modify_storage_update(Node, SpaceId, UpdateArgs),
    [{id, SpaceId}].