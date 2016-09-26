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
    support_space/1, revoke_space_support/1, get_spaces/1, get_space_details/1]).

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

    Register = fun
        (#{oneprovider_register := true}) -> true;
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
        S#step{module = service, function = save, ctx = OpaCtx,
            args = [#service{name = name()}], selection = first},
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
        #step{hosts = service_onepanel:get_hosts(), function = configure,
            ctx = Ctx#{application => ?APP_NAME}},
        #step{hosts = Hosts, function = register, selection = any,
            attempts = onepanel_env:get(oneprovider_register_attempts)}
    ];

get_steps(register, Ctx) ->
    get_steps(register, Ctx#{hosts => service_op_worker:get_hosts()});

get_steps(unregister, #{hosts := Hosts} = Ctx) ->
    [
        #step{hosts = Hosts, module = service, function = delete,
            args = [name()], selection = any},
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
    Action =:= get_space_details ->
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
configure(#{onezone_domain := OzDomain, onezone_verify_cert := OzVerifyCert,
    application := ?APP_NAME}) ->
    lists:foreach(fun({AppName, Key, Value}) ->
        application:set_env(AppName, Key, Value),
        onepanel_env:write([AppName, Key], Value)
    end, [
        {?APP_NAME, onezone_domain, OzDomain},
        {ctool, verify_oz_cert, OzVerifyCert}
    ]);

configure(#{onezone_domain := OzDomain, onezone_verify_cert := OzVerifyCert} = Ctx) ->
    Name = service_op_worker:name(),
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(Name, Host),
    AppConfigPath = service_ctx:get(op_worker_app_config_path, Ctx),

    lists:foreach(fun({AppName, Key, Value}) ->
        rpc:call(Node, application, set_env, [AppName, Key, Value]),
        onepanel_env:write([AppName, Key], Value, AppConfigPath)
    end, [
        {Name, oz_domain, OzDomain},
        {Name, verify_oz_cert, OzVerifyCert},
        {ctool, verify_oz_cert, OzVerifyCert}
    ]);

configure(Ctx) ->
    OzDomain = service_ctx:get(onezone_domain, Ctx),
    OzVerifyCert = service_ctx:get(onezone_verify_cert, Ctx, boolean,
        onepanel_env:get(verify_oz_cert, ctool)),
    configure(Ctx#{onezone_domain => OzDomain, onezone_verify_cert => OzVerifyCert}).


%%--------------------------------------------------------------------
%% @doc Registers provider in the zone.
%% @end
%%--------------------------------------------------------------------
-spec register(Ctx :: service:ctx()) ->
    {ok, ProviderId :: binary()} | no_return().
register(Ctx) ->
    Password = service_ctx:get(oneprovider_key_password, Ctx),
    {ok, Key, Csr} = service_oneprovider_nif:create_csr(Password),

    Hosts = onepanel_cluster:nodes_to_hosts(service_op_worker:get_nodes()),
    Nodes = onepanel_cluster:hosts_to_nodes(Hosts),

    DefaultUrls = lists:map(fun({_, IpAddress}) ->
        IpAddress
    end, onepanel_rpc:call_all(Nodes, onepanel_utils, get_ip_address, [])),
    Params = [
        {<<"urls">>, maps:get(oneprovider_urls, Ctx, DefaultUrls)},
        {<<"csr">>, Csr},
        {<<"redirectionPoint">>,
            service_ctx:get(oneprovider_redirection_point, Ctx, binary)},
        {<<"clientName">>,
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

    lists:foreach(fun({Path, Content}) ->
        onepanel_rpc:call_all(Nodes, onepanel_utils, save_file, [Path, Content])
    end, [
        {oz_plugin:get_key_path(), Key},
        {oz_plugin:get_csr_path(), Csr},
        {oz_plugin:get_cert_path(), Cert}
    ]),

    {ok, ProviderId}.


%%--------------------------------------------------------------------
%% @doc Unregisters provider in the zone.
%% @end
%%--------------------------------------------------------------------
-spec unregister(Ctx :: service:ctx()) -> ok | no_return().
unregister(#{hosts := Hosts}) ->
    Nodes = onepanel_cluster:hosts_to_nodes(Hosts),
    ok = oz_providers:unregister(provider),

    lists:foreach(fun(Path) ->
        onepanel_rpc:call_all(Nodes, file, delete, [Path])
    end, [
        oz_plugin:get_key_path(),
        oz_plugin:get_csr_path(),
        oz_plugin:get_cert_path()
    ]).


%%--------------------------------------------------------------------
%% @doc Modifies configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Ctx :: service:ctx()) -> ok.
modify_details(Ctx) ->
    Params = onepanel_maps:get_store(oneprovider_name, Ctx, <<"clientName">>),
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
support_space(#{storage_id := StorageId, name := Name, size := Size,
    token := Token, node := Node}) ->
    {ok, SpaceId} = oz_providers:create_space(provider, [{<<"name">>, Name},
        {<<"size">>, onepanel_utils:convert(Size, binary)}, {<<"token">>, Token}]),
    {ok, _} = rpc:call(Node, space_storage, add, [SpaceId, StorageId]),
    [{id, SpaceId}];

support_space(#{storage_id := StorageId, size := Size, token := Token,
    node := Node}) ->
    {ok, SpaceId} = oz_providers:support_space(provider, [{<<"token">>, Token},
        {<<"size">>, onepanel_utils:convert(Size, binary)}]),
    {ok, _} = rpc:call(Node, space_storage, add, [SpaceId, StorageId]),
    [{id, SpaceId}];

support_space(#{storage_name := Name, node := _} = Ctx) ->
    [{Name, Props}] = op_worker_storage:get(Name),
    {id, Id} = lists:keyfind(id, 1, Props),
    support_space(Ctx#{storage_id => Id});

support_space(Ctx) ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    support_space(Ctx#{node => Node}).


%%--------------------------------------------------------------------
%% @doc Revokes support for the space given by ID.
%% @end
%%--------------------------------------------------------------------
-spec revoke_space_support(Ctx :: service:ctx()) -> ok.
revoke_space_support(#{id := SpaceId}) ->
    ok = oz_providers:revoke_space_support(provider, SpaceId).


%%--------------------------------------------------------------------
%% @doc Returns list of spaces supported by the provider.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Ctx :: service:ctx()) -> list().
get_spaces(_Ctx) ->
    {ok, SpaceIds} = oz_providers:get_spaces(provider),
    SpaceIds.


%%--------------------------------------------------------------------
%% @doc Returns details of the space given by ID.
%% @end
%%--------------------------------------------------------------------
-spec get_space_details(Ctx :: service:ctx()) -> list().
get_space_details(#{id := SpaceId}) ->
    {ok, #space_details{id = Id, name = Name, providers_supports = Providers}} =
        oz_providers:get_space_details(provider, SpaceId),
    [{id, Id}, {name, Name}, {supportingProviders, Providers}].
