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
-include("modules/onepanel_dns.hrl").
-include("names.hrl").
-include("service.hrl").
-include("modules/models.hrl").
-include("deployment_progress.hrl").
-include_lib("ctool/include/oz/oz_spaces.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, check_oz_availability/1, mark_configured/1,
    register/1, unregister/1, is_registered/1, is_registered/0,
    modify_details/1, get_details/1, get_oz_domain/0,
    support_space/1, revoke_space_support/1, get_spaces/1, is_space_supported/1,
    get_space_details/1, modify_space/1, format_cluster_ips/1,
    get_sync_stats/1, get_auto_cleaning_reports/1, get_auto_cleaning_status/1,
    start_auto_cleaning/1, check_oz_connection/1, update_provider_ips/1,
    configure_files_popularity/1, configure_auto_cleaning/1,
    get_files_popularity_configuration/1, get_auto_cleaning_configuration/1]).
-export([pop_legacy_letsencrypt_config/0]).

-define(SERVICE_OPA, service_onepanel:name()).
-define(SERVICE_LE, service_letsencrypt:name()).
-define(SERVICE_CB, service_couchbase:name()).
-define(SERVICE_CM, service_cluster_manager:name()).
-define(SERVICE_CW, service_cluster_worker:name()).
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
    {ok, LeCtx} = onepanel_maps:get([cluster, ?SERVICE_LE], Ctx),
    StorageCtx = onepanel_maps:get([cluster, storages], Ctx, #{}),
    OpCtx = onepanel_maps:get(name(), Ctx, #{}),

    service:create(#service{name = name()}),
    % separate update to handle upgrade from older version
    service:update(name(), fun(#service{ctx = C} = S) ->
        S#service{ctx = C#{master_host => onepanel_cluster:node_to_host()}}
    end),

    AlreadyRegistered =
        case service:get(name()) of
            {ok, #service{ctx = #{registered := true}}} -> true;
            _ -> false
        end,

    Register = fun
        (#{oneprovider_register := true}) -> not(AlreadyRegistered);
        (_) -> false
    end,

    LeCtx2 =
        case AlreadyRegistered of
            % If provider is already registered the deployment request
            % should not override Let's Encrypt config

            % TODO remove this check when some time passes since introducing manage_restart
            true -> maps:remove(letsencrypt_enabled, LeCtx);
            _ -> LeCtx
        end,
    LeCtx3 = LeCtx2#{letsencrypt_plugin => ?SERVICE_OPW},

    SelfHost = onepanel_cluster:node_to_host(),

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
        Ss#steps{service = ?SERVICE_LE, action = deploy, ctx = LeCtx3},
        S#step{module = onepanel_deployment, function = mark_completed,
            args = [?PROGRESS_CLUSTER], hosts = [SelfHost]},
        Ss#steps{service = ?SERVICE_OPW, action = add_storages, ctx = StorageCtx},
        Ss#steps{action = register, ctx = OpCtx, condition = Register},
        Ss#steps{service = ?SERVICE_LE, action = update, ctx = LeCtx3},
        Ss#steps{service = ?SERVICE_OPA, action = add_users, ctx = OpaCtx},
        S#step{module = onepanel_deployment, function = mark_completed,
            args = [?PROGRESS_READY], hosts = [SelfHost]},
        S#step{function = mark_configured, ctx = OpaCtx, selection = any,
            condition = fun(Ctx) ->
                not maps:get(interactive_deployment, Ctx, true)
            end}
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

% returns any steps only on the master node
get_steps(manage_restart, Ctx) ->
    MasterHost = case service:get(name()) of
        {ok, #service{ctx = #{master_host := Master}}} -> Master;
        _ ->
            [FirstHost | _] = get_hosts(),
            ?info("No master host configured, defaulting to ~p", [FirstHost]),
            service:update(name(), fun(#service{ctx = C} = S) ->
                S#service{ctx = C#{master_host => FirstHost}}
            end),
            FirstHost
    end,

    case onepanel_cluster:node_to_host() == MasterHost of
        true -> [
            #steps{service = ?SERVICE_OPA, action = wait_for_cluster},
            #steps{action = stop},
            #steps{service = ?SERVICE_CB, action = resume},
            #steps{service = ?SERVICE_CM, action = resume},
            #steps{service = ?SERVICE_OPW, action = resume},
            #steps{service = ?SERVICE_LE, action = resume,
                ctx = Ctx#{letsencrypt_plugin => ?SERVICE_OPW}}
        ];
        false ->
            ?info("Waiting for master node \"~s\" to start", [MasterHost]),
            []
    end;

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
        #step{hosts = Hosts, function = check_oz_availability,
            attempts = onepanel_env:get(connect_to_onezone_attempts)},
        #step{hosts = Hosts, function = register, selection = any},
        % explicitely fail on connection problems before executing further steps
        #step{hosts = Hosts, function = check_oz_connection,
            attempts = onepanel_env:get(connect_to_onezone_attempts)},
        #steps{action = set_cluster_ips}
    ];
get_steps(register, Ctx) ->
    get_steps(register, Ctx#{hosts => service_op_worker:get_hosts()});

get_steps(unregister, #{hosts := Hosts} = Ctx) ->
    [
        #step{hosts = Hosts, function = unregister, selection = any, ctx = Ctx}
    ];

get_steps(unregister, Ctx) ->
    get_steps(unregister, Ctx#{hosts => service_op_worker:get_hosts()});

get_steps(modify_details, #{hosts := Hosts}) ->
    [
        #step{hosts = Hosts, function = modify_details, selection = any}
    ];
get_steps(modify_details, Ctx) ->
    get_steps(modify_details, Ctx#{hosts => service_op_worker:get_hosts()});


get_steps(set_cluster_ips, #{hosts := Hosts} = Ctx) ->
    GeneratedConfigFile = service_ctx:get(op_worker_generated_config_file, Ctx),
    Ctx2 = Ctx#{
        generated_config_file => GeneratedConfigFile,
        name => ?SERVICE_OPW
    },
    [
        #steps{action = set_cluster_ips, ctx = Ctx2, service = ?SERVICE_CW},
        #step{function = update_provider_ips, selection = any,
            hosts = Hosts}
    ];
get_steps(set_cluster_ips, Ctx) ->
    get_steps(set_cluster_ips, Ctx#{hosts => service_op_worker:get_hosts()});

get_steps(Action, Ctx) when
    Action =:= get_details;
    Action =:= support_space;
    Action =:= revoke_space_support;
    Action =:= get_spaces;
    Action =:= get_space_details;
    Action =:= modify_space;
    Action =:= get_auto_cleaning_reports;
    Action =:= get_auto_cleaning_status;
    Action =:= get_auto_cleaning_configuration;
    Action =:= get_files_popularity_configuration;
    Action =:= format_cluster_ips;
    Action =:= start_auto_cleaning;
    Action =:= get_sync_stats;
    Action =:= configure_files_popularity;
    Action =:= configure_auto_cleaning
    ->
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
    Node = onepanel_cluster:service_to_node(Name),
    GeneratedConfigFile = service_ctx:get(op_worker_generated_config_file, Ctx),
    rpc:call(Node, application, set_env, [Name, oz_domain, OzDomain]),
    onepanel_env:write([Name, oz_domain], OzDomain, GeneratedConfigFile).


%%--------------------------------------------------------------------
%% @doc Checks if onezone is available at given address
%% @end
%%--------------------------------------------------------------------
-spec check_oz_availability(Ctx :: service:ctx()) -> ok | no_return().
check_oz_availability(Ctx) ->
    Protocol = service_ctx:get(oz_worker_nagios_protocol, Ctx),
    Port = service_ctx:get(oz_worker_nagios_port, Ctx, integer),
    OzDomain = service_ctx:get(onezone_domain, Ctx),
    Url = onepanel_utils:join([Protocol, "://", OzDomain, ":", Port, "/nagios"]),
    Opts = case Protocol of
        "https" ->
            CaCerts = cert_utils:load_ders_in_dir(oz_plugin:get_cacerts_dir()),
            [{ssl_options, [{secure, only_verify_peercert}, {cacerts, CaCerts}]}];
        _ ->
            []
    end,

    case http_client:get(Url, #{}, <<>>, Opts) of
        {ok, 200, _Headers, Body} ->
            {Xml, _} = xmerl_scan:string(onepanel_utils:convert(Body, list)),
            [Status] = [X#xmlAttribute.value || X <- Xml#xmlElement.attributes,
                X#xmlAttribute.name == status],
            case Status of
                "ok" -> ok;
                _ -> ?throw_error(?ERR_ONEZONE_NOT_AVAILABLE)
            end;
        _ ->
            ?throw_error(?ERR_ONEZONE_NOT_AVAILABLE)
    end.


%%--------------------------------------------------------------------
%% @doc Checks if oneprovider is registered and connected to onezone
%% @end
%%--------------------------------------------------------------------
-spec check_oz_connection(Ctx :: service:ctx()) -> ok | no_return().
check_oz_connection(#{node := Node}) ->
    case rpc:call(Node, oneprovider, is_connected_to_oz, []) of
        true -> ok;
        false -> ?throw_error(?ERR_ONEZONE_NOT_AVAILABLE)
    end;
check_oz_connection(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    check_oz_connection(Ctx#{node => Node}).


%%--------------------------------------------------------------------
%% @doc Registers provider in the zone.
%% @end
%%--------------------------------------------------------------------
-spec register(Ctx :: service:ctx()) ->
    {ok, ProviderId :: binary()} | no_return().
register(Ctx) ->
    [OpwNode | _] = service_op_worker:get_nodes(),

    DomainParams = case service_ctx:get(oneprovider_subdomain_delegation, Ctx, boolean, false) of
        true ->
            [{<<"subdomainDelegation">>, true},
                {<<"subdomain">>,
                    service_ctx:get(oneprovider_subdomain, Ctx, binary)},
                {<<"ipList">>, []}]; % IPs will be updated in the step set_cluster_ips
        false ->
            % without subdomain delegtion, Let's Encrypt does not have to be configured
            onepanel_deployment:mark_completed(?PROGRESS_LETSENCRYPT_CONFIG),
            [{<<"subdomainDelegation">>, false},
                {<<"domain">>, service_ctx:get(oneprovider_domain, Ctx, binary)}]
    end,

    Params = [
        {<<"name">>,
            service_ctx:get(oneprovider_name, Ctx, binary)},
        {<<"adminEmail">>,
            service_ctx:get(oneprovider_admin_email, Ctx, binary)},
        {<<"latitude">>,
            service_ctx:get(oneprovider_geo_latitude, Ctx, float, 0.0)},
        {<<"longitude">>,
            service_ctx:get(oneprovider_geo_longitude, Ctx, float, 0.0)}
        | DomainParams
    ],

    Validator = fun
        ({ok, ProviderId, Macaroon}) -> {ProviderId, Macaroon};

        % Return the subdomain_reserved error instead of throwing as it would cause
        % wait_until to repeat attempts. As the error condition will not change
        % it's not necessary
        ({error, subdomain_reserved}) -> {error, subdomain_reserved};

        ({error, Reason}) -> ?throw_error(Reason)
    end,

    case onepanel_utils:wait_until(oz_providers, register, [none, Params],
        {validator, Validator}, 10, timer:seconds(30)) of
        {error, subdomain_reserved} ->
            ?throw_error(?ERR_SUBDOMAIN_NOT_AVAILABLE);
        {ProviderId, Macaroon} ->
            rpc:call(OpwNode, provider_auth, save, [ProviderId, Macaroon]),
            % Force connection healthcheck
            % (reconnect attempt is performed automatically if there is no connection)
            rpc:call(OpwNode, oneprovider, force_oz_connection_start, []),

            service:update(name(), fun(#service{ctx = C} = S) ->
                S#service{ctx = C#{registered => true}}
            end),

            {ok, ProviderId}
    end.

%%--------------------------------------------------------------------
%% @doc Unregisters provider in the zone.
%% @end
%%--------------------------------------------------------------------
-spec unregister(Ctx :: service:ctx()) -> ok | no_return().
unregister(#{node := Node}) ->
    ok = oz_providers:unregister(provider),
    rpc:call(Node, provider_auth, delete, []),

    onepanel_deployment:mark_not_completed(?PROGRESS_LETSENCRYPT_CONFIG),
    service:update(name(), fun(#service{ctx = C} = S) ->
        S#service{ctx = C#{registered => false}}
    end);
unregister(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    ?MODULE:unregister(Ctx#{node => Node}).


%%--------------------------------------------------------------------
%% @doc Returns if this provider is registered in OneZone.
%% @end
%%--------------------------------------------------------------------
-spec is_registered(Ctx :: service:ctx()) -> boolean().
is_registered(#{node := Node}) ->
    try rpc:call(Node, oneprovider, is_registered, []) of
        true -> true;
        falses -> false;
        _ ->
            case service:get(name()) of
                {ok, #service{ctx = C}} -> maps:get(registered, C, false)
            end
    catch _:_ ->
        false
    end;

is_registered(Ctx) ->
    try
        [_ | _] = service_op_worker:get_nodes()
    of Nodes -> is_registered(Ctx#{node => hd(Nodes)})
    catch _:_ ->
        false
    end.

is_registered() ->
    is_registered(#{}).

%%--------------------------------------------------------------------
%% @doc Modifies configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Ctx :: service:ctx()) -> ok | no_return().
modify_details(#{node := Node} = Ctx) ->
    ok = modify_domain_details(Ctx),

    Params = onepanel_maps:get_store_multiple([
        {oneprovider_name, <<"name">>},
        {oneprovider_geo_latitude, <<"latitude">>},
        {oneprovider_geo_longitude, <<"longitude">>},
        {oneprovider_admin_email, <<"adminEmail">>}
    ], Ctx),

    case maps:size(Params) of
        0 -> ok;
        _ -> ok = rpc:call(Node, provider_logic, update, [Params])
    end;

modify_details(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    modify_details(Ctx#{node => Node}).


%%--------------------------------------------------------------------
%% @doc Returns configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Ctx :: service:ctx()) -> #{atom() := term()} | no_return().
get_details(#{node := Node}) ->
    % Graph Sync connection is needed to obtain details
    rpc:call(Node, oneprovider, force_oz_connection_start, []),
    OzDomain = get_oz_domain(),

    Details = case rpc:call(Node, provider_logic, get_as_map, []) of
        {ok, DetailsMap} -> DetailsMap;
        ?ERROR_NO_CONNECTION_TO_OZ -> ?throw_error(?ERR_ONEZONE_NOT_AVAILABLE);
        {error, Reason} -> ?throw_error(Reason)
    end,

    #{latitude := Latitude, longitude := Longitude} = Details,

    Response = onepanel_maps:get_store_multiple([
        {id, id}, {name, name}, {admin_email, adminEmail},
        {subdomain_delegation, subdomainDelegation}, {domain, domain}
    ], Details),

    Response2 = Response#{
        geoLatitude => onepanel_utils:convert(Latitude, float),
        geoLongitude => onepanel_utils:convert(Longitude, float),
        onezoneDomainName => onepanel_utils:convert(OzDomain, binary)
    },

    case maps:get(subdomain_delegation, Details) of
        true -> Response2#{subdomain => maps:get(subdomain, Details)};
        _ -> Response2
    end;

get_details(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    get_details(Ctx#{node => Node}).


%%--------------------------------------------------------------------
%% @doc Returns the onezone domain.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_domain() -> string().
get_oz_domain() ->
    {ok, {_, _, OzDomain, _, _, _}} = http_uri:parse(oz_plugin:get_oz_url()),
    OzDomain.


%%--------------------------------------------------------------------
%% @doc Returns IPs of hosts with op_worker instances.
%% @end
%%--------------------------------------------------------------------
-spec format_cluster_ips(service:ctx()) ->
    #{isConfigured := boolean(), hosts := #{binary() => binary()}}.
format_cluster_ips(Ctx) ->
    service_cluster_worker:get_cluster_ips(Ctx#{name => ?SERVICE_OPW}).


%%--------------------------------------------------------------------
%% @doc Supports space with selected storage.
%% @end
%%--------------------------------------------------------------------
support_space(#{storage_id := StorageId, node := Node} = Ctx) ->
    assert_storage_exists(Node, StorageId),
    SupportSize = onepanel_utils:typed_get(size, Ctx, binary),
    Token = onepanel_utils:typed_get(token, Ctx, binary),

    {ok, SpaceId} = case rpc:call(
        Node, provider_logic, support_space, [Token, SupportSize]
    ) of
        {ok, Id} -> {ok, Id};
        ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, Minimum) ->
            ?throw_error(?ERR_SPACE_SUPPORT_TOO_LOW(Minimum));
        {error, Reason} ->
            ?throw_error(Reason)
    end,

    configure_space(Ctx, SpaceId);

support_space(#{storage_id := _} = Ctx) ->
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
%% @doc Calls op_worker to check if a space is supported.
%% @end
%%--------------------------------------------------------------------
-spec is_space_supported(Ctx :: service:ctx()) -> boolean().
is_space_supported(#{space_id := Id}) ->
    Node = utils:random_element(service_op_worker:get_nodes()),
    case rpc:call(Node, provider_logic, supports_space, [Id]) of
        Result when is_boolean(Result) -> Result
    end.


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
    CurrentSize = rpc:call(Node, space_quota, current_size, [SpaceId]),
    [
        {id, Id},
        {name, Name},
        {supportingProviders, Providers},
        {storageId, StorageId},
        {localStorages, StorageIds},
        {mountInRoot, MountInRoot},
        {storageImport, ImportDetails},
        {storageUpdate, UpdateDetails},
        {spaceOccupancy, CurrentSize}
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
    ok = maybe_update_support_size(Ctx),
    {ok, _} = op_worker_storage_sync:maybe_modify_storage_import(Node, SpaceId, ImportArgs),
    {ok, _} = op_worker_storage_sync:maybe_modify_storage_update(Node, SpaceId, UpdateArgs),
    [{id, SpaceId}];
modify_space(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    modify_space(Ctx#{node => Node}).


%%--------------------------------------------------------------------
%% @doc If new size for a space is specified, enacts the change.
%% @end
%%--------------------------------------------------------------------
maybe_update_support_size(#{node := Node, space_id := SpaceId, size := SupportSize}) ->
    case rpc:call(Node, provider_logic, update_space_support_size, [SpaceId, SupportSize]) of
        ok -> ok;
        ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, Minimum) ->
            ?throw_error(?ERR_SPACE_SUPPORT_TOO_LOW(Minimum));
        {error, Reason} ->
            ?throw_error(Reason)
    end;

maybe_update_support_size(_Ctx) -> ok.


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
%% Notify onezone about IPs change.
%% @end
%%--------------------------------------------------------------------
-spec update_provider_ips(service:ctx()) -> ok.
update_provider_ips(#{node := Node} = Ctx) ->
    case is_registered(Ctx) of
        true ->
            % no check of results - if the provider is not connected to onezone
            % the IPs will be sent automatically after connection is acquired
            rpc:call(Node, provider_logic, update_subdomain_delegation_ips, []),
            ok;
        false -> ok
    end;
update_provider_ips(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    update_provider_ips(Ctx#{node => Node}).


%%-------------------------------------------------------------------
%% @doc
%% Returns list of autocleaning reports started since Since date.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_reports(Ctx :: service:ctx()) -> proplists:proplist().
get_auto_cleaning_reports(Ctx = #{space_id := SpaceId, node := Node}) ->
    Since = onepanel_utils:typed_get(started_after, Ctx, binary),
    Reports = rpc:call(Node, autocleaning_api, list_reports_since, [SpaceId, Since]),
    Entries = lists:map(fun(Report) ->
        onepanel_lists:map_undefined_to_null(
            onepanel_maps:to_list(
                onepanel_maps:get_store_multiple([
                    {started_at, startedAt},
                    {stopped_at, stoppedAt},
                    {released_bytes, releasedBytes},
                    {bytes_to_release, bytesToRelease},
                    {files_number, filesNumber}
                ], Report)))
    end, Reports),
    [{reportEntries, Entries}];
get_auto_cleaning_reports(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    get_auto_cleaning_reports(Ctx#{node => Node}).

%%-------------------------------------------------------------------
%% @doc
%% Returns status of current working autocleaning process for given space.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_status(Ctx :: service:ctx()) -> proplists:proplist().
get_auto_cleaning_status(#{space_id := SpaceId, node := Node}) ->
    Status = rpc:call(Node, autocleaning_api, status, [SpaceId]),
    onepanel_maps:to_list(onepanel_maps:get_store_multiple([
        {in_progress, inProgress},
        {space_occupancy, spaceOccupancy}
    ], Status));
get_auto_cleaning_status(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    get_auto_cleaning_status(Ctx#{node => Node}).


%%-------------------------------------------------------------------
%% @doc
%% Returns configuration of auto-cleaning mechanism in given space.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_configuration(Ctx :: service:ctx()) -> proplists:proplist().
get_auto_cleaning_configuration(#{space_id := SpaceId, node := Node}) ->
    op_worker_storage:get_auto_cleaning_configuration(Node, SpaceId);
get_auto_cleaning_configuration(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    get_auto_cleaning_configuration(Ctx#{node => Node}).


%%-------------------------------------------------------------------
%% @doc
%% Returns configuration of files-popularity mechanism in given space.
%% @end
%%-------------------------------------------------------------------
-spec get_files_popularity_configuration(Ctx :: service:ctx()) -> proplists:proplist().
get_files_popularity_configuration(#{space_id := SpaceId, node := Node}) ->
    op_worker_storage:get_files_popularity_configuration(Node, SpaceId);
get_files_popularity_configuration(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    get_files_popularity_configuration(Ctx#{node => Node}).


%%-------------------------------------------------------------------
%% @doc
%% Manually starts auto-cleaning of given space.
%% @end
%%-------------------------------------------------------------------
-spec start_auto_cleaning(Ctx :: service:ctx()) -> ok.
start_auto_cleaning(#{space_id := SpaceId, node := Node}) ->
    case rpc:call(Node, autocleaning_api, force_start, [SpaceId]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, nothing_to_clean} -> ok;
        {error, Reason} ->
            ?throw_error({?ERR_AUTOCLEANING, Reason})
    end;
start_auto_cleaning(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    start_auto_cleaning(Ctx#{node => Node}).


%%-------------------------------------------------------------------
%% @doc
%% Marks all configuration steps as already performed.
%% @end
%%-------------------------------------------------------------------
-spec mark_configured(service:ctx()) -> ok.
mark_configured(_Ctx) ->
    onepanel_deployment:mark_completed([
        ?PROGRESS_LETSENCRYPT_CONFIG,
        ?PROGRESS_CLUSTER_IPS,
        ?DNS_CHECK_ACKNOWLEDGED
    ]).

%%-------------------------------------------------------------------
%% @doc
%% Removes legacy way of storing letsencrypt configuration and
%% returns its value.
%% @end
%%-------------------------------------------------------------------
-spec pop_legacy_letsencrypt_config() -> boolean().
pop_legacy_letsencrypt_config() ->
    Result = case service:get(name()) of
        {ok, #service{ctx = #{has_letsencrypt_cert:= Enabled}}} ->
            % upgrade from versions 18.02.0-beta5 and older
            service:update(name(), fun(#service{ctx = C} = S) ->
                S#service{ctx = maps:remove(has_letsencrypt_cert, C)}
            end),
            Enabled;
        {ok, #service{ctx = #{configured := GbSet}}} ->
            case gb_sets:is_set(GbSet) of
                true ->
                    % upgrade from versions 18.02.0-beta6..18.02.0-rc1
                    service:update(name(), fun(#service{ctx = C} = S) ->
                        S#service{
                            ctx = C#{configured => gb_sets:del_element(letsencrypt, GbSet)}
                        }
                    end),
                    gb_sets:is_member(letsencrypt, GbSet);
                false -> false
            end;
        _ -> false
    end,
    case Result of
        true ->
            onepanel_deployment:mark_completed(?PROGRESS_LETSENCRYPT_CONFIG),
            Result;
        _ -> Result
    end.

%%-------------------------------------------------------------------
%% @doc
%% Configures file-popularity mechanism
%% @end
%%-------------------------------------------------------------------
-spec configure_files_popularity(Ctx :: service:ctx()) -> list().
configure_files_popularity(#{space_id := SpaceId, node := Node} = Ctx) ->
    ok = op_worker_storage:maybe_update_file_popularity(Node, SpaceId, Ctx),
    [{id, SpaceId}];
configure_files_popularity(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    configure_files_popularity(Ctx#{node => Node}).

%%-------------------------------------------------------------------
%% @doc
%% Configures auto-cleaning mechanism
%% @end
%%-------------------------------------------------------------------
-spec configure_auto_cleaning(Ctx :: service:ctx()) -> list().
configure_auto_cleaning(#{space_id := SpaceId, node := Node} = Ctx) ->
    ok = op_worker_storage:maybe_update_auto_cleaning(Node, SpaceId, Ctx),
    [{id, SpaceId}];
configure_auto_cleaning(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    configure_auto_cleaning(Ctx#{node => Node}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Modify provider details regarding its domain or subdomain.
%% @end
%%--------------------------------------------------------------------
modify_domain_details(#{oneprovider_subdomain_delegation := true, node := Node} = Ctx) ->
    Subdomain = onepanel_utils:typed_get(oneprovider_subdomain, Ctx, binary),

    case rpc:call(Node, provider_logic, is_subdomain_delegated, []) of
        {true, Subdomain} -> ok; % no change
        _ ->
            case rpc:call(Node, provider_logic, set_delegated_subdomain, [Subdomain]) of
                ok ->
                    dns_check:invalidate_cache(op_worker);
                ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>) ->
                    ?throw_error(?ERR_SUBDOMAIN_NOT_AVAILABLE)
            end
    end;

modify_domain_details(#{oneprovider_subdomain_delegation := false, node := Node} = Ctx) ->
    Domain = onepanel_utils:typed_get(oneprovider_domain, Ctx, binary),
    ok = rpc:call(Node, provider_logic, set_domain, [Domain]),
    dns_check:invalidate_cache(op_worker);

modify_domain_details(#{node := _Node}) ->
    ok.


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
-spec configure_space(Ctx :: service:ctx(), SpaceId :: binary()) -> list().
configure_space(#{storage_id := StorageId, node := Node} = Ctx, SpaceId) ->
    MountInRoot = onepanel_utils:typed_get(mount_in_root, Ctx, boolean, false),
    ImportArgs = maps:get(storage_import, Ctx, #{}),
    UpdateArgs = maps:get(storage_update, Ctx, #{}),
    {ok, _} = rpc:call(Node, space_storage, add, [SpaceId, StorageId, MountInRoot]),
    op_worker_storage_sync:maybe_modify_storage_import(Node, SpaceId, ImportArgs),
    op_worker_storage_sync:maybe_modify_storage_update(Node, SpaceId, UpdateArgs),
    [{id, SpaceId}].
