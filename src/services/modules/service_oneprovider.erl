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
-include("modules/models.hrl").
-include("deployment_progress.hrl").
-include_lib("ctool/include/oz/oz_spaces.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, check_oz_availability/1,
    register/1, unregister/1, is_registered/1,
    modify_details/1, get_details/1, get_oz_domain/0,
    support_space/1, revoke_space_support/1, get_spaces/1,
    get_space_details/1, modify_space/1, get_cluster_ips/1,
    get_sync_stats/1, get_autocleaning_reports/1, get_autocleaning_status/1,
    start_cleaning/1, check_oz_connection/1, update_provider_ips/1]).
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

    % TODO VFS-4140 Proper detection of batch config
    case Register(OpCtx) of
        true -> onepanel_deployment:mark_completed(?PROGRESS_LETSENCRYPT_CONFIG);
        _ -> ok
    end,

    LeCtx2 =
        case AlreadyRegistered of
            % If provider is already registered the deployment request
            % should not override Let's Encrypt config
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
            args = [?PROGRESS_READY], hosts = [SelfHost]}
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
get_steps(manage_restart, _Ctx) ->
    MasterHost = case service:get(name()) of
        {ok, #service{ctx = #{master_host := Master}}} -> Master;
        {ok, #service{hosts = [FirstHost | _]}} ->
            ?info("No master host configured, defaulting to \"~s\"", [FirstHost]),
            FirstHost;
        _ -> undefined
    end,

    case onepanel_cluster:node_to_host() == MasterHost of
        true -> [
            #steps{service = ?SERVICE_OPA, action = wait_for_cluster},
            #steps{action = stop},
            #steps{service = ?SERVICE_CB, action = resume},
            #steps{service = ?SERVICE_CM, action = resume},
            #steps{service = ?SERVICE_OPW, action = resume},
            #steps{service = ?SERVICE_LE, action = update}
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
        #step{hosts = Hosts, function = unregister, selection = any, ctx = Ctx},
        #steps{service = ?SERVICE_LE, action = update}
    ];

get_steps(unregister, Ctx) ->
    get_steps(unregister, Ctx#{hosts => service_op_worker:get_hosts()});

get_steps(modify_details, #{hosts := Hosts}) ->
    [
        #step{hosts = Hosts, function = modify_details, selection = any},
        #steps{service = ?SERVICE_LE, action = update}
    ];
get_steps(modify_details, Ctx) ->
    get_steps(modify_details, Ctx#{hosts => service_op_worker:get_hosts()});


get_steps(set_cluster_ips, #{hosts := Hosts} = Ctx) ->
    AppConfigFile = service_ctx:get(op_worker_app_config_file, Ctx),
    Ctx2 = Ctx#{
        app_config_file => AppConfigFile,
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
    Action =:= get_autocleaning_reports;
    Action =:= get_autocleaning_status;
    Action =:= get_cluster_ips;
    Action =:= start_cleaning;
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
    [OpwNode|_] = service_op_worker:get_nodes(),

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
        _ -> false
    catch _:_ ->
        false
    end;
is_registered(Ctx) ->
    try
        [Node | _] = service_op_worker:get_nodes(),
        is_registered(Ctx#{node => Node})
    catch _:_ ->
        false
    end.


%%--------------------------------------------------------------------
%% @doc Modifies configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Ctx :: service:ctx()) -> ok | no_return().
modify_details(#{node := Node} = Ctx) ->
    % If provider domain is modified it is done via rpc call to oneprovider
    case onepanel_maps:get(oneprovider_subdomain_delegation, Ctx, undefined) of
        true ->
            Subdomain = onepanel_utils:typed_get(oneprovider_subdomain, Ctx, binary),
            % check current subdomain
            case rpc:call(Node, provider_logic, is_subdomain_delegated, []) of
                {true, Subdomain} -> ok; % no change
                _ ->
                    case rpc:call(Node, provider_logic, set_delegated_subdomain, [Subdomain]) of
                        ok -> ok;
                        {error, subdomain_exists} ->
                            ?throw_error(?ERR_SUBDOMAIN_NOT_AVAILABLE)
                    end
            end;
        false ->
            Domain = onepanel_utils:typed_get(oneprovider_domain, Ctx, binary),
            ok = rpc:call(Node, provider_logic, set_domain, [Domain]);
        undefined -> ok
    end,

    Params = onepanel_maps:get_store(oneprovider_name, Ctx, <<"name">>, #{}),
    Params2 = onepanel_maps:get_store(oneprovider_geo_latitude, Ctx,
        <<"latitude">>, Params),
    Params3 = onepanel_maps:get_store(oneprovider_geo_longitude, Ctx,
        <<"longitude">>, Params2),
    Params4 = onepanel_maps:get_store(oneprovider_admin_email, Ctx,
        <<"adminEmail">>, Params3),

    case maps:is_key(letsencrypt_enabled, Ctx) of
        true -> onepanel_deployment:mark_completed(?PROGRESS_LETSENCRYPT_CONFIG);
        _ -> ok
    end,

    case maps:size(Params4) of
        0 -> ok;
        _ -> ok = oz_providers:modify_details(provider, maps:to_list(Params4))
    end;
modify_details(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    modify_details(Ctx#{node => Node}).


%%--------------------------------------------------------------------
%% @doc Returns configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Ctx :: service:ctx()) -> list() | no_return().
get_details(#{node := Node}) ->
    % Graph Sync connection is needed to obtain details
    rpc:call(Node, oneprovider, force_oz_connection_start, []),
    OzDomain = get_oz_domain(),
    #{
        id := Id,
        name := Name,
        admin_email := AdminEmail,
        subdomain_delegation := SubdomainDelegation,
        domain := Domain,
        subdomain := Subdomain,
        longitude := Longitude,
        latitude := Latitude
    } = rpc:call(Node, provider_logic, get_as_map, []),

    Details = [
        {id, Id}, {name, Name},
        {subdomainDelegation, SubdomainDelegation}, {domain, Domain},
        {adminEmail, AdminEmail},
        {geoLatitude, onepanel_utils:convert(Latitude, float)},
        {geoLongitude, onepanel_utils:convert(Longitude, float)},
        {onezoneDomainName, onepanel_utils:convert(OzDomain, binary)}
    ],

    Details2 = case onepanel_deployment:is_completed(?PROGRESS_LETSENCRYPT_CONFIG) of
        true ->
            [{letsEncryptEnabled, service_letsencrypt:is_enabled()} | Details];
        false ->
            % do not send letsEncryptEnabled field
            % in order to prompt GUI to display certificate configuration panel
            Details
    end,

    case SubdomainDelegation of
        true -> [{subdomain, Subdomain} | Details2];
        _ -> Details2
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
-spec get_cluster_ips(service:ctx()) ->
    #{isConfigured := boolean(), hosts := #{binary() => binary()}}.
get_cluster_ips(Ctx) ->
    service_cluster_worker:get_cluster_ips(Ctx#{name => ?SERVICE_OPW}).


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
    SoftQuota = op_worker_storage:get_soft_quota(Node),
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
        {softQuota, SoftQuota},
        {storageImport, ImportDetails},
        {storageUpdate, UpdateDetails},
        {filesPopularity, op_worker_storage:get_file_popularity_details(Node, SpaceId)},
        {autoCleaning, op_worker_storage:get_autocleaning_details(Node, SpaceId)},
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
    FilePopularityArgs = maps:get(files_popularity, Ctx, #{}),
    AutoCleaningArgs = maps:get(auto_cleaning, Ctx, #{}),
    {ok, _} = op_worker_storage_sync:maybe_modify_storage_import(Node, SpaceId, ImportArgs),
    {ok, _} = op_worker_storage_sync:maybe_modify_storage_update(Node, SpaceId, UpdateArgs),
    {ok, _} = op_worker_storage:maybe_update_file_popularity(Node, SpaceId, FilePopularityArgs),
    {ok, _} = op_worker_storage:maybe_update_autocleaning(Node, SpaceId, AutoCleaningArgs),
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
%% Notify onezone about IPs change.
%% @end
%%--------------------------------------------------------------------
-spec update_provider_ips(service:ctx()) -> ok.
update_provider_ips(#{node := Node} = Ctx) ->
    case is_registered(Ctx) of
        true ->
            % no check of results - if the provider is not connected to onezone
            % the IPs will be sent automatically after connection is acquired
            rpc:call(Node, provider_logic, update_subdomain_delegation_ips, []);
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
-spec get_autocleaning_reports(Ctx :: service:ctx()) -> proplists:proplist().
get_autocleaning_reports(Ctx = #{space_id := SpaceId, node := Node}) ->
    Since = onepanel_utils:typed_get(started_after, Ctx, binary),
    Reports = rpc:call(Node, space_cleanup_api, list_reports_since, [SpaceId, Since]),
    Entries = lists:map(fun(Report) ->
        onepanel_lists:map_undefined_to_null(Report)
    end, Reports),
    [{reportEntries, Entries}];
get_autocleaning_reports(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    get_autocleaning_reports(Ctx#{node => Node}).

%%-------------------------------------------------------------------
%% @doc
%% Returns status of current working autocleaning process for given space.
%% @end
%%-------------------------------------------------------------------
-spec get_autocleaning_status(Ctx :: service:ctx()) -> proplists:proplist().
get_autocleaning_status(#{space_id := SpaceId, node := Node}) ->
    rpc:call(Node, space_cleanup_api, status, [SpaceId]);
get_autocleaning_status(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    get_autocleaning_status(Ctx#{node => Node}).

%%-------------------------------------------------------------------
%% @doc
%% Manually starts cleaning of given space.
%% @end
%%-------------------------------------------------------------------
-spec start_cleaning(Ctx :: service:ctx()) -> ok.
start_cleaning(#{space_id := SpaceId, node := Node}) ->
    rpc:call(Node, space_cleanup_api, force_cleanup, [SpaceId]);
start_cleaning(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    start_cleaning(Ctx#{node => Node}).


%%-------------------------------------------------------------------
%% @doc
%% Removes legacy way of storing letsencrypt configuration and
%% returns its value.
%% @end
%%-------------------------------------------------------------------
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
