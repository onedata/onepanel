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
-include_lib("ctool/include/oz/oz_spaces.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, check_oz_availability/1,
    register/1, unregister/1, is_registered/1,
    modify_details/1, get_details/1,
    support_space/1, revoke_space_support/1, get_spaces/1,
    get_space_details/1, modify_space/1,
    get_sync_stats/1, get_autocleaning_reports/1, get_autocleaning_status/1,
    start_cleaning/1, check_oz_connection/1]).
-export([restart_listeners/1, restart_provider_listeners/1, async_restart_listeners/1]).
-export([obtain_webcert/1, set_txt_record/1, remove_txt_record/1,
    modify_letsencrypt_enabled/1, clear_pem_cache/1]).

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
    LetsEncryptCondition = fun
        (#{oneprovider_letsencrypt_enabled := true}) ->
            Register(Ctx) andalso should_generate_cert(Ctx);
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
        Ss#steps{action = obtain_webcert, ctx = OpCtx, condition = LetsEncryptCondition},
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
        #step{hosts = Hosts, function = check_oz_availability,
            attempts = onepanel_env:get(connect_to_onezone_attempts)},
        #step{hosts = Hosts, function = register, selection = any}
    ];
get_steps(register, Ctx) ->
    get_steps(register, Ctx#{hosts => service_op_worker:get_hosts()});

get_steps(obtain_webcert, #{hosts := Hosts}) ->
    [
        #step{hosts = Hosts, function = check_oz_availability,
            attempts = onepanel_env:get(connect_to_onezone_attempts)},
        #step{hosts = Hosts, function = obtain_webcert, selection = any},
        #step{hosts = Hosts, service = ?SERVICE_OPW,
            function = setup_certs, selection = all},
        #step{hosts = Hosts, function = clear_pem_cache, selection = any},
        #step{hosts = Hosts, function = async_restart_listeners, selection = any}
    ];
get_steps(obtain_webcert, Ctx) ->
    get_steps(obtain_webcert, Ctx#{hosts => service_op_worker:get_hosts()});


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

get_steps(modify_details, #{hosts := Hosts} = CTX) ->
    [
        #step{hosts = Hosts, function = modify_letsencrypt_enabled, selection = any},
        #step{hosts = Hosts, function = modify_details, selection = any},
        #steps{action = obtain_webcert, condition = fun should_generate_cert/1}
    ];
get_steps(modify_details, Ctx) ->
    get_steps(modify_details, Ctx#{hosts => service_op_worker:get_hosts()});

get_steps(Action, Ctx) when
    Action =:= get_details;
    Action =:= support_space;
    Action =:= revoke_space_support;
    Action =:= get_spaces;
    Action =:= get_space_details;
    Action =:= modify_space;
    Action =:= get_autocleaning_reports;
    Action =:= get_autocleaning_status;
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

    case http_client:get(Url) of
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
    Hosts = onepanel_cluster:nodes_to_hosts(service_op_worker:get_nodes()),
    Nodes = onepanel_cluster:hosts_to_nodes(Hosts),

    % TODO VFS-3765
    AdminEmail = service_ctx:get(oneprovider_admin_email, Ctx, binary),

    DomainParams = case service_ctx:get(oneprovider_subdomain_delegation, Ctx, boolean, false) of
        true ->
            {_, IPs} = lists:unzip(
                onepanel_rpc:call_all(Nodes, onepanel_utils, get_ip_address, [])),
            [{<<"subdomainDelegation">>, true},
                {<<"subdomain">>,
                    service_ctx:get(oneprovider_subdomain, Ctx, binary)},
                {<<"ipList">>, IPs}];
        false -> [
            {<<"subdomainDelegation">>, false},
            {<<"domain">>,
                service_ctx:get(oneprovider_domain, Ctx, binary)}]
    end,

    Params = [
        {<<"name">>,
            service_ctx:get(oneprovider_name, Ctx, binary)},
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
            OpwNodes = onepanel_cluster:hosts_to_nodes(service_op_worker:name(), Hosts),
            rpc:call(hd(OpwNodes), provider_auth, save, [ProviderId, Macaroon]),
            % Force connection healthcheck
            % (reconnect attempt is performed automatically if there is no connection)
            rpc:multicall(OpwNodes, oneprovider, is_connected_to_oz, []),

            service:update(name(), fun(#service{ctx = C} = S) ->
                S#service{ctx = C#{registered => true, admin_email => AdminEmail}}
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

    service:update(name(), fun(#service{ctx = C} = S) ->
        S#service{ctx = C#{registered => false}}
    end),
    mark_letsencrypt_undecided();
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
    [Node | _] = service_op_worker:get_nodes(),
    is_registered(Ctx#{node => Node}).


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
                        ok -> % any previous cert will have the old domain
                            set_has_letsencrypt_cert(false),
                            ok;
                        {error, subdomain_exists} ->
                            ?throw_error(?ERR_SUBDOMAIN_NOT_AVAILABLE)
                    end
            end;
        false ->
            Domain = onepanel_utils:typed_get(oneprovider_domain, Ctx, binary),
            ok = rpc:call(Node, provider_logic, set_domain, [Domain]),
            set_has_letsencrypt_cert(false);
        undefined -> ok
    end,

    Params = onepanel_maps:get_store(oneprovider_name, Ctx, <<"name">>, #{}),
    Params2 = onepanel_maps:get_store(oneprovider_geo_latitude, Ctx,
        <<"latitude">>, Params),
    Params3 = onepanel_maps:get_store(oneprovider_geo_longitude, Ctx,
        <<"longitude">>, Params2),

    % TODO VFS-3765
    case onepanel_maps:get(oneprovider_admin_email, Ctx, undefined) of
        undefined -> ok;
        AdminEmail ->
            service:update(name(), fun(#service{ctx = C} = S) ->
                S#service{ctx = C#{admin_email => AdminEmail}}
            end)
    end,

    case maps:size(Params3) of
        0 -> ok;
        _ -> ok = oz_providers:modify_details(provider, maps:to_list(Params3))
    end;
modify_details(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    modify_details(Ctx#{node => Node}).


%%--------------------------------------------------------------------
%% @doc Returns configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Ctx :: service:ctx()) -> list().
get_details(#{node := Node}) ->
    {ok, {_, _, OzDomain, _, _, _}} = http_uri:parse(oz_plugin:get_oz_url()),
    #{
        id := Id,
        name := Name,
        subdomain_delegation := SubdomainDelegation,
        domain := Domain,
        subdomain := Subdomain,
        longitude := Longitude,
        latitude := Latitude
    } = rpc:call(Node, provider_logic, get_as_map, []),

    % VFS-3765 Use email stored in oz
    {ok, #service{ctx = Ctx}} = service:get(name()),
    AdminEmail = maps:get(admin_email, Ctx, <<>>),

    Details = [
        {id, Id}, {name, Name},
        {subdomainDelegation, SubdomainDelegation}, {domain, Domain},
        {adminEmail, AdminEmail},
        {geoLatitude, onepanel_utils:convert(Latitude, float)},
        {geoLongitude, onepanel_utils:convert(Longitude, float)},
        {onezoneDomainName, onepanel_utils:convert(OzDomain, binary)}
    ],

    Details2 = case get_has_letsencrypt_cert() of
        true -> [{letsEncryptEnabled, true} | Details];
        false -> [{letsEncryptEnabled, false} | Details];
        undecided -> Details
    end,

    case SubdomainDelegation of
        true -> [{subdomain, Subdomain} | Details2];
        _ -> Details2
    end;
get_details(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    get_details(Ctx#{node => Node}).


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
%% @doc
%% Initiliazes has_letsencrypt_cert variable in the model.
%% Requried to indicate that user has been presented with
%% an option to use Let's Encrypt.
%% Stores information about Let's Encrypt being disabled.
%% @end
%%--------------------------------------------------------------------
-spec modify_letsencrypt_enabled(service:ctx()) -> ok.
modify_letsencrypt_enabled(#{oneprovider_letsencrypt_enabled := Request}) ->
    ok = service:update(name(), fun(#service{ctx = ServiceCtx} = Record) ->
        Current = maps:get(has_letsencrypt_cert, ServiceCtx, undecided),
        case {Current, Request} of
            {true, true} ->
                Record#service{ctx = maps:put(has_letsencrypt_cert, true, ServiceCtx)};
            _ ->
                % Even if Request is true, has_letsencrypt_cert will be set
                % after actually obtaining the certificate and now `false`
                % value is needed to trigger certfication
                Record#service{ctx = maps:put(has_letsencrypt_cert, false, ServiceCtx)}
        end
    end);
modify_letsencrypt_enabled(_) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Tries to obtain SSL certificate from Let's Encrypt.
%% Throws if subdomain delegation is not turned on.
%% @end
%%--------------------------------------------------------------------
-spec obtain_webcert(Ctx :: service:ctx()) -> ok | no_return().
obtain_webcert(#{node := Node}) ->
    case rpc:call(Node, provider_logic, is_subdomain_delegated, []) of
        {true, _} ->
            #{domain := Domain} = rpc:call(Node, provider_logic, get_as_map, []),
            try
                ok = letsencrypt_api:run_certification_flow(Domain)
            after
                % if error occured gui user after reload should be presented
                % with the certificate configuration page again
                mark_letsencrypt_undecided()
            end,
            set_has_letsencrypt_cert(true);
        false ->
            set_has_letsencrypt_cert(false),
            ?throw_error(?ERR_SUBDOMAIN_DELEGATION_DISABLED)
    end;
obtain_webcert(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    obtain_webcert(Ctx#{node => Node}).

%%--------------------------------------------------------------------
%% @doc
%% Sets txt record in onezone dns via oneprovider.
%% @end
%%--------------------------------------------------------------------
-spec set_txt_record(Ctx :: service:ctx()) -> ok.
set_txt_record(#{txt_record_name:= Name, txt_record_value:= Value}) ->
    [Node | _] = service_op_worker:get_nodes(),
    ok = rpc:call(Node, provider_logic, set_txt_record, [Name, Value]).

%%--------------------------------------------------------------------
%% @doc
%% Removes txt record from onezone dns via oneprovider.
%% @end
%%--------------------------------------------------------------------
-spec remove_txt_record(Ctx :: service:ctx()) -> ok.
remove_txt_record(#{txt_record_name:= Name}) ->
    [Node | _] = service_op_worker:get_nodes(),
    ok = rpc:call(Node, provider_logic, remove_txt_record, [Name]).

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
%% Restart listeners with a delay to allow response to the current request.
%% @end
%%--------------------------------------------------------------------
-spec async_restart_listeners(service:ctx()) -> ok.
async_restart_listeners(Ctx) ->
    async_restart_listeners(Ctx, timer:seconds(1)).
-spec async_restart_listeners(service:ctx(), Delay :: non_neg_integer()) -> ok.
async_restart_listeners(Ctx, Delay) ->
    service:apply_async(name(), restart_listeners, Ctx#{task_delay => Delay}),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Clears certificate cache on onepanel and oneprovider nodes.
%% @end
%%--------------------------------------------------------------------
clear_pem_cache(#{nodes := Nodes} = _Ctx) ->
    onepanel_rpc:call(ssl_manager, clear_pem_cache, []),
    {_, []} = rpc:multicall(Nodes, ssl_manager, clear_pem_cache, []);
clear_pem_cache(Ctx) ->
    Nodes = service_op_worker:get_nodes(),
    clear_pem_cache(Ctx#{nodes => Nodes}).

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

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%-------------------------------------------------------------------
%% @private
%% @doc
%% Determines if new SSL certificate should be obtained from Let's Encrypt.
%% @end
%%-------------------------------------------------------------------
-spec should_generate_cert(Ctx :: service:ctx()) -> boolean().
should_generate_cert(Ctx) ->
    %% Let's Encrypt certificate should be generated iff it wasn't already obtaind
    %% and the users requests it
    Previous = get_has_letsencrypt_cert(),
    Request = service_ctx:get(oneprovider_letsencrypt_enabled, Ctx, boolean, false),

    case {Previous, Request} of
        {true, true} -> false;
        {_, true} -> true;
        _ -> false
    end.


%%-------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves information whether ssl certificate was obtained from Let's Encrypt.
%% 'undefined' indicates that this has setting has not been yet configured
%% after registration and the user should be asked about it.
%% @end
%%-------------------------------------------------------------------
-spec get_has_letsencrypt_cert() -> boolean() | undecided.
get_has_letsencrypt_cert() ->
    {ok, #service{ctx = Ctx}} = service:get(name()),
    maps:get(has_letsencrypt_cert, Ctx, undecided).


%%-------------------------------------------------------------------
%% @private
%% @doc
%% Stores information whether ssl certificate was obtained from Let's Encrypt.
%% @end
%%-------------------------------------------------------------------
-spec set_has_letsencrypt_cert(boolean()) -> ok | no_return().
set_has_letsencrypt_cert(Used) ->
    ok = service:update(name(), fun(#service{ctx = Ctx} = Record) ->
        Record#service{ctx = maps:put(has_letsencrypt_cert, Used, Ctx)}
    end).


%%-------------------------------------------------------------------
%% @private
%% @doc
%% Marks Let's Encrypt configuration as waiting for user decision.
%% @end
%%-------------------------------------------------------------------
-spec mark_letsencrypt_undecided() -> ok | no_return().
mark_letsencrypt_undecided() ->
    ok = service:update(name(), fun(#service{ctx = Ctx} = Record) ->
        Record#service{ctx = maps:remove(has_letsencrypt_cert, Ctx)}
    end).


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
