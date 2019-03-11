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
-include("authentication.hrl").
-include_lib("ctool/include/oz/oz_spaces.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("ctool/include/auth/onedata_macaroons.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("macaroons/src/macaroon.hrl").

-include("http/rest.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, check_oz_availability/1, mark_configured/0,
    register/1, unregister/0, is_registered/1, is_registered/0,
    modify_details/1, get_details/0, get_details/1, get_oz_domain/0,
    support_space/1, revoke_space_support/1, get_spaces/1, is_space_supported/1,
    get_space_details/1, modify_space/1, format_cluster_ips/1,
    get_sync_stats/1, get_auto_cleaning_reports/1, get_auto_cleaning_report/1,
    get_auto_cleaning_status/1, start_auto_cleaning/1, check_oz_connection/1,
    update_provider_ips/0, configure_file_popularity/1, configure_auto_cleaning/1,
    get_file_popularity_configuration/1, get_auto_cleaning_configuration/1]).
-export([set_up_service_in_onezone/0]).
-export([pop_legacy_letsencrypt_config/0]).
-export([get_id/0, get_auth_token/0]).

-define(OZ_DOMAIN_CACHE, oz_domain).
-define(OZ_DOMAIN_CACHE_TTL, timer:minutes(1)).

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
    nodes:all(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, Ctx) ->
    {ok, OpaCtx} = onepanel_maps:get([cluster, ?SERVICE_PANEL], Ctx),
    {ok, CbCtx} = onepanel_maps:get([cluster, ?SERVICE_CB], Ctx),
    {ok, CmCtx} = onepanel_maps:get([cluster, ?SERVICE_CM], Ctx),
    {ok, OpwCtx} = onepanel_maps:get([cluster, ?SERVICE_OPW], Ctx),
    {ok, LeCtx} = onepanel_maps:get([cluster, ?SERVICE_LE], Ctx),
    StorageCtx = onepanel_maps:get([cluster, storages], Ctx, #{}),
    OpCtx = onepanel_maps:get(name(), Ctx, #{}),

    service:create(#service{name = name()}),
    % separate update to handle upgrade from older version
    service:update(name(), fun(#service{ctx = C} = S) ->
        S#service{ctx = C#{master_host => hosts:self()}}
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

    SelfHost = hosts:self(),

    S = #step{verify_hosts = false},
    Ss = #steps{verify_hosts = false},
    [
        Ss#steps{service = ?SERVICE_PANEL, action = deploy, ctx = OpaCtx},
        Ss#steps{service = ?SERVICE_PANEL, action = add_users, ctx = OpaCtx},
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
        S#step{module = onepanel_deployment, function = mark_completed,
            args = [?PROGRESS_READY], hosts = [SelfHost]},
        S#step{function = mark_configured, ctx = OpaCtx, selection = any, args = [],
            condition = fun(FunCtx) ->
                not maps:get(interactive_deployment, FunCtx, true)
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

    case hosts:self() == MasterHost of
        true -> [
            #steps{service = ?SERVICE_PANEL, action = wait_for_cluster},
            #steps{action = stop},
            #steps{service = ?SERVICE_CB, action = resume},
            #steps{service = ?SERVICE_CM, action = resume},
            #steps{service = ?SERVICE_OPW, action = resume},
            #steps{action = set_up_service_in_onezone},
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
        % explicitly fail on connection problems before executing further steps
        #step{hosts = Hosts, function = check_oz_connection,
            attempts = onepanel_env:get(connect_to_onezone_attempts)},
        #steps{action = set_up_service_in_onezone},
        #steps{action = set_cluster_ips}
    ];
get_steps(register, Ctx) ->
    get_steps(register, Ctx#{hosts => service_op_worker:get_hosts()});

get_steps(unregister, _Ctx) ->
    [#step{function = unregister, selection = any, args = []}];

get_steps(set_up_service_in_onezone, _Ctx) ->
    [#step{function = set_up_service_in_onezone, args = [], selection = any}];

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
            hosts = Hosts, args = []}
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
    Action =:= get_auto_cleaning_report;
    Action =:= get_auto_cleaning_status;
    Action =:= get_auto_cleaning_configuration;
    Action =:= get_file_popularity_configuration;
    Action =:= format_cluster_ips;
    Action =:= start_auto_cleaning;
    Action =:= get_sync_stats;
    Action =:= configure_file_popularity;
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
configure(#{application := ?APP_NAME, oneprovider_token := Token}) ->
    OzDomain = zone_tokens:get_zone_domain(Token),
    application:set_env(?APP_NAME, onezone_domain, OzDomain),
    onepanel_env:write([?APP_NAME, onezone_domain], OzDomain);

configure(#{oneprovider_token := Token} = Ctx) ->
    OzDomain = zone_tokens:get_zone_domain(Token),
    Name = ?SERVICE_OPW,
    Node = nodes:local(Name),
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
    OzDomain = onepanel_env:get(onezone_domain),
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
%% @doc Checks if Oneprovider is registered and connected to onezone
%% @end
%%--------------------------------------------------------------------
-spec check_oz_connection(Ctx :: service:ctx()) -> ok | no_return().
check_oz_connection(Ctx) ->
    {ok, Node} = nodes:any(Ctx#{service => ?SERVICE_OPW}),
    case rpc:call(Node, oneprovider, is_connected_to_oz, []) of
        true -> ok;
        false -> ?throw_error(?ERR_ONEZONE_NOT_AVAILABLE)
    end.


%%--------------------------------------------------------------------
%% @doc Registers provider in the zone.
%% @end
%%--------------------------------------------------------------------
-spec register(Ctx :: service:ctx()) ->
    {ok, ProviderId :: binary()} | no_return().
register(Ctx) ->
    {ok, OpwNode} = nodes:any(?SERVICE_OPW),

    DomainParams = case service_ctx:get(oneprovider_subdomain_delegation, Ctx, boolean, false) of
        true -> #{
            <<"subdomainDelegation">> => true,
            <<"subdomain">> => service_ctx:get(oneprovider_subdomain, Ctx, binary),
            <<"ipList">> => [] % IPs will be updated in the step set_cluster_ips
        };
        false -> #{
            <<"subdomainDelegation">> => false,
            <<"domain">> => service_ctx:get(oneprovider_domain, Ctx, binary)
        }

    end,

    Params = DomainParams#{
        <<"token">> => service_ctx:get(oneprovider_token, Ctx, binary),
        <<"name">> => service_ctx:get(oneprovider_name, Ctx, binary),
        <<"adminEmail">> => service_ctx:get(oneprovider_admin_email, Ctx, binary),
        <<"latitude">> => service_ctx:get(oneprovider_geo_latitude, Ctx, float, 0.0),
        <<"longitude">> => service_ctx:get(oneprovider_geo_longitude, Ctx, float, 0.0)
    },

    case oz_providers:register(none, Params) of
        ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>) ->
            ?throw_error(?ERR_SUBDOMAIN_NOT_AVAILABLE);
        {error, Reason} -> ?throw_error(Reason);
        {ok, #{<<"providerId">> := ProviderId,
            <<"macaroon">> := Macaroon}} ->

            on_registered(OpwNode, ProviderId, Macaroon),
            {ok, ProviderId}
    end.


%%--------------------------------------------------------------------
%% @doc Unregisters provider in the zone.
%% @end
%%--------------------------------------------------------------------
-spec unregister() -> ok | no_return().
unregister() ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    ok = oz_providers:unregister(provider),
    rpc:call(Node, provider_auth, delete, []),
    rpc:call(Node, gs_worker, kill_connection, []),

    onepanel_deployment:mark_not_completed(?PROGRESS_LETSENCRYPT_CONFIG),
    service:update_ctx(name(), fun(ServiceCtx) ->
        maps:without([cluster, cluster_id],
            ServiceCtx#{registered => false})
    end).


%%--------------------------------------------------------------------
%% @doc Returns if this provider is registered in OneZone.
%% @end
%%--------------------------------------------------------------------
-spec is_registered(Ctx :: service:ctx()) -> boolean().
is_registered(#{node := Node}) ->
    try rpc:call(Node, oneprovider, is_registered, []) of
        true -> true;
        false -> false;
        _ ->
            case service:get(name()) of
                {ok, #service{ctx = #{registered := Registered}}} -> Registered;
                _ -> false
            end
    catch _:_ ->
        false
    end;

is_registered(Ctx) ->
    case nodes:any(?SERVICE_OPW) of
        {ok, Node} -> is_registered(Ctx#{node => Node});
        _Error -> false
    end.

is_registered() ->
    is_registered(#{}).

%%--------------------------------------------------------------------
%% @doc Modifies configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Ctx :: service:ctx()) -> ok | no_return().
modify_details(Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    ok = modify_domain_details(Node, Ctx),

    Params = onepanel_maps:get_store_multiple([
        {oneprovider_name, <<"name">>},
        {oneprovider_geo_latitude, <<"latitude">>},
        {oneprovider_geo_longitude, <<"longitude">>},
        {oneprovider_admin_email, <<"adminEmail">>}
    ], Ctx),

    {ok, Node} = nodes:any(?SERVICE_OPW),
    case maps:size(Params) of
        0 -> ok;
        _ -> ok = rpc:call(Node, provider_logic, update, [Params])
    end.


%%--------------------------------------------------------------------
%% @doc Must be executed on an op-worker node.
%% @end
%%--------------------------------------------------------------------
-spec get_id() -> binary().
get_id() ->
    OpNode = nodes:local(?SERVICE_OPW),
    case rpc:call(OpNode, provider_auth, get_provider_id, []) of
        <<ProviderId/binary>> -> ProviderId;
        ?ERROR_UNREGISTERED_PROVIDER = Error -> ?make_error(Error);
        _ -> onepanel_maps:get(provider_id, read_auth_file())
    end.


-spec get_auth_token() -> Macaroon :: binary().
get_auth_token() ->
    OpNode = nodes:local(?SERVICE_OPW),
    case rpc:call(OpNode, provider_auth, get_auth_macaroon, []) of
        {ok, <<Macaroon/binary>>} -> Macaroon;
        ?ERROR_UNREGISTERED_PROVIDER = Error -> ?make_error(Error);
        _ -> auth_macaroon_from_file()
    end.


%%--------------------------------------------------------------------
%% @doc Returns configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Ctx :: service:ctx()) -> #{atom() := term()} | no_return().
get_details(_Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),

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
        {subdomain_delegation, subdomainDelegation}, {domain, domain},
        {cluster, cluster}
    ], Details),

    Response2 = Response#{
        geoLatitude => onepanel_utils:convert(Latitude, float),
        geoLongitude => onepanel_utils:convert(Longitude, float),
        onezoneDomainName => onepanel_utils:convert(OzDomain, binary)
    },

    case maps:get(subdomain_delegation, Details) of
        true -> Response2#{subdomain => maps:get(subdomain, Details)};
        _ -> Response2
    end.

-spec get_details() -> #{atom() := term()} | no_return().
get_details() -> get_details(#{}).


%%--------------------------------------------------------------------
%% @doc Returns the onezone domain.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_domain() -> string().
get_oz_domain() ->
    case onepanel_env:find([onezone_domain]) of
        {ok, Domain} -> onepanel_utils:convert(Domain, list);
        Error -> ?throw_error(Error)
    end.


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
support_space(#{storage_id := StorageId} = Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
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

    configure_space(Node, SpaceId, Ctx).


%%--------------------------------------------------------------------
%% @doc Revokes support for the space given by ID.
%% @end
%%--------------------------------------------------------------------
-spec revoke_space_support(Ctx :: service:ctx()) -> ok.
revoke_space_support(#{id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
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
    {ok, Node} = nodes:any(?SERVICE_OPW),
    case rpc:call(Node, provider_logic, supports_space, [Id]) of
        Result when is_boolean(Result) -> Result
    end.


%%--------------------------------------------------------------------
%% @doc Returns details of the space given by ID.
%% @end
%%--------------------------------------------------------------------
-spec get_space_details(Ctx :: service:ctx()) -> list().
get_space_details(#{id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
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
    ].


%%--------------------------------------------------------------------
%% @doc Modifies space details.
%% @end
%%--------------------------------------------------------------------
-spec modify_space(Ctx :: service:ctx()) -> list().
modify_space(#{space_id := SpaceId} = Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    ImportArgs = maps:get(storage_import, Ctx, #{}),
    UpdateArgs = maps:get(storage_update, Ctx, #{}),
    ok = maybe_update_support_size(Node, SpaceId, Ctx),
    {ok, _} = op_worker_storage_sync:maybe_modify_storage_import(Node, SpaceId, ImportArgs),
    {ok, _} = op_worker_storage_sync:maybe_modify_storage_update(Node, SpaceId, UpdateArgs),
    [{id, SpaceId}].


%%--------------------------------------------------------------------
%% @private
%% @doc If new size for a space is specified, enacts the change.
%% @end
%%--------------------------------------------------------------------
maybe_update_support_size(OpNode, SpaceId, #{size := SupportSize}) ->
    case rpc:call(OpNode, provider_logic, update_space_support_size, [SpaceId, SupportSize]) of
        ok -> ok;
        ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, Minimum) ->
            ?throw_error(?ERR_SPACE_SUPPORT_TOO_LOW(Minimum));
        {error, Reason} ->
            ?throw_error(Reason)
    end;

maybe_update_support_size(_OpNode, _SpaceId, _Ctx) -> ok.


%%--------------------------------------------------------------------
%% @doc Get storage_sync stats
%% @end
%%--------------------------------------------------------------------
-spec get_sync_stats(Ctx :: service:ctx()) -> list().
get_sync_stats(#{space_id := SpaceId} = Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    Period = onepanel_utils:typed_get(period, Ctx, binary, undefined),
    MetricsJoined = onepanel_utils:typed_get(metrics, Ctx, binary, <<"">>),
    Metrics = binary:split(MetricsJoined, <<",">>, [global, trim]),
    op_worker_storage_sync:get_stats(Node, SpaceId, Period, Metrics).


%%--------------------------------------------------------------------
%% @doc
%% Notify onezone about IPs change.
%% @end
%%--------------------------------------------------------------------
-spec update_provider_ips() -> ok.
update_provider_ips() ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    case is_registered() of
        true ->
            % no check of results - if the provider is not connected to onezone
            % the IPs will be sent automatically after connection is acquired
            rpc:call(Node, provider_logic, update_subdomain_delegation_ips, []),
            ok;
        false -> ok
    end.


%%-------------------------------------------------------------------
%% @doc
%% Returns list of auto-cleaning runs reports started since Since date.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_reports(Ctx :: service:ctx()) -> maps:map().
get_auto_cleaning_reports(Ctx = #{space_id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    Offset = onepanel_utils:typed_get(offset, Ctx, integer, 0),
    Limit = onepanel_utils:typed_get(limit, Ctx, integer, all),
    Index = onepanel_utils:typed_get(index, Ctx, binary, undefined),
    {ok, Ids} = rpc:call(Node, autocleaning_api, list_reports, [SpaceId, Index, Offset, Limit]),
    #{ids => Ids}.


%%-------------------------------------------------------------------
%% @doc
%% Returns auto-cleaning run report.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_report(Ctx :: service:ctx()) -> #{atom() => term()}.
get_auto_cleaning_report(#{report_id := ReportId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    case rpc:call(Node, autocleaning_api, get_run_report, [ReportId]) of
        {ok, Report} ->
            onepanel_maps:undefined_to_null(
                onepanel_maps:get_store_multiple([
                    {id, id},
                    {index, index},
                    {started_at, startedAt},
                    {stopped_at, stoppedAt},
                    {released_bytes, releasedBytes},
                    {bytes_to_release, bytesToRelease},
                    {files_number, filesNumber}
                ], Report));
        {error, Reason} ->
            ?throw_error({?ERR_AUTOCLEANING, Reason})
    end.


%%-------------------------------------------------------------------
%% @doc
%% Returns status of current working auto-cleaning process for given space.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_status(Ctx :: service:ctx()) -> #{atom() => term()}.
get_auto_cleaning_status(#{space_id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    Status = rpc:call(Node, autocleaning_api, status, [SpaceId]),
    onepanel_maps:get_store_multiple([
        {in_progress, inProgress},
        {space_occupancy, spaceOccupancy}
    ], Status).


%%-------------------------------------------------------------------
%% @doc
%% Returns configuration of auto-cleaning mechanism in given space.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_configuration(Ctx :: service:ctx()) -> #{atom() => term()}.
get_auto_cleaning_configuration(#{space_id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    op_worker_storage:get_auto_cleaning_configuration(Node, SpaceId).


%%-------------------------------------------------------------------
%% @doc
%% Returns configuration of file-popularity mechanism in given space.
%% @end
%%-------------------------------------------------------------------
-spec get_file_popularity_configuration(Ctx :: service:ctx()) -> #{atom() => term()}.
get_file_popularity_configuration(#{space_id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    op_worker_storage:get_file_popularity_configuration(Node, SpaceId).


%%-------------------------------------------------------------------
%% @doc
%% Manually starts auto-cleaning of given space.
%% @end
%%-------------------------------------------------------------------
-spec start_auto_cleaning(Ctx :: service:ctx()) -> ok.
start_auto_cleaning(#{space_id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    case rpc:call(Node, autocleaning_api, force_start, [SpaceId]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, nothing_to_clean} -> ok;
        {error, Reason} ->
            ?throw_error({?ERR_AUTOCLEANING, Reason})
    end.


%%-------------------------------------------------------------------
%% @doc
%% Marks all configuration steps as already performed.
%% @end
%%-------------------------------------------------------------------
-spec mark_configured() -> ok.
mark_configured() ->
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
-spec configure_file_popularity(Ctx :: service:ctx()) -> list().
configure_file_popularity(#{space_id := SpaceId} = Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    Config = maps:without([space_id, hosts], Ctx),
    ok = op_worker_storage:maybe_update_file_popularity(Node, SpaceId, Config),
    [{id, SpaceId}].


%%-------------------------------------------------------------------
%% @doc
%% Configures auto-cleaning mechanism
%% @end
%%-------------------------------------------------------------------
-spec configure_auto_cleaning(Ctx :: service:ctx()) -> list().
configure_auto_cleaning(#{space_id := SpaceId} = Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    Config = maps:without([space_id, hosts], Ctx),
    ok = op_worker_storage:maybe_update_auto_cleaning(Node, SpaceId, Config),
    [{id, SpaceId}].


%%--------------------------------------------------------------------
%% @doc
%% Sets up Oneprovider panel service in Onezone - updates version info
%% (release, build and GUI versions). If given GUI version is not present in
%% Onezone, the GUI package is uploaded first.
%% @end
%%--------------------------------------------------------------------
-spec set_up_service_in_onezone() -> ok.
set_up_service_in_onezone() ->
    ?info("Setting up Oneprovider panel service in Onezone"),
    GuiPackagePath = https_listener:gui_package_path(),
    GuiHash = gui:package_hash(GuiPackagePath),
    % Try to update version info in Onezone
    case update_version_info(GuiHash) of
        {ok, 204, _, _} ->
            ?info("Skipping GUI upload as it is already present in Onezone");
        {ok, 400, _, _} ->
            ?info("Uploading GUI to Onezone (~s)", [GuiHash]),
            {ok, 200, _, _} = oz_endpoint:request(
                provider,
                "/gui-upload/op_panel",
                post,
                {multipart, [{file, list_to_binary(GuiPackagePath)}]},
                [{endpoint, gui}]
            ),
            ?info("GUI uploaded succesfully"),
            {ok, 204, _, _} = update_version_info(GuiHash)
    end,
    ?info("Oneprovider panel service successfully set up in Onezone").

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes post-registration actions, mainly storing the provider's
%% identity.
%% @end
%%--------------------------------------------------------------------
-spec on_registered(OpwNode :: node(), ProviderId :: binary(),
    Macaroon :: binary()) -> ok.
on_registered(OpwNode, ProviderId, Macaroon) ->
    save_provider_auth(OpwNode, ProviderId, Macaroon),
    service:update_ctx(name(), #{registered => true}),

    % Force connection healthcheck
    % (reconnect attempt is performed automatically if there is no connection)
    rpc:call(OpwNode, oneprovider, force_oz_connection_start, []),

    % preload cache
    (catch clusters:get_current_cluster()),
    ok.


-spec save_provider_auth(OpwNode :: node(), ProviderId :: binary(),
    Macaroon :: binary()) -> ok.
save_provider_auth(OpwNode, ProviderId, Macaroon) ->
    ok = rpc:call(OpwNode, provider_auth, save, [ProviderId, Macaroon]),

    PanelNodes = nodes:all(?SERVICE_PANEL),
    MacaroonPath = rpc:call(OpwNode, provider_auth, get_root_macaroon_file_path, []),
    onepanel_env:write(PanelNodes, [?SERVICE_PANEL, op_worker_root_macaroon_path],
        MacaroonPath, onepanel_env:get_config_path(?SERVICE_PANEL, generated)),
    onepanel_env:set(PanelNodes, op_worker_root_macaroon_path, MacaroonPath, ?APP_NAME),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc Modify provider details regarding its domain or subdomain.
%% @end
%%--------------------------------------------------------------------
-spec modify_domain_details(OpNode :: node(), service:ctx()) -> ok.
modify_domain_details(OpNode, #{oneprovider_subdomain_delegation := true} = Ctx) ->
    Subdomain = onepanel_utils:typed_get(oneprovider_subdomain, Ctx, binary),

    case rpc:call(OpNode, provider_logic, is_subdomain_delegated, []) of
        {true, Subdomain} -> ok; % no change
        _ ->
            case rpc:call(OpNode, provider_logic, set_delegated_subdomain, [Subdomain]) of
                ok ->
                    dns_check:invalidate_cache(op_worker);
                ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>) ->
                    ?throw_error(?ERR_SUBDOMAIN_NOT_AVAILABLE)
            end
    end;

modify_domain_details(OpNode, #{oneprovider_subdomain_delegation := false} = Ctx) ->
    Domain = onepanel_utils:typed_get(oneprovider_domain, Ctx, binary),
    ok = rpc:call(OpNode, provider_logic, set_domain, [Domain]),
    dns_check:invalidate_cache(op_worker);

modify_domain_details(_OpNode, _Ctx) -> ok.


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
-spec configure_space(OpNode :: node(), SpaceId :: binary(), Ctx :: service:ctx()) -> list().
configure_space(Node, SpaceId, #{storage_id := StorageId} = Ctx) ->
    MountInRoot = onepanel_utils:typed_get(mount_in_root, Ctx, boolean, false),
    ImportArgs = maps:get(storage_import, Ctx, #{}),
    UpdateArgs = maps:get(storage_update, Ctx, #{}),
    {ok, _} = rpc:call(Node, space_storage, add, [SpaceId, StorageId, MountInRoot]),
    op_worker_storage_sync:maybe_modify_storage_import(Node, SpaceId, ImportArgs),
    op_worker_storage_sync:maybe_modify_storage_update(Node, SpaceId, UpdateArgs),
    [{id, SpaceId}].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends Onepanel version info to Onezone where the provider is registered.
%% @end
%%--------------------------------------------------------------------
-spec update_version_info(GuiHash :: binary()) -> http_client:response().
update_version_info(GuiHash) ->
    {BuildVersion, AppVersion} = onepanel_app:get_build_and_version(),
    oz_endpoint:request(
        provider,
        str_utils:format("/clusters/~s", [clusters:get_id()]),
        patch,
        json_utils:encode(#{
            <<"onepanelVersion">> => #{
                <<"release">> => AppVersion,
                <<"build">> => BuildVersion,
                <<"gui">> => GuiHash
            }
        })
    ).


%%--------------------------------------------------------------------
%% @private
%% @doc Reads provider root macaroon stored in a file
%% and returns it with an time caveat added.
%% @end
%%--------------------------------------------------------------------
-spec auth_macaroon_from_file() -> Macaroon :: binary().
auth_macaroon_from_file() ->
    {ok, RootMacaroonB64} = onepanel_maps:get(root_macaroon, read_auth_file()),
    {ok, RootMacaroon} = onedata_macaroons:deserialize(RootMacaroonB64),
    {ok, TTL} = onepanel_env:read_effective(
        [?SERVICE_OPW, provider_macaroon_ttl_sec], ?SERVICE_OPW),
    Caveat = ?TIME_CAVEAT(time_utils:system_time_seconds(), TTL),
    Limited = onedata_macaroons:add_caveat(RootMacaroon, Caveat),
    {ok, Macaroon} = onedata_macaroons:serialize(Limited),
    Macaroon.


%%--------------------------------------------------------------------
%% @private
%% @doc Reads provider Id and root macaroon from a file where they are stored.
%% Must be executed on an op-worker node.
%% @end
%%--------------------------------------------------------------------
-spec read_auth_file() -> #{provider_id := binary(), root_macaroon := binary()}.
read_auth_file() ->
    Path = onepanel_env:get(op_worker_root_macaroon_path),
    case file:consult(Path) of
        {ok, [Map]} -> Map;
        {error, Error} -> ?throw_error(Error)
    end.
