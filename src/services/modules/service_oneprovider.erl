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
-include_lib("ctool/include/aai/macaroons.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/oz/oz_spaces.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-include("http/rest.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

-export([get_details_by_graph_sync/0, get_details_by_rest/0]).

%% API
-export([configure/1, check_oz_availability/1, mark_configured/0,
    register/1, unregister/0, is_registered/1, is_registered/0,
    modify_details/1, get_details/0, get_oz_domain/0,
    support_space/1, revoke_space_support/1, get_spaces/1, is_space_supported/1,
    get_space_details/1, modify_space/1, format_cluster_ips/1,
    get_sync_stats/1, get_auto_cleaning_reports/1, get_auto_cleaning_report/1,
    get_auto_cleaning_status/1, start_auto_cleaning/1, check_oz_connection/0,
    update_provider_ips/0, configure_file_popularity/1, configure_auto_cleaning/1,
    get_file_popularity_configuration/1, get_auto_cleaning_configuration/1]).
-export([set_up_service_in_onezone/0]).
-export([pop_legacy_letsencrypt_config/0]).
-export([get_id/0, get_auth_token/0]).

% Internal RPC
-export([auth_macaroon_from_file/0]).

-define(OZ_DOMAIN_CACHE, oz_domain).
-define(OZ_DOMAIN_CACHE_TTL, timer:minutes(1)).
-define(DETAILS_PERSISTENCE, provider_details).

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

    AlreadyRegistered = ?MODULE:is_registered(),
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
        Ss#steps{service = ?SERVICE_PANEL, action = configure, ctx = OpaCtx},
        #steps{service = ?SERVICE_PANEL, action = migrate_emergency_passphrase},
        Ss#steps{service = ?SERVICE_CB, action = deploy, ctx = CbCtx},
        S#step{service = ?SERVICE_CB, function = status, ctx = CbCtx},
        Ss#steps{service = ?SERVICE_CM, action = deploy, ctx = CmCtx},
        S#step{service = ?SERVICE_CM, function = status, ctx = CmCtx},
        Ss#steps{service = ?SERVICE_OPW, action = deploy, ctx = OpwCtx},
        S#step{service = ?SERVICE_OPW, function = status, ctx = OpwCtx},
        Ss#steps{service = ?SERVICE_LE, action = deploy, ctx = LeCtx3},
        S#step{module = onepanel_deployment, function = set_marker,
            args = [?PROGRESS_CLUSTER], hosts = [SelfHost]},
        Ss#steps{service = ?SERVICE_OPW, action = add_storages, ctx = StorageCtx},
        Ss#steps{action = register, ctx = OpCtx, condition = Register},
        Ss#steps{service = ?SERVICE_LE, action = update, ctx = LeCtx3},
        S#step{module = onepanel_deployment, function = set_marker,
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
            #steps{service = ?SERVICE_PANEL, action = migrate_emergency_passphrase},
            #steps{service = ?SERVICE_PANEL, action = clear_users},
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
        #step{hosts = Hosts, function = configure,
            ctx = Ctx#{application => ?SERVICE_OPW}},
        #step{hosts = Hosts, function = configure,
            ctx = Ctx#{application => ?APP_NAME}, selection = first},
        #step{hosts = Hosts, function = check_oz_availability,
            attempts = onepanel_env:get(connect_to_onezone_attempts)},
        #step{hosts = Hosts, function = register, selection = any},
        % explicitly fail on connection problems before executing further steps
        #step{hosts = Hosts, function = check_oz_connection, args = [],
            attempts = onepanel_env:get(connect_to_onezone_attempts)},
        #steps{action = set_up_service_in_onezone},
        #steps{action = set_cluster_ips}
    ];
get_steps(register, Ctx) ->
    get_steps(register, Ctx#{hosts => hosts:all(?SERVICE_OPW)});

get_steps(unregister, _Ctx) ->
    [#step{function = unregister, selection = any, args = []}];

get_steps(set_up_service_in_onezone, _Ctx) ->
    [#step{function = set_up_service_in_onezone, args = [], selection = any}];

get_steps(modify_details, #{hosts := Hosts}) ->
    [
        #step{hosts = Hosts, function = modify_details, selection = any}
    ];
get_steps(modify_details, Ctx) ->
    get_steps(modify_details, Ctx#{hosts => hosts:all(?SERVICE_OPW)});


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
    get_steps(set_cluster_ips, Ctx#{hosts => hosts:all(?SERVICE_OPW)});

get_steps(Action, _Ctx) when
    Action =:= get_details ->
    [#step{function = Action, args = [], selection = any}];

get_steps(Action, Ctx) when
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
                ctx = Ctx#{hosts => hosts:all(?SERVICE_OPW)}}]
    end.


%%%===================================================================
%%% Public API
%%% Functions which can be called on any node.
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns id of this Oneprovider.
%% @end
%%--------------------------------------------------------------------
-spec get_id() -> binary().
get_id() ->
    {ok, OpNode} = nodes:any(?SERVICE_OPW),
    case rpc:call(OpNode, provider_auth, get_provider_id, []) of
        {ok, <<ProviderId/binary>>} ->
            ProviderId;
        ?ERROR_UNREGISTERED_PROVIDER = Error ->
            ?throw_error(Error);
        _ ->
            FileContents = read_auth_file(),
            {ok, Id} = onepanel_maps:get(provider_id, FileContents),
            Id
    end.


%%--------------------------------------------------------------------
%% @doc Returns domain of Onezone to which this Oneprovider belongs.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_domain() -> string().
get_oz_domain() ->
    % onezone_domain variable is set on all nodes
    case onepanel_env:find([onezone_domain]) of
        {ok, Domain} -> onepanel_utils:convert(Domain, list);
        Error -> ?throw_error(Error)
    end.


%%--------------------------------------------------------------------
%% @doc Returns whether this Oneprovider is registered in Onezone.
%% @end
%%--------------------------------------------------------------------
-spec is_registered() -> boolean().
is_registered() ->
    is_registered(#{}).

%%--------------------------------------------------------------------
%% @doc Returns whether this Oneprovider is registered in Onezone.
%% @end
%%--------------------------------------------------------------------
-spec is_registered(Ctx :: service:ctx()) -> boolean().
is_registered(#{node := Node}) ->
    case rpc:call(Node, oneprovider, is_registered, []) of
        Registered when is_boolean(Registered) ->
            service:update_ctx(name(), #{registered => Registered}),
            Registered;
        _RpcError ->
            case service:get_ctx(name()) of
                #{registered := Registered} -> Registered;
                _ -> false
            end
    end;

is_registered(Ctx) ->
    case nodes:any(?SERVICE_OPW) of
        {ok, Node} -> is_registered(Ctx#{node => Node});
        _Error -> false
    end.


%%%===================================================================
%%% Step functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(#{application := ?APP_NAME, oneprovider_token := Token}) ->
    OzDomain = onezone_tokens:read_domain(Token),
    Nodes = nodes:all(?SERVICE_PANEL),
    onepanel_env:set(Nodes, onezone_domain, OzDomain, ?APP_NAME),
    onepanel_env:write(Nodes, [?APP_NAME, onezone_domain], OzDomain,
        onepanel_env:get_config_path(?APP_NAME, generated));

configure(#{oneprovider_token := Token} = Ctx) ->
    OzDomain = onezone_tokens:read_domain(Token),
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
        {ok, ?HTTP_200_OK, _Headers, Body} ->
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
-spec check_oz_connection() -> ok | no_return().
check_oz_connection() ->
    case service_op_worker:is_connected_to_oz() of
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
        true ->
            Subdomain = service_ctx:get(oneprovider_subdomain, Ctx, binary),
            #{
                <<"subdomainDelegation">> => true,
                <<"subdomain">> => string:lowercase(Subdomain),
                <<"ipList">> => [] % IPs will be updated in the step set_cluster_ips
            };
        false ->
            Domain = service_ctx:get(oneprovider_domain, Ctx, binary),
            #{
                <<"subdomainDelegation">> => false,
                <<"domain">> => string:lowercase(Domain)
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
        ?ERROR_BAD_VALUE_TOKEN(<<"token">>) ->
            ?throw_error(?ERR_INVALID_VALUE_TOKEN);
        {error, Reason} ->
            ?throw_error(Reason);
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
    oz_providers:unregister(provider),

    {ok, Node} = nodes:any(?SERVICE_OPW),
    rpc:call(Node, oneprovider, on_deregister, []),
    onepanel_deployment:unset_marker(?PROGRESS_LETSENCRYPT_CONFIG),
    service:update_ctx(name(), fun(ServiceCtx) ->
        maps:without([cluster, cluster_id, ?DETAILS_PERSISTENCE],
            ServiceCtx#{registered => false})
    end).


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

    case maps:size(Params) of
        0 -> ok;
        _ ->
            {ok, Node} = nodes:any(?SERVICE_OPW),
            ok = rpc:call(Node, provider_logic, update, [Params])
    end.

-spec get_auth_token() -> Macaroon :: binary().
get_auth_token() ->
    OpNode = nodes:local(?SERVICE_OPW),
    case rpc:call(OpNode, provider_auth, get_auth_macaroon, []) of
        {ok, <<Macaroon/binary>>} -> Macaroon;
        ?ERROR_UNREGISTERED_PROVIDER = Error -> ?throw_error(Error);
        _ -> auth_macaroon_from_file()
    end.


%%--------------------------------------------------------------------
%% @doc Returns configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec get_details() -> #{atom() := term()} | no_return().
get_details() ->
    try
        case service_op_worker:is_connected_to_oz() of
            true -> get_details_by_graph_sync();
            false -> get_details_by_rest()
        end
    catch
        Type:#error{reason = unregistered_provider} = Error ->
            erlang:Type(Error);
        Type:Error ->
            case service:get_ctx(name()) of
                #{?DETAILS_PERSISTENCE := Cached} -> Cached;
                _ -> erlang:Type(Error)
            end
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

    case rpc:call(Node, provider_logic, support_space, [Token, SupportSize]) of
        {ok, SpaceId} ->
            configure_space(Node, SpaceId, Ctx);
        ?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, Minimum) ->
            ?throw_error(?ERR_SPACE_SUPPORT_TOO_LOW(Minimum));
        {error, Reason} ->
            ?throw_error(Reason)
    end.


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
    {ok, Node} = nodes:any(?SERVICE_OPW),
    {ok, SpaceIds} = rpc:call(Node, provider_logic, get_spaces, []),
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
-spec get_space_details(Ctx :: service:ctx()) -> proplists:proplist().
get_space_details(#{id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    {ok, #{name := Name, providers := Providers}} =
        rpc:call(Node, space_logic, get_as_map, [SpaceId]),
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
        {id, SpaceId},
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
-spec get_auto_cleaning_reports(Ctx :: service:ctx()) -> map().
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
    onepanel_deployment:set_marker([
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
        {ok, #service{ctx = #{has_letsencrypt_cert := Enabled}}} ->
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
            onepanel_deployment:set_marker(?PROGRESS_LETSENCRYPT_CONFIG),
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
    {ok, GuiHash} = gui:package_hash(https_listener:gui_package_path()),

    % Try to update version info in Onezone
    case update_version_info(GuiHash) of
        ok ->
            ?info("Skipping GUI upload as it is already present in Onezone"),
            ?info("Oneprovider panel service successfully set up in Onezone");
        {error, inexistent_gui_version} ->
            ?info("Uploading GUI to Onezone (~s)", [GuiHash]),
            case upload_onepanel_gui() of
                ok ->
                    ?info("GUI uploaded succesfully"),
                    ok = update_version_info(GuiHash),
                    ?info("Oneprovider panel service successfully set up in Onezone");
                {error, _} = Error ->
                    ?alert(
                        "Oneprovider panel service could not be successfully set "
                        "up in Onezone due to an error during GUI upload: ~p",
                        [Error]
                    )
            end
    end.

%%%===================================================================
%%% Internal RPC functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Reads provider root macaroon stored in a file
%% and returns it with an time caveat added.
%% @end
%%--------------------------------------------------------------------
-spec auth_macaroon_from_file() -> Macaroon :: binary().
auth_macaroon_from_file() ->
    Self = node(),
    % ensure correct node for reading op_worker configuration
    case nodes:onepanel_with(?SERVICE_OPW) of
        {_, Self} ->
            {ok, RootMacaroonB64} = onepanel_maps:get(root_macaroon, read_auth_file()),
            {ok, TTL} = onepanel_env:read_effective(
                [?SERVICE_OPW, provider_macaroon_ttl_sec], ?SERVICE_OPW),

            {ok, RootMacaroon} = macaroons:deserialize(RootMacaroonB64),
            Caveat = ?TIME_CAVEAT(time_utils:system_time_seconds(), TTL),
            Limited = macaroons:add_caveat(RootMacaroon, Caveat),
            {ok, Macaroon} = macaroons:serialize(Limited),
            Macaroon;
        {ok, Other} ->
            <<_/binary>> = rpc:call(Other, ?MODULE, ?FUNCTION_NAME, [])
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Reads provider Id and root macaroon from a file where they are stored.
%% @end
%%--------------------------------------------------------------------
-spec read_auth_file() -> #{provider_id := binary(), root_macaroon := binary()}.
read_auth_file() ->
    {_, Node} = nodes:onepanel_with(?SERVICE_OPW),
    Path = onepanel_env:get(op_worker_root_macaroon_path),
    case rpc:call(Node, file, consult, [Path]) of
        {ok, [Map]} -> Map;
        {error, Error} -> ?throw_error(Error)
    end.


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
    (catch get_details()),
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


%% @private
-spec get_details_by_graph_sync() -> #{atom() := term()} | no_return().
get_details_by_graph_sync() ->
    {ok, Node} = nodes:any(?SERVICE_OPW),

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

    Result = case maps:get(subdomain_delegation, Details) of
        true -> Response2#{subdomain => maps:get(subdomain, Details)};
        _ -> Response2
    end,
    service:update_ctx(name(), #{?DETAILS_PERSISTENCE => Result}),
    Result.


%% @private
-spec get_details_by_rest() -> #{atom() := term()} | no_return().
get_details_by_rest() ->
    {ok, ?HTTP_200_OK, _, ProviderDetailsBody} = oz_endpoint:request(provider, "/provider", get),
    {ok, ?HTTP_200_OK, _, DomainConfigBody} = oz_endpoint:request(provider, "/provider/domain_config", get),
    ProviderDetails = json_utils:decode(ProviderDetailsBody),
    DomainConfig = json_utils:decode(DomainConfigBody),
    OzDomain = onepanel_utils:convert(get_oz_domain(), binary),

    % NOTE: the REST endpoint returns protected data, which do not contain adminEmail
    Result1 = onepanel_maps:get_store_multiple([
        {<<"providerId">>, id},
        {<<"name">>, name},
        {<<"domain">>, domain},
        {<<"latitude">>, geoLatitude},
        {<<"longitude">>, geoLongitude}
    ], ProviderDetails, #{
        onezoneDomainName => OzDomain
    }),

    DomainConfigKeys = [
        {<<"subdomainDelegation">>, subdomainDelegation},
        {<<"domain">>, domain}
    ],
    DomainConfigKeys2 = case DomainConfig of
        #{<<"subdomainDelegation">> := true} ->
            [{<<"subdomain">>, subdomain} | DomainConfigKeys];
        #{<<"subdomainDelegation">> := false} ->
            DomainConfigKeys
    end,

    onepanel_maps:get_store_multiple(DomainConfigKeys2,
        DomainConfig, Result1).


%%--------------------------------------------------------------------
%% @private
%% @doc Modify provider details regarding its domain or subdomain.
%% @end
%%--------------------------------------------------------------------
-spec modify_domain_details(OpNode :: node(), service:ctx()) -> ok.
modify_domain_details(OpNode, #{oneprovider_subdomain_delegation := true} = Ctx) ->
    Subdomain = string:lowercase(onepanel_utils:typed_get(
        oneprovider_subdomain, Ctx, binary)),

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
    Domain = string:lowercase(onepanel_utils:typed_get(
        oneprovider_domain, Ctx, binary)),
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
-spec update_version_info(GuiHash :: binary()) -> ok | {error, inexistent_gui_version}.
update_version_info(GuiHash) ->
    {BuildVersion, AppVersion} = onepanel_app:get_build_and_version(),
    Result = oz_endpoint:request(
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
    ),

    case Result of
        {ok, ?HTTP_204_NO_CONTENT, _, _} -> ok;
        {ok, ?HTTP_400_BAD_REQUEST, _, _} -> {error, inexistent_gui_version}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to upload Onepanel GUI package to Onezone.
%% @end
%%--------------------------------------------------------------------
-spec upload_onepanel_gui() -> ok | {error, term()}.
upload_onepanel_gui() ->
    GuiPrefix = onedata:gui_prefix(?ONEPANEL_GUI),
    Result = oz_endpoint:request(
        provider,
        str_utils:format("/~s/~s/gui-upload", [GuiPrefix, clusters:get_id()]),
        post,
        {multipart, [{file, str_utils:to_binary(https_listener:gui_package_path())}]},
        [{endpoint, gui}]
    ),

    case Result of
        {ok, ?HTTP_200_OK, _, _} ->
            ok;
        Other ->
            try
                {ok, ?HTTP_400_BAD_REQUEST, _, Body} = Other,
                {error, json_utils:decode(Body)}
            catch _:_ ->
                {error, {unexpected_gui_upload_result, Other}}
            end
    end.
