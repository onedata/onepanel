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
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/oz/oz_spaces.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-include("http/rest.hrl").

-define(DETAILS_PERSISTENCE, provider_details).

-define(OZ_CONNECTION_CHECK_INTERVAL_SECONDS, 5).

% @formatter:off
-type id() :: binary().

-type model_ctx() :: #{
    % dedicated host for initiating cluster startup process after restart
    master_host => service:host(),

    % set after registration
    onezone_domain => binary(),

    %% Caches (i.e. not the primary source of truth):
    % is_registered cache (op_worker's datastore is the source of truth)
    registered => boolean(),
    % service status
    status => #{service:host() => service:status()},
    % 'dns_check' module cache
    ?DNS_CHECK_TIMESTAMP_KEY => clock:seconds(),
    ?DNS_CHECK_CACHE_KEY => dns_check:result(),
    % 'clusters' module cache
    cluster => #{atom() := term()},
    % provider details cache (for when op_worker is down)
    ?DETAILS_PERSISTENCE => #{atom() := term()},

    %% Deprecated
    % removed after upgrading past 18.02.0-beta5
    % see {@link pop_legacy_letsencrypt_config/0}
    has_letsencrypt_cert => boolean(),
    % emptied (not removed) after upgrading past 18.02.0-rc1
    % see {@link pop_legacy_letsencrypt_config/0}
    configured => gb_sets:set(atom())
}.
% @formatter:on

-export_type([id/0, model_ctx/0]).


%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

-export([get_details_by_graph_sync/0, get_details_by_rest/0]).

%% API
-export([check_oz_availability/1, mark_configured/0,
    register/1, unregister/0, is_registered/1, is_registered/0,
    modify_details/1, get_details/0, get_oz_domain/0,
    support_space/1, revoke_space_support/1, get_spaces/0, is_space_supported/1,
    get_space_details/1, modify_space/1, format_cluster_ips/1,
    get_auto_storage_import_stats/1, get_auto_storage_import_info/1, get_manual_storage_import_example/1,
    get_auto_cleaning_reports/1, get_auto_cleaning_report/1,
    get_auto_cleaning_status/1, start_auto_cleaning/1, cancel_auto_cleaning/1,
    force_start_auto_storage_import_scan/1, force_stop_auto_storage_import_scan/1,
    check_oz_connection/0,
    update_provider_ips/0, configure_file_popularity/1, configure_auto_cleaning/1,
    get_file_popularity_configuration/1, get_auto_cleaning_configuration/1]).
-export([connect_and_set_up_in_onezone/1]).
-export([store_absolute_auth_file_path/0]).
-export([pop_legacy_letsencrypt_config/0]).
-export([get_id/0, get_access_token/0, get_identity_token/0]).

% Internal RPC
-export([root_token_from_file/0]).
% Export for eunit tests
-export([set_up_onepanel_in_onezone/0]).

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
        service:get_hosts(?SERVICE_OPW),
        service:get_hosts(?SERVICE_CEPH)
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
-spec get_steps(Action :: service:action(), Args :: service:step_ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, Ctx) ->
    OpaCtx = kv_utils:get([cluster, ?SERVICE_PANEL], Ctx),
    CbCtx = kv_utils:get([cluster, ?SERVICE_CB], Ctx),
    CmCtx = kv_utils:get([cluster, ?SERVICE_CM], Ctx),
    OpwCtx = kv_utils:get([cluster, ?SERVICE_OPW], Ctx),
    LeCtx = kv_utils:get([cluster, ?SERVICE_LE], Ctx),
    CephCtx = kv_utils:get([ceph], Ctx, #{}),
    StorageCtx = kv_utils:get([cluster, storages], Ctx, #{}),
    OpCtx = kv_utils:get(name(), Ctx, #{}),

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
        Ss#steps{service = ?SERVICE_CEPH, action = deploy,
            ctx = CephCtx, condition = CephCtx /= #{}},
        Ss#steps{service = ?SERVICE_LE, action = deploy, ctx = LeCtx3},
        S#step{module = onepanel_deployment, function = set_marker,
            args = [?PROGRESS_CLUSTER], hosts = [SelfHost]},
        Ss#steps{action = register, ctx = OpCtx, condition = Register},
        Ss#steps{service = ?SERVICE_OPW, action = add_storages, ctx = StorageCtx},
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
        #steps{service = ?SERVICE_CB, action = resume},
        #steps{service = ?SERVICE_CM, action = resume},
        #steps{service = ?SERVICE_OPW, action = resume}
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
            #step{function = connect_and_set_up_in_onezone, args = [fallback_to_async], selection = any},
            #step{function = store_absolute_auth_file_path, args = [], selection = any},
            #steps{service = ?SERVICE_LE, action = resume,
                ctx = Ctx#{letsencrypt_plugin => ?SERVICE_OPW}},
            #steps{service = ?SERVICE_CEPH, action = resume}
        ];
        false ->
            ?info("Waiting for master node \"~s\" to start the Oneprovider", [MasterHost]),
            []
    end;

get_steps(status, _Ctx) ->
    [
        #steps{service = ?SERVICE_CB, action = status},
        #steps{service = ?SERVICE_CM, action = status},
        #steps{service = ?SERVICE_OPW, action = status}
    ];

get_steps(register, #{hosts := _Hosts}) ->
    [
        #step{function = check_oz_availability, attempts = onepanel_env:get(connect_to_onezone_attempts)},
        #step{function = register, selection = any},
        #step{function = connect_and_set_up_in_onezone, args = [no_fallback], selection = any},
        % explicitly fail on connection problems before executing further steps
        #step{function = check_oz_connection, args = [],
            attempts = onepanel_env:get(connect_to_onezone_attempts)},
        #steps{action = set_cluster_ips}
    ];
get_steps(register, Ctx) ->
    get_steps(register, Ctx#{hosts => hosts:all(?SERVICE_OPW)});

get_steps(unregister, _Ctx) ->
    [#step{function = unregister, selection = any, args = []}];

get_steps(modify_details, #{hosts := Hosts}) ->
    [
        #step{hosts = Hosts, function = modify_details, selection = any}
    ];
get_steps(modify_details, Ctx) ->
    get_steps(modify_details, Ctx#{hosts => hosts:all(?SERVICE_OPW)});


get_steps(set_cluster_ips, #{hosts := Hosts} = Ctx) ->
    Ctx2 = Ctx#{name => ?SERVICE_OPW},
    [
        #steps{action = set_cluster_ips, ctx = Ctx2, service = ?SERVICE_CW},
        #step{function = update_provider_ips, selection = any,
            hosts = Hosts, args = []}
    ];
get_steps(set_cluster_ips, Ctx) ->
    get_steps(set_cluster_ips, Ctx#{hosts => hosts:all(?SERVICE_OPW)});

get_steps(Action, _Ctx) when
    Action =:= get_spaces;
    Action =:= get_details ->
    [#step{function = Action, args = [], selection = any}];

get_steps(Action, Ctx) when
    Action =:= support_space;
    Action =:= revoke_space_support;
    Action =:= get_space_details;
    Action =:= modify_space;
    Action =:= get_auto_cleaning_reports;
    Action =:= get_auto_cleaning_report;
    Action =:= get_auto_cleaning_status;
    Action =:= get_auto_cleaning_configuration;
    Action =:= get_file_popularity_configuration;
    Action =:= format_cluster_ips;
    Action =:= start_auto_cleaning;
    Action =:= cancel_auto_cleaning;
    Action =:= force_start_auto_storage_import_scan;
    Action =:= force_stop_auto_storage_import_scan;
    Action =:= get_auto_storage_import_stats;
    Action =:= get_auto_storage_import_info;
    Action =:= get_manual_storage_import_example;
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
-spec get_id() -> id().
get_id() ->
    service_oneprovider:is_registered()
        orelse throw(?ERROR_UNREGISTERED_ONEPROVIDER),
    case op_worker_rpc:get_provider_id() of
        {ok, <<ProviderId/binary>>} ->
            ProviderId;
        ?ERROR_UNREGISTERED_ONEPROVIDER = Error ->
            throw(Error);
        _ ->
            FileContents = read_auth_file(),
            maps:get(provider_id, FileContents)
    end.


%%--------------------------------------------------------------------
%% @doc Returns domain of Onezone to which this Oneprovider belongs.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_domain() -> string().
get_oz_domain() ->
    is_registered()
        orelse throw(?ERROR_UNREGISTERED_ONEPROVIDER),
    case service:get_ctx(name()) of
        #{onezone_domain := OnezoneDomain} ->
            unicode:characters_to_list(OnezoneDomain);
        #{} ->
            % versions before 20.02 stored the onezone_domain in app config
            OnezoneDomain = onepanel_env:typed_get(onezone_domain, binary),
            service:update_ctx(name(), #{onezone_domain => OnezoneDomain}),
            get_oz_domain()
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
-spec is_registered(Ctx :: service:step_ctx()) -> boolean().
is_registered(#{node := Node}) ->
    case op_worker_rpc:is_registered(Node) of
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


-spec get_access_token() -> tokens:serialized().
get_access_token() ->
    case op_worker_rpc:get_access_token() of
        {ok, <<Token/binary>>} -> Token;
        ?ERROR_UNREGISTERED_ONEPROVIDER = Error -> throw(Error);
        _ -> root_token_from_file()
    end.


-spec get_identity_token() -> tokens:serialized().
get_identity_token() ->
    case op_worker_rpc:get_identity_token() of
        {ok, <<Token/binary>>} ->
            Token;
        ?ERROR_UNREGISTERED_ONEPROVIDER = Err1 ->
            throw(Err1);
        _ ->
            case clusters:acquire_provider_identity_token() of
                {ok, T} -> T;
                {error, _} = Err2 -> throw(Err2)
            end
    end.


%%%===================================================================
%%% Step functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Checks if onezone is available at given address
%% @end
%%--------------------------------------------------------------------
-spec check_oz_availability(Ctx :: service:step_ctx()) -> ok | no_return().
check_oz_availability(Ctx) ->
    Protocol = onepanel_env:get(oz_worker_nagios_protocol),
    Port = onepanel_env:get(oz_worker_nagios_port),
    OzDomain = maps:get(onezone_domain, Ctx),
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
                "ok" ->
                    ok;
                _ ->
                    ?debug("Onezone availability check failed - nagios status was ~p", [Status]),
                    throw(?ERROR_NO_CONNECTION_TO_ONEZONE)
            end;
        Other ->
            ?debug("Onezone availability check failed - ~p", [Other]),
            throw(?ERROR_NO_CONNECTION_TO_ONEZONE)
    end.


%%--------------------------------------------------------------------
%% @doc Checks if Oneprovider is registered and connected to onezone
%% @end
%%--------------------------------------------------------------------
-spec check_oz_connection() -> ok | no_return().
check_oz_connection() ->
    case service_op_worker:is_connected_to_oz() of
        true -> ok;
        false -> throw(?ERROR_NO_CONNECTION_TO_ONEZONE)
    end.


%%--------------------------------------------------------------------
%% @doc Registers provider in the zone.
%% @end
%%--------------------------------------------------------------------
-spec register(Ctx :: service:step_ctx()) ->
    {ok, ProviderId :: id()} | no_return().
register(Ctx) ->
    {ok, OpwNode} = nodes:any(?SERVICE_OPW),

    OnezoneDomain = maps:get(onezone_domain, Ctx),
    DomainParams = case onepanel_utils:get_converted(oneprovider_subdomain_delegation, Ctx, boolean, false) of
        true ->
            Subdomain = onepanel_utils:get_converted(oneprovider_subdomain, Ctx, binary),
            #{
                <<"subdomainDelegation">> => true,
                <<"subdomain">> => string:lowercase(Subdomain),
                <<"ipList">> => [] % IPs will be updated in the step set_cluster_ips
            };
        false ->
            Domain = onepanel_utils:get_converted(oneprovider_domain, Ctx, binary),
            #{
                <<"subdomainDelegation">> => false,
                <<"domain">> => string:lowercase(Domain)
            }

    end,

    Params = DomainParams#{
        <<"token">> => onepanel_utils:get_converted(oneprovider_token, Ctx, binary),
        <<"name">> => onepanel_utils:get_converted(oneprovider_name, Ctx, binary),
        <<"adminEmail">> => onepanel_utils:get_converted(oneprovider_admin_email, Ctx, binary),
        <<"latitude">> => onepanel_utils:get_converted(oneprovider_geo_latitude, Ctx, float, 0.0),
        <<"longitude">> => onepanel_utils:get_converted(oneprovider_geo_longitude, Ctx, float, 0.0)
    },

    case onezone_client:register_provider(OnezoneDomain, Params) of
        {ok, #{provider_id := ProviderId, root_token := RootToken}} ->
            on_registered(OpwNode, ProviderId, RootToken, OnezoneDomain),
            {ok, ProviderId};
        {error, _} = Error ->
            throw(Error)
    end.


%%--------------------------------------------------------------------
%% @doc Unregisters provider in the zone.
%% @end
%%--------------------------------------------------------------------
-spec unregister() -> ok | no_return().
unregister() ->
    oz_providers:unregister(provider),

    op_worker_rpc:on_deregister(),
    onepanel_deployment:unset_marker(?PROGRESS_LETSENCRYPT_CONFIG),
    service:update_ctx(name(), fun(ServiceCtx) ->
        maps:without([cluster, onezone_domain, ?DETAILS_PERSISTENCE],
            ServiceCtx#{registered => false})
    end).


%%--------------------------------------------------------------------
%% @doc Modifies configuration details of the provider.
%% @end
%%--------------------------------------------------------------------
-spec modify_details(Ctx :: service:step_ctx()) -> ok | no_return().
modify_details(Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    ok = modify_domain_details(Node, Ctx),

    Params = kv_utils:copy_found([
        {oneprovider_name, <<"name">>},
        {oneprovider_geo_latitude, <<"latitude">>},
        {oneprovider_geo_longitude, <<"longitude">>},
        {oneprovider_admin_email, <<"adminEmail">>}
    ], Ctx, #{}),

    case maps:size(Params) of
        0 -> ok;
        _ -> ok = op_worker_rpc:provider_logic_update(Params)
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
        Type:?ERROR_UNREGISTERED_ONEPROVIDER = Error ->
            erlang:raise(Type, Error, erlang:get_stacktrace());
        Type:Error ->
            case service:get_ctx(name()) of
                #{?DETAILS_PERSISTENCE := Cached} -> Cached;
                _ -> erlang:raise(Type, Error, erlang:get_stacktrace())
            end
    end.


%%--------------------------------------------------------------------
%% @doc Returns IPs of hosts with op_worker instances.
%% @end
%%--------------------------------------------------------------------
-spec format_cluster_ips(service:step_ctx()) ->
    #{isConfigured := boolean(), hosts := #{binary() => binary()}}.
format_cluster_ips(Ctx) ->
    service_cluster_worker:get_cluster_ips(Ctx#{name => ?SERVICE_OPW}).


%%--------------------------------------------------------------------
%% @doc Supports space with selected storage.
%% @end
%%--------------------------------------------------------------------
-spec support_space(service:step_ctx()) -> SpaceId :: binary().
support_space(#{storage_id := StorageId} = Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    assert_storage_exists(Node, StorageId),
    SupportSize = onepanel_utils:get_converted(size, Ctx, binary),
    Token = onepanel_utils:get_converted(token, Ctx, binary),

    case op_worker_rpc:support_space(StorageId, Token, SupportSize) of
        {ok, SpaceId} -> configure_space(Node, SpaceId, StorageId, Ctx);
        Error -> throw(Error)
    end.


%%--------------------------------------------------------------------
%% @doc Revokes support for the space given by ID.
%% @end
%%--------------------------------------------------------------------
-spec revoke_space_support(Ctx :: service:step_ctx()) -> ok.
revoke_space_support(#{id := SpaceId}) ->
    ok = op_worker_rpc:revoke_space_support(SpaceId).


%%--------------------------------------------------------------------
%% @doc Returns list of spaces supported by the provider.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces() -> [binary()].
get_spaces() ->
    {ok, SpaceIds} = op_worker_rpc:get_spaces(),
    SpaceIds.


%%--------------------------------------------------------------------
%% @doc Calls op_worker to check if a space is supported.
%% @end
%%--------------------------------------------------------------------
-spec is_space_supported(Ctx :: service:step_ctx()) -> boolean().
is_space_supported(#{space_id := Id}) ->
    op_worker_rpc:supports_space(Id).


%%--------------------------------------------------------------------
%% @doc Returns details of the space given by ID.
%% @end
%%--------------------------------------------------------------------
-spec get_space_details(Ctx :: service:step_ctx()) -> json_utils:json_term().
get_space_details(#{id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    {Name, Providers} = case op_worker_rpc:get_space_details(Node, SpaceId) of
        {ok, #{name := Name0, providers := Providers0}} -> {Name0, Providers0};
        {error, _} = Error -> throw(Error)
    end,
    {ok, StorageIds} = op_worker_storage:get_supporting_storages(Node, SpaceId),
    StorageId = hd(StorageIds),
    ImportedStorage = op_worker_storage:is_imported_storage(Node, StorageId),
    StorageImportDetails = op_worker_storage_import:get_storage_import_details(Node, SpaceId),
    CurrentSize = op_worker_rpc:space_quota_current_size(Node, SpaceId),
    maps_utils:remove_undefined(#{
        id => SpaceId,
        importedStorage => ImportedStorage,
        localStorages => StorageIds,
        name => Name,
        spaceOccupancy => CurrentSize,
        storageId => StorageId,
        storageImport => StorageImportDetails,
        supportingProviders => Providers
    }).


%%--------------------------------------------------------------------
%% @doc Modifies space details.
%% @end
%%--------------------------------------------------------------------
-spec modify_space(Ctx :: service:step_ctx()) -> #{id => op_worker_rpc:od_space_id()}.
modify_space(#{space_id := SpaceId} = Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    AutoStorageImportConfig = maps:get(auto_storage_import_config, Ctx, #{}),
    ok = maybe_update_support_size(Node, SpaceId, Ctx),
    op_worker_storage_import:maybe_reconfigure_storage_import(Node, SpaceId, AutoStorageImportConfig),
    #{id => SpaceId}.


%%--------------------------------------------------------------------
%% @private
%% @doc If new size for a space is specified, enacts the change.
%% @end
%%--------------------------------------------------------------------
maybe_update_support_size(OpNode, SpaceId, #{size := SupportSize}) ->
    case op_worker_rpc:update_space_support_size(OpNode, SpaceId, SupportSize) of
        ok -> ok;
        Error -> throw(Error)
    end;

maybe_update_support_size(_OpNode, _SpaceId, _Ctx) -> ok.


-spec get_auto_storage_import_stats(Ctx :: service:step_ctx()) -> json_utils:json_term().
get_auto_storage_import_stats(#{space_id := SpaceId} = Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    Period = onepanel_utils:get_converted(period, Ctx, binary),
    MetricsJoined = onepanel_utils:get_converted(metrics, Ctx, binary),
    Metrics = binary:split(MetricsJoined, <<",">>, [global, trim]),
    op_worker_storage_import:get_stats(Node, SpaceId, Period, Metrics).


-spec get_auto_storage_import_info(Ctx :: service:step_ctx()) -> json_utils:json_term().
get_auto_storage_import_info(#{space_id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    op_worker_storage_import:get_info(Node, SpaceId).


-spec get_manual_storage_import_example(Ctx :: service:step_ctx()) -> json_utils:json_term().
get_manual_storage_import_example(#{space_id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    op_worker_storage_import:get_manual_example(Node, SpaceId).


%%--------------------------------------------------------------------
%% @doc
%% Notify onezone about IPs change.
%% @end
%%--------------------------------------------------------------------
-spec update_provider_ips() -> ok.
update_provider_ips() ->
    case is_registered() of
        true ->
            % no check of results - if the provider is not connected to onezone
            % the IPs will be sent automatically after connection is acquired
            op_worker_rpc:update_subdomain_delegation_ips(),
            ok;
        false -> ok
    end.


%%-------------------------------------------------------------------
%% @doc
%% Returns list of auto-cleaning runs reports started since Since date.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_reports(Ctx :: service:step_ctx()) -> [ReportId :: binary()].
get_auto_cleaning_reports(Ctx = #{space_id := SpaceId}) ->
    Offset = onepanel_utils:get_converted(offset, Ctx, integer, 0),
    Limit = onepanel_utils:get_converted(limit, Ctx, integer, all),
    Index = onepanel_utils:get_converted(index, Ctx, binary, undefined),
    {ok, Ids} = op_worker_rpc:autocleaning_list_reports(
        SpaceId, Index, Offset, Limit),
    Ids.


%%-------------------------------------------------------------------
%% @doc
%% Returns auto-cleaning run report.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_report(Ctx :: service:step_ctx()) -> #{atom() => term()}.
get_auto_cleaning_report(#{report_id := ReportId}) ->
    case op_worker_rpc:autocleaning_get_run_report(ReportId) of
        {ok, Report} ->
            maps_utils:undefined_to_null(
                kv_utils:copy_found([
                    {id, id},
                    {index, index},
                    {started_at, startedAt},
                    {stopped_at, stoppedAt},
                    {released_bytes, releasedBytes},
                    {bytes_to_release, bytesToRelease},
                    {files_number, filesNumber},
                    {status, status}
                ], Report));
        {error, _} = Error ->
            throw(Error)
    end.


%%-------------------------------------------------------------------
%% @doc
%% Returns status of current working auto-cleaning process for given space.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_status(Ctx :: service:step_ctx()) -> #{atom() => term()}.
get_auto_cleaning_status(#{space_id := SpaceId}) ->
    Status = op_worker_rpc:autocleaning_status(SpaceId),
    kv_utils:copy_found([
        {in_progress, inProgress},
        {space_occupancy, spaceOccupancy}
    ], Status).


%%-------------------------------------------------------------------
%% @doc
%% Returns configuration of auto-cleaning mechanism in given space.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_configuration(Ctx :: service:step_ctx()) -> #{atom() => term()}.
get_auto_cleaning_configuration(#{space_id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    op_worker_storage:get_auto_cleaning_configuration(Node, SpaceId).


%%-------------------------------------------------------------------
%% @doc
%% Returns configuration of file-popularity mechanism in given space.
%% @end
%%-------------------------------------------------------------------
-spec get_file_popularity_configuration(Ctx :: service:step_ctx()) -> #{atom() => term()}.
get_file_popularity_configuration(#{space_id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    op_worker_storage:get_file_popularity_configuration(Node, SpaceId).


%%-------------------------------------------------------------------
%% @doc
%% Manually starts auto-cleaning of given space.
%% @end
%%-------------------------------------------------------------------
-spec start_auto_cleaning(Ctx :: service:step_ctx()) ->
    {ok, ReportId :: binary()} | no_need.
start_auto_cleaning(#{space_id := SpaceId}) ->
    case op_worker_rpc:autocleaning_force_run(SpaceId) of
        {ok, ReportId} -> {ok, ReportId};
        {error, {already_started, _}} -> no_need;
        {error, nothing_to_clean} -> no_need;
        {error, _} = Error -> throw(Error)
    end.


%%-------------------------------------------------------------------
%% @doc
%% Manually cancels auto-cleaning of given space.
%% @end
%%-------------------------------------------------------------------
-spec cancel_auto_cleaning(Ctx :: service:step_ctx()) -> ok.
cancel_auto_cleaning(#{space_id := SpaceId}) ->
    ok = op_worker_rpc:autocleaning_cancel_run(SpaceId).


%%-------------------------------------------------------------------
%% @doc
%% Manually starts scan of storage import mechanism.
%% @end
%%-------------------------------------------------------------------
-spec force_start_auto_storage_import_scan(Ctx :: service:step_ctx()) -> ok.
force_start_auto_storage_import_scan(#{space_id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    ok = op_worker_storage_import:start_scan(Node, SpaceId).


%%-------------------------------------------------------------------
%% @doc
%% Manually stops scan of storage import mechanism.
%% @end
%%-------------------------------------------------------------------
-spec force_stop_auto_storage_import_scan(Ctx :: service:step_ctx()) -> ok.
force_stop_auto_storage_import_scan(#{space_id := SpaceId}) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    ok = op_worker_storage_import:stop_scan(Node, SpaceId).


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
-spec configure_file_popularity(Ctx :: service:step_ctx()) -> list().
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
-spec configure_auto_cleaning(Ctx :: service:step_ctx()) -> list().
configure_auto_cleaning(#{space_id := SpaceId} = Ctx) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    Config = maps:without([space_id, hosts], Ctx),
    ok = op_worker_storage:maybe_update_auto_cleaning(Node, SpaceId, Config),
    [{id, SpaceId}].


-spec connect_and_set_up_in_onezone(no_fallback | fallback_to_async) -> ok | no_return().
connect_and_set_up_in_onezone(FallbackPolicy) ->
    OnezoneDomain = get_oz_domain(),
    ?notice("Trying to connect to Onezone (~ts)...", [OnezoneDomain]),
    case {try_to_establish_onezone_connection(), FallbackPolicy} of
        {true, _} ->
            set_up_in_onezone();
        {false, no_fallback} ->
            ?error("Failed to establish Onezone connection"),
            error(failed_to_establish_onezone_connection);
        {false, fallback_to_async} ->
            PeriodicCheck = fun() ->
                case try_to_establish_onezone_connection() of
                    true ->
                        set_up_in_onezone(),
                        onepanel_cron:remove_job(?FUNCTION_NAME);
                    false ->
                        {_, Time} = time_format:seconds_to_datetime(clock:timestamp_seconds()),
                        case Time of
                            {_, Min, Sec} when Min rem 5 == 0 andalso Sec < ?OZ_CONNECTION_CHECK_INTERVAL_SECONDS ->
                                % log every 5 minutes (at the first check within the minute)
                                ?warning(
                                    "Onezone connection cannot be established, is the service online (~ts)? "
                                    "Retrying as long as it takes...", [OnezoneDomain]
                                );
                            _ ->
                                ok
                        end
                end
            end,
            onepanel_cron:add_job(?FUNCTION_NAME, PeriodicCheck, timer:seconds(?OZ_CONNECTION_CHECK_INTERVAL_SECONDS))
    end.


%%--------------------------------------------------------------------
%% @doc
%% Stores absolute path to oneprovider token file in Onepanel config.
%% This operation requires op-worker to be up.
%% This function should be executed after cluster deployment, and after
%% upgrading to version 19.10 when the path variable name and contents
%% have changed.
%%
%% The absolute path is mostly relevant to one-env source deployments,
%% since in production the default in app.config is already absolute.
%% @end
%%--------------------------------------------------------------------
-spec store_absolute_auth_file_path() -> ok.
store_absolute_auth_file_path() ->
    PanelNodes = nodes:all(?SERVICE_PANEL),
    RootTokenPath = op_worker_rpc:get_root_token_file_path(),
    onepanel_env:write(PanelNodes,
        [?SERVICE_PANEL, op_worker_root_token_path], RootTokenPath, ?SERVICE_PANEL),
    onepanel_env:set(PanelNodes,
        op_worker_root_token_path, RootTokenPath, ?APP_NAME),
    ok.


%%%===================================================================
%%% Internal RPC functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Reads provider root token stored in a file
%% and returns it with a time caveat added.
%% @end
%%--------------------------------------------------------------------
-spec root_token_from_file() -> tokens:serialized().
root_token_from_file() ->
    Self = node(),
    % ensure correct node for reading op_worker configuration
    case nodes:onepanel_with(?SERVICE_OPW) of
        {_, Self} ->
            RootTokenBin = maps:get(root_token, read_auth_file()),
            {ok, TTL} = onepanel_env:read_effective(
                [?SERVICE_OPW, provider_token_ttl_sec], ?SERVICE_OPW),
            Now = clock:timestamp_seconds(),
            tokens:confine(RootTokenBin, #cv_time{valid_until = Now + TTL});
        {ok, Other} ->
            <<_/binary>> = rpc:call(Other, ?MODULE, ?FUNCTION_NAME, [])
    end.


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
-spec on_registered(OpwNode :: node(), ProviderId :: id(),
    tokens:serialized(), OnezoneDomain :: binary()) -> ok.
on_registered(OpwNode, ProviderId, RootToken, OnezoneDomain) ->
    OpwNodes = nodes:all(?SERVICE_OPW),
    OnezoneDomainStr = unicode:characters_to_list(OnezoneDomain),
    ok = onepanel_env:set_remote(OpwNodes, oz_domain,
        OnezoneDomainStr, ?SERVICE_OPW),
    ok = onepanel_env:write([?SERVICE_OPW, oz_domain],
        OnezoneDomainStr, ?SERVICE_OPW),
    ok = op_worker_rpc:provider_auth_save(OpwNode, ProviderId, RootToken),
    ?info("Oneprovider registered in Onezone ~ts", [OnezoneDomain]),

    service:update_ctx(name(), #{
        registered => true,
        onezone_domain => OnezoneDomain
    }),
    store_absolute_auth_file_path(),

    % preload cache
    (catch clusters:get_current_cluster()),
    (catch get_details()),
    ok.


%% @private
-spec try_to_establish_onezone_connection() -> boolean().
try_to_establish_onezone_connection() ->
    {ok, #service{ctx = Ctx}} = service:get(name()),
    case (catch check_oz_availability(Ctx)) of
        ok ->
            ?notice("Onezone connection established"),
            true;
        _ ->
            false
    end.


%% @private
-spec set_up_in_onezone() -> ok.
set_up_in_onezone() ->
    set_up_onepanel_in_onezone(),
    oneprovider_cluster_clocks:restart_periodic_sync(),
    % connection can be started only after clocks are synchronized
    {ok, OpwNode} = nodes:any(service_op_worker:name()),
    op_worker_rpc:force_oz_connection_start(OpwNode),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets up Oneprovider panel service in Onezone - updates version info
%% (release, build and GUI versions). If given GUI version is not present in
%% Onezone, the GUI package is uploaded first.
%% @end
%%--------------------------------------------------------------------
-spec set_up_onepanel_in_onezone() -> ok.
set_up_onepanel_in_onezone() ->
    ?info("Setting up Oneprovider panel service in Onezone"),
    {ok, GuiHash} = gui:package_hash(https_listener:gui_package_path()),

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


%%--------------------------------------------------------------------
%% @private
%% @doc Reads provider Id and root token from a file where they are stored.
%% @end
%%--------------------------------------------------------------------
-spec read_auth_file() -> #{provider_id := id(), root_token := binary()}.
read_auth_file() ->
    {_, Node} = nodes:onepanel_with(?SERVICE_OPW),
    Path = onepanel_env:get(op_worker_root_token_path),
    case rpc:call(Node, file, read_file, [Path]) of
        {ok, Json} ->
            kv_utils:copy_all([
                {<<"provider_id">>, provider_id},
                {<<"root_token">>, root_token}
            ], json_utils:decode(Json), #{});
        {error, Error} -> throw(?ERROR_FILE_ACCESS(Path, Error))
    end.


%% @private
-spec get_details_by_graph_sync() -> #{atom() := term()} | no_return().
get_details_by_graph_sync() ->
    OzDomain = get_oz_domain(),
    Details = case op_worker_rpc:get_provider_details() of
        {ok, DetailsMap} -> DetailsMap;
        {error, _} = Error -> throw(Error)
    end,

    #{latitude := Latitude, longitude := Longitude} = Details,

    Response = kv_utils:copy_found([
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
    Result1 = kv_utils:copy_found([
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

    kv_utils:copy_found(DomainConfigKeys2, DomainConfig, Result1).


%%--------------------------------------------------------------------
%% @private
%% @doc Modify provider details regarding its domain or subdomain.
%% @end
%%--------------------------------------------------------------------
-spec modify_domain_details(OpNode :: node(), service:step_ctx()) -> ok.
modify_domain_details(OpNode, #{oneprovider_subdomain_delegation := true} = Ctx) ->
    Subdomain = string:lowercase(onepanel_utils:get_converted(
        oneprovider_subdomain, Ctx, binary)),

    case op_worker_rpc:is_subdomain_delegated(OpNode) of
        {true, Subdomain} -> ok; % no change
        _ ->
            case op_worker_rpc:set_delegated_subdomain(OpNode, Subdomain) of
                ok -> dns_check:invalidate_cache(op_worker);
                Error -> throw(Error)
            end
    end;

modify_domain_details(OpNode, #{oneprovider_subdomain_delegation := false} = Ctx) ->
    Domain = string:lowercase(onepanel_utils:get_converted(
        oneprovider_domain, Ctx, binary)),
    ok = op_worker_rpc:set_domain(OpNode, Domain),
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
    case op_worker_rpc:storage_exists(Node, StorageId) of
        true -> ok;
        _ -> throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"storageId">>))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Configures storage of a supported space.
%% @end
%%--------------------------------------------------------------------
-spec configure_space(OpNode :: node(), SpaceId :: binary(), StorageId :: binary(),
    Ctx :: service:step_ctx()) -> Id :: binary().
configure_space(Node, SpaceId, StorageId, Ctx) ->
    case op_worker_storage:is_imported_storage(Node, StorageId) of
        true ->
            StorageImportConfig = maps:get(storage_import, Ctx, #{}),
            op_worker_storage_import:maybe_configure_storage_import(Node, SpaceId, StorageImportConfig);
        false ->
            ok
    end,
    SpaceId.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends Onepanel version info to Onezone where the provider is registered.
%% @end
%%--------------------------------------------------------------------
-spec update_version_info(GuiHash :: binary()) -> ok | {error, inexistent_gui_version}.
update_version_info(GuiHash) ->
    {BuildVersion, AppVersion} = onepanel:get_build_and_version(),
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
        {ok, ?HTTP_400_BAD_REQUEST, _, _} -> {error, inexistent_gui_version};
        {error, _} -> throw(?ERROR_NO_CONNECTION_TO_ONEZONE)
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
