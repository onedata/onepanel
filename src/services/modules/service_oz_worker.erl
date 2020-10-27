%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains oz_worker service management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_oz_worker).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).
-behaviour(letsencrypt_plugin_behaviour).

-include("names.hrl").
-include("deployment_progress.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include("authentication.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

% @formatter:off
-type model_ctx() :: #{
    %% Caches (i.e. not the primary source of truth):
    % service status cache
    status => #{service:host() => service:status()}
}.

-type policies() :: #{
    oneprovider_registration => open | restricted | binary(),
    subdomain_delegation => boolean(),
    gui_package_verification => boolean(),
    harvester_gui_package_verification => boolean()
}.
% @formatter:on

-export_type([model_ctx/0]).

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).
%% LE behaviour callbacks
-export([set_txt_record/1, remove_txt_record/1, get_dns_server/0,
    reload_webcert/1, get_domain/0, get_admin_email/0, set_http_record/2,
    supports_letsencrypt_challenge/1]).

%% API functions
-export([get_auth_by_token/2]).
-export([get_user_details/1]).

%% Step functions
-export([configure/1, start/1, stop/1, status/1, health/1, wait_for_init/1,
    configure_additional_node/1,
    get_nagios_response/1, get_nagios_status/1]).
-export([reconcile_dns/1, get_ns_hosts/0]).
-export([migrate_generated_config/1, rename_variables/0]).
-export([get_policies/0, set_policies/1]).
-export([get_details/1, get_details/0]).

-define(DETAILS_CACHE_KEY, onezone_details).
-define(DETAILS_CACHE_TTL, timer:minutes(1)).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    ?SERVICE_OZW.


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_hosts/0}
%% @end
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    service:get_hosts(name()).


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
get_steps(add_nodes, #{new_hosts := NewHosts} = Ctx) ->
    case get_hosts() of
        [] ->
            {ok, Ctx2} = kv_utils:rename_entry(new_hosts, hosts, Ctx),
            [#steps{action = deploy, ctx = Ctx2}];
        [ExistingHost | _] ->
            Ctx2 = Ctx#{name => name(), reference_host => ExistingHost},
            [
                #step{function = configure_additional_node, hosts = NewHosts, ctx = Ctx2}
                | service_cluster_worker:add_nodes_steps(Ctx2)
            ]
    end;

get_steps(get_policies, _Ctx) ->
    [#step{function = get_policies, selection = any, args = []}];

get_steps(set_policies, _Ctx) ->
    [#step{function = set_policies, selection = all}];

get_steps(configure_dns_check, Ctx) ->
    [
        #steps{action = set_policies, ctx = Ctx#{subdomain_delegation => false},
            condition = fun(Ctx2) -> not maps:get(built_in_dns_server, Ctx2, true) end}
        | service_cluster_worker:get_steps(configure_dns_check, Ctx)
    ];

get_steps(resume, Ctx) ->
    [
        #step{function = rename_variables, selection = all, args = []}
        | service_cluster_worker:get_steps(resume, Ctx#{name => name()})
    ];

get_steps(Action, Ctx) ->
    service_cluster_worker:get_steps(Action, Ctx#{name => name()}).


%%%===================================================================
%%% Public API
%%%===================================================================


-spec get_auth_by_token(AccessToken :: binary(), ip_utils:ip()) ->
    {ok, aai:auth()} | errors:error().
get_auth_by_token(AccessToken, PeerIp) ->
    case nodes:any(name()) of
        {ok, OzNode} ->
            case oz_worker_rpc:authenticate_by_token(OzNode, AccessToken, PeerIp) of
                {true, Auth} -> {ok, Auth};
                {error, _} = Error -> Error
            end;
        Error -> Error
    end.


-spec get_user_details(aai:auth()) -> {ok, #user_details{}} | errors:error().
get_user_details(Auth) ->
    case oz_worker_rpc:get_user_details(Auth) of
        {ok, User} -> {ok, User};
        {error, _} = Error -> Error
    end.


-spec get_policies() -> policies().
get_policies() ->
    {ok, Node} = nodes:any(name()),

    ProviderRegistration = onepanel_env:get_remote(Node,
        provider_registration_policy, name()),
    SubdomainDelegation = onepanel_env:get_remote(Node,
        subdomain_delegation_supported, name()),
    GuiVerification = onepanel_env:get_remote(Node,
        gui_package_verification, name()),
    HarversterGuiVerification = onepanel_env:get_remote(Node,
        harvester_gui_package_verification, name()),
    #{
        oneprovider_registration => ProviderRegistration,
        subdomain_delegation => SubdomainDelegation,
        gui_package_verification => GuiVerification,
        harvester_gui_package_verification => HarversterGuiVerification
    }.


%%%===================================================================
%%% Step functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:step_ctx()) -> ok | no_return().
configure(Ctx) ->
    VmArgsFile = onepanel_env:get(oz_worker_vm_args_file),
    OzName = onepanel_utils:get_converted(onezone_name, Ctx, list),
    OzDomain = string:lowercase(
        onepanel_utils:get_converted(onezone_domain, Ctx, list)
    ),

    % TODO VFS-4140 Mark IPs configured only in batch mode
    onepanel_deployment:set_marker(?PROGRESS_CLUSTER_IPS),

    AppConfig = maps:with([gui_debug_mode, oz_name, http_domain], Ctx#{
        oz_name => OzName,
        http_domain => OzDomain
    }),
    set_policies(maps:get(policies, Ctx, #{})),

    node_cache:clear(?DETAILS_CACHE_KEY),
    service_cluster_worker:configure(Ctx#{
        name => name(),
        app_config => AppConfig,
        vm_args_file => VmArgsFile,
        initialize_ip => true
    }).


%%--------------------------------------------------------------------
%% @doc Configures new oz_worker node, assuming there are already
%% some cluster_manager and oz_worker instances.
%% @end
%%--------------------------------------------------------------------
-spec configure_additional_node(#{reference_host := service:host(), _ => _}) -> ok.
configure_additional_node(#{reference_host := RefHost} = Ctx) ->
    ReferenceNode = nodes:service_to_node(?SERVICE_PANEL, RefHost),
    {ok, MainCmHost} = service_cluster_manager:get_main_host(),
    CmHosts = service_cluster_manager:get_hosts(),
    DbHosts  = service_couchbase:get_hosts(),

    {ok, OzName} = rpc:call(ReferenceNode,
        onepanel_env, read_effective, [[oz_worker, oz_name], name()]),
    {ok, HttpDomain} = rpc:call(ReferenceNode,
        onepanel_env, read_effective, [[oz_worker, http_domain], name()]),

    NewCtx = maps:merge(
        #{
            main_cm_host => MainCmHost, cm_hosts => CmHosts,
            db_hosts => DbHosts,
            % skip setting policies, will be copied as part of the autogenerated config
            policies => #{},
            onezone_domain => HttpDomain,
            onezone_name => OzName
        },
        Ctx
    ),
    ok = onepanel_env:import_generated_from_node(
        name(), ReferenceNode, _SetInRuntime = false,
        #{cluster_worker => [external_ip]}
    ),
    configure(NewCtx).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:step_ctx()) -> ok | no_return().
start(Ctx) ->
    NewCtx = maps:merge(#{
        open_files => onepanel_env:get(oz_worker_open_files_limit)
    }, Ctx),
    service_cluster_worker:start(NewCtx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:step_ctx()) -> ok | no_return().
stop(Ctx) ->
    service_cluster_worker:stop(Ctx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:step_ctx()) -> service:status().
status(Ctx) ->
    % Since this function is invoked periodically by onepanel_cron
    % use it to schedule DNS check refresh on a single node
    catch maybe_check_dns(),
    service_cluster_worker:status(Ctx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc Checks if a running service is in a fully functional state.
%% @end
%%--------------------------------------------------------------------
-spec health(service:step_ctx()) -> service:status().
health(Ctx) ->
    case (catch get_nagios_status(Ctx)) of
        ok -> healthy;
        _ -> unhealthy
    end.


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:wait_for_init/1}
%% @end
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:step_ctx()) -> ok | no_return().
wait_for_init(Ctx) ->
    service_cluster_worker:wait_for_init(Ctx#{
        name => name(),
        wait_for_init_attempts => onepanel_env:get(oz_worker_wait_for_init_attempts),
        wait_for_init_delay => onepanel_env:get(oz_worker_wait_for_init_delay)
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:nagios_report/1}
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_response(Ctx :: service:step_ctx()) ->
    Response :: http_client:response().
get_nagios_response(Ctx) ->
    service_cluster_worker:get_nagios_response(Ctx#{
        nagios_protocol => onepanel_env:get(oz_worker_nagios_protocol),
        nagios_port => onepanel_env:get(oz_worker_nagios_port)
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:get_nagios_status/1}
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_status(Ctx :: service:step_ctx()) -> Status :: atom().
get_nagios_status(Ctx) ->
    service_cluster_worker:get_nagios_status(Ctx#{
        nagios_protocol => onepanel_env:get(oz_worker_nagios_protocol),
        nagios_port => onepanel_env:get(oz_worker_nagios_port)
    }).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:set_http_record/2}
%% @end
%%--------------------------------------------------------------------
-spec set_http_record(Name :: binary(), Value :: binary()) -> ok.
set_http_record(Name, Value) ->
    Nodes = get_nodes(),
    {Results, []} = rpc:multicall(Nodes, http_listener,
        set_response_to_letsencrypt_challenge, [Name, Value]),
    lists:foreach(fun(R) -> ok = R end, Results).


%%-------------------------------------------------------------------
%% @doc
%% Returns nameserver hostnames and IPs as generated by Onezone's DNS server.
%% @end
%%-------------------------------------------------------------------
-spec get_ns_hosts() -> [{Name :: binary(), IP :: inet:ip4_address()}].
get_ns_hosts() ->
    oz_worker_rpc:dns_config_get_ns_hosts().


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:set_txt_record/1}
%% @end
%%--------------------------------------------------------------------
-spec set_txt_record(service:step_ctx()) -> ok | no_return().
set_txt_record(#{txt_name := Name, txt_value := Value, txt_ttl := _TTL} = Ctx) ->
    [Node | _] = Nodes = get_nodes(),
    % oz_worker does not support custom TTL times
    CurrentRecords = onepanel_env:get_remote(Node, [dns_static_txt_records], name()),
    ok = onepanel_env:set_remote(Nodes, [dns_static_txt_records],
        [{Name, Value} | CurrentRecords], name()),
    reconcile_dns(Ctx).


%%--------------------------------------------------------------------
%% @doc
%% Returns hostname of the dns server responsible for setting txt record.
%% @end
%%--------------------------------------------------------------------
-spec get_dns_server() -> string() | no_return().
get_dns_server() ->
    binary_to_list(get_domain()).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:remove_txt_record/1}
%% @end
%%--------------------------------------------------------------------
remove_txt_record(#{txt_name := Name} = Ctx) ->
    [Node | _] = Nodes = get_nodes(),
    CurrentRecords = onepanel_env:get_remote(Node,
        [dns_static_txt_records], name()),

    ok = onepanel_env:set_remote(Nodes, [dns_static_txt_records],
        proplists:delete(Name, CurrentRecords), name()),

    reconcile_dns(Ctx).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:reload_webcert/0}
%% @end
%%--------------------------------------------------------------------
reload_webcert(Ctx) ->
    service_cluster_worker:reload_webcert(Ctx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:get_domain/1}
%% @end
%%--------------------------------------------------------------------
-spec get_domain() -> binary().
get_domain() ->
    maps:get(domain, ?MODULE:get_details(#{})).


%%--------------------------------------------------------------------
%% @doc
%% Returns details of the oz worker. Routes request to a host
%% with oz_worker deployed.
%% Result is cached to speed up usages such as rest_handler:allowed_origin/0.
%% @end
%%--------------------------------------------------------------------
-spec get_details(service:step_ctx()) ->
    #{name := binary() | undefined, domain := binary()}.
get_details(_Ctx) ->
    {ok, Cached} = node_cache:acquire(?DETAILS_CACHE_KEY, fun() ->
        {_, Node} = nodes:onepanel_with(name()),
        case rpc:call(Node, ?MODULE, get_details, []) of
            Map when is_map(Map) -> {ok, Map, ?DETAILS_CACHE_TTL};
            Error -> {false, Error}
        end
    end),
    Cached.


%%--------------------------------------------------------------------
%% @doc
%% Returns details of the oz worker. Must be invoked on a node
%% with oz_worker deployed.
%% @end
%%--------------------------------------------------------------------
-spec get_details() -> #{name := binary() | undefined, domain := binary() | undefined}.
get_details() ->
    OzName = onepanel_env:read_effective([name(), oz_name], name(), undefined),
    {ok, OzDomain} = onepanel_env:read_effective([name(), http_domain], name()),
    #{
        name => to_binary_or_undefined(OzName),
        domain => to_binary_or_undefined(OzDomain)
    }.

%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:get_admin_email/0}
%% @end
%%--------------------------------------------------------------------
-spec get_admin_email() -> binary() | undefined.
get_admin_email() ->
    undefined.


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:supports_letsencrypt_challenge/0}
%% @end
%%--------------------------------------------------------------------
-spec supports_letsencrypt_challenge(letsencrypt_api:challenge_type()) ->
    boolean().
supports_letsencrypt_challenge(Challenge) when
    Challenge == http;
    Challenge == dns
->
    OzNode = case nodes:any(name()) of
        {ok, N} -> N;
        Error -> throw(Error)
    end,
    service:is_healthy(name()) orelse throw(?ERROR_SERVICE_UNAVAILABLE),
    case Challenge of
        http -> true;
        dns ->
            case onepanel_env:get_remote(OzNode, [subdomain_delegation_supported], name()) of
                true -> true;
                false -> false
            end
    end;

supports_letsencrypt_challenge(_) -> false.


%%--------------------------------------------------------------------
%% @doc Triggers update of dns server config in oz_worker.
%% @end
%%--------------------------------------------------------------------
-spec reconcile_dns(service:step_ctx()) -> ok.
reconcile_dns(_Ctx) ->
    ok = oz_worker_rpc:reconcile_dns_config().


%%--------------------------------------------------------------------
%% @doc Copies given variables from old app.config file to the "generated"
%% app config file. Afterwards moves the legacy file to a backup location.
%% @end
%%--------------------------------------------------------------------
-spec migrate_generated_config(service:step_ctx()) -> ok | no_return().
migrate_generated_config(Ctx) ->
    service_cluster_worker:migrate_generated_config(Ctx#{
        name => name(),
        variables => [
            [name(), http_domain],
            [name(), oz_name]
        ]
    }).


%%-------------------------------------------------------------------
%% @doc
%% Finds outdated environment variables in 'generated' app config
%% and migrates them.
%% @end
%%-------------------------------------------------------------------
-spec rename_variables() -> ok.
rename_variables() ->
    Changes = [
        {[name(), subdomain_delegation_enabled], [name(), subdomain_delegation_supported]}
    ],
    lists:foreach(fun({Old, New}) ->
        onepanel_env:rename(name(), Old, New)
    end, Changes).


%%-------------------------------------------------------------------
%% @doc
%% Applies zone policy changes. This function should be run on all
%% nodes with oz_worker to ensure consistent app.config state.
%% @end
%%-------------------------------------------------------------------
-spec set_policies(policies()) -> ok.
set_policies(Ctx) ->
    lists:foreach(fun
        ({oneprovider_registration, OpenOrRestricted}) ->
            Atom = onepanel_utils:convert(OpenOrRestricted, atom),
            env_write_and_set(provider_registration_policy, Atom);
        ({subdomain_delegation, Supported}) ->
            env_write_and_set(subdomain_delegation_supported, Supported);
        ({gui_package_verification, Enabled}) ->
            env_write_and_set(gui_package_verification, Enabled);
        ({harvester_gui_package_verification, Enabled}) ->
            env_write_and_set(harvester_gui_package_verification, Enabled);
        ({_, _}) ->
            ok
    end, maps:to_list(Ctx)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc
%% If this function is run on the first of service's nodes
%% triggers DNS check cache refresh.
%% @end
%%-------------------------------------------------------------------
maybe_check_dns() ->
    case hosts:self() == hd(get_hosts())
        andalso dns_check:should_update_cache(name()) of

        true -> dns_check:async_update_cache(name());
        false -> ok
    end.


%% @private
-spec to_binary_or_undefined(term()) -> binary() | undefined.
to_binary_or_undefined(undefined) -> undefined;
to_binary_or_undefined(Value) -> onepanel_utils:convert(Value, binary).


%%-------------------------------------------------------------------
%% @private
%% @doc
%% Writes oz_worker app variable to autogenerated app config file
%% and sets it in the live config on local node.
%% @end
%%-------------------------------------------------------------------
-spec env_write_and_set(Variable :: atom(), Value :: term()) -> ok | no_return().
env_write_and_set(Variable, Value) ->
    Node = nodes:local(name()),
    onepanel_env:write([name(), Variable], Value, ?SERVICE_OZW),
    % catch - failure of set_remote indicates that oz_worker node is down.
    % In such case onepanel_env:write suffices since configuration will
    % be read from file on next startup.
    catch onepanel_env:set_remote(Node, Variable, Value, name()),
    ok.
