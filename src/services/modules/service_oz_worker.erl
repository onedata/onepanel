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
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).
%% LE behaviour callbacks
-export([set_txt_record/1, remove_txt_record/1, get_dns_server/0,
    reload_webcert/1, get_domain/0, get_admin_email/1, set_http_record/2,
    supports_letsencrypt_challenge/1]).

%% API
-export([configure/1, start/1, stop/1, status/1, health/1, wait_for_init/1,
    get_nagios_response/1, get_nagios_status/1]).
-export([reconcile_dns/1, get_ns_hosts/0]).
-export([migrate_generated_config/1, rename_variables/0]).
-export([get_policies/0, set_policies/1]).
-export([get_details/1, get_details/0]).
-export([get_logic_client/1, get_user_details/1]).

-define(INIT_SCRIPT, "oz_worker").
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
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, Ctx) ->
    service_cluster_worker:get_steps(deploy, Ctx#{name => name()})
    ++ case Ctx of
        #{policies := Policies} ->
            PoliciesCtx = onepanel_maps:get_store(hosts, Ctx, hosts, Policies),
            [#steps{action = set_policies, ctx = PoliciesCtx}];
        _ -> []
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
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(Ctx) ->
    GeneratedConfigFile = onepanel_env:get_config_path(name(), generated),
    VmArgsFile = service_ctx:get(oz_worker_vm_args_file, Ctx),
    OzName = service_ctx:get(onezone_name, Ctx),
    OzDomain = service_ctx:get_domain(onezone_domain, Ctx),

    % TODO VFS-4140 Mark IPs configured only in batch mode
    onepanel_deployment:mark_completed(?PROGRESS_CLUSTER_IPS),

    simple_cache:clear(?DETAILS_CACHE_KEY),
    service_cluster_worker:configure(Ctx#{
        name => name(),
        app_config => #{
            oz_name => OzName,
            http_domain => OzDomain
        },
        generated_config_file => GeneratedConfigFile,
        vm_args_file => VmArgsFile,
        initialize_ip => true
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    NewCtx = maps:merge(#{
        open_files => service_ctx:get(oz_worker_open_files_limit, Ctx)
    }, Ctx),
    service_cluster_worker:start(NewCtx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(Ctx) ->
    service_cluster_worker:stop(Ctx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> service:status().
status(Ctx) ->
    % Since this function is invoked periodically by service_watcher
    % use it to schedule DNS check refresh on a single node
    catch maybe_check_dns(),
    service_cluster_worker:status(Ctx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc Checks if a running service is in a fully functional state.
%% @end
%%--------------------------------------------------------------------
-spec health(service:ctx()) -> service:status().
health(Ctx) ->
    case (catch get_nagios_status(Ctx)) of
        ok -> healthy;
        _ -> unhealthy
    end.


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:wait_for_init/1}
%% @end
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:ctx()) -> ok | no_return().
wait_for_init(Ctx) ->
    service_cluster_worker:wait_for_init(Ctx#{
        name => name(),
        wait_for_init_attempts => service_ctx:get(
            oz_worker_wait_for_init_attempts, Ctx, integer),
        wait_for_init_delay => service_ctx:get(
            oz_worker_wait_for_init_delay, Ctx, integer)
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:nagios_report/1}
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_response(Ctx :: service:ctx()) ->
    Response :: http_client:response().
get_nagios_response(Ctx) ->
    service_cluster_worker:get_nagios_response(Ctx#{
        nagios_protocol => service_ctx:get(oz_worker_nagios_protocol, Ctx),
        nagios_port => service_ctx:get(oz_worker_nagios_port, Ctx, integer)
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:get_nagios_status/1}
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_status(Ctx :: service:ctx()) -> Status :: atom().
get_nagios_status(Ctx) ->
    service_cluster_worker:get_nagios_status(Ctx#{
        nagios_protocol => service_ctx:get(oz_worker_nagios_protocol, Ctx),
        nagios_port => service_ctx:get(oz_worker_nagios_port, Ctx, integer)
    }).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:set_http_record/2}
%% @end
%%--------------------------------------------------------------------
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
    {ok, Node} = nodes:any(name()),
    case rpc:call(Node, dns_config, get_ns_hosts, []) of
        Hosts when is_list(Hosts) -> Hosts
    end.


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:set_txt_record/1}
%% @end
%%--------------------------------------------------------------------
-spec set_txt_record(service:ctx()) -> ok | no_return().
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
%% Result is cached to speed up usages such as rest_utils:allowed_origin/0.
%% @end
%%--------------------------------------------------------------------
-spec get_details(service:ctx()) ->
    #{name := binary() | undefined, domain := binary()}.
get_details(_Ctx) ->
    {ok, Cached} = simple_cache:get(?DETAILS_CACHE_KEY, fun() ->
        {_, Node} = nodes:onepanel_with(name()),
        case rpc:call(Node, ?MODULE, get_details, []) of
            Map when is_map(Map) -> {true, Map, ?DETAILS_CACHE_TTL};
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
-spec get_admin_email(service:ctx()) -> binary() | undefined.
get_admin_email(_Ctx) ->
    undefined.


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:supports_letsencrypt_challenge/0}
%% @end
%%--------------------------------------------------------------------
-spec supports_letsencrypt_challenge(letsencrypt_api:challenge_type()) ->
    boolean() | unknown.
supports_letsencrypt_challenge(http) ->
    service:healthy(name());
supports_letsencrypt_challenge(dns) ->
    case nodes:any(name()) of
        {ok, Node} ->
            service:healthy(name()) andalso
                onepanel_env:get_remote(Node, [subdomain_delegation_supported], name());
        _Error -> false
    end;
supports_letsencrypt_challenge(_) -> false.


%%--------------------------------------------------------------------
%% @doc Triggers update of dns server config in oz_worker.
%% @end
%%--------------------------------------------------------------------
-spec reconcile_dns(service:ctx()) -> ok.
reconcile_dns(Ctx) ->
    {ok, Node} = nodes:any(Ctx#{service => name()}),
    ok = rpc:call(Node, node_manager_plugin, reconcile_dns_config, []).


%%--------------------------------------------------------------------
%% @doc {@link onepanel_env:migrate_generated_config/2}
%% @end
%%--------------------------------------------------------------------
-spec migrate_generated_config(service:ctx()) -> ok | no_return().
migrate_generated_config(Ctx) ->
    service_cluster_worker:migrate_generated_config(Ctx#{
        name => name(),
        variables => [
            [name(), http_domain],
            [name(), oz_name]
        ]
    }).


%%-------------------------------------------------------------------
%% @private
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
        onepanel_env:migrate(name(), Old, New)
    end, Changes).


-spec get_policies() -> #{atom() := term()}.
get_policies() ->
    Node = nodes:local(name()),
    SubdomainDelegation = onepanel_env:get_remote(
        Node, subdomain_delegation_supported, name()),
    #{
        subdomainDelegation => SubdomainDelegation
    }.


-spec set_policies(Ctx :: service:ctx()) -> ok.
set_policies(#{subdomain_delegation := Delegation} = Ctx) ->
    Node = nodes:local(name()),
    Path = onepanel_env:get_config_path(name(), generated),

    onepanel_env:write([name(), subdomain_delegation_supported], Delegation, Path),
    onepanel_env:set_remote(Node, subdomain_delegation_supported, Delegation, name()),

    set_policies(maps:remove(subdomain_delegation, Ctx));
set_policies(_Ctx) ->
    ok.


-spec get_logic_client(Auth :: AccessToken | root) -> {ok, rest_handler:zone_auth()} | #error{}
    when AccessToken :: binary().
get_logic_client(root) ->
    case nodes:any(name()) of
        {ok, OzNode} ->
            case rpc:call(OzNode, entity_logic, root_client, []) of
                {badrpc, _} = Error -> ?make_error(Error);
                LogicClient -> {ok, LogicClient}
            end;
        Error -> Error
    end;

get_logic_client(AccessToken) ->
    case nodes:any(name()) of
        {ok, OzNode} ->
            case rpc:call(OzNode, auth_logic,
                authorize_by_onezone_gui_macaroon, [AccessToken, undefined]
            ) of
                {true, LogicClient} ->
                    {ok, LogicClient};  % record to be used in rpc calls to oz_worker
                {error, ApiError} -> ?make_error(ApiError)
            end;
        Error -> Error
    end.


-spec get_user_details(LogicClient :: term()) -> {ok, #user_details{}} | #error{}.
get_user_details(LogicClient) ->
    {ok, OzNode} = nodes:any(name()),
    case rpc:call(OzNode, user_logic, get_as_user_details, [LogicClient]) of
        {ok, User} -> {ok, User};
        {error, Reason} -> ?make_error(Reason)
    end.


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
