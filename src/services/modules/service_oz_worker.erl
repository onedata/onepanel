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

-include("deployment_progress.hrl").
-include("modules/errors.hrl").
-include("service.hrl").
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).
%% LE behaviour callbacks
-export([set_txt_record/1, remove_txt_record/1, get_dns_server/0,
    reload_webcert/1, get_domain/0, get_admin_email/1, set_http_record/2,
    supports_letsencrypt_challenge/1]).

%% API
-export([configure/1, start/1, stop/1, status/1, health/1, wait_for_init/1,
    get_nagios_response/1, get_nagios_status/1]).
-export([reconcile_dns/1, get_ns_hosts/0, get_ns_hosts/1]).
-export([migrate_generated_config/1]).
-export([get_policies/0, set_policies/1]).
-export([get_details/1, get_details/0]).

-define(INIT_SCRIPT, "oz_worker").

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    oz_worker.


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
    service:get_nodes(name()).


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
            [#steps{action = set_policies,
                ctx = Policies#{hosts => maps:get(hosts, Ctx, get_hosts())}}];
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
    lists:foreach(fun(R) -> ok = R end, Results),
    ok.


%%-------------------------------------------------------------------
%% @doc
%% Returns nameserver hostnames and IPs as generated by Onezone's DNS server.
%% @end
%%-------------------------------------------------------------------
-spec get_ns_hosts(service:ctx()) -> [{Name :: binary(), IP :: inet:ip4_address()}].
get_ns_hosts(#{node := Node}) ->
    case rpc:call(Node, dns_config, get_ns_hosts, []) of
        Hosts when is_list(Hosts) -> Hosts
    end.

-spec get_ns_hosts() -> [{Name :: binary(), IP :: inet:ip4_address()}].
get_ns_hosts() ->
    get_ns_hosts(#{node => hd(get_nodes())}).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:set_txt_record/1}
%% @end
%%--------------------------------------------------------------------
-spec set_txt_record(service:ctx()) -> ok | no_return().
set_txt_record(#{txt_name := Name, txt_value := Value, txt_ttl := _TTL,
    nodes := [Node | _] = Nodes} = Ctx) ->
    % oz_worker does not support custom TTL times
    CurrentRecords = onepanel_env:get_remote(Node, [dns_static_txt_records], name()),

    ok = onepanel_env:set_remote(Nodes, [dns_static_txt_records],
        [{Name, Value} | CurrentRecords], name()),
    reconcile_dns(Ctx);
set_txt_record(#{txt_name := _, txt_value := _} = Ctx) ->
    set_txt_record(Ctx#{nodes => get_nodes()}).


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
remove_txt_record(#{txt_name := Name, nodes := Nodes} = Ctx) ->
    CurrentRecords = onepanel_env:get_remote(hd(Nodes),
        [dns_static_txt_records], name()),

    ok = onepanel_env:set_remote(Nodes, [dns_static_txt_records],
        proplists:delete(Name, CurrentRecords), name()),
    reconcile_dns(Ctx);
remove_txt_record(#{txt_name := _} = Ctx) ->
    remove_txt_record(Ctx#{nodes => get_nodes()}).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:reload_webcert/0}
%% @end
%%--------------------------------------------------------------------
reload_webcert(Ctx) ->
    service_cluster_worker:reload_webcert(Ctx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:get_domain/0}
%% @end
%%--------------------------------------------------------------------
-spec get_domain() -> binary().
get_domain() ->
    maps:get(domain, get_details(#{})).


%%--------------------------------------------------------------------
%% @doc
%% Returns details of the oz worker. Routes request to a host
%% with oz_worker deployed.
%% @end
%%--------------------------------------------------------------------
-spec get_details(service:ctx()) -> #{name := binary(), domain := binary()}.
get_details(#{hosts := Hosts}) ->
    Node = case Hosts of
        [H|_] -> onepanel_cluster:service_to_node(?APP_NAME, H);
        [] -> node()
    end,
    rpc:call(Node, ?MODULE, get_details, []);

get_details(Ctx) ->
    get_details(Ctx#{hosts => get_hosts()}).


%%--------------------------------------------------------------------
%% @doc
%% Returns details of the oz worker. Must be invoked on a node
%% with oz_worker deployed.
%% @end
%%--------------------------------------------------------------------
-spec get_details() -> #{name := binary(), domain := binary()}.
get_details() ->
    {ok, OzName} = onepanel_env:read_effective([name(), oz_name], name()),
    {ok, OzDomain} = onepanel_env:read_effective([name(), http_domain], name()),
    #{
        name => onepanel_utils:convert(OzName, binary),
        domain => onepanel_utils:convert(OzDomain, binary)
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
supports_letsencrypt_challenge(http) -> true;
supports_letsencrypt_challenge(dns) ->
    try
        onepanel_env:get_remote(hd(get_nodes()),
            [subdomain_delegation_enabled], name())
    catch _:_ ->
        unknown
    end;
supports_letsencrypt_challenge(_) -> false.


%%--------------------------------------------------------------------
%% @doc Triggers update of dns server config in oz_worker.
%% @end
%%--------------------------------------------------------------------
-spec reconcile_dns(service:ctx()) -> ok.
reconcile_dns(_Ctx) ->
    ok = rpc:call(hd(get_nodes()), node_manager_plugin, reconcile_dns_config, []).


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


-spec get_policies() -> #{atom() := term()}.
get_policies() ->
    Node = onepanel_cluster:service_to_node(name()),
    SubdomainDelegation = onepanel_env:get_remote(
        Node, subdomain_delegation_enabled, name()),
    #{
        subdomainDelegation => SubdomainDelegation
    }.


-spec set_policies(Ctx :: service:ctx()) -> ok.
set_policies(#{subdomain_delegation := Delegation} = Ctx) ->
    Node = onepanel_cluster:service_to_node(name()),
    Path = onepanel_env:get_config_path(name(), generated),

    onepanel_env:write([name(), subdomain_delegation_enabled], Delegation, Path),
    onepanel_env:set_remote(Node, subdomain_delegation_enabled, Delegation, name()),

    set_policies(maps:remove(subdomain_delegation, Ctx));
set_policies(_Ctx) ->
    ok.


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
    ThisNode = node(),
    ThisHost = onepanel_cluster:node_to_host(ThisNode),
    case ThisHost == hd(get_hosts())
        andalso dns_check:should_update_cache(name()) of

        true -> dns_check:async_update_cache(name());
        false -> ok
    end.
