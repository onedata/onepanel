%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains cluster_worker service management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_cluster_worker).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include("names.hrl").
-include("deployment_progress.hrl").
-include("modules/onepanel_dns.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("ctool/include/global_definitions.hrl").

-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([
    get_worker_https_server_port/0,
    add_nodes_steps/1, configure/1,
    start/1, stop/1, status/1,
    wait_for_node_manager/1, is_node_manager_running/1, wait_for_init/1,
    register_host/1, get_nagios_response/1, get_nagios_status/1,
    set_node_ip/1, get_cluster_ips/1, get_hosts_ips/1,
    reload_webcert/1, migrate_generated_config/1
]).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    cluster_worker.


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
get_steps(deploy, #{hosts := Hosts, name := ServiceName} = Ctx) ->
    service:create(#service{name = ServiceName}),
    ClusterHosts = hosts:all(ServiceName),
    AllHosts = lists_utils:union(Hosts, ClusterHosts),
    [
        #step{function = migrate_generated_config,
            condition = fun(_) -> onepanel_env:legacy_config_exists(ServiceName) end},
        #step{hosts = AllHosts, function = configure},
        #steps{action = restart, ctx = Ctx#{hosts => AllHosts}},
        #step{hosts = [hd(AllHosts)], function = wait_for_init},
        #step{hosts = AllHosts, function = synchronize_clock_upon_start},
        % refresh status cache
        #steps{action = status, ctx = #{hosts => AllHosts}}
    ];

% Used only to resume a worker service that has been detected to be down. Will
% skip the intermediate steps between init_resume and finalize_resume, normally
% applied during deployment or complete Oneprovider/Onezone restart.
get_steps(resume, #{name := WorkerName}) ->
    [
        #steps{service = WorkerName, action = init_resume},
        #steps{service = WorkerName, action = finalize_resume}
    ];

get_steps(init_resume, #{name := ServiceName}) ->
    [
        #step{function = migrate_generated_config,
            condition = fun(_) -> onepanel_env:legacy_config_exists(ServiceName) end},
        #steps{action = start},
        % ensure that node manager has started, as steps between 'init_resume' and
        % 'finalize_resume' might require that the application is up
        #step{function = wait_for_node_manager}
    ];

get_steps(finalize_resume, _Ctx) ->
    [
        #step{function = wait_for_init, selection = first},
        #step{hosts = [hosts:self()], function = synchronize_clock_upon_start},
        % refresh status cache
        #steps{action = status}
    ];

get_steps(start, _Ctx) ->
    [#step{function = start}];

get_steps(stop, _Ctx) ->
    [#step{function = stop}];

get_steps(restart, _Ctx) ->
    [#step{function = stop}, #step{function = start}];

get_steps(status, _Ctx) ->
    [#step{function = status}];

get_steps(set_cluster_ips, #{hosts := Hosts} = _Ctx) ->
    [#step{function = set_node_ip, hosts = Hosts}];
get_steps(set_cluster_ips, #{cluster_ips := HostsToIps} = Ctx) ->
    % execute only on nodes where ip is explicitly provided
    get_steps(set_cluster_ips, Ctx#{hosts => maps:keys(HostsToIps)});
get_steps(set_cluster_ips, #{name := ServiceName} = Ctx) ->
    % execute on all service hosts, "guessing" IP if necessary
    Hosts = hosts:all(ServiceName),
    get_steps(set_cluster_ips, Ctx#{hosts => Hosts});

get_steps(get_nagios_response, #{name := ServiceName}) ->
    [#step{
        function = get_nagios_response,
        hosts = hosts:all(ServiceName),
        selection = any
    }];

get_steps(reload_webcert, _Ctx) ->
    [#step{function = reload_webcert, selection = all}];

get_steps(get_dns_check_configuration, _Ctx) ->
    [#step{module = dns_check, function = get_configuration,
        args = [], selection = any}];

get_steps(configure_dns_check, _Ctx) ->
    [#step{module = dns_check, function = configure_dns_check, selection = all}];

get_steps(dns_check, #{name := ServiceName, force_check := ForceCheck}) ->
    [#step{module = dns_check, function = get,
        selection = first, args = [ServiceName, ForceCheck]}].

%%%===================================================================
%%% API functions
%%%===================================================================

-spec get_worker_https_server_port() -> integer().
get_worker_https_server_port() ->
    application:get_env(?APP_NAME, worker_https_server_port, 443).


%%--------------------------------------------------------------------
%% @doc Defines part of the steps, which complete list is generated
%% in service_oz/op_worker.
%% @end
%%--------------------------------------------------------------------

-spec add_nodes_steps(#{
    name := service:name(),
    reference_host := service:host(), new_hosts := [service:host()]
}) -> [service:step()].
add_nodes_steps(#{reference_host := _, name := WorkerName, new_hosts := NewHosts} = Ctx) ->
    % set Ctx in records to use modifications made to it in op/oz service module
    [
        #step{module = ?MODULE, function = register_host,
            hosts = NewHosts, ctx = Ctx},
        #steps{service = ?SERVICE_CM, action = update_workers_number, ctx = Ctx},
        #steps{service = WorkerName, action = stop, ctx = Ctx},
        #steps{service = ?SERVICE_CM, action = stop, ctx = Ctx},
        #steps{service = ?SERVICE_CM, action = resume, ctx = Ctx},
        #steps{service = WorkerName, action = init_resume, ctx = Ctx},
        #steps{service = WorkerName, action = finalize_resume, ctx = Ctx}
    ].


%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:step_ctx()) -> ok | no_return().
configure(#{name := ServiceName, main_cm_host := MainCmHost, cm_hosts := CmHosts,
    db_hosts := DbHosts, app_config := AppConfig, initialize_ip := InitIp,
    vm_args_file := VmArgsFile} = Ctx) ->

    Host = hosts:self(),
    Node = nodes:local(ServiceName),
    CmNodes = nodes:service_to_nodes(
        ?SERVICE_CM,
        [MainCmHost | lists:delete(MainCmHost, CmHosts)]
    ),
    DbPort = onepanel_env:typed_get(couchbase_port, list),
    DbNodes = lists:map(fun(DbHost) ->
        onepanel_utils:convert(string:join([DbHost, DbPort], ":"), atom)
    end, DbHosts),

    onepanel_env:write([ServiceName, cm_nodes], CmNodes, ServiceName),
    onepanel_env:write([ServiceName, db_nodes], DbNodes, ServiceName),

    maps:fold(fun(Key, Value, _) ->
        onepanel_env:write([ServiceName, Key], Value, ServiceName)
    end, #{}, AppConfig),

    case InitIp of
        true ->
            IP = case kv_utils:get([cluster_ips, Host], Ctx, undefined) of
                undefined -> get_initial_ip(ServiceName);
                Found ->
                    onepanel_deployment:set_marker(?PROGRESS_CLUSTER_IPS),
                    {ok, IPTuple} = ip_utils:to_ip4_address(Found),
                    IPTuple
            end,
            onepanel_env:write([name(), external_ip], IP, ServiceName);
        false -> ok
    end,

    HttpsPort = get_worker_https_server_port(),
    onepanel_env:write([ServiceName, https_server_port], HttpsPort, ServiceName),

    onepanel_vm:write("name", Node, VmArgsFile),
    onepanel_vm:write("setcookie", erlang:get_cookie(), VmArgsFile),

    setup_cert_paths(ServiceName, ServiceName),

    service:add_host(ServiceName, Host).


-spec start(service:step_ctx()) -> ok | no_return().
start(#{name := ServiceName} = Ctx) ->
    service:update_status(ServiceName, starting),
    service_cli:start(ServiceName, Ctx),
    service:update_status(ServiceName, unhealthy),
    service:register_healthcheck(ServiceName, Ctx),
    ok.


-spec stop(service:step_ctx()) -> ok.
stop(#{name := ServiceName} = Ctx) ->
    service:deregister_healthcheck(ServiceName, Ctx),
    service:update_status(ServiceName, stopping),
    service_cli:stop(ServiceName),
    % check status before updating it as service_cli:stop/1 does not throw on failure
    status(Ctx),
    ok.


-spec status(service:step_ctx()) -> service:status().
status(#{name := ServiceName} = Ctx) ->
    Module = service:get_module(ServiceName),
    service:update_status(ServiceName,
        case service_cli:status(ServiceName, ping) of
            running -> Module:health(Ctx);
            stopped -> stopped;
            missing -> missing
        end).


-spec wait_for_node_manager(service:step_ctx()) -> ok | no_return().
wait_for_node_manager(#{wait_for_init_attempts := Attempts, wait_for_init_delay := Delay} = Ctx) ->
    onepanel_utils:wait_until(?MODULE, is_node_manager_running, [Ctx], {equal, true},
        Attempts, Delay),
    ok.


-spec is_node_manager_running(service:step_ctx()) -> boolean().
is_node_manager_running(#{name := ServiceName}) ->
    Node = nodes:local(ServiceName),
    case rpc:call(Node, erlang, whereis, [?NODE_MANAGER_NAME]) of
        Pid when is_pid(Pid) -> true;
        _ -> false
    end.


%%--------------------------------------------------------------------
%% @doc Waits for initialization of the service.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:step_ctx()) -> ok | no_return().
wait_for_init(#{name := ServiceName, wait_for_init_attempts := Attempts,
    wait_for_init_delay := Delay} = Ctx) ->
    Module = service:get_module(ServiceName),
    onepanel_utils:wait_until(Module, health, [Ctx], {equal, healthy},
        Attempts, Delay),
    service:update_status(ServiceName, healthy),
    ok.


-spec register_host(#{name := service:name(), _ => _}) -> ok.
register_host(#{name := ServiceName}) ->
    service:add_host(ServiceName, hosts:self()).


%%--------------------------------------------------------------------
%% @doc Returns nagios report for the service.
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_response(Ctx :: service:step_ctx()) ->
    Response :: http_client:response().
get_nagios_response(#{nagios_protocol := Protocol, nagios_port := Port}) ->
    Host = hosts:self(),
    Url = onepanel_utils:join([Protocol, "://", Host, ":", Port, "/nagios"]),
    Opts = case Protocol of
        "https" ->
            % Nagios polling does not need to be secure (we only depend on
            % it to check if the service is up), plus skipping cert validation
            % allows to start the service on self-signed certs (for test
            % purposes or before Let's Encrypt is used to generate certs).
            [{ssl_options, [{secure, false}]}];
        _ ->
            []
    end,
    case http_client:get(Url, #{}, <<>>, Opts) of
        {error, econnrefused} ->
            {ok, ?HTTP_503_SERVICE_UNAVAILABLE, #{}, <<>>};
        Result ->
            Result
    end.


%%--------------------------------------------------------------------
%% @doc Returns the service status from the nagios report.
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_status(Ctx :: service:step_ctx()) -> Status :: atom().
get_nagios_status(Ctx) ->
    {ok, ?HTTP_200_OK, _Headers, Body} = get_nagios_response(Ctx),

    {Xml, _} = xmerl_scan:string(onepanel_utils:convert(Body, list)),
    [Status] = [X#xmlAttribute.value || X <- Xml#xmlElement.attributes,
        X#xmlAttribute.name == status],

    list_to_atom(Status).


%%--------------------------------------------------------------------
%% @doc Writes node IP to app.config on the current node's worker.
%% If IP is not given explicitly in cluster_ips map
%% and worker has none in its app config onepanel tries to determine it.
%% @end
%%--------------------------------------------------------------------
-spec set_node_ip(Ctx :: service:step_ctx()) -> ok | no_return().
set_node_ip(#{name := ServiceName} = Ctx) ->
    Host = hosts:self(),
    Node = nodes:local(ServiceName),

    {ok, IP} = case kv_utils:find([cluster_ips, Host], Ctx) of
        {ok, NewIP} ->
            onepanel_deployment:set_marker(?PROGRESS_CLUSTER_IPS),
            ip_utils:to_ip4_address(NewIP);
        _ -> {ok, get_initial_ip(ServiceName)}
    end,

    onepanel_env:write([name(), external_ip], IP, ServiceName),
    onepanel_env:set_remote(Node, [external_ip], IP, name()),
    dns_check:invalidate_cache(ServiceName).


%%--------------------------------------------------------------------
%% @doc Creates response about cluster IPs with all worker
%% hosts and their IPs.
%% @end
%%--------------------------------------------------------------------
-spec get_cluster_ips(service:step_ctx()) ->
    #{isConfigured := boolean(), hosts := #{binary() => binary()}}.
get_cluster_ips(#{name := _ServiceName} = Ctx) ->
    HostsToIps = lists:map(fun
        ({Host, undefined}) ->
            {onepanel_utils:convert(Host, binary), null};
        ({Host, IP}) ->
            {onepanel_utils:convert(Host, binary), onepanel_ip:ip4_to_binary(IP)}
    end, get_hosts_ips(Ctx)),
    #{
        isConfigured => onepanel_deployment:is_set(?PROGRESS_CLUSTER_IPS),
        hosts => maps:from_list(HostsToIps)
    }.


%%--------------------------------------------------------------------
%% @doc Gathers external IPs of each worker node.
%% @end
%%--------------------------------------------------------------------
-spec get_hosts_ips(service:step_ctx()) ->
    [{Host :: service:host(), IP :: inet:ip4_address()}].
get_hosts_ips(#{name := ServiceName}) ->
    lists:map(fun(Host) ->
        Node = nodes:service_to_node(?SERVICE_PANEL, Host),
        {ok, IPTuple} = rpc:call(Node, onepanel_env, read_effective,
            [[name(), external_ip], ServiceName]),
        {Host, IPTuple}
    end, hosts:all(ServiceName)).


%%--------------------------------------------------------------------
%% @doc
%% Reloads webcerts.
%% @end
%%--------------------------------------------------------------------
reload_webcert(#{name := ServiceName}) ->
    Node = nodes:local(ServiceName),
    ok = rpc:call(Node, https_listener, reload_web_certs, []).


%%--------------------------------------------------------------------
%% @doc Copies given variables from old app.config file to the "generated"
%% app config file. Afterwards moves the legacy file to a backup location.
%% @end
%%--------------------------------------------------------------------
-spec migrate_generated_config(service:step_ctx()) -> ok | no_return().
migrate_generated_config(#{name := ServiceName, variables := Variables}) ->
    onepanel_env:upgrade_app_config(ServiceName, [
        [ServiceName, cm_nodes],
        [ServiceName, db_nodes],
        [ServiceName, web_key_file],
        [ServiceName, web_cert_file],
        [ServiceName, web_cert_chain_file],
        [ServiceName, cacerts_dir],
        % external_ip is in the namespace of cluster_worker application
        [name(), external_ip]
        | Variables
    ]);
migrate_generated_config(#{name := _ServiceName} = Ctx) ->
    migrate_generated_config(Ctx#{variables => []}).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Writes certificate paths to the app config file of the service.
%% @end
%%--------------------------------------------------------------------
-spec setup_cert_paths(ServiceName :: ?SERVICE_OPW | ?SERVICE_OZW,
    AppName :: op_worker | oz_worker) -> ok | no_return().
setup_cert_paths(ServiceName, AppName) ->
    lists:foreach(fun({SrcEnv, DstEnv}) ->
        Path = filename:absname(onepanel_env:get(SrcEnv)),
        ok = onepanel_env:write([AppName, DstEnv], Path, ServiceName)
    end, [
        {web_key_file, web_key_file},
        {web_cert_file, web_cert_file},
        {web_cert_chain_file, web_cert_chain_file},
        {cacerts_dir, cacerts_dir}
    ]).


%%--------------------------------------------------------------------
%% @private
%% @doc Get IP preconfigured by user or determine it.
%% @end
%%--------------------------------------------------------------------
-spec get_initial_ip(service:name()) -> inet:ip4_address().
get_initial_ip(ServiceName) ->
    case onepanel_env:read_effective([name(), external_ip], ServiceName) of
        {ok, {_, _, _, _} = IP} -> IP;
        {ok, IPList} when is_list(IPList) ->
            {ok, IP} = ip_utils:to_ip4_address(IPList),
            IP;
        _ -> onepanel_ip:determine_ip()
    end.
