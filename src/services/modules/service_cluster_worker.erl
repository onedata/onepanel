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

-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, wait_for_init/1, start/1, stop/1, status/1,
    get_nagios_response/1, get_nagios_status/1, set_node_ip/1,
    get_cluster_ips/1, get_hosts_ips/1, reload_webcert/1, migrate_generated_config/1]).

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
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := Hosts, name := Name} = Ctx) ->
    service:create(#service{name = Name}),
    ClusterHosts = hosts:all(Name),
    AllHosts = lists_utils:union(Hosts, ClusterHosts),
    [
        #step{function = migrate_generated_config,
            condition = fun(_) -> onepanel_env:legacy_config_exists(Name) end},
        #step{hosts = AllHosts, function = configure},
        #steps{action = restart, ctx = Ctx#{hosts => AllHosts}},
        #step{hosts = [hd(AllHosts)], function = wait_for_init},
        % refresh status cache
        #steps{action = status, ctx = #{hosts => AllHosts}}
    ];

get_steps(resume, #{name := Name}) ->
    [
        #step{function = migrate_generated_config,
            condition = fun(_) -> onepanel_env:legacy_config_exists(Name) end},
        #steps{action = start},
        #step{function = wait_for_init, selection = first},
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
    % execute only on nodes where ip is explicitely provided
    get_steps(set_cluster_ips, Ctx#{hosts => maps:keys(HostsToIps)});
get_steps(set_cluster_ips, #{name := ServiceName} = Ctx) ->
    % execute on all service hosts, "guessing" IP if necessary
    Hosts = hosts:all(ServiceName),
    get_steps(set_cluster_ips, Ctx#{hosts => Hosts});

get_steps(get_nagios_response, #{name := Name}) ->
    [#step{
        function = get_nagios_response,
        hosts = hosts:all(Name),
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

%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(#{name := Name, main_cm_host := MainCmHost, cm_hosts := CmHosts,
    db_hosts := DbHosts, app_config := AppConfig, initialize_ip := InitIp,
    vm_args_file := VmArgsFile} = Ctx) ->

    Host = hosts:self(),
    Node = nodes:local(Name),
    CmNodes = nodes:service_to_nodes(
        ?SERVICE_CM,
        [MainCmHost | lists:delete(MainCmHost, CmHosts)]
    ),
    DbPort = onepanel_env:typed_get(couchbase_port, list),
    DbNodes = lists:map(fun(DbHost) ->
        onepanel_utils:convert(string:join([DbHost, DbPort], ":"), atom)
    end, DbHosts),

    onepanel_env:write([Name, cm_nodes], CmNodes, Name),
    onepanel_env:write([Name, db_nodes], DbNodes, Name),

    maps:fold(fun(Key, Value, _) ->
        onepanel_env:write([Name, Key], Value, Name)
    end, #{}, AppConfig),

    case InitIp of
        true ->
            IP = case kv_utils:get([cluster_ips, Host], Ctx, undefined) of
                undefined -> get_initial_ip(Name);
                Found ->
                    onepanel_deployment:set_marker(?PROGRESS_CLUSTER_IPS),
                    {ok, IPTuple} = ip_utils:to_ip4_address(Found),
                    IPTuple
            end,
            onepanel_env:write([name(), external_ip], IP, Name);
        false -> ok
    end,

    onepanel_vm:write("name", Node, VmArgsFile),
    onepanel_vm:write("setcookie", maps:get(cookie, Ctx, erlang:get_cookie()),
        VmArgsFile),

    setup_cert_paths(Ctx),

    service:add_host(Name, Host).


-spec start(service:ctx()) -> ok | no_return().
start(#{name := Name} = Ctx) ->
    service_cli:start(Name, Ctx),
    service:update_status(Name, unhealthy),
    service:register_healthcheck(Name, #{hosts => [hosts:self()]}),
    ok.


-spec stop(service:ctx()) -> ok.
stop(#{name := Name} = Ctx) ->
    onepanel_cron:remove_job(Name),
    service_cli:stop(Name),
    % check status before updating it as service_cli:stop/1 does not throw on failure
    status(Ctx),
    ok.


-spec status(service:ctx()) -> service:status().
status(#{name := Name} = Ctx) ->
    Module = service:get_module(Name),
    service:update_status(Name,
        case service_cli:status(Name, ping) of
            running -> Module:health(Ctx);
            stopped -> stopped;
            missing -> missing
        end).


%%--------------------------------------------------------------------
%% @doc Waits for initialization of the service.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:ctx()) -> ok | no_return().
wait_for_init(#{name := Name, wait_for_init_attempts := Attempts,
    wait_for_init_delay := Delay} = Ctx) ->
    Module = service:get_module(Name),
    onepanel_utils:wait_until(Module, health, [Ctx], {equal, healthy},
        Attempts, Delay),
    service:update_status(Name, healthy),
    ok.


%%--------------------------------------------------------------------
%% @doc Returns nagios report for the service.
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_response(Ctx :: service:ctx()) ->
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
    http_client:get(Url, #{}, <<>>, Opts).


%%--------------------------------------------------------------------
%% @doc Returns the service status from the nagios report.
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_status(Ctx :: service:ctx()) -> Status :: atom().
get_nagios_status(Ctx) ->
    {ok, ?HTTP_200_OK, _Headers, Body} = get_nagios_response(Ctx),

    {Xml, _} = xmerl_scan:string(onepanel_utils:convert(Body, list)),
    [Status] = [X#xmlAttribute.value || X <- Xml#xmlElement.attributes,
        X#xmlAttribute.name == status],

    list_to_atom(Status).


%%--------------------------------------------------------------------
%% @doc Writes node IP to app.config on the current node's worker.
%% If IP is not given explicitely in cluster_ips map
%% and worker has none in its app config onepanel tries to determine it.
%% @end
%%--------------------------------------------------------------------
-spec set_node_ip(Ctx :: service:ctx()) -> ok | no_return().
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
-spec get_cluster_ips(service:ctx()) ->
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
-spec get_hosts_ips(service:ctx()) ->
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
%% Clears worker ssl cache to ensure certificates changed on disk
%% are reloaded.
%% @end
%%--------------------------------------------------------------------
reload_webcert(#{name := ServiceName}) ->
    Node = nodes:local(ServiceName),
    ok = rpc:call(Node, ssl, clear_pem_cache, []).


%%--------------------------------------------------------------------
%% @doc Copies given variables from old app.config file to the "generated"
%% app config file. Afterwards moves the legacy file to a backup location.
%% @end
%%--------------------------------------------------------------------
-spec migrate_generated_config(service:ctx()) -> ok | no_return().
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
-spec setup_cert_paths(service:ctx()) -> ok | no_return().
setup_cert_paths(#{name := AppName}) ->
    lists:foreach(fun({SrcEnv, DstEnv}) ->
        Path = filename:absname(onepanel_env:get(SrcEnv)),
        ok = onepanel_env:write([AppName, DstEnv], Path, AppName)
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
