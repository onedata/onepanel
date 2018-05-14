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
-include("deployment_progress.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, start/1, stop/1, status/1, wait_for_init/1,
    get_nagios_response/1, get_nagios_status/1, set_node_ip/1,
    get_cluster_ips/1, reload_webcert/1]).

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
    service:get_nodes(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := Hosts, name := Name} = Ctx) ->
    service:create(#service{name = Name}),
    ClusterHosts = (service:get_module(Name)):get_hosts(),
    AllHosts = onepanel_lists:union(Hosts, ClusterHosts),
    [
        #step{hosts = AllHosts, function = configure},
        #steps{action = restart, ctx = Ctx#{hosts => AllHosts}},
        #step{hosts = [hd(AllHosts)], function = wait_for_init}
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
    Hosts = (service:get_module(ServiceName)):get_hosts(),
    get_steps(set_cluster_ips, Ctx#{hosts => Hosts});

get_steps(get_nagios_response, #{name := Name}) ->
    [#step{
        function = get_nagios_response,
        hosts = (service:get_module(Name)):get_hosts(),
        selection = any
    }];
get_steps(reload_webcert, Ctx) ->
    [#step{function = reload_webcert, selection = all}].

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
    app_config_file := AppConfigFile, vm_args_file := VmArgsFile} = Ctx) ->

    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(Name, Host),
    CmNodes = onepanel_cluster:hosts_to_nodes(
        service_cluster_manager:name(),
        [MainCmHost | lists:delete(MainCmHost, CmHosts)]
    ),
    DbPort = service_ctx:get(couchbase_port, Ctx),
    DbNodes = lists:map(fun(DbHost) ->
        onepanel_utils:convert(string:join([DbHost, DbPort], ":"), atom)
    end, DbHosts),

    onepanel_env:write([Name, cm_nodes], CmNodes, AppConfigFile),
    onepanel_env:write([Name, db_nodes], DbNodes, AppConfigFile),

    case InitIp of
        true ->
            IP = case onepanel_maps:get([cluster_ips, Host], Ctx, undefined) of
                undefined -> get_initial_ip(AppConfigFile);
                Found ->
                    onepanel_deployment:mark_completed(?PROGRESS_CLUSTER_IPS),
                    {ok, IPTuple} = onepanel_ip:parse_ip4(Found),
                    IPTuple
            end,
            onepanel_env:write([name(), external_ip], IP, AppConfigFile);
        false -> ok
    end,

    maps:fold(fun(Key, Value, _) ->
        onepanel_env:write([Name, Key], Value, AppConfigFile)
    end, #{}, AppConfig),

    onepanel_vm:write("name", Node, VmArgsFile),
    onepanel_vm:write("setcookie", maps:get(cookie, Ctx, erlang:get_cookie()),
        VmArgsFile),

    setup_cert_paths(Ctx),

    service:add_host(Name, Host).

%%--------------------------------------------------------------------
%% @doc {@link service:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(#{init_script := InitScript, custom_cmd_env := CustomCmdEnv} = Ctx) ->
    service:start(InitScript, Ctx, CustomCmdEnv).

%%--------------------------------------------------------------------
%% @doc {@link service:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(#{init_script := InitScript, custom_cmd_env := CustomCmdEnv}) ->
    service:stop(InitScript, CustomCmdEnv).


%%--------------------------------------------------------------------
%% @doc {@link service:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> running | stopped | not_found.
status(#{init_script := InitScript, custom_cmd_env := CustomCmdEnv}) ->
    service:status(InitScript, CustomCmdEnv).


%%--------------------------------------------------------------------
%% @doc Waits for initialization of the service.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:ctx()) -> ok | no_return().
wait_for_init(#{name := Name, wait_for_init_attempts := Attempts,
    wait_for_init_delay := Delay} = Ctx) ->
    Module = service:get_module(Name),
    onepanel_utils:wait_until(Module, get_nagios_status, [Ctx], {equal, ok},
        Attempts, Delay).


%%--------------------------------------------------------------------
%% @doc Returns nagios report for the service.
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_response(Ctx :: service:ctx()) ->
    Response :: http_client:response().
get_nagios_response(#{nagios_protocol := Protocol, nagios_port := Port}) ->
    Host = onepanel_cluster:node_to_host(),
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
    {ok, 200, _Headers, Body} = get_nagios_response(Ctx),

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
set_node_ip(#{name := ServiceName, app_config_file := AppConfigFile} = Ctx) ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(ServiceName, Host),

    {ok, IP} = case onepanel_maps:get([cluster_ips, Host], Ctx) of
        {ok, NewIP} ->
            onepanel_deployment:mark_completed(?PROGRESS_CLUSTER_IPS),
            onepanel_ip:parse_ip4(NewIP);
        _ -> {ok, get_initial_ip(AppConfigFile)}
    end,

    onepanel_env:write([name(), external_ip], IP, AppConfigFile),
    ok = rpc:call(Node, application, set_env, [name(), external_ip, IP]).


%%--------------------------------------------------------------------
%% @doc Creates response about cluster IPs with all worker
%% hosts and their IPs
%% @end
%%--------------------------------------------------------------------
-spec get_cluster_ips(service:ctx()) ->
    #{isConfigured := boolean(), hosts := #{binary() => binary()}}.
get_cluster_ips(#{name := ServiceName} = _Ctx) ->
    Pairs = lists:map(fun(Host) ->
        Node = onepanel_cluster:host_to_node(ServiceName, Host),
        {_, _, _, _} = IP = rpc:call(Node, node_manager, get_ip_address, []),
        {binary:list_to_bin(Host), onepanel_ip:ip4_to_binary(IP)}
    end, (service:get_module(ServiceName)):get_hosts()),
    #{
        isConfigured => onepanel_deployment:is_completed(?PROGRESS_CLUSTER_IPS),
        hosts => maps:from_list(Pairs)
    }.


%%--------------------------------------------------------------------
%% @doc
%% Clears worker ssl cache to ensure certificates changed on disk
%% are reloaded.
%% @end
%%--------------------------------------------------------------------
reload_webcert(#{name := ServiceName} = Ctx) ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(ServiceName, Host), ok,
    ok = rpc:call(Node, ssl, clear_pem_cache, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Writes certificate paths to the app config file of the service.
%% @end
%%--------------------------------------------------------------------
-spec setup_cert_paths(service:ctx()) -> ok | no_return().
setup_cert_paths(#{name := AppName, app_config_file := AppConfigFile}) ->
    lists:foreach(fun({Src, Dst}) ->
        Path = filename:absname(onepanel_env:get(Src)),
        ok = onepanel_env:write([AppName, Dst], Path, AppConfigFile)
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
-spec get_initial_ip(file:name()) -> inet:ip4_address().
get_initial_ip(AppConfigFile) ->
    case onepanel_env:read([name(), external_ip], AppConfigFile) of
        {ok, {_, _, _, _} = IP} -> IP;
        {ok, IPList} when is_list(IPList) ->
            {ok, IP} = onepanel_ip:parse_ip4(IPList),
            IP;
        _ -> onepanel_ip:determine_ip()
    end.
