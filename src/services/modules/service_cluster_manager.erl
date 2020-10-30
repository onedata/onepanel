%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains cluster_manager service management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_cluster_manager).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("service.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/global_definitions.hrl").

% @formatter:off
-type model_ctx() :: #{
    % the working host, as opposed to backup instances
    main_host => service:host(),

    %% Caches (i.e. not the primary source of truth):
    % service status cache
    status => #{service:host() => service:status()}
}.
% @formatter:on

-export_type([model_ctx/0]).

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% Public API
-export([get_main_host/0]).

%% Step functions
-export([configure/1, start/1, stop/1, status/1, wait_for_init/1, health/1,
    synchronize_clock_upon_start/1, migrate_generated_config/1, update_workers_number/1]).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    cluster_manager.


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
get_steps(deploy, #{hosts := [_ | _] = Hosts} = Ctx) ->
    service:create(#service{name = name()}),

    {ClusterHosts, MainHost} = case service:get(name()) of
        {ok, #service{hosts = CHosts, ctx = #{main_host := Host}}} ->
            {CHosts, maps:get(main_host, Ctx, Host)};
        {ok, #service{hosts = CHosts}} ->
            {CHosts, maps:get(main_host, Ctx, hd(Hosts))}
    end,
    AllHosts = lists_utils:union(Hosts, ClusterHosts),
    [
        #step{module = service, function = save, args = [#service{name = name(),
            ctx = #{main_host => MainHost}}], selection = first},
        #step{function = migrate_generated_config,
            condition = fun(_) -> onepanel_env:legacy_config_exists(name()) end},
        #step{hosts = [MainHost], function = configure, ctx = Ctx#{
            main_host => MainHost, wait_for_process => "cluster_manager_sup"}},
        #step{hosts = lists_utils:subtract(Hosts, [MainHost]),
            function = configure, ctx = Ctx#{
                main_host => MainHost, wait_for_process => "kernel_sup"
            }
        },
        #steps{action = restart, ctx = Ctx#{hosts => AllHosts}},
        #step{hosts = AllHosts, function = wait_for_init},
        #step{hosts = AllHosts, function = synchronize_clock_upon_start},
        % refresh status cache
        #steps{action = status, ctx = #{hosts => AllHosts}}
    ];

get_steps(deploy, _Ctx) ->
    [];

get_steps(resume, _Ctx) -> [
    #step{function = migrate_generated_config,
        condition = fun(_) -> onepanel_env:legacy_config_exists(name()) end},
    #steps{action = start},
    #step{hosts = [hosts:self()], function = wait_for_init},
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

get_steps(update_workers_number, _Ctx) ->
    [#step{function = update_workers_number}].


%%%===================================================================
%%% Public API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the primary Cluster Manager host
%% (as opposed to backup instances).
%% @end
%%--------------------------------------------------------------------
-spec get_main_host() -> {ok, service:host()} | {error, term()}.
get_main_host() ->
    case service:get_ctx(name()) of
        #{main_host := MainHost} -> {ok, MainHost};
        {error, _} = Error -> Error
    end.


%%%===================================================================
%%% Step functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:step_ctx()) -> ok | no_return().
configure(#{main_host := MainHost, hosts := Hosts,
    wait_for_process := Process} = Ctx) ->

    VmArgsFile = onepanel_env:get(cluster_manager_vm_args_file),
    EnvFile = onepanel_env:get(cluster_manager_env_file),

    Host = hosts:self(),
    Node = nodes:local(name()),
    Nodes = nodes:service_to_nodes(name(), Hosts),
    MainNode = nodes:service_to_node(name(), MainHost),

    WorkerNum = maps:get(worker_num, Ctx, undefined),
    Cookie = erlang:get_cookie(),

    onepanel_env:write([name(), cm_nodes], Nodes, ?SERVICE_CM),
    onepanel_env:write([name(), worker_num], WorkerNum, ?SERVICE_CM),

    onepanel_env:write([kernel, distributed], [{
        name(),
        onepanel_env:get(cluster_manager_failover_timeout),
        [MainNode, list_to_tuple(Nodes -- [MainNode])]
    }], ?SERVICE_CM),
    onepanel_env:write([kernel, sync_nodes_mandatory],
        Nodes -- [Node], ?SERVICE_CM),
    onepanel_env:write([kernel, sync_nodes_timeout],
        onepanel_env:get(cluster_manager_sync_nodes_timeout),
        ?SERVICE_CM),

    onepanel_vm:write("name", Node, VmArgsFile),
    onepanel_vm:write("setcookie", Cookie, VmArgsFile),

    onepanel_shell:sed("WAIT_FOR_PROCESS=.*",
        "WAIT_FOR_PROCESS=" ++ Process, EnvFile),

    service:add_host(name(), Host).


%%--------------------------------------------------------------------
%% @doc Writes current worker nodes number to cluster manager's app config.
%% @end
%%--------------------------------------------------------------------
-spec update_workers_number(#{worker_num => non_neg_integer(), _ => _}) -> ok.
update_workers_number(#{worker_num := WorkerNum}) ->
    % Cluster manager/workers do not currently support dynamic resizing.
    % Therefore the value is just set in the config file and the whole
    % cluster is restarted in further steps.
    onepanel_env:write([name(), worker_num], WorkerNum, ?SERVICE_CM);

update_workers_number(_) ->
    WorkerNum = case onepanel_env:get_cluster_type() of
        ?ONEPROVIDER -> length(service_op_worker:get_hosts());
        ?ONEZONE -> length(service_oz_worker:get_hosts())
    end,
    update_workers_number(#{worker_num => WorkerNum}).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:step_ctx()) -> ok | no_return().
start(_Ctx) ->
    Limits = #{
        open_files => onepanel_env:get(cluster_manager_open_files_limit)
    },
    service_cli:start(name(), Limits),
    service:update_status(name(), healthy),
    service:register_healthcheck(name(), #{hosts => [hosts:self()]}),
    ok.


%%--------------------------------------------------------------------
%% @doc {@link service_cli:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:step_ctx()) -> ok.
stop(Ctx) ->
    service:deregister_healthcheck(name(), Ctx),
    service_cli:stop(name()),
    % check status before updating it as service_cli:stop/1 does not throw on failure
    status(Ctx),
    ok.


%%--------------------------------------------------------------------
%% @doc {@link service_cli:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:step_ctx()) -> service:status().
status(Ctx) ->
    service:update_status(name(),
        case service_cli:status(name(), ping) of
            running -> health(Ctx);
            stopped -> stopped;
            missing -> missing
        end).


-spec wait_for_init(Ctx :: service:step_ctx()) -> ok | no_return().
wait_for_init(Ctx) ->
    ServiceName = name(),
    Attempts = onepanel_env:get(cluster_manager_wait_for_init_attempts),
    Delay = onepanel_env:get(cluster_manager_wait_for_init_delay),
    Module = service:get_module(ServiceName),
    onepanel_utils:wait_until(Module, health, [Ctx], {equal, healthy}, Attempts, Delay),
    service:update_status(ServiceName, healthy),
    ok.


%%--------------------------------------------------------------------
%% @doc Checks if a running service is in a fully functional state.
%% @end
%%--------------------------------------------------------------------
-spec health(service:step_ctx()) -> service:status().
health(_) ->
    CmNode = nodes:local(name()),
    case rpc:call(CmNode, global, whereis_name, [?CLUSTER_MANAGER]) of
        Pid when is_pid(Pid) -> healthy;
        undefined -> unhealthy;
        {badrpc, _} -> stopped
    end.


-spec synchronize_clock_upon_start(Ctx :: service:step_ctx()) -> ok | no_return().
synchronize_clock_upon_start(_) ->
    CmNode = nodes:local(name()),
    case onepanel_env:get_cluster_type() of
        ?ONEZONE -> onezone_cluster_clocks:synchronize_node_upon_start(CmNode);
        ?ONEPROVIDER -> oneprovider_cluster_clocks:synchronize_node_upon_start(CmNode)
    end.


%%--------------------------------------------------------------------
%% @doc {@link onepanel_env:migrate_generated_config/2}
%% @end
%%--------------------------------------------------------------------
-spec migrate_generated_config(service:step_ctx()) -> ok | no_return().
migrate_generated_config(_Ctx) ->
    onepanel_env:upgrade_app_config(name(), [
        [cluster_manager, cm_nodes],
        [cluster_manager, worker_num],
        [kernel, distributed],
        [kernel, sync_nodes_mandatory],
        [kernel, sync_nodes_timeout]
    ]).