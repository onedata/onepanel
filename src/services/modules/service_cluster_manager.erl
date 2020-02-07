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
-include("service.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, start/1, stop/1, status/1, migrate_generated_config/1]).

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
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := [_ | _] = Hosts} = Ctx) ->
    service:create(#service{name = name()}),

    {ClusterHosts, MainHost} = case service:get(name()) of
        {ok, #service{hosts = CHosts, ctx = #{main_host := Host}}} ->
            {CHosts, maps:get(main_host, Ctx, Host)};
        {ok, #service{hosts = CHosts}} ->
            {CHosts, maps:get(main_host, Ctx, hd(Hosts))}
    end,
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
        #steps{action = restart, ctx = Ctx#{
            hosts => lists_utils:union(ClusterHosts, Hosts)
        }}
    ];

get_steps(deploy, _Ctx) ->
    [];

get_steps(resume, _Ctx) -> [
    #step{function = migrate_generated_config,
        condition = fun(_) -> onepanel_env:legacy_config_exists(name()) end},
    #steps{action = start}
];

get_steps(start, _Ctx) ->
    [#step{function = start}];

get_steps(stop, _Ctx) ->
    [#step{function = stop}];

get_steps(restart, _Ctx) ->
    [#step{function = stop}, #step{function = start}];

get_steps(status, _Ctx) ->
    [#step{function = status}].

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(#{main_host := MainHost, hosts := Hosts,
    wait_for_process := Process} = Ctx) ->

    GeneratedConfigFile = service_ctx:get(cluster_manager_generated_config_file, Ctx),
    VmArgsFile = service_ctx:get(cluster_manager_vm_args_file, Ctx),
    EnvFile = service_ctx:get(cluster_manager_env_file, Ctx),

    Host = hosts:self(),
    Node = nodes:local(name()),
    Nodes = nodes:service_to_nodes(name(), Hosts),
    MainNode = nodes:service_to_node(name(), MainHost),

    WorkerNum = maps:get(worker_num, Ctx, undefined),
    Cookie = maps:get(cookie, Ctx, erlang:get_cookie()),

    onepanel_env:write([name(), cm_nodes], Nodes, GeneratedConfigFile),
    onepanel_env:write([name(), worker_num], WorkerNum, GeneratedConfigFile),

    onepanel_env:write([kernel, distributed], [{
        name(),
        service_ctx:get(cluster_manager_failover_timeout, Ctx, integer),
        [MainNode, list_to_tuple(Nodes -- [MainNode])]
    }], GeneratedConfigFile),
    onepanel_env:write([kernel, sync_nodes_mandatory],
        Nodes -- [Node], GeneratedConfigFile),
    onepanel_env:write([kernel, sync_nodes_timeout],
        service_ctx:get(cluster_manager_sync_nodes_timeout, Ctx, integer),
        GeneratedConfigFile),

    onepanel_vm:write("name", Node, VmArgsFile),
    onepanel_vm:write("setcookie", Cookie, VmArgsFile),

    onepanel_shell:sed("WAIT_FOR_PROCESS=.*",
        "WAIT_FOR_PROCESS=" ++ Process, EnvFile),

    service:add_host(name(), Host).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    Limits = #{
        open_files => service_ctx:get(cluster_manager_open_files_limit, Ctx)
    },
    service_cli:start(name(), Limits),
    service:update_status(name(), healthy),
    service:register_healthcheck(name(), #{hosts => [hosts:self()]}),
    ok.


%%--------------------------------------------------------------------
%% @doc {@link service_cli:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok.
stop(Ctx) ->
    onepanel_cron:remove_job(name()),
    service_cli:stop(name()),
    % check status before updating it as service_cli:stop/1 does not throw on failure
    status(Ctx),
    ok.


%%--------------------------------------------------------------------
%% @doc {@link service_cli:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> service:status().
status(_Ctx) ->
    service:update_status(name(),
        case service_cli:status(name(), ping) of
            running -> healthy;
            stopped -> stopped;
            missing -> missing
        end).


%%--------------------------------------------------------------------
%% @doc {@link onepanel_env:migrate_generated_config/2}
%% @end
%%--------------------------------------------------------------------
-spec migrate_generated_config(service:ctx()) -> ok | no_return().
migrate_generated_config(_Ctx) ->
    onepanel_env:upgrade_app_config(name(), [
        [cluster_manager, cm_nodes],
        [cluster_manager, worker_num],
        [kernel, distributed],
        [kernel, sync_nodes_mandatory],
        [kernel, sync_nodes_timeout]
    ]).