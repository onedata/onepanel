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

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, start/1, stop/1, status/1]).

-define(INIT_SCRIPT, "cluster_manager").

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
    service:get_nodes(name()).


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
        #step{hosts = [MainHost], function = configure, ctx = Ctx#{
            main_host => MainHost, wait_for_process => "cluster_manager_sup"}},
        #step{hosts = onepanel_lists:subtract(Hosts, [MainHost]),
            function = configure, ctx = Ctx#{
                main_host => MainHost, wait_for_process => "kernel_sup"
            }
        },
        #steps{action = restart, ctx = Ctx#{
            hosts => onepanel_lists:union(ClusterHosts, Hosts)
        }}
    ];

get_steps(deploy, _Ctx) ->
    [];

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

    AppConfigFile = service_ctx:get(cluster_manager_app_config_file, Ctx),
    VmArgsFile = service_ctx:get(cluster_manager_vm_args_file, Ctx),
    EnvFile = service_ctx:get(cluster_manager_env_file, Ctx),
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(name(), Host),
    Nodes = onepanel_cluster:hosts_to_nodes(name(), Hosts),
    MainNode = onepanel_cluster:host_to_node(name(), MainHost),
    WorkerNum = maps:get(worker_num, Ctx, undefined),
    Cookie = maps:get(cookie, Ctx, erlang:get_cookie()),

    onepanel_env:write([name(), cm_nodes], Nodes, AppConfigFile),
    onepanel_env:write([name(), worker_num], WorkerNum, AppConfigFile),

    onepanel_env:write([kernel, distributed], [{
        name(),
        service_ctx:get(cluster_manager_failover_timeout, Ctx, integer),
        [MainNode, list_to_tuple(Nodes -- [MainNode])]
    }], AppConfigFile),
    onepanel_env:write([kernel, sync_nodes_mandatory],
        Nodes -- [Node], AppConfigFile),
    onepanel_env:write([kernel, sync_nodes_timeout],
        service_ctx:get(cluster_manager_sync_nodes_timeout, Ctx, integer),
        AppConfigFile),

    onepanel_vm:write("name", Node, VmArgsFile),
    onepanel_vm:write("setcookie", Cookie, VmArgsFile),

    onepanel_shell:sed("WAIT_FOR_PROCESS=.*",
        "WAIT_FOR_PROCESS=" ++ Process, EnvFile),

    service:add_host(name(), Host).


%%--------------------------------------------------------------------
%% @doc {@link service:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    service:start(?INIT_SCRIPT, #{
        open_files => service_ctx:get(cluster_manager_open_files_limit, Ctx)
    }, cluster_manager_start_cmd),
    service_watcher:register_service(name()).


%%--------------------------------------------------------------------
%% @doc {@link service:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(_Ctx) ->
    service_watcher:unregister_service(name()),
    service:stop(?INIT_SCRIPT, cluster_manager_stop_cmd),
    ok.


%%--------------------------------------------------------------------
%% @doc {@link service:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> running | stopped | not_found.
status(_Ctx) ->
    service:status(?INIT_SCRIPT, cluster_manager_status_cmd).