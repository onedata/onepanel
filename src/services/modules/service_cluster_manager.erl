%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(service_cluster_manager).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("modules/logger.hrl").
-include("modules/models.hrl").
-include("service.hrl").

%% Service behaviour callbacks
-export([name/0, get_steps/2]).

%% API
-export([configure/1, start/1, stop/1, status/1]).

-define(INIT_SCRIPT, "cluster_manager").

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see service_behaviour:name/0
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    cluster_manager.


%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_steps/2
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := Hosts} = Ctx) ->
    MainHost = maps:get(main_host, Ctx, hd(Hosts)),
    [
        #step{module = service, function = save, args = [#service{name = name(),
            ctx = #{main_host => MainHost}}], selection = first},
        #step{hosts = [MainHost], function = configure, ctx = Ctx#{
            main_host => MainHost, wait_for_process => "cluster_manager_sup"}},
        #step{hosts = Hosts -- [MainHost], function = configure, ctx = Ctx#{
            main_host => MainHost, wait_for_process => "kernel_sup"}},
        #step{function = start}
    ];

get_steps(start, _Ctx) ->
    [#step{function = start}];

get_steps(stop, _Ctx) ->
    [#step{function = stop}];

get_steps(restart, _Ctx) ->
    [#step{function = stop}, #step{function = start}];

get_steps(status, _Ctx) ->
    [#step{function = status}];

get_steps(Action, _Ctx) ->
    ?throw({action_not_supported, Action}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(#{main_host := MainHost, hosts := Hosts, wait_for_process := Process}
    = Ctx) ->
    AppConfigPath = service_ctx:get(cluster_manager_app_config_path, Ctx),
    VmArgsPath = service_ctx:get(cluster_manager_vm_args_path, Ctx),
    EnvPath = service_ctx:get(cluster_manager_env_path, Ctx),
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(name(), Host),
    Nodes = onepanel_cluster:hosts_to_nodes(name(), Hosts),
    MainNode = onepanel_cluster:host_to_node(name(), MainHost),

    onepanel_env:write([name(), cm_nodes],
        onepanel_cluster:hosts_to_nodes(name(), Hosts), AppConfigPath),
    onepanel_env:write([name(), worker_num],
        maps:get(worker_num, Ctx, undefined), AppConfigPath),

    onepanel_env:write([kernel, distributed], [{
        name(),
        service_ctx:get(cluster_manager_failover_timeout, Ctx),
        [MainNode, list_to_tuple(Nodes -- [MainNode])]
    }], AppConfigPath),
    onepanel_env:write([kernel, sync_nodes_mandatory],
        Nodes -- [Node], AppConfigPath),
    onepanel_env:write([kernel, sync_nodes_timeout],
        service_ctx:get(cluster_manager_sync_nodes_timeout, Ctx),
        AppConfigPath),

    onepanel_vm:write(<<"name">>, erlang:atom_to_binary(Node, utf8),
        VmArgsPath),
    onepanel_vm:write(<<"setcookie">>,
        erlang:atom_to_binary(maps:get(cookie, Ctx, erlang:get_cookie()), utf8),
        VmArgsPath),

    onepanel_shell:sed("WAIT_FOR_PROCESS=.*",
        "WAIT_FOR_PROCESS=" ++ Process, EnvPath),

    service:add_host(name(), Host).


%%--------------------------------------------------------------------
%% @doc @see service:start/1
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    service:start(?INIT_SCRIPT, #{
        open_files => service_ctx:get(cluster_manager_open_files_limit, Ctx)
    }).


%%--------------------------------------------------------------------
%% @doc @see service:stop/1
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(_Ctx) ->
    service:stop(?INIT_SCRIPT).


%%--------------------------------------------------------------------
%% @doc @see service:status/1
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> ok | no_return().
status(_Ctx) ->
    service:status(?INIT_SCRIPT).