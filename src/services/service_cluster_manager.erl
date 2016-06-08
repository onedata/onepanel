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

-include("db/models.hrl").
-include("service.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([get_steps/2]).

%% API
-export([name/0, configure/1, start/1, stop/1, status/1]).

-define(NAME, cluster_manager).
-define(INIT_SCRIPT, "cluster_manager").

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_steps/2
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := Hosts} = Ctx) ->
    [
        #step{function = configure,
            ctx = Ctx#{main_host => maps:get(main_host, Ctx, hd(Hosts))}},
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

get_steps(_Action, _Ctx) ->
    throw(action_not_supported).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    ?NAME.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(#{main_host := MainHost, hosts := Hosts} = Ctx) ->
    AppConfigPath = service:param(cluster_manager_app_config_path, Ctx),
    VmArgsPath = service:param(cluster_manager_vm_args_path, Ctx),
    Host = onepanel_utils:node_to_host(),
    Node = onepanel_utils:host_to_node(?NAME, Host),

    ok = app_config_editor:write([?NAME, cm_nodes],
        onepanel_utils:hosts_to_nodes(?NAME, Hosts), AppConfigPath),
    ok = app_config_editor:write([?NAME, worker_num],
        maps:get(worker_num, Ctx, undefined), AppConfigPath),

    set_wait_process(Host, Ctx),
    set_distribution(Host, Ctx),

    ok = vm_config_editor:write(<<"name">>, erlang:atom_to_binary(Node, utf8),
        VmArgsPath),
    ok = vm_config_editor:write(<<"setcookie">>,
        erlang:atom_to_binary(maps:get(cookie, Ctx, erlang:get_cookie()), utf8),
        VmArgsPath),

    service:create(#service{name = ?NAME, hosts = []}),
    service:add_host(?NAME, Host),
    set_main_host(MainHost, Host).


%%--------------------------------------------------------------------
%% @doc @see service:start/1
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    service:start(?INIT_SCRIPT, #{
        open_files => service:param(cluster_manager_open_files_limit, Ctx)
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec set_wait_process(Host :: service:host(), Ctx :: service:ctx()) ->
    ok | no_return().
set_wait_process(Host, #{main_host := MainHost} = Ctx) ->
    EnvPath = service:param(cluster_manager_env_path, Ctx),

    WaitProcess = case Host of
        MainHost -> "cluster_manager_sup";
        _ -> "kernel_sup"
    end,

    onepanel_shell:sed("WAIT_FOR_PROCESS=.*",
        "WAIT_FOR_PROCESS=" ++ WaitProcess, EnvPath).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec set_distribution(Host :: service:host(), Ctx :: service:ctx()) ->
    ok | no_return().
set_distribution(_Host, #{hosts := [_]}) ->
    ok;

set_distribution(Host, #{main_host := MainHost, hosts := Hosts} = Ctx) ->
    AppConfigPath = service:param(cluster_manager_app_config_path, Ctx),
    Node = onepanel_utils:host_to_node(?NAME, Host),
    Nodes = onepanel_utils:hosts_to_nodes(?NAME, Hosts),
    MainNode = onepanel_utils:host_to_node(?NAME, MainHost),

    ok = app_config_editor:write([kernel, distributed], [{
        ?NAME,
        service:param(cluster_manager_failover_timeout, Ctx),
        [MainNode, list_to_tuple(Nodes -- [MainNode])]
    }], AppConfigPath),
    ok = app_config_editor:write([kernel, sync_nodes_mandatory],
        Nodes -- [Node], AppConfigPath),
    ok = app_config_editor:write([kernel, sync_nodes_timeout],
        service:param(cluster_manager_sync_nodes_timeout, Ctx),
        AppConfigPath).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec set_main_host(MainHost :: string(), Host :: string()) -> ok.
set_main_host(MainHost, MainHost) ->
    ok = service:update(?NAME, fun(#service{params = Params} = Service) ->
        Service#service{params = maps:put(main_host, MainHost, Params)}
    end);

set_main_host(_MainHost, _Host) ->
    ok.