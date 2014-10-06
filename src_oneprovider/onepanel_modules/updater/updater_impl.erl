%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Passive implementation of update procedures. This module behaves as callback provider
%%       for updater_engine.
%% @end
%% ===================================================================
-module(updater_impl).
-author("Rafal Slota").

-include("onepanel_modules/updater/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([get_error_level/5, finalize_stage/3, dispatch_object/4, rollback_object/4]).
-export([handle_stage/3, handle_rollback/3]).


%% ====================================================================
%% General callbacks
%% ====================================================================


%% get_error_level/5
%% ====================================================================
%% @doc For given Stage, Job, Object and {error, Reason}, decides whether it shall be an 'error' (interrupting)
%%      update process, or just 'warning'. Note that all rollback error are by default warnings since
%%      rollback procedure cannot be interrupted.
%% @end
-spec get_error_level(Stage :: atom(), Job :: atom(), Object :: term(), Reason :: term(), State :: #?U_STATE{}) -> error | warning.
%% ====================================================================
get_error_level(?STAGE_NODE_RESTART, _Job, _Object, _Reason, _State) ->
    warning;
get_error_level(?STAGE_DAO_POST_SETUP_VIEWS, ?JOB_CLEANUP_VIEWS, _Object, _Reason, _State) ->
    warning;
get_error_level(_Stage, _Job, _Object, _Reason, _State) ->
    error.


%% ====================================================================
%% Install callbacks
%% ====================================================================


%% finalize_stage/3
%% ====================================================================
%% @doc Callback-hook called after every Job.
%% @end
-spec finalize_stage(Stage :: atom(), Job :: atom(), State :: #?U_STATE{}) -> NewState :: #?U_STATE{}.
%% ====================================================================
finalize_stage(?STAGE_INIT, ?JOB_DOWNLOAD_BINARY, #?U_STATE{previous_data = PData} = State) ->
    #{package := #package{} = Pkg} = PData,
    State#?U_STATE{package = Pkg, previous_data = maps:remove(package, PData)};

finalize_stage(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, #?U_STATE{previous_data = PData} = State) ->
    #{views := Views} = PData,
    ?info("Installed views: ~p", [Views]),
    State#?U_STATE{installed_views = Views, previous_data = maps:remove(views, PData)};

finalize_stage(?STAGE_SOFT_RELOAD, ?JOB_DEFAULT, #?U_STATE{previous_data = PData} = State) ->
    ModMap = maps:to_list(PData),
    NotReloaded = [{Node, [Module || {Module, false} <- IModMap]} || {Node, IModMap} <- ModMap],
    ?info("Not-reloaded modules: ~p", [NotReloaded]),
    State#?U_STATE{not_reloaded_modules = maps:from_list(NotReloaded)};

finalize_stage(?STAGE_DEPLOY_FILES, ?JOB_DEPLOY, #?U_STATE{nodes = Nodes, previous_data = PData, force_node_restart = ForceRestart} = State) ->
    LMap = maps:to_list(PData),
    ToRestart =
        case ForceRestart of
            true -> Nodes;
            _ -> [Node || {Node, true} <- LMap]
        end,
    ?info("Nodes to restart: ~p", [ToRestart]),
    State#?U_STATE{nodes_to_restart = ToRestart};

finalize_stage(Stage, Job, State) ->
    ?debug("Unknown finalize: ~p:~p", [Stage, Job]),
    State.


%% handle_stage/3
%% ====================================================================
%% @doc Callback-hook called to generate list of Objects (that are passed to dispatch_object/4 in order to start them).
%%      This function can also modify current updater's state.
%% @end
-spec handle_stage(Stage :: atom(), Job :: atom(), State :: #?U_STATE{}) ->
    [Object :: term()] | {[Object :: term()], NewState :: #?U_STATE{}}.
%% ====================================================================
handle_stage(?STAGE_IDLE, _, #?U_STATE{} = _State) ->
    [];

handle_stage(?STAGE_REPAIR_NODES, _, #?U_STATE{}) ->
    [];

handle_stage(?STAGE_INIT, ?JOB_RELOAD_EXPORTS, #?U_STATE{nodes = _Nodes} = State) ->
    handle_stage(?STAGE_INIT, ?JOB_LOAD_EXPORTS, State);

handle_stage(?STAGE_INIT, ?JOB_LOAD_EXPORTS, #?U_STATE{nodes = Nodes} = _State) ->
    Nodes;

handle_stage(?STAGE_INIT, ?JOB_CHECK_CONNECTIVITY, #?U_STATE{nodes = Nodes} = _State) ->
    Hostnames = lists:usort([onepanel_utils:get_host(Node) || Node <- Nodes]),
    OnePanelNodes = [onepanel_utils:get_node(Host) || Host <- Hostnames],
    Nodes ++ OnePanelNodes;

handle_stage(?STAGE_INIT, ?JOB_DOWNLOAD_BINARY, #?U_STATE{} = _State) ->
    package;

handle_stage(?STAGE_INIT, ?JOB_INSTALL_PACKAGE, #?U_STATE{nodes = Nodes} = _State) ->
    updater_utils:select_only_workers(Nodes);

handle_stage(?STAGE_DAO_UPDATER_LOAD, _, #?U_STATE{nodes = Nodes} = _State) ->
    Nodes;

handle_stage(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, #?U_STATE{nodes = _Nodes} = _State) ->
    views;

handle_stage(?STAGE_DAO_SETUP_VIEWS, _, #?U_STATE{nodes = Nodes} = _State) ->
    updater_utils:select_only_workers(Nodes);

handle_stage(?STAGE_DAO_REFRESH_VIEWS, _, #?U_STATE{nodes = _Nodes, installed_views = Views} = _State) ->
    Views;

handle_stage(?STAGE_DEPLOY_FILES, _, #?U_STATE{nodes = Nodes} = _State) ->
    Nodes;

handle_stage(?STAGE_SOFT_RELOAD, _, #?U_STATE{nodes = Nodes} = _State) ->
    Nodes;

handle_stage(?STAGE_FORCE_RELOAD, _, #?U_STATE{nodes = Nodes} = _State) ->
    Nodes;

handle_stage(?STAGE_DAO_POST_SETUP_VIEWS, _, #?U_STATE{} = _State) ->
    views_cleanup;

handle_stage(?STAGE_NODE_RESTART, RestartNode, #?U_STATE{} = _State) ->
    RestartNode;

handle_stage(Stage, Job, #?U_STATE{}) ->
    throw({invalid_stage, {Stage, Job}}).


%% dispatch_object/4
%% ====================================================================
%% @doc Callback-hook called to generate given Object. This function shall return
%%      Pid of process that processes the Object. The process has to send ok | {ok, Data} | {error, Reason} to self().
%% @end
-spec dispatch_object(Stage :: atom(), Job :: atom(), Obj :: term(), State :: #?U_STATE{}) -> ObjectProc :: pid().
%% ====================================================================
dispatch_object(?STAGE_INIT, ?JOB_RELOAD_EXPORTS, Obj, State) ->
    dispatch_object(?STAGE_INIT, ?JOB_LOAD_EXPORTS, Obj, State);
dispatch_object(?STAGE_INIT, ?JOB_LOAD_EXPORTS, Obj, #?U_STATE{}) ->
    Node = Obj,
    updater_utils:local_cast(fun() -> load_module_to_remote(Node, updater_export) end);

dispatch_object(?STAGE_INIT, ?JOB_CHECK_CONNECTIVITY, Obj, #?U_STATE{}) ->
    Node = Obj,
    updater_utils:local_cast(fun() -> check_connectivity(Node) end);

dispatch_object(?STAGE_INIT, ?JOB_DOWNLOAD_BINARY, _Obj, #?U_STATE{version = Vsn}) ->
    updater_utils:local_cast(fun() -> updater_repos:get_package(Vsn) end);

dispatch_object(?STAGE_INIT, ?JOB_INSTALL_PACKAGE, Obj, #?U_STATE{package = Pkg}) ->
    Node = Obj,
    updater_utils:cast(Node, install_package, [Pkg]);

dispatch_object(?STAGE_DAO_UPDATER_LOAD, ?JOB_MOVE_BEAMS, Obj, #?U_STATE{}) ->
    Node = Obj,
    updater_utils:cast(Node, move_file, ["dao_update.beam"]);

dispatch_object(?STAGE_DAO_UPDATER_LOAD, ?JOB_LOAD_BEAMS, Obj, #?U_STATE{}) ->
    Node = Obj,
    updater_utils:cast(Node, force_reload_module, [dao_update]);

dispatch_object(?STAGE_DAO_UPDATER_LOAD, ?JOB_PRE_UPDATE, Obj, #?U_STATE{version = Vsn}) ->
    Node = Obj,
    updater_utils:cast(Node, run_pre_update, [Vsn]);

dispatch_object(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEW_SOURCES, Obj, #?U_STATE{}) ->
    Node = Obj,
    updater_utils:cast(Node, install_view_sources, []);

dispatch_object(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, _Obj, #?U_STATE{nodes = Nodes}) ->
    updater_utils:anycast(updater_utils:select_only_workers(Nodes), install_views, []);

dispatch_object(?STAGE_DAO_REFRESH_VIEWS, ?JOB_DEFAULT, Obj, #?U_STATE{nodes = Nodes}) ->
    View = Obj,
    updater_utils:anycast(updater_utils:select_only_workers(Nodes), refresh_view, [View]);

dispatch_object(?STAGE_DEPLOY_FILES, ?JOB_BACKUP, Obj, #?U_STATE{}) ->
    Node = Obj,
    updater_utils:cast(Node, backup_instalation, []);

dispatch_object(?STAGE_DEPLOY_FILES, ?JOB_DEPLOY, Obj, #?U_STATE{}) ->
    Node = Obj,
    updater_utils:cast(Node, move_all_files, []);

dispatch_object(?STAGE_SOFT_RELOAD, ?JOB_DEFAULT, Obj, #?U_STATE{}) ->
    Node = Obj,
    updater_utils:cast(Node, soft_reload_all_modules, []);

dispatch_object(?STAGE_FORCE_RELOAD, ?JOB_DEFAULT, Obj, #?U_STATE{not_reloaded_modules = NotReloaded}) ->
    Node = Obj,
    WaitTime =
        case length(maps:get(Node, NotReloaded)) of
            0 -> 0;
            _ -> 2 * 60 * 1000
        end,
    updater_utils:cast(Node, force_reload_modules, [maps:get(Node, NotReloaded), WaitTime]);

dispatch_object(?STAGE_DAO_POST_SETUP_VIEWS, ?JOB_CLEANUP_VIEWS, _Obj, #?U_STATE{nodes = Nodes}) ->
    updater_utils:anycast(Nodes, remove_outdated_views, []);

dispatch_object(?STAGE_NODE_RESTART, _, Obj, #?U_STATE{}) ->
    Node = Obj,
    updater_utils:local_cast(fun() -> oneprovider_restart(Node) end);

dispatch_object(Stage, Job, Obj, #?U_STATE{}) ->
    throw({unknown_dispatch, {Stage, Job, Obj}}).


%% ====================================================================
%% Rollback callbacks
%% ====================================================================


%% handle_rollback/3
%% ====================================================================
%% @doc Callback-hook called to generate list of Objects (that are passed to rollback_object/4 in order to start them).
%%      This function can also modify current updater's state.
%% @end
-spec handle_rollback(Stage :: atom(), Job :: atom(), State :: #?U_STATE{}) ->
    [Object :: term()] | {[Object :: term()], NewState :: #?U_STATE{}}.
%% ====================================================================
handle_rollback(?STAGE_REPAIR_NODES, RepairNode, #?U_STATE{}) ->
    RepairNode;

handle_rollback(?STAGE_DEPLOY_FILES, ?JOB_DEPLOY, #?U_STATE{nodes = Nodes} = State) ->
    {Nodes, State#?U_STATE{nodes_to_repair = Nodes}};

handle_rollback(_Stage, _Job, #?U_STATE{}) ->
    [].


%% rollback_object/4
%% ====================================================================
%% @doc Callback-hook called to generate given rollback Object. This function shall return
%%      Pid of process that processes the Object. The process has to send ok | {ok, Data} | {error, Reason} to self().
%% @end
-spec rollback_object(Stage :: atom(), Job :: atom(), Obj :: term(), State :: #?U_STATE{}) -> ObjectProc :: pid().
%% ====================================================================
rollback_object(?STAGE_DEPLOY_FILES, ?JOB_DEPLOY, Obj, #?U_STATE{}) ->
    Node = Obj,
    updater_utils:cast(Node, revert_instalation, []);

rollback_object(?STAGE_REPAIR_NODES, _, Obj, #?U_STATE{}) ->
    Node = Obj,
    updater_utils:local_cast(fun() -> oneprovider_restart(Node) end);

rollback_object(Stage, Job, Obj, #?U_STATE{}) ->
    throw({unknown_rollback, {Stage, Job, Obj}}).


%% ====================================================================
%% Internal functions
%% ====================================================================


%% oneprovider_restart/1
%% ====================================================================
%% @doc Restarts given oneprovider_node, awaits its start up and waits a dozen or so seconds
%%      to generate some time window between successive node restarts.
%% @end
-spec oneprovider_restart(Node :: atom()) -> ok | {error, {node_down, Node :: atom()}}.
%% ====================================================================
oneprovider_restart(Node) ->
    [NodeType, _] = string:tokens(atom_to_list(Node), "@"),
    OnePanelNode = onepanel_utils:get_node(onepanel_utils:get_host(Node)),
    Mod = list_to_atom("installer_" ++ NodeType),
    case rpc:call(OnePanelNode, Mod, local_restart, []) of
        {ok, _} ->
            updater_utils:wait_for_node(Node, ?NODE_STARTUP_TIMEOUT),
            timer:sleep(?DELAY_BETWEEN_NODE_RESTARTS);
        {error, _Reason} ->
            {error, {restart_fail, Node}}
    end.


%% check_connectivity/1
%% ====================================================================
%% @doc Checks if the node is up.
%% @end
-spec check_connectivity(Node :: atom()) -> ok | {error, {node_down, Node :: atom()}}.
%% ====================================================================
check_connectivity(Node) ->
    case net_adm:ping(Node) of
        pong -> ok;
        pang -> {error, {node_down, Node}}
    end.


%% load_module_to_remote/2
%% ====================================================================
%% @doc Injects given local module into remote Node.
%% @end
-spec load_module_to_remote(Node :: atom(), Mod :: atom()) -> ok | {error, Reason :: any()}.
%% ====================================================================
load_module_to_remote(Node, Module) ->
    code:purge(Module),
    code:load_file(Module),
    code:purge(Module),
    {Module, Bin, _FileName} = code:get_object_code(Module),
    rpc:call(Node, code, purge, [Module]),
    case rpc:call(Node, code, load_binary, [Module, preloaded, Bin]) of
        {module, _} -> ok;
        {error, Reason} ->
            {error, Reason}
    end.