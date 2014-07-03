%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Write me !
%% @end
%% ===================================================================
-module(updater_worker).
-behaviour(gen_server).
-author("Rafal Slota").

-include("registered_names.hrl").
-include("onepanel_modules/db_logic.hrl").
-include("onepanel_modules/install_logic.hrl").
-include("onepanel_modules/updater/common.hrl").
-include_lib("ctool/include/logging.hrl").



%% API
-export([flatten_stages/1]).

%% gen_server callbacks
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([cast/3]).

%% ====================================================================
%% API functions
%% ====================================================================

finalize_stage(#u_state{stage = Stage, job = Job} = State) ->
    finalize_stage(Stage, Job, State).


finalize_stage(?STAGE_INIT, ?JOB_DOWNLOAD_BINARY, #u_state{previous_data = PData} = State) ->
    #{package := #package{} = Pkg} = PData,
    State#u_state{package = Pkg, previous_data = maps:remove(package, PData)};
finalize_stage(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, #u_state{previous_data = PData} = State) ->
    #{views := Views} = PData,
    ?info("Installed views: ~p", [Views]),
    State#u_state{installed_views = Views, previous_data = maps:remove(views, PData)};
finalize_stage(?STAGE_SOFT_RELOAD, ?JOB_DEFAULT, #u_state{previous_data = PData} = State) ->
    ModMap = maps:to_list(PData),
    NotReloaded = [{Node, [Module || {Module, false} <- IModMap]} || {Node, IModMap} <- ModMap],
    ?info("Not-reloaded modules: ~p", [NotReloaded]),
    State#u_state{not_reloaded_modules = maps:from_list(NotReloaded)};
finalize_stage(?STAGE_DEPLOY_FILES, ?JOB_DEPLOY, #u_state{nodes = Nodes, previous_data = PData, force_node_restart = ForceRestart} = State) ->
    LMap = maps:to_list(PData),
    ToRestart =
        case ForceRestart of
            true -> Nodes;
            _    -> [Node || {Node, true} <- LMap]
        end,
    ?info("Nodes to restart: ~p", [ToRestart]),
    State#u_state{nodes_to_restart = ToRestart};
finalize_stage(Stage, Job, State) ->
    ?debug("Unknown finalize: ~p:~p", [Stage, Job]),
    State.


dispatch_object(Obj, #u_state{stage = Stage, job = Job} = State) ->
    ?info("Dispatching ~p:~p obj: ~p", [Stage, Job, Obj]),
    {dispatch_object(Stage, Job, Obj, State), Obj}.

dispatch_object(?STAGE_INIT, ?JOB_RELOAD_EXPORTS, Obj, State) ->
    dispatch_object(?STAGE_INIT, ?JOB_LOAD_EXPORTS, Obj, State);
dispatch_object(?STAGE_INIT, ?JOB_LOAD_EXPORTS, Obj, #u_state{}) ->
    Node = Obj,
    local_cast(fun() -> load_module_to_remote(Node, updater_export) end);

dispatch_object(?STAGE_INIT, ?JOB_CHECK_CONNECTIVITY, Obj, #u_state{}) ->
    Node = Obj,
    local_cast(fun() -> check_connectivity(Node) end);

dispatch_object(?STAGE_INIT, ?JOB_DOWNLOAD_BINARY, _Obj, #u_state{version = Vsn}) ->
    local_cast(fun() -> updater_repos:get_package(Vsn) end);

dispatch_object(?STAGE_INIT, ?JOB_INSTALL_PACKAGE, Obj, #u_state{package = Pkg}) ->
    Node = Obj,
    cast(Node, install_package, [Pkg]);

dispatch_object(?STAGE_DAO_UPDATER_LOAD, ?JOB_MOVE_BEAMS, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, move_file, ["dao_update.beam"]);

dispatch_object(?STAGE_DAO_UPDATER_LOAD, ?JOB_LOAD_BEAMS, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, force_reload_module, [dao_update]);

dispatch_object(?STAGE_DAO_UPDATER_LOAD, ?JOB_PRE_UPDATE, Obj, #u_state{version = Vsn}) ->
    Node = Obj,
    cast(Node, run_pre_update, [Vsn]);

dispatch_object(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEW_SOURCES, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, install_view_sources, []);

dispatch_object(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, _Obj, #u_state{nodes = Nodes}) ->
    anycast(Nodes, install_views, []);

dispatch_object(?STAGE_DAO_REFRESH_VIEWS, ?JOB_DEFAULT, Obj, #u_state{nodes = Nodes}) ->
    View = Obj,
    anycast(Nodes, refresh_view, [View]);

dispatch_object(?STAGE_DEPLOY_FILES, ?JOB_BACKUP, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, backup_instalation, []);

dispatch_object(?STAGE_DEPLOY_FILES, ?JOB_DEPLOY, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, move_all_files, []);

dispatch_object(?STAGE_SOFT_RELOAD, ?JOB_DEFAULT, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, soft_reload_all_modules, []);

dispatch_object(?STAGE_FORCE_RELOAD, ?JOB_DEFAULT, Obj, #u_state{not_reloaded_modules = NotReloaded}) ->
    Node = Obj,
    WaitTime =
        case length(maps:get(Node, NotReloaded)) of
            0 -> 0;
            _ -> 2 * 60 * 1000
        end,
    cast(Node, force_reload_modules, [maps:get(Node, NotReloaded), WaitTime]);

dispatch_object(?STAGE_DAO_POST_SETUP_VIEWS, ?JOB_CLEANUP_VIEWS, _Obj, #u_state{nodes = Nodes}) ->
    anycast(Nodes, remove_outdated_views, []);

dispatch_object(Stage, Job, Obj, #u_state{}) ->
    throw({unknown_dispatch, {Stage, Job, Obj}}).


rollback_object(Obj, #u_state{stage = Stage, job = Job} = State) ->
    ?info("Rollbacking ~p:~p obj: ~p", [Stage, Job, Obj]),
    {rollback_object(Stage, Job, Obj, State), Obj}.

rollback_object(?STAGE_DEPLOY_FILES, ?JOB_DEPLOY, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, restore_instalation, []);

rollback_object(Stage, Job, Obj, #u_state{}) ->
    throw({unknown_dispatch, {Stage, Job, Obj}}).


handle_stage(#u_state{stage = Stage, job = Job} = State) ->
    ?info("Handle stage ~p:~p", [Stage, Job]),
    handle_stage(Stage, Job, State).


handle_stage(?STAGE_IDLE, _, #u_state{} = _State) ->
    [];

handle_stage(?STAGE_INIT, ?JOB_RELOAD_EXPORTS, #u_state{nodes = _Nodes} = State) ->
    handle_stage(?STAGE_INIT, ?JOB_LOAD_EXPORTS, State);

handle_stage(?STAGE_INIT, ?JOB_LOAD_EXPORTS, #u_state{nodes = Nodes} = _State) ->
    Nodes;
handle_stage(?STAGE_INIT, ?JOB_CHECK_CONNECTIVITY, #u_state{nodes = Nodes} = _State) ->
    Hostnames = lists:usort([ install_utils:get_host(Node) || Node <- Nodes ]),
    OnePanelNodes = [install_utils:get_node(Host) || Host <- Hostnames],
    Nodes ++ OnePanelNodes;

handle_stage(?STAGE_INIT, ?JOB_DOWNLOAD_BINARY, #u_state{version = Vsn} = _State) ->
    package;

handle_stage(?STAGE_INIT, ?JOB_INSTALL_PACKAGE, #u_state{nodes = Nodes} = _State) ->
    select_only_workers(Nodes);

handle_stage(?STAGE_DAO_UPDATER_LOAD, _, #u_state{nodes = Nodes} = _State) ->
    Nodes;

handle_stage(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, #u_state{nodes = _Nodes} = _State) ->
    views;

handle_stage(?STAGE_DAO_SETUP_VIEWS, _, #u_state{nodes = Nodes} = _State) ->
    select_only_workers(Nodes);

handle_stage(?STAGE_DAO_REFRESH_VIEWS, _, #u_state{nodes = _Nodes, installed_views = Views} = _State) ->
    Views;

handle_stage(?STAGE_DEPLOY_FILES, _, #u_state{nodes = Nodes} = _State) ->
    Nodes;

handle_stage(?STAGE_SOFT_RELOAD, _, #u_state{nodes = Nodes} = _State) ->
    Nodes;

handle_stage(?STAGE_FORCE_RELOAD, _, #u_state{nodes = Nodes} = _State) ->
    Nodes;

handle_stage(?STAGE_DAO_POST_SETUP_VIEWS, _, #u_state{} = _State) ->
    views_cleanup;

handle_stage(?STAGE_NODE_RESTART, _, #u_state{} = _State) ->
    restart;

handle_stage(Stage, Job, #u_state{}) ->
    throw({invalid_stage, {Stage, Job}}).





handle_rollback(#u_state{stage = Stage, job = Job} = State) ->
    handle_rollback(Stage, Job, State).

handle_rollback(?STAGE_DEPLOY_FILES, ?JOB_DEPLOY, #u_state{nodes = Nodes} = State) ->
    Nodes;

handle_rollback(_Stage, _Job, #u_state{}) ->
    [].



dispatch_all(Objects, #u_state{} = State, DispatchFun) ->
    lists:map(fun(Obj) -> DispatchFun(Obj, State) end, Objects).


next_stage(#u_state{stage = ?STAGE_IDLE, job = _, action_type = install} = State) ->
    ?info("next_stage ~p", [das]),
    [{Stage, Job} | _] = flatten_stages(updater_state:get_all_stages(State)),
    {Stage, Job};
next_stage(#u_state{stage = Stage, job = Job, action_type = install} = State) ->
    ?info("next_stage ~p:~p", [Stage, Job]),
    [_, {NStage, NJob} | _] =
        lists:dropwhile(
            fun({CStage, CJob}) ->
                {CStage, CJob} =/= {Stage, Job}
            end, flatten_stages(updater_state:get_all_stages(State)) ++ [{?STAGE_IDLE, ?JOB_DEFAULT}]),
    {NStage, NJob};
next_stage(#u_state{stage = ?STAGE_IDLE, job = _, action_type = rollback}) ->
    {?STAGE_IDLE, ?JOB_DEFAULT};
next_stage(#u_state{stage = Stage, job = Job, action_type = rollback} = State) ->
    ?info("previous_stage ~p:~p", [Stage, Job]),
    Stages = [{?STAGE_IDLE, ?JOB_DEFAULT}] ++ flatten_stages(updater_state:get_all_stages(State)),
    PrevStages =
        lists:takewhile(
            fun({CStage, CJob}) ->
                {CStage, CJob} =/= {Stage, Job}
            end, Stages),
    ?info("previous_stage ~p:~p ~p", [Stage, Job, PrevStages]),
    lists:last(PrevStages).


enter_stage({Stage, Job}, #u_state{object_data = ObjData, callback = CFun, action_type = ActionType} = State) ->
    {DispatchFun, HandleFun, EventName, NewState0} =
        case ActionType of
            install  -> {fun dispatch_object/2, fun handle_stage/1, enter_stage, finalize_stage(State#u_state{previous_data = ObjData})};
            rollback -> {fun rollback_object/2, fun handle_rollback/1, rollback_stage, State}
        end,

    ?info("Entering stage ~p:~p...", [Stage, Job]),

    NewState1 = NewState0#u_state{stage = Stage, job = Job, objects = #{}, error_counter = #{}, object_data = #{}},

    ObjectList = lists:flatten( [ HandleFun(NewState1) ] ),
    Dispatch = dispatch_all(ObjectList, NewState1, DispatchFun),

    NewState2 = NewState1#u_state{objects = maps:from_list( Dispatch )},

    CFun(EventName, NewState2),

    case maps:size(NewState2#u_state.objects) =:= 0 andalso Stage =/= ?STAGE_IDLE of
        true -> enter_stage(next_stage(NewState2), NewState2);
        _    -> NewState2
    end.


local_cast(Fun) ->
    Host = self(),
    spawn_link(fun() -> Host ! {self(), Fun()} end).

%% ====================================================================
%% Callback functions
%% ====================================================================


start_link() ->
    gen_server:start_link({global, ?UPDATE_SERVICE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    inets:start(),
    %%?info("[Updater] Initialized."),
    {ok, #u_state{}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({update_to, #version{} = Vsn, ForceNodeRestart, CallbackFun}, _From, #u_state{stage = ?STAGE_IDLE} = State) ->
    case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
        {ok, #?GLOBAL_CONFIG_RECORD{workers = InstalledWorkers, opt_ccms = OptCCM, main_ccm = MCCM}} ->
            {WorkerHosts, CCMHosts} = {InstalledWorkers, OptCCM ++ [MCCM]},
            Workers = [list_to_atom(?DEFAULT_WORKER_NAME ++ "@" ++ Host) || Host <- WorkerHosts],
            CCMs = [list_to_atom(?DEFAULT_CCM_NAME ++ "@" ++ Host) || Host <- CCMHosts],

            %%?info("Installed workers ~p", [Workers]),
            %%?info("Installed CCMs ~p", [CCMs]),

            NewState0 = State#u_state{action_type = install, error_stack = [], nodes = Workers ++ CCMs, version = Vsn, callback = CallbackFun, force_node_restart = ForceNodeRestart},

            NewState2 = enter_stage(next_stage(State), NewState0),

            {reply, ok, NewState2};
        _ ->
            {reply, {error, no_nodes}, State}
    end;

handle_call(abort, _From, #u_state{stage = ?STAGE_IDLE} = State) ->
    {reply, ok, State};
handle_call(abort, _From, #u_state{stage = Stage, job = Job} = State) ->
    NewState = State#u_state{action_type = rollback, objects = #{}},
    {reply, ok, enter_stage({Stage, Job}, NewState)};


handle_call({update_to, #version{}, _, _}, _From, #u_state{stage = _Stage} = State) ->
    {reply, {error, update_already_in_progress}, State};

handle_call(Info, _From, State) ->
    ?info("[Updater] Unknown call: ~p", [Info]),
    {noreply, State}.

handle_cast(Info, State) ->
    ?info("[Updater] Unknown cast: ~p", [Info]),
    {noreply, State}.


handle_info({Pid, ok}, #u_state{objects = Objects, callback = CallbackFun} = State) ->
    NObjects = maps:remove(Pid, Objects),
    CallbackFun(update_objects, State),
    NState =
        case {maps:size(NObjects), maps:size(Objects)} of
            {0, 1}  ->
                enter_stage(next_stage(State), State);
            _  -> State#u_state{objects = NObjects}
        end,
    {noreply, NState};

handle_info({Pid, {ok, Data}}, #u_state{objects = Objects, object_data = ObjData} = State) ->
    %%?info("Result form ~p: ~p", [Pid, Data]),
    NState =
        case maps:is_key(Pid, Objects) of
            true ->
                Obj = maps:get(Pid, Objects),
                {_, NState0} = handle_info({Pid, ok}, State#u_state{object_data = maps:put(Obj, Data, ObjData)}),
                NState0;
            _ ->
                State
        end,
    {noreply, NState};


handle_info({Pid, {error, Reason}}, #u_state{objects = Objects, object_data = _ObjData, error_counter = EC} = State) ->
    ?error("Error form ~p: ~p", [Pid, Reason]),
    MapsGetOrDefault =
        fun(Key, Map, Default) ->
            case maps:is_key(Key, Map) of
                true -> maps:get(Key, Map);
                _    -> Default
            end
        end,
    NState =
        case maps:is_key(Pid, Objects) of
            true ->
                Obj = maps:get(Pid, Objects),
                handle_error(Pid, Obj, Reason,
                    State#u_state{
                        objects = maps:remove(Pid, Objects),
                        error_counter = maps:put(Obj, MapsGetOrDefault(Obj, EC, 0) + 1, EC)
                    });
            _ ->
                State
        end,
    {noreply, NState};


handle_info({'EXIT', _Pid, normal}, #u_state{} = State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, #u_state{} = State) ->
    handle_info({Pid, {error, {exit, Reason}}}, State);
handle_info(Unknown, #u_state{} = State) ->
    ?info("Unknown info ~p", [Unknown]),
    {noreply, State}.

handle_error(_, Obj, Reason, #u_state{error_counter = EC, objects = Objects, error_stack = EStack, callback = CallbackFun} = State) ->
    ErrorCount = maps:get(Obj, EC),
    if
        ErrorCount < 3 ->
            {NewPid, Obj} = dispatch_object(Obj, State),
            State#u_state{objects = maps:put(NewPid, Obj, Objects)};
        true ->
            ?error("Critical error ~p: ~p", [Obj, Reason]),

            NewState0 = State#u_state{objects = #{}, error_stack = [Reason | EStack]},
            NewState1 = init_rollback(NewState0),
            CallbackFun(error, NewState1),
            enter_stage(updater_state:get_stage_and_job(NewState1), NewState1)
    end.


terminate(Reason, State) ->
    ?info("[Updater] terminate: ~p", [Reason]),
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

call(Node, Fun, Args) ->
    rpc:call(Node, updater_export, Fun, Args).


cast(Node, Fun, Args) ->
    ?debug("Cast: ~p ~p ~p", [Node, Fun, Args]),
    Host = self(),
    spawn_link(Node, updater_export, runner, [Host, Fun, Args]).

multicast(Nodes, Fun, Args) ->
    lists:foreach(fun(Node) -> cast(Node, Fun, Args) end, Nodes).

anycast(Nodes, Fun, Args) ->
    ?debug("Anycast: ~p ~p ~p", [Nodes, Fun, Args]),
    Node = lists:nth(crypto:rand_uniform(1, length(Nodes) + 1), Nodes),
    cast(Node, Fun, Args).

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

select_only_workers([]) ->
    [];
select_only_workers(Node) when is_atom(Node) ->
    case string:tokens(atom_to_list(Node), "@") of
        [?DEFAULT_WORKER_NAME, _] -> [Node];
        _ -> []
    end;
select_only_workers([Node | T]) ->
    select_only_workers(Node) ++ select_only_workers(T).

flatten_stages(Stages) ->
    lists:flatten(
        lists:map(
            fun({Stage, Jobs}) ->
                lists:map(fun(Job) -> {Stage, Job} end, Jobs)
            end, Stages)).


check_connectivity(Node) ->
    case net_adm:ping(Node) of
        pong -> ok;
        pang -> {error, {node_down, Node}}
    end.

init_rollback(#u_state{} = State) ->
    State#u_state{action_type = rollback}.