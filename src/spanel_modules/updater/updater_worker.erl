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
-include("spanel_modules/db.hrl").
-include("spanel_modules/install.hrl").
-include("spanel_modules/updater/common.hrl").



%% API
-export([]).

%% gen_server callbacks
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([cast/3]).

%% ====================================================================
%% API functions
%% ====================================================================

finalize_stage(#u_state{stage = Stage, job = Job} = State) ->
    finalize_stage(Stage, Job, State).


finalize_stage(?STAGE_INIT, ?JOB_DOWNLOAD_BINARY, #u_state{previous_data = [{_, #package{} = Pkg} | _]} = State) ->
    lager:info("Finalize?!?!"),
    State#u_state{package = Pkg};
finalize_stage(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, #u_state{previous_data = [{_, Views} | _]} = State) ->
    lager:info("Installed views: ~p?!?!", [Views]),
    State#u_state{installed_views = Views};
finalize_stage(?STAGE_SOFT_RELOAD, _, #u_state{previous_data = [{_, ModMap} | _]} = State) ->
    lager:info("ModMap: ~p?!?!", [ModMap]),
    State#u_state{not_reloaded_modules = ModMap};
finalize_stage(_, _, State) ->
    State.


dispatch(Obj, #u_state{stage = Stage, job = Job} = State) ->
    lager:info("Dispatching ~p ~p obj: ~p", [Stage, Job, Obj]),
    {dispatch(Stage, Job, Obj, State), Obj}.

dispatch(?STAGE_INIT, ?JOB_LOAD_EXPORTS, Obj, #u_state{}) ->
    Host = self(),
    Node = Obj,
    spawn_link(fun() -> Host ! {self(), load_module_to_remote(Node, updater_export)} end);

dispatch(?STAGE_INIT, ?JOB_INSTALL_PACKAGE, Obj, #u_state{package = Pkg}) ->
    Node = Obj,
    cast(Node, install_package, [Pkg]);

dispatch(?STAGE_DAO_UPDATER_LOAD, ?JOB_MOVE_BEAMS, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, move_file, ["dao_update.beam"]);

dispatch(?STAGE_DAO_UPDATER_LOAD, ?JOB_LOAD_BEAMS, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, force_reload_module, [dao_update]);

dispatch(?STAGE_DAO_UPDATER_LOAD, ?JOB_PRE_UPDATE, Obj, #u_state{version = Vsn}) ->
    Node = Obj,
    cast(Node, run_pre_update, [Vsn]);

dispatch(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEW_SOURCES, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, install_view_sources, []);

dispatch(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, install_views, []);

dispatch(?STAGE_DAO_REFRESH_VIEWS, ?JOB_DEFAULT, Obj, #u_state{nodes = Nodes}) ->
    View = Obj,
    anycast(Nodes, refresh_view, [View]);

dispatch(?STAGE_DEPLOY_FILES, ?JOB_BACKUP, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, backup_instalation, []);

dispatch(?STAGE_DEPLOY_FILES, ?JOB_DEPLOY, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, move_all_files, []);

dispatch(?STAGE_SOFT_RELOAD, ?JOB_DEFAULT, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, soft_reload_all_modules, []);

dispatch(?STAGE_FORCE_RELOAD, ?JOB_DEFAULT, Obj, #u_state{}) ->
    Node = Obj,
    cast(Node, force_reload_all_modules, []);

dispatch(Stage, Job, Obj, #u_state{}) ->
    throw({unknown_dispatch, {Stage, Job, Obj}}).



handle_stage(#u_state{stage = Stage, job = Job} = State) ->
    handle_stage(Stage, Job, State).


handle_stage(?STAGE_IDLE, _, #u_state{} = _State) ->
    [];

handle_stage(?STAGE_INIT, ?JOB_LOAD_EXPORTS, #u_state{nodes = Nodes} = State) ->
    default_dispatch_to_all_nodes(Nodes, State);

handle_stage(?STAGE_INIT, ?JOB_DOWNLOAD_BINARY, #u_state{version = Vsn} = State) ->
    Pid = local_cast(fun() -> updater_repos:get_package(Vsn) end),
    [{Pid, def}];

handle_stage(?STAGE_INIT, ?JOB_INSTALL_PACKAGE, #u_state{nodes = Nodes} = State) ->
    default_dispatch_to_all_nodes(select_only_workers(Nodes), State);

handle_stage(?STAGE_DAO_UPDATER_LOAD, _, #u_state{nodes = Nodes} = State) ->
    default_dispatch_to_all_nodes(select_only_workers(Nodes), State);

handle_stage(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, #u_state{nodes = Nodes} = State) ->
    [Worker | _] = select_only_workers(Nodes),
    default_dispatch_to_all_nodes([Worker], State);

handle_stage(?STAGE_DAO_SETUP_VIEWS, _, #u_state{nodes = Nodes} = State) ->
    default_dispatch_to_all_nodes(select_only_workers(Nodes), State);

handle_stage(?STAGE_DAO_REFRESH_VIEWS, _, #u_state{nodes = Nodes, installed_views = Views} = State) ->
    Objects = Views,
    lists:map(fun(Obj) -> dispatch(Obj, State) end, Objects);

handle_stage(?STAGE_DEPLOY_FILES, _, #u_state{nodes = Nodes} = State) ->
    default_dispatch_to_all_nodes(Nodes, State);

handle_stage(?STAGE_SOFT_RELOAD, _, #u_state{nodes = Nodes} = State) ->
    default_dispatch_to_all_nodes(Nodes, State);

handle_stage(?STAGE_FORCE_RELOAD, _, #u_state{nodes = Nodes} = State) ->
    default_dispatch_to_all_nodes(Nodes, State);

handle_stage(Stage, Job, #u_state{}) ->
    throw({invalid_stage, {Stage, Job}}).


default_dispatch_to_all_nodes(Nodes, #u_state{} = State) ->
    Objects = Nodes,
    lists:map(fun(Obj) -> dispatch(Obj, State) end, Objects).


next_stage(#u_state{stage = Stage, job = Job}) ->
    next_stage(Stage, Job).

next_stage(?STAGE_IDLE, _) ->
    [{Stage, Job} | _] = flatten_stages(?STAGES),
    {Stage, Job};
next_stage(Stage, Job) ->
    [{NStage, NJob} | _] =
        lists:dropwhile(
            fun({CStage, CJob}) ->
                {CStage, CJob} =/= {Stage, Job}
            end, flatten_stages(?STAGES) ++ [{?STAGE_IDLE, ?JOB_DEFAULT}]),
    {NStage, NJob}.


enter_stage({Stage, Job}, #u_state{object_data = ObjData} = State) ->
    lager:info("Entering stage ~p:~p... ~p", [Stage, Job, ObjData]),
    NewState0 = finalize_stage(State#u_state{previous_data = ObjData}),
    NewState1 = NewState0#u_state{stage = Stage, job = Job, objects = [], error_stack = [], object_data = []},
    NewState2 = NewState1#u_state{objects = handle_stage(NewState1)},
    lager:info("Objects for stage ~p:~p: ~p", [Stage, Job, NewState2#u_state.objects]),
    NewState2.


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
    lager:info("[Updater] Initialized."),
    {ok, #u_state{}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({update_to, #version{} = Vsn}, _From, #u_state{stage = ?STAGE_IDLE} = State) ->

    {WorkerHosts, CCMHosts} =
        case dao:get_record(configurations, last) of
            {ok, #configuration{workers = InstalledWorkers, opt_ccms = OptCCM, main_ccm = MCCM}} ->
                {InstalledWorkers, OptCCM ++ [MCCM]};
            _ -> []
        end,
    Workers = [list_to_atom(?DEFAULT_WORKER_NAME ++ "@" ++ Host) || Host <- WorkerHosts],
    CCMs = [list_to_atom(?DEFAULT_CCM_NAME ++ "@" ++ Host) || Host <- CCMHosts],

    lager:info("Installed workers ~p", [Workers]),
    lager:info("Installed CCMs ~p", [CCMs]),

    NewState0 = State#u_state{nodes = Workers ++ CCMs, workers = Workers, ccms = CCMs, version = Vsn},

    NewState2 = enter_stage({?STAGE_INIT, ?JOB_LOAD_EXPORTS}, NewState0),

    {reply, ok, NewState2};


handle_call({update_to, #version{}}, _From, #u_state{stage = _Stage} = State) ->
    {reply, {error, update_already_in_progress}, State};

handle_call(Info, _From, State) ->
    lager:info("[Updater] Unknown call: ~p", [Info]),
    {noreply, State}.

handle_cast(Info, State) ->
    lager:info("[Updater] Unknown cast: ~p", [Info]),
    {noreply, State}.


handle_info({Pid, ok}, #u_state{objects = Objects} = State) ->
    NObjects = lists:keydelete(Pid, 1, Objects),
    NState =
        case NObjects of
            []  -> enter_stage(next_stage(State), State);
            _   -> State#u_state{objects = NObjects}
        end,
    {noreply, NState};

handle_info({Pid, {ok, Data}}, #u_state{objects = Objects, object_data = ObjData} = State) ->
    lager:info("Result form ~p: ~p", [Pid, Data]),
    NState =
        case lists:keyfind(Pid, 1, Objects) of
            {_, Obj} ->
                {_, NState0} = handle_info({Pid, ok}, State#u_state{object_data = ObjData ++ [{Obj, Data}]}),
                NState0;
            false ->
                State
        end,
    {noreply, NState};


handle_info({Pid, {error, Reason}}, #u_state{objects = Objects, object_data = _ObjData} = State) ->
    lager:error("Error form ~p: ~p", [Pid, Reason]),
    NState =
        case lists:keyfind(Pid, 1, Objects) of
            {_, Obj} ->
                handle_error(Pid, Obj, Reason, State);
            false ->
                State
        end,
    {noreply, NState};


handle_info({'EXIT', _Pid, normal}, #u_state{} = State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, #u_state{} = State) ->
    handle_info({Pid, {error, {exit, Reason}}}, State);
handle_info(Unknown, #u_state{} = State) ->
    lager:info("Unknown info ~p (state ~p)", [Unknown, State]),
    {noreply, State}.

handle_error(_, _, _, State) ->
    State.




































%%
%%
%%
%% handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_INIT, stage_state = load_exports} = State) ->
%%     NewState0 = on_error({Node, Reason}, State),
%%     {noreply, NewState0};
%% handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_INIT, stage_state = load_exports, data = Installed, nodes = All, version = Vsn} = State) ->
%%     NewInstalled = Installed ++ [Node],
%%     NewState0 = State#u_state{data = NewInstalled},
%%     NewState1 =
%%         case All -- NewInstalled of
%%             [] ->
%%                 Self = self(),
%%                 Pid = spawn_link(fun() -> Self ! {self(), updater_repos:get_package(Vsn)} end),
%%                 State0 = set_stage(?STAGE_INIT, downloading_package, NewState0),
%%                 set_linked_procs(Pid, State0);
%%             _ ->
%%                 NewState0
%%         end,
%%     {noreply, NewState1};
%%
%%
%% handle_info({Pid, #package{} = Pkg}, #u_state{stage = ?STAGE_INIT, stage_state = downloading_package, nodes = All} = State) ->
%%
%%     NewState0 = set_stage(?STAGE_INIT, {installing_package, {Pkg, [], select_only_workers(All)}}, State),
%%     NewState2 = remove_linked_procs([Pid], NewState0),
%%
%%     Self = self(),
%%     Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, install_package, [Pkg])} end) end, select_only_workers(All)),
%%     NewState3 = add_linked_procs(Pids, NewState2),
%%
%%     {noreply, NewState3};
%%
%% handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_INIT, stage_state = installing_package} = State) ->
%%     NewState0 = on_error({Node, Reason}, State),
%%     {noreply, NewState0};
%% handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_INIT, stage_state = installing_package, data = {Pkg, Installed, All}, nodes = Nodes} = State) ->
%%     NewInstalled = Installed ++ [Node],
%%     NewState0 = State#u_state{data = {Pkg, NewInstalled, All}},
%%     NewState1 =
%%         case All -- NewInstalled of
%%             [] ->
%%                 Self = self(),
%%                 Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, move_file, ["dao_update.beam"])} end) end, Nodes),
%%                 State0 = set_stage(?STAGE_DAO_UPDATER_LOAD, {copy_beams, []}, NewState0),
%%                 set_linked_procs(Pids, State0);
%%             _ ->
%%                 NewState0
%%         end,
%%     {noreply, NewState1};
%%
%%
%% handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = copy_beams} = State) ->
%%     NewState0 = on_error({Node, Reason}, State),
%%     {noreply, NewState0};
%% handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = copy_beams, data = Installed, nodes = All} = State) ->
%%     NewInstalled = Installed ++ [Node],
%%     NewState0 = State#u_state{data = NewInstalled},
%%     NewState1 =
%%         case All -- NewInstalled of
%%             [] ->
%%                 Self = self(),
%%                 Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, force_reload_module, [dao_update])} end) end, All),
%%                 State0 = set_stage(?STAGE_DAO_UPDATER_LOAD, {load_beams, []}, NewState0),
%%                 set_linked_procs(Pids, State0);
%%             _ ->
%%                 NewState0
%%         end,
%%     {noreply, NewState1};
%%
%%
%% handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = load_beams} = State) ->
%%     NewState0 = on_error({Node, Reason}, State),
%%     {noreply, NewState0};
%% handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = load_beams, data = Installed, nodes = All, version = Vsn} = State) ->
%%     NewInstalled = Installed ++ [Node],
%%     NewState0 = State#u_state{data = NewInstalled},
%%     NewState1 =
%%         case All -- NewInstalled of
%%             [] ->
%%                 Self = self(),
%%                 Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, run_pre_update, [Vsn])} end) end, All),
%%                 State0 = set_stage(?STAGE_DAO_UPDATER_LOAD, {pre_update, []}, NewState0),
%%                 set_linked_procs(Pids, State0);
%%             _ ->
%%                 NewState0
%%         end,
%%     {noreply, NewState1};
%%
%%
%% handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = pre_update} = State) ->
%%     NewState0 = on_error({Node, Reason}, State),
%%     {noreply, NewState0};
%% handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = pre_update, data = Installed, nodes = All} = State) ->
%%     NewInstalled = Installed ++ [Node],
%%     NewState0 = State#u_state{data = NewInstalled},
%%     NewState1 =
%%         case All -- NewInstalled of
%%             [] ->
%%                 Self = self(),
%%                 Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, install_view_sources, [])} end) end, All),
%%                 State0 = set_stage(?STAGE_DAO_SETUP_VIEWS, {view_sources, []}, NewState0),
%%                 set_linked_procs(Pids, State0);
%%             _ ->
%%                 NewState0
%%         end,
%%     {noreply, NewState1};
%%
%%
%% handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_SETUP_VIEWS, stage_state = view_sources} = State) ->
%%     NewState0 = on_error({Node, Reason}, State),
%%     {noreply, NewState0};
%% handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DAO_SETUP_VIEWS, stage_state = view_sources, data = Installed, nodes = All} = State) ->
%%     NewInstalled = Installed ++ [Node],
%%     NewState0 = State#u_state{data = NewInstalled},
%%     NewState1 =
%%         case All -- NewInstalled of
%%             [] ->
%%                 Self = self(),
%%                 [TNode | _] = All,
%%                 Pids = spawn_link(fun() -> Self ! {{self(), TNode}, call(TNode, install_views, [])} end),
%%                 State0 = set_stage(?STAGE_DAO_SETUP_VIEWS, install_views, NewState0),
%%                 set_linked_procs(Pids, State0);
%%             _ ->
%%                 NewState0
%%         end,
%%     {noreply, NewState1};
%%
%%
%% handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_SETUP_VIEWS, stage_state = install_views} = State) ->
%%     NewState0 = on_error({Node, Reason}, State),
%%     {noreply, NewState0};
%% handle_info({{_Pid, Node}, {ok, Views}}, #u_state{stage = ?STAGE_DAO_SETUP_VIEWS, stage_state = install_views, nodes = All} = State) ->
%%     Self = self(),
%%     [TNode | _] = All,
%%     Pids = lists:map(fun(View) -> spawn_link(fun() -> Self ! {{self(), View}, call(TNode, refresh_view, [View])} end) end, Views),
%%     State0 = set_stage(?STAGE_DAO_REFRESH_VIEWS, {none, {[], Views}}, State),
%%     NewState = set_linked_procs(Pids, State0),
%%
%%     {noreply, NewState};
%%
%%
%% handle_info({{_Pid, View}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_REFRESH_VIEWS} = State) ->
%%     NewState0 = on_error({View, Reason}, State),
%%     self() ! {{_Pid, View}, ok},
%%     {noreply, NewState0};
%% handle_info({{_Pid, View}, ok}, #u_state{stage = ?STAGE_DAO_REFRESH_VIEWS, data = {Installed, All}, nodes = Nodes} = State) ->
%%     NewInstalled = Installed ++ [View],
%%     NewState0 = State#u_state{data = {NewInstalled, All}},
%%     NewState1 =
%%         case All -- NewInstalled of
%%             [] ->
%%                 Self = self(),
%%                 Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, backup_instalation, [])} end) end, Nodes),
%%                 State0 = set_stage(?STAGE_DEPLOY_FILES, {backup, []}, NewState0),
%%                 set_linked_procs(Pids, State0);
%%             _ ->
%%                 NewState0
%%         end,
%%     {noreply, NewState1};
%%
%%
%% handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DEPLOY_FILES} = State) ->
%%     NewState0 = on_error({Node, Reason}, State),
%%     {noreply, NewState0};
%% handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DEPLOY_FILES, stage_state = backup, data = Installed, nodes = All} = State) ->
%%     NewInstalled = Installed ++ [Node],
%%     NewState0 = State#u_state{data = NewInstalled},
%%     NewState1 =
%%         case All -- NewInstalled of
%%             [] ->
%%                 Self = self(),
%%                 Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, move_all_files, [])} end) end, All),
%%                 State0 = set_stage(?STAGE_DEPLOY_FILES, {do_delpoy, []}, NewState0),
%%                 set_linked_procs(Pids, State0);
%%             _ ->
%%                 NewState0
%%         end,
%%     {noreply, NewState1};
%%
%%
%% handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DEPLOY_FILES} = State) ->
%%     NewState0 = on_error({Node, Reason}, State),
%%     {noreply, NewState0};
%% handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DEPLOY_FILES, stage_state = do_delpoy, data = Installed, nodes = All} = State) ->
%%     NewInstalled = Installed ++ [Node],
%%     NewState0 = State#u_state{data = NewInstalled},
%%     NewState1 =
%%         case All -- NewInstalled of
%%             [] ->
%%                 Self = self(),
%%                 Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, spawn(Node, updater_export, soft_reload_all_modules, [])} end) end, All),
%%                 State0 = set_stage(?STAGE_SOFT_RELOAD, {none, []}, NewState0),
%%                 set_linked_procs(Pids, State0);
%%             _ ->
%%                 NewState0
%%         end,
%%     {noreply, NewState1};
%%
%%
%% handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_SOFT_RELOAD} = State) ->
%%     NewState0 = on_error({Node, Reason}, State),
%%     {noreply, NewState0};
%% handle_info({{_Pid, Node}, {ok, ModMap}}, #u_state{stage = ?STAGE_SOFT_RELOAD, data = Installed, nodes = All} = State) ->
%%     lager:info("ModMap on node ~p: ~p", [Node, ModMap]),
%%     NewInstalled = Installed ++ [Node],
%%     NewState0 = State#u_state{data = NewInstalled},
%%     NewState1 =
%%         case All -- NewInstalled of
%%             [] ->
%%                 Self = self(),
%%                 Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, spawn(Node, updater_export, force_reload_all_modules, [])} end) end, select_only_workers(All)),
%%                 State0 = set_stage(?STAGE_FORCE_RELOAD, {none, [], select_only_workers(All)}, NewState0),
%%                 set_linked_procs(Pids, State0);
%%             _ ->
%%                 NewState0
%%         end,
%%     {noreply, NewState1};
%%
%%
%% handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_FORCE_RELOAD} = State) ->
%%     NewState0 = on_error({Node, Reason}, State),
%%     {noreply, NewState0};
%% handle_info({{_Pid, Node}, {ok, ModMap}}, #u_state{stage = ?STAGE_FORCE_RELOAD, data = {Installed, All}} = State) ->
%%     NewInstalled = Installed ++ [Node],
%%     NewState0 = State#u_state{data = {NewInstalled, All}},
%%     NewState1 =
%%         case All -- NewInstalled of
%%             [] ->
%%                 State0 = set_stage(?STAGE_IDLE, none, NewState0),
%%                 set_linked_procs([], State0);
%%             _ ->
%%                 NewState0
%%         end,
%%     {noreply, NewState1};
%%
%% handle_info({'EXIT', _Pid, normal}, #u_state{} = State) ->
%%     {noreply, State};
%% handle_info({'EXIT', _Pid, Reason}, #u_state{} = State) ->
%%     NewState = on_error(Reason, State),
%%     {noreply, NewState};
%% handle_info({_Pid, {error, Reason}}, #u_state{} = State) ->
%%     NewState = on_error(Reason, State),
%%     {noreply, NewState};
%%
%% handle_info(Info, State) ->
%%     lager:info("[Updater] Unknown info: ~p", [Info]),
%%     {noreply, State}.

terminate(Reason, State) ->
    lager:info("[Updater] terminate: ~p", [Reason]),
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

set_stage(Stage, {StageState, Data}, #u_state{stage = CurrStage, stage_history = History} = State) ->
    lager:info("[Updater] Entering stage ~p -> ~p with data: ~p", [Stage, StageState, Data]),
    State#u_state{stage = Stage, stage_history = History ++ [CurrStage], stage_state = StageState, linked_procs = [], data = Data};
set_stage(Stage, StageState, #u_state{} = State) ->
    set_stage(Stage, {StageState, undefined}, State).

on_error(Error, #u_state{stage = Stage, error_stack = Stack} = State) ->
    lager:error("[Updater] Error while processing stage ~p, reason ~p", [Stage, Error]),
    State#u_state{error_stack = [#stage_error{stage = Stage, error = Error} | Stack]}.

add_linked_procs(Pids, #u_state{linked_procs = Procs} = State) ->
    State#u_state{linked_procs = Procs ++ Pids}.

set_linked_procs(Pids, #u_state{} = State) ->
    State#u_state{linked_procs = Pids}.

remove_linked_procs(Pids, #u_state{linked_procs = Procs} = State) ->
    State#u_state{linked_procs = Procs -- Pids}.

call(Node, Fun, Args) ->
    rpc:call(Node, updater_export, Fun, Args).


cast(Node, Fun, Args) ->
    Host = self(),
    spawn_link(Node, updater_export, runner, [Host, Fun, Args]).

%% cast(Node, Fun, Args) ->
%%     lager:info("Cast ~p ~p ~p", [Node, Fun, Args]),
%%     Host = self(),
%%     spawn_link(Node, fun() -> Host ! apply(updater_export, Fun, Args) end).


multicast(Nodes, Fun, Args) ->
    lists:foreach(fun(Node) -> cast(Node, Fun, Args) end, Nodes).

anycast(Nodes, Fun, Args) ->
    Host = self(),
    Node = lists:nth(crypto:rand_uniform(1, length(Nodes) + 1), Nodes),
    cast(Node, Fun, Args).

load_module_to_remote(Node, Module) ->
    {Module, Bin, FileName} = code:get_object_code(Module),
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