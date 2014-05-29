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
-include("spanel_modules/updater_module/common.hrl").

%% API
-export([]).

%% gen_server callbacks
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================




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

    NewState0 = State#u_state{nodes = Workers ++ CCMs, version = Vsn},

    Self = self(),
    Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, load_module_to_remote(Node, updater_export)} end) end, Workers ++ CCMs),
    NewState1 = set_linked_procs(Pids, NewState0),

    NewState2 = set_stage(?STAGE_INIT, {load_exports, []}, NewState1),
    NewState3 = NewState2#u_state{error_stack = []},

    {reply, ok, NewState3};
handle_call({update_to, #version{}}, _From, #u_state{stage = _Stage} = State) ->
    {reply, {error, update_already_in_progress}, State};

handle_call(Info, _From, State) ->
    lager:info("[Updater] Unknown call: ~p", [Info]),
    {noreply, State}.

handle_cast(Info, State) ->
    lager:info("[Updater] Unknown cast: ~p", [Info]),
    {noreply, State}.


handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_INIT, stage_state = load_exports} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_INIT, stage_state = load_exports, data = Installed, nodes = All, version = Vsn} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pid = spawn_link(fun() -> Self ! {self(), updater_repos:get_package(Vsn)} end),
                State0 = set_stage(?STAGE_INIT, downloading_package, NewState0),
                set_linked_procs(Pid, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({Pid, #package{} = Pkg}, #u_state{stage = ?STAGE_INIT, stage_state = downloading_package, nodes = All} = State) ->

    NewState0 = set_stage(?STAGE_INIT, {installing_package, {Pkg, [], select_only_workers(All)}}, State),
    NewState2 = remove_linked_procs([Pid], NewState0),

    Self = self(),
    Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, install_package, [Pkg])} end) end, select_only_workers(All)),
    NewState3 = add_linked_procs(Pids, NewState2),

    {noreply, NewState3};

handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_INIT, stage_state = installing_package} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_INIT, stage_state = installing_package, data = {Pkg, Installed, All}, nodes = Nodes} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = {Pkg, NewInstalled, All}},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, move_file, ["dao_update.beam"])} end) end, Nodes),
                State0 = set_stage(?STAGE_DAO_UPDATER_LOAD, {copy_beams, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = copy_beams} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = copy_beams, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, force_reload_module, [dao_update])} end) end, All),
                State0 = set_stage(?STAGE_DAO_UPDATER_LOAD, {load_beams, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = load_beams} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = load_beams, data = Installed, nodes = All, version = Vsn} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, run_pre_update, [Vsn])} end) end, All),
                State0 = set_stage(?STAGE_DAO_UPDATER_LOAD, {pre_update, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = pre_update} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = pre_update, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, install_view_sources, [])} end) end, All),
                State0 = set_stage(?STAGE_DAO_SETUP_VIEWS, {view_sources, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_SETUP_VIEWS, stage_state = view_sources} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DAO_SETUP_VIEWS, stage_state = view_sources, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                [TNode | _] = All,
                Pids = spawn_link(fun() -> Self ! {{self(), TNode}, call(TNode, install_views, [])} end),
                State0 = set_stage(?STAGE_DAO_SETUP_VIEWS, install_views, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_SETUP_VIEWS, stage_state = install_views} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({{_Pid, Node}, {ok, Views}}, #u_state{stage = ?STAGE_DAO_SETUP_VIEWS, stage_state = install_views, nodes = All} = State) ->
    Self = self(),
    [TNode | _] = All,
    Pids = lists:map(fun(View) -> spawn_link(fun() -> Self ! {{self(), View}, call(TNode, refresh_view, [View])} end) end, Views),
    State0 = set_stage(?STAGE_DAO_REFRESH_VIEWS, {none, {[], Views}}, State),
    NewState = set_linked_procs(Pids, State0),

    {noreply, NewState};


handle_info({{_Pid, View}, {error, Reason}}, #u_state{stage = ?STAGE_DAO_REFRESH_VIEWS} = State) ->
    NewState0 = on_error({View, Reason}, State),
    self() ! {{_Pid, View}, ok},
    {noreply, NewState0};
handle_info({{_Pid, View}, ok}, #u_state{stage = ?STAGE_DAO_REFRESH_VIEWS, data = {Installed, All}, nodes = Nodes} = State) ->
    NewInstalled = Installed ++ [View],
    NewState0 = State#u_state{data = {NewInstalled, All}},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, backup_instalation, [])} end) end, Nodes),
                State0 = set_stage(?STAGE_DEPLOY_FILES, {backup, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DEPLOY_FILES} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DEPLOY_FILES, stage_state = backup, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, move_all_files, [])} end) end, All),
                State0 = set_stage(?STAGE_DEPLOY_FILES, {do_delpoy, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_DEPLOY_FILES} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({{_Pid, Node}, ok}, #u_state{stage = ?STAGE_DEPLOY_FILES, stage_state = do_delpoy, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, soft_reload_all_modules, [])} end) end, All),
                State0 = set_stage(?STAGE_SOFT_RELOAD, {none, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_SOFT_RELOAD} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({{_Pid, Node}, {ok, ModMap}}, #u_state{stage = ?STAGE_SOFT_RELOAD, data = Installed, nodes = All} = State) ->
    lager:info("ModMap on node ~p: ~p", [Node, ModMap]),
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {{self(), Node}, call(Node, force_reload_all_modules, [])} end) end, All),
                State0 = set_stage(?STAGE_FORCE_RELOAD, {none, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({{_Pid, Node}, {error, Reason}}, #u_state{stage = ?STAGE_FORCE_RELOAD} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({{_Pid, Node}, {ok, ModMap}}, #u_state{stage = ?STAGE_FORCE_RELOAD, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                State0 = set_stage(?STAGE_IDLE, none, NewState0),
                set_linked_procs([], State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};

handle_info({'EXIT', _Pid, normal}, #u_state{} = State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, Reason}, #u_state{} = State) ->
    NewState = on_error(Reason, State),
    {noreply, NewState};
handle_info({_Pid, {error, Reason}}, #u_state{} = State) ->
    NewState = on_error(Reason, State),
    {noreply, NewState};

handle_info(Info, State) ->
    lager:info("[Updater] Unknown info: ~p", [Info]),
    {noreply, State}.

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

load_module_to_remote(Node, Module) ->
    {Module, Bin, FileName} = code:get_object_code(Module),
    rpc:call(Node, code, purge, [Module]),
    case rpc:call(Node, code, load_binary, [Module, FileName, Bin]) of
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