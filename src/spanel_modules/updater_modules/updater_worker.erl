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
    lager:info("[Updater] Initialized."),
    {ok, #u_state{}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({update_to, #version{} = Vsn}, _From, #u_state{stage = ?STAGE_IDLE} = State) ->
    Worker = self(),
    Pid = spawn_link(fun() -> Worker ! {self(), updater_repos:get_package(Vsn)} end),
    NewState = set_stage(?STAGE_INIT, downloading_package, State),
    NewState1 = add_linked_procs([Pid], NewState),
    NewState2 = NewState1#u_state{error_stack = []},

    {reply, ok, NewState2};
handle_call({update_to, #version{}}, _From, #u_state{stage = _Stage} = State) ->
    {reply, {error, update_already_in_progress}, State};

handle_call(Info, _From, State) ->
    lager:info("[Updater] Unknown call: ~p", [Info]),
    {noreply, State}.

handle_cast(Info, State) ->
    lager:info("[Updater] Unknown cast: ~p", [Info]),
    {noreply, State}.




handle_info({Pid, #package{} = Pkg}, #u_state{stage = ?STAGE_INIT, stage_state = downloading_package} = State) ->

    %% @todo: get all install nodes
    InstallNodes = 
        case dao:get_record(configurations, last) of
            {ok, #configuration{workers = InstalledWorkers}} -> InstalledWorkers;
            _ -> []
        end,
    lager:info("Installed workers ~p", [InstallNodes]),

    NewState0 = State#u_state{stage_state = {installing_package, {Pkg, [], []}}, nodes = InstallNodes},
    NewState1 = remove_linked_procs([Pid], NewState0),

    Self = self(),
    Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {self(), {Node, updater_repos:install_package(Node, Pkg)}} end) end, InstallNodes),
    NewState2 = add_linked_procs(Pids, NewState1),

    {noreply, NewState2};

handle_info({_Pid, {Node, {error, Reason}}}, #u_state{stage = ?STAGE_INIT, stage_state = installing_package} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({_Pid, {Node, ok}}, #u_state{stage = ?STAGE_INIT, stage_state = installing_package, data = {Pkg, Installed, _}, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = {Pkg, NewInstalled, All}},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {Node, {Node, updater_files:move_file(Node, "dao_update.beam")}} end) end, All),
                State0 = set_stage(?STAGE_DAO_UPDATER_LOAD, {copy_beams, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({_Pid, {Node, {error, Reason}}}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = copy_beams} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({_Pid, {Node, ok}}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = copy_beams, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {Node, updater_code_loader:force_load(Node, dao_update)} end) end, All),
                State0 = set_stage(?STAGE_DAO_UPDATER_LOAD, {load_beams, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({_Pid, {Node, {error, Reason}}}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = load_beams} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({_Pid, {Node, ok}}, #u_state{stage = ?STAGE_DAO_UPDATER_LOAD, stage_state = load_beams, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                [Node | _] = All,
                Pids = spawn_link(fun() -> Self ! {Self, updater_code_loader:force_load(Node, dao_update)} end),
                State0 = set_stage(?STAGE_DAO_SETUP_VIEWS, none, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({_Pid, {Node, {error, Reason}}}, #u_state{stage = ?STAGE_DAO_SETUP_VIEWS, stage_state = load_beams} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({_Pid, {Node, {ok, Views}}}, #u_state{stage = ?STAGE_DAO_SETUP_VIEWS, nodes = All} = State) ->
    Self = self(),
    [Node | _] = All,
    Pids = lists:map(fun(View) -> spawn_link(fun() -> Self ! {Node, updater_views:refresh_view(Node, View)} end) end, Views),
    State0 = set_stage(?STAGE_DAO_REFRESH_VIEWS, {none, []}, State),
    NewState = set_linked_procs(Pids, State0),

    {noreply, NewState};


handle_info({_Pid, {Node, {error, Reason}}}, #u_state{stage = ?STAGE_DAO_REFRESH_VIEWS} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({_Pid, {Node, ok}}, #u_state{stage = ?STAGE_DAO_REFRESH_VIEWS, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {Node, updater_files:backup_instalation(Node)} end) end, All),
                State0 = set_stage(?STAGE_DEPLOY_FILES, {backup, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({_Pid, {Node, {error, Reason}}}, #u_state{stage = ?STAGE_DAO_REFRESH_VIEWS} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({_Pid, {Node, ok}}, #u_state{stage = ?STAGE_DAO_REFRESH_VIEWS, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {Node, updater_files:backup_instalation(Node)} end) end, All),
                State0 = set_stage(?STAGE_DEPLOY_FILES, {backup, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({_Pid, {Node, {error, Reason}}}, #u_state{stage = ?STAGE_DEPLOY_FILES} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({_Pid, {Node, ok}}, #u_state{stage = ?STAGE_DEPLOY_FILES, stage_state = backup, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {Node, updater_files:move_all_files(Node)} end) end, All),
                State0 = set_stage(?STAGE_DEPLOY_FILES, {do_delpoy, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};


handle_info({_Pid, {Node, {error, Reason}}}, #u_state{stage = ?STAGE_DEPLOY_FILES} = State) ->
    NewState0 = on_error({Node, Reason}, State),
    {noreply, NewState0};
handle_info({_Pid, {Node, ok}}, #u_state{stage = ?STAGE_DEPLOY_FILES, stage_state = do_delpoy, data = Installed, nodes = All} = State) ->
    NewInstalled = Installed ++ [Node],
    NewState0 = State#u_state{data = NewInstalled},
    NewState1 =
        case All -- NewInstalled of
            [] ->
                Self = self(),
                [Node | _] = All,
                Pids = lists:map(fun(Node) -> spawn_link(fun() -> Self ! {Node, updater_code_loader:soft_load_all(Node)} end) end, All),
                State0 = set_stage(?STAGE_SOFT_RELOAD, {none, []}, NewState0),
                set_linked_procs(Pids, State0);
            _ ->
                NewState0
        end,
    {noreply, NewState1};

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
    State#u_state{stage = Stage, stage_history = History ++ CurrStage, stage_state = StageState, linked_procs = [], data = Data};
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


call(Node, Fun) ->
    rpc:call(Node, erlang, apply, [Fun, []]).