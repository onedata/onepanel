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
    lager:info("Package ~p ?!?!", [Pkg]),
    State#u_state{package = Pkg, previous_data = maps:remove(package, PData)};
finalize_stage(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, #u_state{previous_data = PData} = State) ->
    #{views := Views} = PData,
    lager:info("Installed views: ~p ?!?!", [Views]),
    State#u_state{installed_views = Views, previous_data = maps:remove(views, PData)};
finalize_stage(?STAGE_SOFT_RELOAD, ?JOB_DEFAULT, #u_state{previous_data = PData} = State) ->
    #{mod_map := ModMap} = PData,
    lager:info("ModMap: ~p ?!?!", [ModMap]),
    State#u_state{not_reloaded_modules = ModMap, previous_data = maps:remove(mod_mapa, PData)};
finalize_stage(_, _, State) ->
    State.


dispatch(Obj, #u_state{stage = Stage, job = Job} = State) ->
    lager:info("Dispatching ~p:~p obj: ~p", [Stage, Job, Obj]),
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

dispatch(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, _Obj, #u_state{nodes = Nodes}) ->
    anycast(Nodes, install_views, []);

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
    [{Pid, package}];

handle_stage(?STAGE_INIT, ?JOB_INSTALL_PACKAGE, #u_state{nodes = Nodes} = State) ->
    default_dispatch_to_all_nodes(select_only_workers(Nodes), State);

handle_stage(?STAGE_DAO_UPDATER_LOAD, _, #u_state{nodes = Nodes} = State) ->
    default_dispatch_to_all_nodes(Nodes, State);

handle_stage(?STAGE_DAO_SETUP_VIEWS, ?JOB_INSTALL_VIEWS, #u_state{nodes = Nodes} = State) ->
    [dispatch(views, State)];

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
    [_, {NStage, NJob} | _] =
        lists:dropwhile(
            fun({CStage, CJob}) ->
                {CStage, CJob} =/= {Stage, Job}
            end, flatten_stages(?STAGES) ++ [{?STAGE_IDLE, ?JOB_DEFAULT}]),
    {NStage, NJob}.


enter_stage({Stage, Job}, #u_state{object_data = ObjData, callback = CFun} = State) ->
    lager:info("Entering stage ~p:~p...", [Stage, Job]),
    NewState0 = finalize_stage(State#u_state{previous_data = ObjData}),
    NewState1 = NewState0#u_state{stage = Stage, job = Job, objects = #{}, error_stack = [], error_counter = #{}},
    NewState2 = NewState1#u_state{objects = maps:from_list( lists:flatten( [handle_stage(NewState1)] ) )},
    CFun(enter_stage, NewState2),
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

handle_call(abort, _From, State) ->
    {reply, ok, State};

handle_call({update_to, #version{} = Vsn, CallbackFun}, _From, #u_state{stage = ?STAGE_IDLE} = State) ->

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

    NewState0 = State#u_state{nodes = Workers ++ CCMs, version = Vsn, callback = CallbackFun},

    NewState2 = enter_stage(next_stage(State), NewState0),

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
    NObjects = maps:remove(Pid, Objects),
    NState =
        case {maps:size(NObjects), maps:size(Objects)} of
            {0, 1}  -> enter_stage(next_stage(State), State);
            _  -> State#u_state{objects = NObjects}
        end,
    {noreply, NState};

handle_info({Pid, {ok, Data}}, #u_state{objects = Objects, object_data = ObjData} = State) ->
    lager:info("Result form ~p: ~p", [Pid, Data]),
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
    lager:error("Error form ~p: ~p", [Pid, Reason]),
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
    lager:info("Unknown info ~p", [Unknown]),
    {noreply, State}.

handle_error(_, Obj, _Reason, #u_state{error_counter = EC, objects = Objects} = State) ->
    ErrorCount = maps:get(Obj, EC),
    if
        ErrorCount < 3 ->
            {NewPid, Obj} = dispatch(Obj, State),
            State#u_state{objects = maps:put(NewPid, Obj, Objects)};
        true ->
            lager:error("Critical error ~p: ~p", [Obj, _Reason]),
            State#u_state{stage = ?STAGE_IDLE, job = ?JOB_DEFAULT, objects = #{}}
    end.


terminate(Reason, State) ->
    lager:info("[Updater] terminate: ~p", [Reason]),
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% set_stage(Stage, {StageState, Data}, #u_state{stage = CurrStage, stage_history = History} = State) ->
%%     lager:info("[Updater] Entering stage ~p -> ~p with data: ~p", [Stage, StageState, Data]),
%%     State#u_state{stage = Stage, stage_history = History ++ [CurrStage], stage_state = StageState, linked_procs = [], data = Data};
%% set_stage(Stage, StageState, #u_state{} = State) ->
%%     set_stage(Stage, {StageState, undefined}, State).
%%
%% on_error(Error, #u_state{stage = Stage, error_stack = Stack} = State) ->
%%     lager:error("[Updater] Error while processing stage ~p, reason ~p", [Stage, Error]),
%%     State#u_state{error_stack = [#stage_error{stage = Stage, error = Error} | Stack]}.
%%
%% add_linked_procs(Pids, #u_state{linked_procs = Procs} = State) ->
%%     State#u_state{linked_procs = Procs ++ Pids}.
%%
%% set_linked_procs(Pids, #u_state{} = State) ->
%%     State#u_state{linked_procs = Pids}.
%%
%% remove_linked_procs(Pids, #u_state{linked_procs = Procs} = State) ->
%%     State#u_state{linked_procs = Procs -- Pids}.

call(Node, Fun, Args) ->
    rpc:call(Node, updater_export, Fun, Args).


cast(Node, Fun, Args) ->
    lager:info("Cast: ~p ~p ~p", [Node, Fun, Args]),
    Host = self(),
    spawn_link(Node, updater_export, runner, [Host, Fun, Args]).

multicast(Nodes, Fun, Args) ->
    lists:foreach(fun(Node) -> cast(Node, Fun, Args) end, Nodes).

anycast(Nodes, Fun, Args) ->
    lager:info("Anycast: ~p ~p ~p", [Nodes, Fun, Args]),
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