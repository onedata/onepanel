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
-include("onepanel_modules/updater/internals.hrl").
-include_lib("ctool/include/logging.hrl").


%% gen_server callbacks
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================


%% start_link/0
%% ====================================================================
%% @doc Starts the server
%% @end
-spec start_link() -> Result when
    Result :: {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
%% ====================================================================
start_link() ->
    gen_server:start_link({global, ?UPDATE_SERVICE}, ?MODULE, [], []).


%% init/1
%% ====================================================================
%% @doc Initializes the server
%% @end
-spec init(Args :: term()) -> Result when
    Result :: {ok, State :: #?u_state{}} |
    {ok, State :: #?u_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} |
    ignore.
%% ====================================================================
init(_Args) ->
    process_flag(trap_exit, true),
    inets:start(),
    ?info("[Updater] Initialized."),

    State =
        case dao:get_record(?UPDATER_STATE_TABLE, ?UPDATER_STATE_ID) of
            {ok, #?u_state{stage = Stage} = SavedState} when Stage =/= ?STAGE_IDLE ->
                %% Restart interrupted update process
                case Stage of
                    ?STAGE_INIT ->
                        updater_engine:enter_stage(updater_engine:next_stage(SavedState#?u_state{stage = ?STAGE_IDLE}), SavedState);
                    _ ->
                        updater_engine:enter_stage(updater_state:get_stage_and_job(SavedState), SavedState)
                end;
            {ok, _Unk} ->
                ?warning("Unknown updater state in DB, ignoring."),
                #?u_state{};
            _ ->
                #?u_state{}
        end,

    {ok, State}.


%% handle_call/3
%% ====================================================================
%% @doc Handling call messages
%% @end
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #?u_state{}) -> Result when
    Result :: {reply, Reply :: term(), NewState :: #?u_state{}} |
    {reply, Reply :: term(), NewState :: #?u_state{}, timeout() | hibernate} |
    {noreply, NewState :: #?u_state{}} |
    {noreply, NewState :: #?u_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #?u_state{}} |
    {stop, Reason :: term(), NewState :: #?u_state{}}.
%% ====================================================================
handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({update_to, #version{} = Vsn, ForceNodeRestart, CallbackFun}, _From, #?u_state{stage = ?STAGE_IDLE} = State) ->
    case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
        {ok, #?GLOBAL_CONFIG_RECORD{workers = InstalledWorkers, opt_ccms = OptCCM, main_ccm = MCCM}} ->
            {WorkerHosts, CCMHosts} = {InstalledWorkers, OptCCM ++ [MCCM]},
            Workers = [list_to_atom(?DEFAULT_WORKER_NAME ++ "@" ++ Host) || Host <- WorkerHosts],
            CCMs = [list_to_atom(?DEFAULT_CCM_NAME ++ "@" ++ Host) || Host <- CCMHosts],

            NewState0 = State#?u_state{action_type = install, warning_stack = [], error_stack = [], nodes = Workers ++ CCMs, version = Vsn, callback = CallbackFun, force_node_restart = ForceNodeRestart},

            NewState2 = updater_engine:enter_stage(updater_engine:next_stage(NewState0), NewState0),

            {reply, ok, NewState2};
        _ ->
            {reply, {error, no_nodes}, State}
    end;

handle_call(abort, _From, #?u_state{stage = ?STAGE_IDLE} = State) ->
    {reply, ok, State};
handle_call(abort, _From, #?u_state{stage = Stage, job = Job, callback = CallbackFun} = State) ->
    NewState = State#?u_state{action_type = rollback, objects = #{}},
    CallbackFun(abort, NewState),
    {reply, ok, updater_engine:enter_stage({Stage, Job}, NewState)};
handle_call({set_callback, Fun}, _From, #?u_state{} = State) ->
    {reply, ok, State#?u_state{callback = Fun}};


handle_call({update_to, #version{}, _, _}, _From, #?u_state{stage = _Stage} = State) ->
    {reply, {error, update_already_in_progress}, State};

handle_call(Info, _From, State) ->
    ?info("[Updater] Unknown call: ~p", [Info]),
    {noreply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc Handling cast messages
%% @end
-spec handle_cast(Request :: term(), State :: #?u_state{}) -> Result when
    Result :: {noreply, NewState :: #?u_state{}} |
    {noreply, NewState :: #?u_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #?u_state{}}.
%% ====================================================================
handle_cast(Info, State) ->
    ?info("[Updater] Unknown cast: ~p", [Info]),
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc Handling all non call/cast messages
%% @end
-spec handle_info(Info :: timeout() | term(), State :: #?u_state{}) -> Result when
    Result :: {noreply, NewState :: #?u_state{}} |
    {noreply, NewState :: #?u_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #?u_state{}}.
%% ====================================================================
handle_info({Pid, ok}, #?u_state{objects = Objects, callback = CallbackFun} = State) ->
    NObjects = maps:remove(Pid, Objects),
    CallbackFun(update_objects, State#?u_state{objects = NObjects}),
    NState =
        case {maps:size(NObjects), maps:size(Objects)} of
            {0, 1}  ->
                updater_engine:enter_stage(updater_engine:next_stage(State), State);
            _  -> State#?u_state{objects = NObjects}
        end,
    {noreply, NState};

handle_info({Pid, {ok, Data}}, #?u_state{objects = Objects, object_data = ObjData} = State) ->
    NState =
        case maps:is_key(Pid, Objects) of
            true ->
                Obj = maps:get(Pid, Objects),
                {_, NState0} = handle_info({Pid, ok}, State#?u_state{object_data = maps:put(Obj, Data, ObjData)}),
                NState0;
            _ ->
                State
        end,
    {noreply, NState};


handle_info({Pid, {error, Reason}}, #?u_state{objects = Objects, object_data = _ObjData, error_counter = EC} = State) ->
    ?error("Error form ~p: ~p", [Pid, updater_utils:normalize_error_reason(Reason)]),
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
                updater_engine:handle_error(Pid, Obj, Reason,
                    State#?u_state{
                        objects = maps:remove(Pid, Objects),
                        error_counter = maps:put(Obj, MapsGetOrDefault(Obj, EC, 0) + 1, EC)
                    });
            _ ->
                State
        end,
    {noreply, NState};


handle_info({'EXIT', _Pid, normal}, #?u_state{} = State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, #?u_state{} = State) ->
    handle_info({Pid, {error, {exit, Reason}}}, State);
handle_info(Unknown, #?u_state{} = State) ->
    ?info("Unknown info ~p", [Unknown]),
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #?u_state{}) -> Result when
    Result :: term().
%% ====================================================================
terminate(Reason, _State) ->
    ?info("[Updater] terminate: ~p", [Reason]),
    ok.


%% code_change/3
%% ====================================================================
%% @doc Convert process state when code is changed
%% @end
-spec code_change(OldVsn :: term() | {down, term()}, State :: #?u_state{}, Extra :: term()) -> Result when
    Result :: {ok, NewState :: #?u_state{}} | {error, Reason :: term()}.
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================











