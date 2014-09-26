%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Active elements of updater_engine based on gen_server.
%%       This gen_server has presistent state which makes updater immune to node restarts/crashes.
%% @end
%% ===================================================================
-module(updater_worker).
-behaviour(gen_server).
-author("Rafal Slota").

-include("registered_names.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
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
    Result :: {ok, State :: #?U_STATE{}} |
    {ok, State :: #?U_STATE{}, timeout() | hibernate} |
    {stop, Reason :: term()} |
    ignore.
%% ====================================================================
init(_Args) ->
    process_flag(trap_exit, true),
    ?info("[Updater] Initialized."),

    State =
        case dao:get_record(?UPDATER_STATE_TABLE, ?UPDATER_STATE_ID) of
            {ok, #?U_STATE{stage = Stage} = SavedState} when Stage =/= ?STAGE_IDLE ->
                %% Restart interrupted update process
                case Stage of
                    ?STAGE_INIT ->
                        updater_engine:enter_stage(updater_engine:next_stage(SavedState#?U_STATE{stage = ?STAGE_IDLE}), SavedState);
                    _ ->
                        updater_engine:enter_stage(updater_state:get_stage_and_job(SavedState), SavedState)
                end;
            {ok, #?U_STATE{error_stack = ES, warning_stack = WS, action_type = ActionType}} ->
                #?U_STATE{error_stack = ES, warning_stack = WS, action_type = ActionType};
            {ok, _Unk} ->
                ?warning("Unknown updater state in DB, ignoring."),
                #?U_STATE{};
            _ ->
                #?U_STATE{}
        end,

    {ok, State}.


%% handle_call/3
%% ====================================================================
%% @doc Handling call messages
%% @end
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #?U_STATE{}) -> Result when
    Result :: {reply, Reply :: term(), NewState :: #?U_STATE{}} |
    {reply, Reply :: term(), NewState :: #?U_STATE{}, timeout() | hibernate} |
    {noreply, NewState :: #?U_STATE{}} |
    {noreply, NewState :: #?U_STATE{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #?U_STATE{}} |
    {stop, Reason :: term(), NewState :: #?U_STATE{}}.
%% ====================================================================
handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({update_to, #version{} = Vsn, ForceNodeRestart, CallbackFun}, _From, #?U_STATE{stage = ?STAGE_IDLE} = State) ->
    case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
        {ok, #?GLOBAL_CONFIG_RECORD{ccms = CCMHosts, workers = WorkerHosts}} ->
            Workers = [onepanel_utils:get_node(?DEFAULT_WORKER_NAME, Host) || Host <- WorkerHosts],
            CCMs = [onepanel_utils:get_node(?DEFAULT_CCM_NAME, Host) || Host <- CCMHosts],

            NewState0 = State#?U_STATE{action_type = install, warning_stack = [], error_stack = [], nodes = Workers ++ CCMs, version = Vsn, callback = CallbackFun, force_node_restart = ForceNodeRestart},

            NewState2 = updater_engine:enter_stage(updater_engine:next_stage(NewState0), NewState0),

            {reply, ok, NewState2};
        _ ->
            {reply, {error, no_nodes}, State}
    end;

handle_call(abort, _From, #?U_STATE{stage = ?STAGE_IDLE} = State) ->
    {reply, ok, State};
handle_call(abort, _From, #?U_STATE{action_type = rollback} = State) ->
    {reply, ok, State};
handle_call(abort, _From, #?U_STATE{stage = Stage, job = Job, callback = CallbackFun} = State) ->
    NewState = State#?U_STATE{action_type = rollback, objects = #{}},
    CallbackFun(abort, NewState),
    {reply, ok, updater_engine:enter_stage({Stage, Job}, NewState)};
handle_call({set_callback, Fun}, _From, #?U_STATE{} = State) ->
    {reply, ok, State#?U_STATE{callback = Fun}};


handle_call({update_to, #version{}, _, _}, _From, #?U_STATE{stage = _Stage} = State) ->
    {reply, {error, update_already_in_progress}, State};

handle_call(Info, _From, State) ->
    ?warning("[Updater] Unknown call: ~p", [Info]),
    {noreply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc Handling cast messages
%% @end
-spec handle_cast(Request :: term(), State :: #?U_STATE{}) -> Result when
    Result :: {noreply, NewState :: #?U_STATE{}} |
    {noreply, NewState :: #?U_STATE{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #?U_STATE{}}.
%% ====================================================================
handle_cast(Info, State) ->
    ?warning("[Updater] Unknown cast: ~p", [Info]),
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc Handling all non call/cast messages
%% @end
-spec handle_info(Info :: timeout() | term(), State :: #?U_STATE{}) -> Result when
    Result :: {noreply, NewState :: #?U_STATE{}} |
    {noreply, NewState :: #?U_STATE{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #?U_STATE{}}.
%% ====================================================================
handle_info({Pid, ok}, #?U_STATE{objects = Objects, callback = CallbackFun} = State) ->
    NObjects = maps:remove(Pid, Objects),
    CallbackFun(update_objects, State#?U_STATE{objects = NObjects}),
    NState =
        case {maps:size(NObjects), maps:size(Objects)} of
            {0, 1} ->
                updater_engine:enter_stage(updater_engine:next_stage(State), State);
            _ -> State#?U_STATE{objects = NObjects}
        end,
    {noreply, NState};

handle_info({Pid, {ok, Data}}, #?U_STATE{objects = Objects, object_data = ObjData} = State) ->
    NState =
        case maps:is_key(Pid, Objects) of
            true ->
                Obj = maps:get(Pid, Objects),
                {_, NState0} = handle_info({Pid, ok}, State#?U_STATE{object_data = maps:put(Obj, Data, ObjData)}),
                NState0;
            _ ->
                State
        end,
    {noreply, NState};


handle_info({Pid, {error, Reason}}, #?U_STATE{objects = Objects, object_data = _ObjData, error_counter = EC} = State) ->
    ?error("Error form ~p: ~p", [Pid, updater_utils:normalize_error_reason(Reason)]),
    MapsGetOrDefault =
        fun(Key, Map, Default) ->
            case maps:is_key(Key, Map) of
                true -> maps:get(Key, Map);
                _ -> Default
            end
        end,
    NState =
        case maps:is_key(Pid, Objects) of
            true ->
                Obj = maps:get(Pid, Objects),
                updater_engine:handle_error(Pid, Obj, Reason,
                    State#?U_STATE{
                        objects = maps:remove(Pid, Objects),
                        error_counter = maps:put(Obj, MapsGetOrDefault(Obj, EC, 0) + 1, EC)
                    });
            _ ->
                State
        end,
    {noreply, NState};


handle_info({'EXIT', _Pid, normal}, #?U_STATE{} = State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, #?U_STATE{} = State) ->
    handle_info({Pid, {error, {exit, Reason}}}, State);
handle_info(Unknown, #?U_STATE{} = State) ->
    ?warning("Unknown info ~p", [Unknown]),
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #?U_STATE{}) -> Result when
    Result :: term().
%% ====================================================================
terminate(Reason, _State) ->
    ?info("[Updater] terminate: ~p", [Reason]),
    ok.


%% code_change/3
%% ====================================================================
%% @doc Convert process state when code is changed
%% @end
-spec code_change(OldVsn :: term() | {down, term()}, State :: #?U_STATE{}, Extra :: term()) -> Result when
    Result :: {ok, NewState :: #?U_STATE{}} | {error, Reason :: term()}.
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================











