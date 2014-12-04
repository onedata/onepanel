%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module is a gen_server that executes installation
%% operations.
%% @end
%% ===================================================================
-module(installer).
-behaviour(gen_server).

-include("registered_names.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/stages.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([install/1, install/3, start_link/0]).
-export([get_state/0, get_stages/0, get_job_index/2, get_flatten_stages/0, get_stage_and_job/1, get_error/1, set_callback/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% install/1
%% ====================================================================
%% @doc Starts installation process.
%% @end
-spec install(Config :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(Config) ->
    install(Config, undefined, fun(_Event, _State) -> ok end).


%% install/2
%% ====================================================================
%% @doc Starts installation process. Allows to pass a callback function
%% which will be called each time there has been a change in installation
%% state or an error occured.
%% Callback :: function(Event :: state_changed | error, State :: #?I_STATE{})
%% @end
-spec install(Config :: [{Name :: atom(), Value :: term()}], ErrorState :: undefined | #?I_STATE{},
    Callback :: function()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(Config, ErrorState, Callback) ->
    case start() of
        ok -> gen_server:call({global, ?INSTALL_SERVICE}, {install, Config, ErrorState, Callback});
        {error, Reason} -> {error, Reason}
    end.


%% get_state/0
%% ====================================================================
%% @doc Returns current state of installer service.
%% @end
-spec get_state() -> Result when
    Result :: #?I_STATE{}.
%% ====================================================================
get_state() ->
    case start() of
        ok -> gen_server:call({global, ?INSTALL_SERVICE}, get_state);
        {error, Reason} -> {error, Reason}
    end.


%% get_stage_and_job/1
%% ====================================================================
%% @doc Returns stage and job for given state.
%% @end
-spec get_stage_and_job(State :: #?I_STATE{}) -> Result when
    Result :: {State :: atom(), Job :: atom()}.
%% ====================================================================
get_stage_and_job(#?I_STATE{stage = Stage, job = Job}) ->
    {Stage, Job}.


%% get_stages/0
%% ====================================================================
%% @doc Returns all stages and associated jobs.
%% @end
-spec get_stages() -> Result when
    Result :: [{State :: atom(), Jobs :: [atom()]}].
%% ====================================================================
get_stages() ->
    ?STAGES.


%% get_flatten_stages/0
%% ====================================================================
%% @doc Returns all stages and jobs as a list of tuples.
%% @end
-spec get_flatten_stages() -> Result when
    Result :: integer().
%% ====================================================================
get_flatten_stages() ->
    lists:foldl(fun({Stage, Jobs}, FlatStages) ->
        FlatStages ++ lists:zip(lists:duplicate(length(Jobs), Stage), Jobs)
    end, [], get_stages()).


%% get_job_index/2
%% ====================================================================
%% @doc Returns overall position of job in all jobs and stages.
%% @end
-spec get_job_index(Stage :: atom(), Job :: atom()) -> Result when
    Result :: integer().
%% ====================================================================
get_job_index(Stage, Job) ->
    FlattenStages = get_flatten_stages(),
    length(FlattenStages) - length(
        lists:dropwhile(fun(FlattenStage) ->
            FlattenStage =/= {Stage, Job}
        end, FlattenStages)
    ) + 1.


%% get_error/1
%% ====================================================================
%% @doc Returns an error if occured in given state.
%% @end
-spec get_error(State :: #?I_STATE{}) -> Result when
    Result :: term().
%% ====================================================================
get_error(#?I_STATE{error = Error}) ->
    Error.


%% set_callback/1
%% ====================================================================
%% @doc Allows to set installation callback function which will be called
%% each time installer state changes or an error occured.
%% Callback :: function(Event :: state_changed | error, State :: #?I_STATE{})
%% @end
-spec set_callback(Callback :: function()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
set_callback(Callback) ->
    case start() of
        ok -> gen_server:call({global, ?INSTALL_SERVICE}, {set_callback, Callback});
        {error, Reason} -> {error, Reason}
    end.


%% start_link/0
%% ====================================================================
%% @doc Starts the server
%% @end
-spec start_link() -> Result when
    Result :: {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
%% ====================================================================
start_link() ->
    gen_server:start_link({global, ?INSTALL_SERVICE}, ?MODULE, [], []).


%% ====================================================================
%% gen_server callbacks
%% ====================================================================

%% init/0
%% ====================================================================
%% @doc Initializes the server
%% @end
-spec init(Args :: term()) -> Result when
    Result :: {ok, State :: #?I_STATE{}} |
    {ok, State :: #?I_STATE{}, timeout() | hibernate} |
    {stop, Reason :: term()} |
    ignore.
%% ====================================================================
init([]) ->
    {ok, #?I_STATE{stage = ?STAGE_INIT}}.


%% handle_call/3
%% ====================================================================
%% @doc Handling call messages
%% @end
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #?I_STATE{}) -> Result when
    Result :: {reply, Reply :: term(), NewState :: #?I_STATE{}} |
    {reply, Reply :: term(), NewState :: #?I_STATE{}, timeout() | hibernate} |
    {noreply, NewState :: #?I_STATE{}} |
    {noreply, NewState :: #?I_STATE{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #?I_STATE{}} |
    {stop, Reason :: term(), NewState :: #?I_STATE{}}.
%% ====================================================================
handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({set_callback, Callback}, _From, State) ->
    {reply, ok, State#?I_STATE{callback = Callback}};

handle_call({install, Config, undefined, Callback}, _From, #?I_STATE{stage = ?STAGE_INIT} = State) ->
    NextState = get_next_state(State),
    gen_server:cast({global, ?INSTALL_SERVICE}, next_state),
    {reply, ok, NextState#?I_STATE{config = Config, callback = Callback}};

handle_call({install, Config, State, Callback}, _From, _) ->
    gen_server:cast({global, ?INSTALL_SERVICE}, next_state),
    {reply, ok, State#?I_STATE{config = Config, callback = Callback, error = undefined}};

handle_call({install, _, _}, _From, State) ->
    {reply, {error, <<"Installation already in progress">>}, State};

handle_call({result, ok}, _From, State) ->
    NextState = get_next_state(State),
    gen_server:cast({global, ?INSTALL_SERVICE}, next_state),
    {reply, ok, NextState};

handle_call({result, Error}, _From, #?I_STATE{callback = Callback} = State) ->
    NextState = State#?I_STATE{error = Error},
    Callback(?EVENT_ERROR, NextState),
    {stop, shutdown, NextState};

handle_call(Request, _From, State) ->
    ?warning("[Installer] Wrong call: ~p", [Request]),
    {reply, {error, wrong_request}, State}.


%% handle_cast/2
%% ====================================================================
%% @doc Handling cast messages
%% @end
-spec handle_cast(Request :: term(), State :: #?I_STATE{}) -> Result when
    Result :: {noreply, NewState :: #?I_STATE{}} |
    {noreply, NewState :: #?I_STATE{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #?I_STATE{}}.
%% ====================================================================
handle_cast(next_state, #?I_STATE{stage = ?STAGE_IDLE, callback = Callback} = State) ->
    Callback(?EVENT_STATE_CHANGED, State),
    {stop, normal, State};

handle_cast(next_state, #?I_STATE{stage = Stage, job = Job, config = Config, callback = Callback} = State) ->
    Callback(?EVENT_STATE_CHANGED, State),
    spawn_link(fun() ->
        Result = Stage:Job(Config),
        gen_server:call({global, ?INSTALL_SERVICE}, {result, Result})
    end),
    {noreply, State};

handle_cast(Request, State) ->
    ?warning("[Installer] Wrong cast: ~p", [Request]),
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc Handling all non call/cast messages
%% @end
-spec handle_info(Info :: timeout() | term(), State :: #?I_STATE{}) -> Result when
    Result :: {noreply, NewState :: #?I_STATE{}} |
    {noreply, NewState :: #?I_STATE{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #?I_STATE{}}.
%% ====================================================================
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

handle_info({'EXIT', _Pid, Error}, #?I_STATE{callback = Callback} = State) ->
    NextState = State#?I_STATE{error = Error},
    Callback(?EVENT_ERROR, NextState),
    {stop, Error, NextState};

handle_info(Info, State) ->
    ?warning("[Installer] Wrong info: ~p", [Info]),
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #?I_STATE{}) -> Result when
    Result :: term().
%% ====================================================================
terminate(Reason, _State) ->
    ?warning("[Installer] Terminate: ~p", [Reason]),
    ok.


%% code_change/3
%% ====================================================================
%% @doc Convert process state when code is changed
%% @end
-spec code_change(OldVsn :: term() | {down, term()}, State :: #?I_STATE{}, Extra :: term()) -> Result when
    Result :: {ok, NewState :: #?I_STATE{}} | {error, Reason :: term()}.
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


%% start/0
%% ====================================================================
%% @doc Checks whether installer gen_server is running and if is not
%% starts it.
%% @end
-spec start() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
start() ->
    case global:whereis_name(?INSTALL_SERVICE) of
        undefined ->
            supervisor:delete_child(?ONEPANEL_SUP, installer),
            case supervisor:start_child(?ONEPANEL_SUP,
                {
                    installer,
                    {installer, start_link, []},
                    transient,
                    10000,
                    worker,
                    [installer]
                })
            of
                {error, Reason} ->
                    ?error("[Installer] Cannot start installer gen_server: ~p", [Reason]),
                    {error, Reason};
                _ -> ok
            end;
        _ -> ok
    end.


%% get_next_state/1
%% ====================================================================
%% @doc Returns installer state that follows given state.
%% @end
-spec get_next_state(State :: #?I_STATE{}) -> Result when
    Result :: #?I_STATE{}.
%% ====================================================================
get_next_state(#?I_STATE{stage = ?STAGE_IDLE} = State) ->
    State;

get_next_state(#?I_STATE{stage = ?STAGE_INIT} = State) ->
    case get_flatten_stages() of
        [] -> State#?I_STATE{stage = ?STAGE_IDLE};
        [{FirstStage, FirstJob} | _] -> State#?I_STATE{stage = FirstStage, job = FirstJob}
    end;

get_next_state(#?I_STATE{stage = Stage, job = Job} = State) ->
    case tl(lists:dropwhile(fun(FlattenStage) -> FlattenStage =/= {Stage, Job} end, get_flatten_stages())) of
        [] -> State#?I_STATE{stage = ?STAGE_IDLE, job = undefined};
        [{NextStage, NextJob} | _] -> State#?I_STATE{stage = NextStage, job = NextJob}
    end.
