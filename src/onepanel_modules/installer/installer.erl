%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module is a gen_server that executes installation
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
-export([install/1, install/2, start_link/0]).
-export([get_stages/0, get_job_index/2, get_flatten_stages/0, get_stage_and_job/1, get_error/1, set_callback/1, get_next_state/1]).

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
    install(Config, fun(_Event, _State) -> ok end).


%% install/2
%% ====================================================================
%% @doc Starts installation process. Allows to pass a callback function
%% which will be called each time there has been a change in installation
%% state or an error occured.
%% Callback :: function(Event :: state_changed | error, State :: #?i_state{})
%% @end
-spec install(Config :: [{Name :: atom(), Value :: term()}], Callback :: function()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(Config, Callback) ->
    case start() of
        ok -> gen_server:call({global, ?INSTALL_SERVICE}, {install, Config, Callback});
        {error, Reason} -> {error, Reason}
    end.


%% get_stage_and_job/1
%% ====================================================================
%% @doc Returns stage and job for given state.
%% @end
-spec get_stage_and_job(State :: #?i_state{}) -> Result when
    Result :: {State :: atom(), Job :: atom()}.
%% ====================================================================
get_stage_and_job(#?i_state{stage = Stage, job = Job}) ->
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
-spec get_error(State :: #?i_state{}) -> Result when
    Result :: term().
%% ====================================================================
get_error(#?i_state{error = Error}) ->
    Error.


%% set_callback/1
%% ====================================================================
%% @doc Allows to set installation callback function which will be called
%% each time installer state changes or an error occured.
%% Callback :: function(Event :: state_changed | error, State :: #?i_state{})
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
    Result :: {ok, State :: #?i_state{}} |
    {ok, State :: #?i_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} |
    ignore.
%% ====================================================================
init([]) ->
    {ok, #?i_state{stage = ?STAGE_INIT}}.


%% handle_call/3
%% ====================================================================
%% @doc Handling call messages
%% @end
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #?i_state{}) -> Result when
    Result :: {reply, Reply :: term(), NewState :: #?i_state{}} |
    {reply, Reply :: term(), NewState :: #?i_state{}, timeout() | hibernate} |
    {noreply, NewState :: #?i_state{}} |
    {noreply, NewState :: #?i_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #?i_state{}} |
    {stop, Reason :: term(), NewState :: #?i_state{}}.
%% ====================================================================
handle_call({set_callback, Callback}, _From, State) ->
    {reply, ok, State#?i_state{callback = Callback}};

handle_call({install, Config, Callback}, _From, #?i_state{stage = ?STAGE_INIT} = State) ->
    NextState = get_next_state(State),
    gen_server:cast({global, ?INSTALL_SERVICE}, {execute, Config}),
    {reply, ok, NextState#?i_state{callback = Callback}};

handle_call({install, _, _}, _From, State) ->
    {reply, {error, installation_already_in_progress}, State};

handle_call(Request, _From, State) ->
    ?warning("[Installer] Wrong call: ~p", [Request]),
    {reply, {error, wrong_request}, State}.


%% handle_cast/2
%% ====================================================================
%% @doc Handling cast messages
%% @end
-spec handle_cast(Request :: term(), State :: #?i_state{}) -> Result when
    Result :: {noreply, NewState :: #?i_state{}} |
    {noreply, NewState :: #?i_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #?i_state{}}.
%% ====================================================================
handle_cast({execute, _}, #?i_state{stage = ?STAGE_IDLE, callback = Callback} = State) ->
    Callback(?EVENT_STATE_CHANGED, State),
    {stop, normal, State};

handle_cast({execute, Config}, #?i_state{stage = Stage, job = Job, callback = Callback} = State) ->
    Callback(?EVENT_STATE_CHANGED, State),
    case Stage:Job(Config) of
        ok ->
            NextState = get_next_state(State),
            gen_server:cast({global, ?INSTALL_SERVICE}, {execute, Config}),
            {noreply, NextState};
        Error ->
            NextState = State#?i_state{error = Error},
            Callback(?EVENT_ERROR, NextState),
            {stop, shutdown, NextState}
    end;

handle_cast(Request, State) ->
    ?warning("[Installer] Wrong cast: ~p", [Request]),
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc Handling all non call/cast messages
%% @end
-spec handle_info(Info :: timeout() | term(), State :: #?i_state{}) -> Result when
    Result :: {noreply, NewState :: #?i_state{}} |
    {noreply, NewState :: #?i_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #?i_state{}}.
%% ====================================================================
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
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #?i_state{}) -> Result when
    Result :: term().
%% ====================================================================
terminate(Reason, _State) ->
    ?warning("[Installer] Terminate: ~p", [Reason]),
    ok.


%% code_change/3
%% ====================================================================
%% @doc Convert process state when code is changed
%% @end
-spec code_change(OldVsn :: term() | {down, term()}, State :: #?i_state{}, Extra :: term()) -> Result when
    Result :: {ok, NewState :: #?i_state{}} | {error, Reason :: term()}.
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
-spec get_next_state(State :: #?i_state{}) -> Result when
    Result :: #?i_state{}.
%% ====================================================================
get_next_state(#?i_state{stage = ?STAGE_IDLE} = State) ->
    State;

get_next_state(#?i_state{stage = ?STAGE_INIT} = State) ->
    case get_flatten_stages() of
        [] -> State#?i_state{stage = ?STAGE_IDLE};
        [{FirstStage, FirstJob} | _] -> State#?i_state{stage = FirstStage, job = FirstJob}
    end;

get_next_state(#?i_state{stage = Stage, job = Job} = State) ->
    case tl(lists:dropwhile(fun(FlattenStage) -> FlattenStage =/= {Stage, Job} end, get_flatten_stages())) of
        [] -> State#?i_state{stage = ?STAGE_IDLE, job = undefined};
        [{NextStage, NextJob} | _] -> State#?i_state{stage = NextStage, job = NextJob}
    end.
