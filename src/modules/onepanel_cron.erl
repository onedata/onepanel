%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module is responsible for restarting registered services when
%%% not running.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_cron).
-author("Krzysztof Trzepla").
-author("Wojciech Geisler").

-behaviour(gen_server).

-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0]).
-export([register/3, register/4, unregister/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(TIMEOUT, timer:minutes(5)).
-define(DEFAULT_DELAY, onepanel_env:get(services_check_delay)).
-define(TICK_PERIOD, timer:seconds(10)).

-type condition() :: fun(() -> boolean()).
-type action() :: fun(() -> term()).

-record(job, {
    condition :: condition(),
    action :: action(),

    period = ?DEFAULT_DELAY :: non_neg_integer(),
    last_run = 0 :: non_neg_integer(),

    % pid of the process executing action
    pid :: pid() | undefined
}).

-type job() :: #job{}.
-type job_name() :: atom().

-type state() :: #{
job_name() => job()
}.

-export_type([condition/0, action/0, job/0, job_name/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?ONEPANEL_CRON_NAME}, ?MODULE, [], []).


-spec register(JobName :: job_name(), Action :: action(),
    Period :: non_neg_integer()) -> ok.
register(JobName, Action, Period) ->
    register(JobName, Action, Period, fun() -> true end).


-spec register(JobName :: job_name(), Action :: action(),
    Period :: non_neg_integer(), Condition :: condition()) -> ok.
register(JobName, Action, Period, Condition) ->
    Job = #job{
        condition = Condition, action = Action, period = Period
    },
    gen_server:call(?ONEPANEL_CRON_NAME, {register, JobName, Job}, ?TIMEOUT).


-spec unregister(JobName :: job_name()) -> ok.
unregister(JobName) ->
    gen_server:call(?ONEPANEL_CRON_NAME, {unregister, JobName}, ?TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Initializes the server.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    timer:send_interval(?TICK_PERIOD, tick),
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @private @doc Handles call messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {reply, Reply :: term(), NewState :: state(), timeout() | hibernate} |
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
    {stop, Reason :: term(), NewState :: state()}.
handle_call({register, Name, #job{} = Job}, _From, State) ->
    NewState = case State of
        #{Name := #job{last_run = LastRun}} ->
            State#{Name => Job#job{last_run = LastRun}};
        _ -> State#{Name => Job}
    end,
    {reply, ok, NewState};

handle_call({unregister, Job}, _From, State) ->
    {reply, ok, maps:remove(Job, State)};

handle_call(Request, _From, State) ->
    ?log_bad_request(Request),
    {reply, {error, {invalid_request, Request}}, State}.

%%--------------------------------------------------------------------
%% @private @doc Handles cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}.
handle_cast(Request, State) ->
    ?log_bad_request(Request),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private @doc Handles all non call/cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}.
handle_info(tick, State) ->
    {noreply, run_jobs(State)};

handle_info(Info, State) ->
    ?log_bad_request(Info),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private @doc Converts process state when code is changed.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: state(),
    Extra :: term()) ->
    {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Runs all jobs which conditions are met.
%% @end
%%--------------------------------------------------------------------
-spec run_jobs(State :: state()) -> NewState :: state().
run_jobs(State) ->
    Now = time_utils:system_time_millis(),
     maps:map(fun(_JobName, Job) ->
        case period_passed(Job, Now) andalso job_finished(Job) of
            true ->
                Pid = spawn(execute_job_fun(Job)),
                Job#job{pid = Pid, last_run = Now};
            false ->
                Job
        end
    end, State).


%%--------------------------------------------------------------------
%% @private @doc Checks if previous execution of the job has finished
%% or the job was never run.
%% @end
%%--------------------------------------------------------------------
-spec job_finished(#job{}) -> boolean().
job_finished(#job{pid = undefined}) -> true;
job_finished(#job{pid = Pid}) -> not erlang:is_process_alive(Pid).


%%--------------------------------------------------------------------
%% @private @doc Checks if time passed since last execution of a job
%% exceeds its configured period.
%% @end
%%--------------------------------------------------------------------
-spec period_passed(#job{}, NowMillis :: non_neg_integer()) -> boolean().
period_passed(#job{period = Period, last_run = LastRun}, Now) ->
    Now - LastRun > Period.


%%--------------------------------------------------------------------
%% @private @doc Returns fun for executing job's action
%% if its condition is met.
%% @end
%%--------------------------------------------------------------------
-spec execute_job_fun(#job{}) -> fun(() -> term()).
execute_job_fun(#job{condition = Cond, action = Action}) ->
    fun() ->
        case Cond() of
            true -> Action();
            false -> ok
        end
    end.
