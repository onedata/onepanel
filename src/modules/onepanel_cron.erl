%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module handles periodic invocation of registered jobs.
%%% A job may be guarded by a Condition, which must evalute to 'true'
%%% for the job's action to be executed.
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
-export([add_job/3, add_job/4, remove_job/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(TIMEOUT, timer:seconds(10)).
-define(JOB_TIMEOUT, onepanel_env:get(cron_job_timeout)).

%% Frequency of checks
-define(TICK_PERIOD, onepanel_env:get(cron_period)).

-define(NOW(), time_utils:system_time_millis()).

-type condition() :: fun(() -> boolean()).
-type action() :: fun(() -> term()).

-record(job, {
    condition :: condition(),
    action :: action(),

    %% Period in milliseconds to pass between job runs
    period :: non_neg_integer(),

    %% Millisecond timestamp of the last attempt to run the job,
    %% including times when the 'condition' was not met.
    last_run = 0 :: non_neg_integer(),

    % pid of the last invocation
    pid :: pid() | undefined
}).

-type job_name() :: term().
-type job() :: #job{}.

-type state() :: #{job_name() => job()}.

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


%%--------------------------------------------------------------------
%% @doc Adds a job which Condition is always met.
%% @end
%%--------------------------------------------------------------------
-spec add_job(JobName :: job_name(), Action :: action(),
    Period :: non_neg_integer()) -> ok.
add_job(JobName, Action, Period) ->
    add_job(JobName, Action, Period, fun() -> true end).


%%--------------------------------------------------------------------
%% @doc Adds a job. If job with given name already exists,
%% the new one overrides it, inheriting time and pid of the last run.
%% The Action will be executed only when Condition returns 'true'.
%% @end
%%--------------------------------------------------------------------
-spec add_job(JobName :: job_name(), Action :: action(),
    Period :: non_neg_integer(), Condition :: condition()) -> ok.
add_job(JobName, Action, Period, Condition) ->
    Job = #job{
        action = Action, period = Period, condition = Condition,
        % start with the current timestamp to delay first run of the job
        last_run = ?NOW()
    },
    gen_server:call(?ONEPANEL_CRON_NAME, {add_job, JobName, Job}, ?TIMEOUT).


%%--------------------------------------------------------------------
%% @doc Removes scheduled job.
%% Currently running process will be continued but
%% @end
%%--------------------------------------------------------------------
-spec remove_job(JobName :: job_name()) -> ok.
remove_job(JobName) ->
    gen_server:call(?ONEPANEL_CRON_NAME, {remove_job, JobName}, ?TIMEOUT).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    timer:send_interval(?TICK_PERIOD, tick),
    {ok, #{}}.


%%--------------------------------------------------------------------
%% @doc Handles call messages.
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
handle_call({add_job, Name, #job{} = Job}, _From, State) ->
    NewState = case State of
        #{Name := #job{last_run = LastRun, pid = Pid}} ->
            State#{Name => Job#job{last_run = LastRun, pid = Pid}};
        _ -> State#{Name => Job}
    end,
    {reply, ok, NewState};

handle_call({remove_job, Job}, _From, State) ->
    {reply, ok, maps:remove(Job, State)};

handle_call(Request, _From, State) ->
    ?log_bad_request(Request),
    {reply, {error, {invalid_request, Request}}, State}.


%%--------------------------------------------------------------------
%% @doc Handles cast messages.
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
%% @doc Handles all non call/cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}.
handle_info(tick, State) ->
    NewState = run_jobs(State),
    {noreply, NewState};

handle_info(Info, State) ->
    ?log_bad_request(Info),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
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
%% @doc Converts process state when code is changed.
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
    Now = ?NOW(),
    maps:map(fun(_JobName, Job) ->
        case period_passed(Job, Now) andalso job_finished(Job) of
            true ->
                Pid = spawn(execute_job_fun(Job)),
                timer:kill_after(?JOB_TIMEOUT, Pid),
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
