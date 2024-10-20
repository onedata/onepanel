%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module allows for synchronous and asynchronous service action
%%% execution.
%%%
%%% Service actions are a mechanism for executing functions on selected hosts.
%%% Steps are defined in service_*:get_steps/2 callback functions.
%%% Each #step{} record describes invocation of one function.
%%% A #steps{} record allows nesting of another action.
%%% A single step is executed in parallel on all selected hosts.
%%% All nested steps are resolved into a flat list of #step{} records before
%%% action execution starts.
%%%
%%% Each action invocation (service:apply_sync and service:apply_async)
%%% causes creation of 2 processes by the service_executor:
%%% - worker - this process resolves the steps list and performs onepanel_rpc
%%%            calls to execute the functions
%%% - handler - a simple process storing the executed steps in its state.
%%%
%%% A started action is given a task id, which can be used to retrieve
%%% the execution results as long as the handler process exists.
%%% It is removed after task_ttl milliseconds.
%%% @end
%%%--------------------------------------------------------------------
-module(service_executor).
-author("Krzysztof Trzepla").

-behaviour(gen_server).

-include("modules/errors.hrl").
-include("service.hrl").
-include_lib("ctool/include/logging.hrl").
-include("names.hrl").

%% API
-export([start_link/0, handle_results/0, handle_results/2,
    receive_results/2, receive_count/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

% @formatter:off
-type task_id() :: binary().
-type hosts_results() :: {
    GoodResults :: onepanel_rpc:results(),
    BadResults :: onepanel_rpc:results()
}.
-type step_result() :: #step_begin{} | #step_end{}.
-type action_result() :: #action_end{}.
-type result() :: action_result() | step_result().
-type results() :: [result()].
% @formatter:on

-export_type([task_id/0, hosts_results/0, step_result/0, action_result/0,
    result/0, results/0]).

-record(task, {
    owner :: pid(),
    worker :: pid(),
    handler :: pid()
}).

-record(state, {
    tasks = #{} :: #{TaskId :: task_id() => {Worker :: pid(), Handler :: pid()}},
    workers = #{} :: #{Worker :: pid() => TaskId :: task_id()},
    handlers = #{} :: #{Handler :: pid() => TaskId :: task_id()}
}).

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
    gen_server:start_link({local, ?SERVICE_EXECUTOR_NAME}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc Loop that stores the asynchronous operation results.
%% @end
%%--------------------------------------------------------------------
-spec handle_results() -> no_return().
handle_results() ->
    handle_results([], 0).

-spec handle_results(service_executor:results(), StepsCount :: non_neg_integer()) ->
    no_return().
handle_results(History, StepsCount) ->
    receive
        #action_steps_count{count = NewStepsCount} ->
            ?MODULE:handle_results(History, NewStepsCount);
        #step_begin{} = Result ->
            ?MODULE:handle_results([Result | History], StepsCount);
        #step_end{} = Result ->
            ?MODULE:handle_results([Result | History], StepsCount);
        #action_end{} = Result ->
            ?MODULE:handle_results([Result | History], StepsCount);
        {forward_count, TaskId, Pid} ->
            Pid ! {step_count, TaskId, StepsCount},
            ?MODULE:handle_results(History, StepsCount);
        {forward_results, TaskId, Pid} ->
            Pid ! {task, TaskId, lists:reverse(History)},
            ?MODULE:handle_results(History, StepsCount);
        _ ->
            ?MODULE:handle_results(History, StepsCount)
    end.

%%--------------------------------------------------------------------
%% @doc Returns the asynchronous operation results.
%% @end
%%--------------------------------------------------------------------
-spec receive_results(TaskId :: task_id(), timeout()) ->
    Results :: service_executor:results() | ?ERROR_TIMEOUT.
receive_results(TaskId, Timeout) ->
    receive
        {task, TaskId, Result} -> Result
    after
        Timeout -> ?ERROR_TIMEOUT
    end.

%%--------------------------------------------------------------------
%% @doc Returns total number of steps resolved for given task.
%% @end
%%--------------------------------------------------------------------
-spec receive_count(TaskId :: task_id(), timeout()) ->
    Count :: non_neg_integer() | ?ERROR_TIMEOUT.
receive_count(TaskId, Timeout) ->
    receive
        {step_count, TaskId, Count} -> Count
    after
        Timeout -> ?ERROR_TIMEOUT
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Initializes the server.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private @doc Handles call messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_call({apply, Service, Action, Ctx}, {Owner, _}, #state{tasks = Tasks,
    workers = Workers, handlers = Handlers} = State) ->
    TaskId = onepanel_utils:gen_uuid(),
    {Handler, _} = erlang:spawn_monitor(?MODULE, handle_results, []),
    {Worker, _} = erlang:spawn_monitor(service, apply,
        [Service, Action, Ctx, Handler]),
    Task = #task{owner = Owner, worker = Worker, handler = Handler},
    {reply, TaskId, State#state{
        tasks = maps:put(TaskId, Task, Tasks),
        workers = maps:put(Worker, TaskId, Workers),
        handlers = maps:put(Handler, TaskId, Handlers)
    }};

handle_call({abort_task, TaskId}, _From, State) ->
    case task_cleanup(TaskId, State) of
        {true, NewState} -> {reply, ok, NewState};
        {false, NewState} -> {reply, {error, not_found}, NewState}
    end;

handle_call({exists_task, TaskId}, _From, #state{tasks = Tasks} = State) ->
    case maps:find(TaskId, Tasks) of
        {ok, #task{}} -> {reply, true, State};
        error -> {reply, false, State}
    end;

handle_call({get_count, TaskId}, {From, _}, #state{tasks = Tasks} = State) ->
    case maps:find(TaskId, Tasks) of
        {ok, #task{handler = Handler}} ->
            Handler ! {forward_count, TaskId, From},
            {reply, ok, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_results, TaskId}, {From, _}, #state{tasks = Tasks} = State) ->
    case maps:find(TaskId, Tasks) of
        {ok, #task{handler = Handler}} ->
            Handler ! {forward_results, TaskId, From},
            {reply, ok, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(Request, _From, State) ->
    ?log_bad_request(Request),
    {reply, {error, {invalid_request, Request}}, State}.

%%--------------------------------------------------------------------
%% @private @doc Handles cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(Request, State) ->
    ?log_bad_request(Request),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private @doc Handles all non call/cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info({'DOWN', _, _, Pid, normal}, #state{tasks = Tasks, workers = Workers} =
    State) ->
    case maps:find(Pid, Workers) of
        {ok, TaskId} ->
            case maps:find(TaskId, Tasks) of
                {ok, #task{owner = Owner, handler = Handler}} ->
                    Handler ! {forward_results, TaskId, Owner},
                    schedule_task_cleanup(TaskId);
                error ->
                    ?warning("Task ~tp not found", [TaskId])
            end;
        error -> ?warning("Worker ~tp not found", [Pid])
    end,
    {noreply, State#state{workers = maps:remove(Pid, Workers)}};

handle_info({'DOWN', _, _, Pid, _}, #state{workers = Workers, handlers = Handlers}
    = State) ->
    Result = case {maps:find(Pid, Workers), maps:find(Pid, Handlers)} of
        {{ok, Id}, _} -> {ok, Id};
        {_, {ok, Id}} -> {ok, Id};
        {_, _} -> not_found
    end,
    NewState = case Result of
        {ok, TaskId} ->
            {_, CleanState} = task_cleanup(TaskId, State),
            CleanState;
        not_found -> State
    end,
    {noreply, NewState};

handle_info({task_cleanup, TaskId}, State) ->
    {_, NewState} = task_cleanup(TaskId, State),
    {noreply, NewState};

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
    State :: #state{}) -> term().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private @doc Converts process state when code is changed.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Removes an asynchronous operation associated with the provided ID.
%% @end
%%--------------------------------------------------------------------
-spec task_cleanup(TaskId :: task_id(), State :: #state{}) ->
    {Cleaned :: boolean(), NewState :: #state{}}.
task_cleanup(TaskId, #state{tasks = Tasks, workers = Workers,
    handlers = Handlers} = State) ->
    case maps:find(TaskId, Tasks) of
        {ok, #task{worker = Worker, handler = Handler}} ->
            erlang:exit(Worker, shutdown),
            erlang:exit(Handler, shutdown),
            {true, State#state{
                tasks = maps:remove(TaskId, Tasks),
                workers = maps:remove(Worker, Workers),
                handlers = maps:remove(Handler, Handlers)
            }};
        error -> {false, State}
    end.


%%--------------------------------------------------------------------
%% @private @doc Schedules removal of an asynchronous operation associated with
%% the provided ID.
%% @end
%%--------------------------------------------------------------------
-spec schedule_task_cleanup(TaskId :: task_id()) -> ok.
schedule_task_cleanup(TaskId) ->
    Delay = onepanel_env:get(task_ttl),
    erlang:send_after(Delay, self(), {task_cleanup, TaskId}),
    ok.