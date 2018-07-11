%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module allows for synchronous and asynchronous service action
%%% execution.
%%% @end
%%%--------------------------------------------------------------------
-module(service_executor).
-author("Krzysztof Trzepla").

-behaviour(gen_server).

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include("names.hrl").

%% API
-export([start_link/0, handle_results/1, receive_results/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-type task_id() :: binary().
-type hosts_results() :: {GoodResults :: onepanel_rpc:results(),
    BadResults :: onepanel_rpc:results()}.
-type step_result() :: {Module :: module(), Function :: atom()} |
{Module :: module(), Function :: atom(), HostsResults :: hosts_results()}.
-type action_result() :: {task_finished, Service :: service:name(),
    Action :: service:action(), Result :: ok | #error{}}.
-type result() :: action_result() | step_result().
-type results() :: [result()].

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
-spec handle_results(Results :: service_executor:results()) -> no_return().
handle_results(Results) ->
    receive
        {step_begin, Result} ->
            ?MODULE:handle_results([Result | Results]);
        {step_end, Result} ->
            ?MODULE:handle_results([Result | Results]);
        {action_end, Result} ->
            ?MODULE:handle_results([{task_finished, Result} | Results]);
        {forward_results, TaskId, Pid} ->
            Pid ! {task, TaskId, lists:reverse(Results)},
            ?MODULE:handle_results(Results);
        _ ->
            ?MODULE:handle_results(Results)
    end.

%%--------------------------------------------------------------------
%% @doc Returns the asynchronous operation results.
%% @end
%%--------------------------------------------------------------------
-spec receive_results(TaskId :: task_id(), Timeout :: timeout()) ->
    Results :: service_executor:results() | #error{}.
receive_results(TaskId, Timeout) ->
    receive
        {task, TaskId, Result} -> Result
    after
        Timeout -> ?make_error(?ERR_TIMEOUT, [TaskId, Timeout])
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
    {Handler, _} = erlang:spawn_monitor(?MODULE, handle_results, [[]]),
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
        {false, NewState} -> {reply, ?make_error(?ERR_NOT_FOUND), NewState}
    end;

handle_call({exists_task, TaskId}, _From, #state{tasks = Tasks} = State) ->
    case maps:find(TaskId, Tasks) of
        {ok, #task{}} -> {reply, true, State};
        error -> {reply, false, State}
    end;

handle_call({get_results, TaskId}, {From, _}, #state{tasks = Tasks} = State) ->
    case maps:find(TaskId, Tasks) of
        {ok, #task{handler = Handler}} ->
            Handler ! {forward_results, TaskId, From},
            {reply, ok, State};
        error ->
            {reply, ?make_error(?ERR_NOT_FOUND), State}
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
                    ?warning("Task ~p not found", [TaskId])
            end;
        error -> ?warning("Worker ~p not found", [Pid])
    end,
    {noreply, State#state{workers = maps:remove(Pid, Workers)}};

handle_info({'DOWN', _, _, Pid, _}, #state{workers = Workers, handlers = Handlers}
    = State) ->
    Result = case {maps:find(Pid, Workers), maps:find(Pid, Handlers)} of
        {{ok, Id}, _} -> {ok, Id};
        {_, {ok, Id}} -> {ok, Id};
        {_, _} -> ?make_error(?ERR_NOT_FOUND)
    end,
    NewState = case Result of
        {ok, TaskId} ->
            {_, CleanState} = task_cleanup(TaskId, State),
            CleanState;
        #error{reason = ?ERR_NOT_FOUND} -> State
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