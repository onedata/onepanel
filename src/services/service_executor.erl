%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(service_executor).
-author("Krzysztof Trzepla").

-behaviour(gen_server).

-include("service.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0, apply_async/3, apply_sync/3, apply_sync/4, get_results/1, 
    abort_task/1, handle_results/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-type task_id() :: binary().

-export_type([task_id/0]).

-record(task, {
    owner :: pid(),
    worker :: pid(),
    handler :: pid()
}).

-record(state, {
    tasks = #{} :: #{Uuid :: binary() => {Worker :: pid(), Handler :: pid()}},
    workers = #{} :: #{Worker :: pid() => Uuid :: binary()},
    handlers = #{} :: #{Handler :: pid() => Uuid :: binary()}
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVICE_EXECUTOR}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec apply_async(Services :: service:name() | [service:name()],
    Action :: service:action(), Ctx :: service:ctx()) -> TaskId :: task_id().
apply_async(Services, Action, Ctx) ->
    gen_server:call(?SERVICE_EXECUTOR, {apply, Services, Action, Ctx}).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec apply_sync(Services :: service:name() | [service:name()],
    Action :: service:action(), Ctx :: service:ctx()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.
apply_sync(Services, Action, Ctx) ->
    apply_sync(Services, Action, Ctx, infinity).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec apply_sync(Services :: service:name() | [service:name()],
    Action :: service:action(), Ctx :: service:ctx(), Timeout :: timeout()) ->
    {ok, Result :: term()} | {error, Reason :: term()}.
apply_sync(Services, Action, Ctx, Timeout) ->
    TaskId = apply_async(Services, Action, Ctx),
    Result = receive_results(TaskId, Timeout),
    abort_task(TaskId),
    Result.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_results(TaskId :: task_id()) ->
    {ok, Results :: list()} | {error, Reason :: term()}.
get_results(TaskId) ->
    get_results(TaskId, infinity).

get_results(TaskId, Timeout) ->
    case gen_server:call(?SERVICE_EXECUTOR, {get_results, TaskId}) of
        ok -> receive_results(TaskId, Timeout);
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec abort_task(Uuid :: binary()) -> ok | {error, Reason :: term()}.
abort_task(TaskId) ->
    gen_server:call(?SERVICE_EXECUTOR, {abort_task, TaskId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
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
handle_call({apply, Services, Action, Ctx}, {Owner, _}, #state{tasks = Tasks,
    workers = Workers, handlers = Handlers} = State) ->
    TaskId = onepanel_utils:gen_uuid(),
    Handler = erlang:spawn_link(?MODULE, handle_results, [[]]),
    Worker = erlang:spawn_link(service, apply,
        [Services, Action, Ctx#{notify => Handler}]),
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

handle_call({get_results, TaskId}, From, #state{tasks = Tasks} = State) ->
    case maps:find(TaskId, Tasks) of
        {ok, #task{handler = Handler}} ->
            Handler ! {forward_results, TaskId, From},
            {reply, ok, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(Request, _From, State) ->
    ?log_bad_request(Request),
    {reply, {invalid_request, Request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
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
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info({'EXIT', Pid, normal}, #state{tasks = Tasks, workers = Workers} =
    State) ->
    case maps:find(Pid, Workers) of
        {ok, TaskId} ->
            case maps:find(TaskId, Tasks) of
                {ok, #task{owner = Owner, handler = Handler}} ->
                    Handler ! task_finished,
                    Handler ! {forward_results, TaskId, Owner},
                    schedule_task_cleanup(TaskId);
                error ->
                    ?warning("Task ~p not found", [TaskId])
            end;
        error -> ?warning("Worker ~p not found", [Pid])
    end,
    {noreply, State#state{workers = maps:remove(Pid, Workers)}};

handle_info({'EXIT', Pid, _}, #state{workers = Workers, handlers = Handlers}
    = State) ->
    Result = case {maps:find(Pid, Workers), maps:find(Pid, Handlers)} of
        {{ok, Id}, _} -> {ok, Id};
        {_, {ok, Id}} -> {ok, Id};
        {_, _} -> {error, not_found}
    end,
    NewState = case Result of
        {ok, TaskId} ->
            {_, CleanState} = task_cleanup(TaskId, State),
            CleanState;
        {error, not_found} -> State
    end,
    {noreply, NewState};

handle_info({task_cleanup, TaskId}, State) ->
    {_, NewState} = task_cleanup(TaskId, State),
    {noreply, NewState};

handle_info(Info, State) ->
    ?log_bad_request(Info),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
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
%% @private
%% @doc
%% Convert process state when code is changed
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


schedule_task_cleanup(TaskId) ->
    Delay = onepanel:get_env(task_ttl),
    erlang:send_after(Delay, self(), {task_cleanup, TaskId}).


handle_results(Results) ->
    receive
        task_finished ->
            ?MODULE:handle_results([task_finished | Results]);
        {step_end, Result} ->
            ?MODULE:handle_results([Result | Results]);
        {forward_results, TaskId, Pid} ->
            Pid ! {task, TaskId, lists:reverse(Results)},
            ?MODULE:handle_results(Results);
        _ ->
            ?MODULE:handle_results(Results)
    end.

receive_results(TaskId, Timeout) ->
    receive
        {task, TaskId, Result} -> {ok, Result}
    after
        Timeout -> {error, timeout}
    end.
