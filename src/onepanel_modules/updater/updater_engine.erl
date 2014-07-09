%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Updater generic engine. Handles stage/job/objects logic.
%% @end
%% ===================================================================
-module(updater_engine).
-author("Rafal Slota").

-include("onepanel_modules/updater/internals.hrl").
-include_lib("ctool/include/logging.hrl").
-include("registered_names.hrl").

%% API
-export([next_stage/1, enter_stage/2, handle_error/4, dispatch_object/2]).

%% ====================================================================
%% API functions
%% ====================================================================


%% enter_stage/2
%% ====================================================================
%% @doc Enters/starts given {Stage, Job}. Generates and dispatches all objects associated with this stage.
%% @end
-spec enter_stage({Stage :: atom(), Job :: atom()}, State :: #?u_state{}) ->
    NewState :: #?u_state{}.
%% ====================================================================
enter_stage({Stage, Job}, #?u_state{object_data = ObjData, callback = CFun, action_type = ActionType} = State) ->

    %% Set behaviour (install / rollback) dependent actions
    {DispatchFun, HandleFun, EventName, NewState0} =
        case ActionType of
            install  -> {fun dispatch_object/2, fun handle_stage/1, enter_stage, finalize_stage(State#?u_state{previous_data = ObjData})};
            rollback -> {fun rollback_object/2, fun handle_rollback/1, rollback_stage, State}
        end,

    ?info("Entering stage ~p:~p...", [Stage, Job]),

    NewState1 = NewState0#?u_state{stage = Stage, job = Job, objects = #{}, error_counter = #{}, object_data = #{}},

    %% State persistence
    dao:save_record(?UPDATER_STATE_TABLE, NewState1#?u_state{package = #package{}}),

    %% Generate objects
    {ObjectList0, NewState2} = HandleFun(NewState1),
    ObjectList1 = lists:flatten( [ ObjectList0 ] ),

    %% Dispatch objects
    Dispatch = dispatch_all(ObjectList1, NewState2, DispatchFun),
    NewState3 = NewState1#?u_state{objects = maps:from_list( Dispatch )},

    %% Notify
    CFun(EventName, NewState3),

    %% Skip stage if empty
    case maps:size(NewState3#?u_state.objects) =:= 0 andalso Stage =/= ?STAGE_IDLE of
        true -> enter_stage(next_stage(NewState3), NewState3);
        _    -> NewState3
    end.


%% next_stage/1
%% ====================================================================
%% @doc Returns next {Stage, Job} based on current stage and rollback/install behaviour.
%% @end
-spec next_stage(State :: #?u_state{}) ->
    {Stage :: atom(), Job :: atom()}.
%% ====================================================================
next_stage(#?u_state{stage = ?STAGE_IDLE, job = _, action_type = install} = State) ->
    [{Stage, Job} | _] = updater_utils:flatten_stages(updater_state:get_all_stages(State)),
    {Stage, Job};
next_stage(#?u_state{stage = Stage, job = Job, action_type = install} = State) ->
    [_, {NStage, NJob} | _] =
        lists:dropwhile(
            fun({CStage, CJob}) ->
                {CStage, CJob} =/= {Stage, Job}
            end, updater_utils:flatten_stages(updater_state:get_all_stages(State)) ++ [{?STAGE_IDLE, ?JOB_DEFAULT}]),
    {NStage, NJob};
next_stage(#?u_state{stage = ?STAGE_IDLE, job = _, action_type = rollback}) ->
    {?STAGE_IDLE, ?JOB_DEFAULT};
next_stage(#?u_state{stage = Stage, job = Job, action_type = rollback} = State) ->
    Stages = [{?STAGE_IDLE, ?JOB_DEFAULT}] ++ updater_utils:flatten_stages(updater_state:get_all_stages(State)),
    PrevStages =
        lists:takewhile(
            fun({CStage, CJob}) ->
                {CStage, CJob} =/= {Stage, Job}
            end, Stages),
    lists:last(PrevStages).


%% handle_error/4
%% ====================================================================
%% @doc Generic error handler. Determines if given object's error is critical or if it's just a warning.
%%      Tries also to re-dispatch object.
%% @end
-spec handle_error(Pid :: pid(), Obj :: term(), Reason :: any(), State :: #?u_state{}) ->
    NewState :: #?u_state{}.
%% ====================================================================
handle_error(Pid, Obj, Reason, #?u_state{error_counter = EC, objects = Objects,
    callback = CallbackFun,
    stage = Stage, job = Job, action_type = ActionType} = State) ->
    ErrorCount = maps:get(Obj, EC),
    if
        ErrorCount < 3 ->
            {NewPid, Obj} = dispatch_object(Obj, State),
            State#?u_state{objects = maps:put(NewPid, Obj, Objects)};
        true ->
            ErrorLevel = updater_impl:get_error_level(Stage, Job, Obj, Reason, State),
            case ActionType =:= rollback orelse ErrorLevel =:= warning of
                true ->
                    NewState0 = insert_warning(Obj, Reason, State),
                    {_, NewState1} = updater_worker:handle_info({Pid, ok}, NewState0#?u_state{objects = maps:put(Pid, Obj, Objects)}),
                    NewState1;
                false ->
                    ?error("Critical error ~p: ~p", [Obj, Reason]),
                    NewState0 = State,
                    NewState1 = init_rollback(NewState0),
                    NewState2 = insert_error(Obj, Reason, NewState1),
                    CallbackFun(ErrorLevel, NewState2),
                    enter_stage(updater_state:get_stage_and_job(NewState2), NewState2)
            end
    end.


%% dispatch_object/2
%% ====================================================================
%% @doc Convenience entry point for updater_impl:dispatch_object/4
%% @end
-spec dispatch_object(Obj :: term(), State :: #?u_state{}) -> {PidOfObjectWorker :: pid(), Obj :: term()}.
%% ====================================================================
dispatch_object(Obj, #?u_state{stage = Stage, job = Job} = State) ->
    %%?info("Dispatching ~p:~p obj: ~p", [Stage, Job, Obj]),
    {updater_impl:dispatch_object(Stage, Job, Obj, State), Obj}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% finalize_stage/1
%% ====================================================================
%% @doc Convenience entry point for updater_impl:finalize_stage/3
%% @end
-spec finalize_stage(State :: #?u_state{}) -> NewState :: #?u_state{}.
%% ====================================================================
finalize_stage(#?u_state{ stage = Stage, job = Job} = State) ->
    updater_impl:finalize_stage(Stage, Job, State).


%% rollback_object/2
%% ====================================================================
%% @doc Convenience entry point for updater_impl:rollback_object/4
%% @end
-spec rollback_object(Obj :: term(), State :: #?u_state{}) -> {PidOfObjectWorker :: pid(), Obj :: term()}.
%% ====================================================================
rollback_object(Obj, #?u_state{stage = Stage, job = Job} = State) ->
    %%?info("Rollback of ~p:~p obj: ~p", [Stage, Job, Obj]),
    {updater_impl:rollback_object(Stage, Job, Obj, State), Obj}.


%% handle_stage/1
%% ====================================================================
%% @doc Convenience entry point for updater_impl:handle_stage/3
%% @end
-spec handle_stage(State :: #?u_state{}) ->
    {[Object :: term()], NewState :: #?u_state{}}.
%% ====================================================================
handle_stage(#?u_state{stage = Stage, job = Job} = State) ->
    ?info("Handle stage ~p:~p", [Stage, Job]),
    case updater_impl:handle_stage(Stage, Job, State) of
        {Objects, #?u_state{} = NewState} ->
            {Objects, NewState};
        Objects -> {Objects, State}
    end.


%% handle_rollback/1
%% ====================================================================
%% @doc Convenience entry point for updater_impl:handle_rollback/3
%% @end
-spec handle_rollback(State :: #?u_state{}) ->
    {[Object :: term()], NewState :: #?u_state{}}.
%% ====================================================================
handle_rollback(#?u_state{stage = Stage, job = Job} = State) ->
    ?info("Handle rollback of ~p:~p", [Stage, Job]),
    case updater_impl:handle_rollback(Stage, Job, State) of
        {Objects, #?u_state{} = NewState} ->
            {Objects, NewState};
        Objects -> {Objects, State}
    end.


%% dispatch_all/3
%% ====================================================================
%% @doc Maps all Objects using DispatchFun. DispatchFun shall be either rollback_object/2 or dispatch_object/2.
%% @end
-spec dispatch_all(Objects :: [term()], State :: #?u_state{}, DispatchFun :: function()) ->
    {[Object :: term()], NewState :: #?u_state{}}.
%% ====================================================================
dispatch_all(Objects, #?u_state{} = State, DispatchFun) ->
    lists:map(fun(Obj) -> DispatchFun(Obj, State) end, Objects).


%% init_rollback/1
%% ====================================================================
%% @doc Initializes state for rollback (just after rollback trigger).
%% @end
-spec init_rollback(State :: #?u_state{}) ->
    NewState :: #?u_state{}.
%% ====================================================================
init_rollback(#?u_state{} = State) ->
    State#?u_state{action_type = rollback, objects = #{}}.


%% insert_error/3
%% ====================================================================
%% @doc Inserts error to error_stack.
%% @end
-spec insert_error(Object :: term(), Reason :: term(), State :: #?u_state{}) ->
    NewState :: #?u_state{}.
%% ====================================================================
insert_error(Obj, Reason, #?u_state{stage = Stage, job = Job, action_type = ActionType, error_stack = EC} = State)  ->
    State#?u_state{error_stack = [{{Stage, Job, ActionType}, Obj, updater_utils:normalize_error_reason(Reason)} | EC]}.


%% insert_warning/3
%% ====================================================================
%% @doc Inserts warning to error_stack.
%% @end
-spec insert_warning(Object :: term(), Reason :: term(), State :: #?u_state{}) ->
    NewState :: #?u_state{}.
%% ====================================================================
insert_warning(Obj, Reason, #?u_state{stage = Stage, job = Job, action_type = ActionType, warning_stack = EC} = State)  ->
    State#?u_state{warning_stack = [{{Stage, Job, ActionType}, Obj, updater_utils:normalize_error_reason(Reason)} | EC]}.

