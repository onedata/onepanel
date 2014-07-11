%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module gives tools for updater's state interpretation.
%% @end
%% ===================================================================
-module(updater_state).
-author("Rafal Slota").

-include("onepanel_modules/updater/state.hrl").
-include("onepanel_modules/updater/internals.hrl").

%% API
-export([is_abortable/1, get_stage_and_job/1, get_all_stages/0, get_object_count/1, get_all_stages/1, get_error_stack/1, get_action_type/1]).

%% ====================================================================
%% API functions
%% ====================================================================


%% is_abortable/1
%% ====================================================================
%% @doc Checks if 'abort' command is available.
%% @end
-spec is_abortable(#?u_state{}) -> boolean().
%% ====================================================================
is_abortable(#?u_state{}) ->
    %% @todo: implement 'is_abortable' logic
    true andalso ?ABORT_AVAILABLE.


%% get_stage_and_job/1
%% ====================================================================
%% @doc Extracts stage and job from the state.
%% @end
-spec get_stage_and_job(#?u_state{}) -> {Stage :: atom(), Job :: atom()}.
%% ====================================================================
get_stage_and_job(#?u_state{stage = Stage, job = Job}) ->
    {Stage, Job}.


%% get_all_stages/1
%% ====================================================================
%% @doc Generates dynamic Stage -> Job map.
%% @end
-spec get_all_stages(#?u_state{}) -> [Stage :: {StageName :: atom(), [JobName :: atom()]}].
%% ====================================================================
get_all_stages(#?u_state{nodes = Nodes, force_node_restart = ForceRestart, nodes_to_restart = NodesToRestart, nodes_to_repair = NodesToRepair}) ->

    RestartJobs =
        case ForceRestart of
            true  -> lists:usort(Nodes);
            false -> lists:usort(NodesToRestart)
        end,
    RepairJobs = lists:usort(NodesToRepair),

    Stages0 = lists:keyreplace(?STAGE_NODE_RESTART, 1, ?STAGES, {?STAGE_NODE_RESTART, RestartJobs}),
    lists:keyreplace(?STAGE_REPAIR_NODES, 1, Stages0, {?STAGE_REPAIR_NODES, RepairJobs}).


%% get_all_stages/0
%% ====================================================================
%% @doc Returns non-dynamic Stage -> Job map. Use get_all_stages/1 instead if you
%%      updater state is available (i.e. update is in progress). Otherwise mapping won't be accurate.
%% @end
-spec get_all_stages() -> [Stage :: {StageName :: atom(), [JobName :: atom()]}].
%% ====================================================================
get_all_stages() ->
    ?STAGES.


%% get_object_count/1
%% ====================================================================
%% @doc Extracts pending 'object' count.
%% @end
-spec get_object_count(#?u_state{}) -> integer().
%% ====================================================================
get_object_count(#?u_state{objects = Objects}) ->
    maps:size(Objects).


%% get_error_stack/1
%% ====================================================================
%% @doc Returns all accumulated errors and warnings. Lists are sorted from most recent.
%% @end
-spec get_error_stack(State :: #?u_state{}) ->
    {Errors :: [updater_error()], Warnings :: [updater_error()]}.
%% ====================================================================
get_error_stack(#?u_state{error_stack = Errors, warning_stack = Warnings}) ->
    {lists:flatten([Errors]), lists:flatten([Warnings])}.


%% get_action_type/1
%% ====================================================================
%% @doc Returns current action type.
%% @end
-spec get_action_type(State :: #?u_state{}) ->
    ActionType :: atom().
%% ====================================================================
get_action_type(#?u_state{action_type = ActionType}) ->
    ActionType.

%% ====================================================================
%% Internal functions
%% ====================================================================
