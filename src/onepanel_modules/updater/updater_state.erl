%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Write me !
%% @end
%% ===================================================================
-module(updater_state).
-author("Rafal Slota").

-include("onepanel_modules/updater/state.hrl").

%% API
-export([is_abortable/1, get_stage_and_job/1, get_all_stages/0, get_object_count/1, get_all_stages/1, get_error_stack/1]).

%% ====================================================================
%% API functions
%% ====================================================================

is_abortable(#?u_state{}) ->
    true.

get_stage_and_job(#?u_state{stage = Stage, job = Job}) ->
    {Stage, Job}.

get_all_stages(#?u_state{nodes = Nodes, force_node_restart = ForceRestart, nodes_to_restart = NodesToRestart, nodes_to_repair = NodesToRepair}) ->

    RestartJobs =
        case ForceRestart of
            true  -> lists:usort(Nodes);
            false -> lists:usort(NodesToRestart)
        end,
    RepairJobs = lists:usort(NodesToRepair),

    Stages0 = lists:keyreplace(?STAGE_NODE_RESTART, 1, ?STAGES, {?STAGE_NODE_RESTART, RestartJobs}),
    lists:keyreplace(?STAGE_REPAIR_NODES, 1, Stages0, {?STAGE_REPAIR_NODES, RepairJobs}).

get_all_stages() ->
    ?STAGES.

get_object_count(#?u_state{objects = Objects}) ->
    maps:size(Objects).


%% Head is the latest error
-type updater_error() :: {{Stage :: atom(), Job :: atom(), ActionType :: install | rollback}, Object :: any(), Reason :: any()}.
-spec get_error_stack(State :: #?u_state{}) ->
    {Errors :: [updater_error()], Warnings :: [updater_error()]}.
get_error_stack(#?u_state{error_stack = Errors, warning_stack = Warnings}) ->
    {lists:flatten([Errors]), lists:flatten([Warnings])}.


%% ====================================================================
%% Internal functions
%% ====================================================================
