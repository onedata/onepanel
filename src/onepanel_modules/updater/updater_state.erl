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

-include("onepanel_modules/updater/common.hrl").

%% API
-export([is_abortable/1, get_stage_and_job/1, get_all_stages/0, get_object_count/1, get_all_stages/1]).

%% ====================================================================
%% API functions
%% ====================================================================

is_abortable(#u_state{}) ->
    true.

get_stage_and_job(#u_state{stage = Stage, job = Job}) ->
    {Stage, Job}.

get_all_stages(#u_state{nodes_to_restart = Nodes}) ->
    Hostnames = lists:usort([install_utils:get_host(Node) || Node <- Nodes]),
    RestartJobs = [list_to_atom("restart_" ++ Hostname) || Hostname <- Hostnames],
    lists:keyreplace(?STAGE_NODE_RESTART, 1, ?STAGES, {?STAGE_NODE_RESTART, [RestartJobs]}).

get_all_stages() ->
    ?STAGES.

get_object_count(#u_state{objects = Objects}) ->
    maps:size(Objects).


%% ====================================================================
%% Internal functions
%% ====================================================================
