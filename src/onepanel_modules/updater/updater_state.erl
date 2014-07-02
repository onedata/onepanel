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
-export([is_abortable/1, get_stage/1, get_all_stages/0, get_object_count/1]).

%% ====================================================================
%% API functions
%% ====================================================================

is_abortable(#u_state{}) ->
    true.


get_stage(#u_state{stage = Stage, job = Job}) ->
    {Stage, Job}.

get_all_stages() ->
    ?STAGES.

get_object_count(#u_state{objects = Objects}) ->
    maps:size(Objects).


%% ====================================================================
%% Internal functions
%% ====================================================================
