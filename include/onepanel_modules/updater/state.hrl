%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Write me !
%% @end
%% ===================================================================
-author("Rafal Slota").

-ifndef(UPDATER_STATE_HRL).
-define(UPDATER_STATE_HRL, 1).

-include("onepanel_modules/updater/common.hrl").
-include("onepanel_modules/updater/stages.hrl").

-define(UPDATER_STATE_TABLE, updater_state).
-define(u_state, u_state).
-define(UPDATER_STATE_ID, last).

-record(?u_state, {id = ?UPDATER_STATE_ID, action_type = install, nodes_to_restart = [], nodes_to_repair = [], force_node_restart = false, callback, stage = ?STAGE_IDLE, job, objects = [], object_data = #{}, previous_data = [], error_stack = [],
    warning_stack = [], package, nodes = [], installed_views = [], error_counter = #{}, not_reloaded_modules = [], version = #version{}}).

-endif.