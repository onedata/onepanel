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
-author("Rafal Slota").

-ifndef(UPDATER_COMMON_HRL).
-define(UPDATER_COMMON_HRL, 1).

%% Update stages
-define(STAGE_IDLE, idle).
-define(STAGE_INIT, init).
-define(STAGE_DAO_UPDATER_LOAD, dao_updater_load).
-define(STAGE_DAO_SETUP_VIEWS, dao_setup_views).
-define(STAGE_DAO_REFRESH_VIEWS, dao_refresh_views).
-define(STAGE_DEPLOY_FILES, deploy_files).
-define(STAGE_SOFT_RELOAD, soft_reload).
-define(STAGE_HARD_RELOAD, hard_reload).
-define(STAGE_FORCE_RELOAD, force_reload).
-define(STAGE_NODE_RESTART, node_restart).
-define(STAGE_ROLLBACK, rollback).

-record(u_state, {stage = ?STAGE_IDLE, stage_state, stage_history = [], error_stack = [], linked_procs = [], data, nodes = []}).


-record(version, {major = 0, minor = 0, patch = 0}).

-record(package, {type = rpm, binary = <<>>}).

-record(stage_error, {stage, error}).

-endif.