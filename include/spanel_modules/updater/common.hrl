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


-define(JOB_DOWNLOAD_BINARY, download_binary).
-define(JOB_LOAD_EXPORTS, load_exports).
-define(JOB_INSTALL_PACKAGE, install_package).
-define(JOB_DEFAULT, default).
-define(JOB_MOVE_BEAMS, move_beams).
-define(JOB_LOAD_BEAMS, load_beams).
-define(JOB_PRE_UPDATE, pre_update).
-define(JOB_INSTALL_VIEW_SOURCES, install_view_sources).
-define(JOB_INSTALL_VIEWS, install_views).
-define(JOB_BACKUP, backup).
-define(JOB_DEPLOY, deploy).

-define(STAGES, [
    {?STAGE_INIT, [?JOB_DOWNLOAD_BINARY, ?JOB_LOAD_EXPORTS, ?JOB_INSTALL_PACKAGE]},
    {?STAGE_DAO_UPDATER_LOAD, [?JOB_MOVE_BEAMS, ?JOB_LOAD_BEAMS, ?JOB_PRE_UPDATE]},
    {?STAGE_DAO_SETUP_VIEWS, [?JOB_INSTALL_VIEW_SOURCES, ?JOB_INSTALL_VIEWS]},
    {?STAGE_DAO_REFRESH_VIEWS, [?JOB_DEFAULT]},
    {?STAGE_DEPLOY_FILES, [?JOB_BACKUP, ?JOB_DEPLOY]},
    {?STAGE_SOFT_RELOAD, [?JOB_DEFAULT]},
    {?STAGE_FORCE_RELOAD, [?JOB_DEFAULT]}
]).



-record(version, {major = 0, minor = 0, patch = 0}).

-record(u_state, {callback, stage = ?STAGE_IDLE, job, objects = [], object_data = #{}, previous_data = [], error_stack = [],
    package, nodes = [], installed_views = [], error_counter = #{}, not_reloaded_modules = [], version = #version{}}).


-record(package, {type = rpm, binary = <<>>}).

-record(stage_error, {stage, error}).

-endif.