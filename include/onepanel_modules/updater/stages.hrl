%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Updater Stages and Jobs definitions
%% @end
%% ===================================================================
-author("Rafal Slota").

-ifndef(UPDATER_STAGES_HRL).
-define(UPDATER_STAGES_HRL, 1).

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
-define(STAGE_DAO_POST_SETUP_VIEWS, dao_post_setup_views).
-define(STAGE_REPAIR_NODES, repair_nodes).


%% Update jobs
-define(JOB_DOWNLOAD_BINARY, download_binary).
-define(JOB_LOAD_EXPORTS, load_exports).
-define(JOB_RELOAD_EXPORTS, reload_exports).
-define(JOB_INSTALL_PACKAGE, install_package).
-define(JOB_DEFAULT, default).
-define(JOB_MOVE_BEAMS, move_beams).
-define(JOB_LOAD_BEAMS, load_beams).
-define(JOB_PRE_UPDATE, pre_update).
-define(JOB_INSTALL_VIEW_SOURCES, install_view_sources).
-define(JOB_INSTALL_VIEWS, install_views).
-define(JOB_BACKUP, backup).
-define(JOB_DEPLOY, deploy).
-define(JOB_CLEANUP_VIEWS, cleanup_views).
-define(JOB_CHECK_CONNECTIVITY, check_connectivity).


%% Static Stage -> Job mapping
-define(STAGES, [
    {?STAGE_REPAIR_NODES, []}, %% Dynamic job list (known after STAGE_DEPLOY_FILES rollback) !
    {?STAGE_INIT, [?JOB_CHECK_CONNECTIVITY, ?JOB_DOWNLOAD_BINARY, ?JOB_LOAD_EXPORTS, ?JOB_INSTALL_PACKAGE, ?JOB_RELOAD_EXPORTS]},
    {?STAGE_DAO_UPDATER_LOAD, [?JOB_MOVE_BEAMS, ?JOB_LOAD_BEAMS, ?JOB_PRE_UPDATE]},
    {?STAGE_DAO_SETUP_VIEWS, [?JOB_INSTALL_VIEW_SOURCES, ?JOB_INSTALL_VIEWS]},
    {?STAGE_DAO_REFRESH_VIEWS, [?JOB_DEFAULT]},
    {?STAGE_DEPLOY_FILES, [?JOB_BACKUP, ?JOB_DEPLOY]},
    {?STAGE_SOFT_RELOAD, [?JOB_DEFAULT]},
    {?STAGE_FORCE_RELOAD, [?JOB_DEFAULT]},
    {?STAGE_DAO_POST_SETUP_VIEWS, [?JOB_CLEANUP_VIEWS]},
    {?STAGE_NODE_RESTART, []} %% Dynamic job list (known after STAGE_DEPLOY_FILES) !
]).


-endif.