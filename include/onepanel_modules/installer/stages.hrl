%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains installer stages and jobs definitions.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_INSTALLER_STAGES_HRL).
-define(ONEPANEL_INSTALLER_STAGES_HRL, 1).

%% Callback events
-define(EVENT_ERROR, error).
-define(EVENT_STATE_CHANGED, state_changed).

%% Installer stages
-define(STAGE_IDLE, idle).
-define(STAGE_INIT, init).
-define(STAGE_DB, installer_db).
-define(STAGE_CM, installer_cm).
-define(STAGE_WORKER, installer_worker).
-define(STAGE_FINAL, installer_utils).

%% Installer jobs
-define(JOB_INSTALL, install).
-define(JOB_START, start).
-define(JOB_FINALIZE_INSTALLATION, finalize_installation).

%% Description of stages in terms of jobs
-define(STAGES, [
    {?STAGE_DB, [?JOB_START]},
    {?STAGE_CM, [?JOB_START]},
    {?STAGE_WORKER, [?JOB_START]},
    {?STAGE_FINAL, [?JOB_FINALIZE_INSTALLATION]}
]).

-endif.