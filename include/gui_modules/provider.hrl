%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains common macros and records for
%% provider web pages.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_GUI_PROVIDER_HRL).
-define(ONEPANEL_GUI_PROVIDER_HRL, 1).

-include("common.hrl").

%% Macros used as ids of errors that can appear on GUI pages
-define(SOFTWARE_NOT_INSTALLED_ERROR, <<"software_not_installed_error">>).
-define(UNREGISTERED_PROVIDER_ERROR, <<"unregistered_provider_error">>).
-define(SPACE_PERMISSION_DENIED_ERROR, <<"space_permission_denied_error">>).
-define(SPACE_NOT_FOUND_ERROR, <<"space_not_found_error">>).

%% Current installation step saved in user session
-define(CURRENT_INSTALLATION_PAGE, install_page).

%% Current registration step saved in user session
-define(CURRENT_REGISTRATION_PAGE, register_page).

%% Current update step saved in user session
-define(CURRENT_UPDATE_PAGE, update_page).

%% Currently selected version in update process
-define(CHOSEN_VERSION, chosen_version).

%% Installation pages
-define(PAGE_INSTALLATION, <<"/software/installation">>).
-define(PAGE_HOST_SELECTION, <<"/software/installation/hosts_selection">>).
-define(PAGE_PRIMARY_CCM_SELECTION, <<"/software/installation/primary_ccm_selection">>).
-define(PAGE_SYSTEM_LIMITS, <<"/software/installation/system_limits">>).
-define(PAGE_STORAGE, <<"/software/installation/storage">>).
-define(PAGE_INSTALLATION_SUMMARY, <<"/software/installation/summary">>).
-define(PAGE_INSTALLATION_SUCCESS, <<"/software/installation/success">>).

%% Update pages
-define(PAGE_UPDATE, <<"/software/update">>).
-define(PAGE_VERSION_SELECTION, <<"/software/update/version_selection">>).
-define(PAGE_UPDATE_SUMMARY, <<"/software/update/summary">>).
-define(PAGE_UPDATE_SUCCESS, <<"/software/update/success">>).

%% Registration pages
-define(PAGE_CONNECTION_CHECK, <<"/spaces/registration/connection_check">>).
-define(PAGE_PORTS_CHECK, <<"/spaces/registration/ports_check">>).
-define(PAGE_REGISTRATION_SUMMARY, <<"/spaces/registration/summary">>).
-define(PAGE_REGISTRATION_SUCCESS, <<"/spaces/registration/success">>).

%% Spaces pages
-define(PAGE_SPACE_DETAILS, <<"/spaces">>).
-define(PAGE_SPACES_ACCOUNT, <<"/spaces/account">>).
-define(PAGE_SPACES_SETTINGS, <<"/spaces/settings">>).

-endif.
