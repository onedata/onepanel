%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains all web pages definitions.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_GUI_PAGES_HRL).
-define(ONEPANEL_GUI_PAGES_HRL, 1).

%% Root page
-define(PAGE_ROOT, <<"/">>).

%% Installation pages
-define(PAGE_INSTALLATION, <<"/software/installation">>).
-define(PAGE_HOST_SELECTION, <<"/software/installation/hosts_selection">>).
-define(PAGE_MAIN_CCM_SELECTION, <<"/software/installation/main_ccm_selection">>).
-define(PAGE_SYSTEM_LIMITS, <<"/software/installation/system_limits">>).
-define(PAGE_ADD_STORAGE, <<"/software/installation/add_storage">>).
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
-define(PAGE_SPACES_ACCOUNT, <<"/spaces/account">>).
-define(PAGE_SPACES_SETTINGS, <<"/spaces/settings">>).

%% Management pages
-define(PAGE_LOGIN, <<"/login">>).
-define(PAGE_LOGIN_VALIDATION, <<"/login_validation">>).
-define(PAGE_LOGOUT, <<"/logout">>).
-define(PAGE_ABOUT, <<"/about">>).
-define(PAGE_ACCOUNT_SETTINGS, <<"/account/settings">>).
-define(PAGE_SOFTWARE_SETTINGS, <<"/software/settings">>).

-endif.
