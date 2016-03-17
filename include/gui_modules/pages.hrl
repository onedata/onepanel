%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains all web pages definitions.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_GUI_PAGES_HRL).
-define(ONEPANEL_GUI_PAGES_HRL, 1).

%% Root page
-define(PAGE_ROOT, <<"/">>).

%% Management pages
-define(PAGE_LOGIN, <<"/login">>).
-define(PAGE_LOGIN_VALIDATION, <<"/login_validation">>).
-define(PAGE_LOGOUT, <<"/logout">>).
-define(PAGE_ABOUT, <<"/about">>).
-define(PAGE_ERROR, <<"/error">>).
-define(PAGE_ACCOUNT_SETTINGS, <<"/account/settings">>).
-define(PAGE_PRIVACY_POLICY, <<"/privacy_policy">>).

%% Installation pages
-define(PAGE_INSTALLATION, <<"/software/installation">>).
-define(PAGE_HOST_SELECTION, <<"/software/installation/hosts_selection">>).
-define(PAGE_PRIMARY_CM_SELECTION, <<"/software/installation/primary_cm_selection">>).
-define(PAGE_APP_PORTS_CHECK, <<"/software/installation/ports_check">>).
-define(PAGE_SYSTEM_LIMITS, <<"/software/installation/system_limits">>).
-define(PAGE_INSTALLATION_SUMMARY, <<"/software/installation/summary">>).
-define(PAGE_INSTALLATION_SUCCESS, <<"/software/installation/success">>).

%% Current installation step saved in user session
-define(CURRENT_INSTALLATION_PAGE, install_page).

-ifdef(oneprovider).

%% Current registration step saved in user session
-define(CURRENT_REGISTRATION_PAGE, register_page).

%% Installation pages
-define(PAGE_STORAGE, <<"/software/installation/storage">>).

%% Registration pages
-define(PAGE_CONNECTION_CHECK, <<"/spaces/registration/connection_check">>).
-define(PAGE_PORTS_CHECK, <<"/spaces/registration/ports_check">>).
-define(PAGE_REGISTRATION_SUMMARY, <<"/spaces/registration/summary">>).
-define(PAGE_REGISTRATION_SUCCESS, <<"/spaces/registration/success">>).

%% Spaces pages
-define(PAGE_SPACE_DETAILS, <<"/spaces">>).
-define(PAGE_SPACES_ACCOUNT, <<"/spaces/account">>).
-define(PAGE_SPACES_MANAGEMENT, <<"/spaces/management">>).

-endif.

-endif.
