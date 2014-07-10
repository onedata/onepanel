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
-define(PAGE_INSTALLATION, <<"/installation">>).
-define(PAGE_HOST_SELECTION, <<"/hosts_selection">>).
-define(PAGE_MAIN_CCM_SELECTION, <<"/main_ccm_selection">>).
-define(PAGE_ULIMITS, <<"/ulimits">>).
-define(PAGE_ADD_STORAGE, <<"/add_storage">>).
-define(PAGE_INSTALLATION_SUMMARY, <<"/installation_summary">>).
-define(PAGE_INSTALLATION_SUCCESS, <<"/installation_success">>).

%% Registration pages
-define(PAGE_REGISTRATION, <<"/registration">>).
-define(PAGE_CONNECTION_CHECK, <<"/connection_check">>).
-define(PAGE_PORTS_CHECK, <<"/ports_check">>).
-define(PAGE_REGISTRATION_SUCCESS, <<"/registration_success">>).

%% Update pages
-define(PAGE_UPDATE, <<"/update">>).
-define(PAGE_CHOOSE_VERSION, <<"/choose_version">>).
-define(PAGE_UPDATE_SUMMARY, <<"/update_summary">>).
-define(PAGE_UPDATE_SUCCESS, <<"/update_success">>).

%% Management pages
-define(PAGE_LOGIN, <<"/login">>).
-define(PAGE_LOGOUT, <<"/logout">>).
-define(PAGE_ABOUT, <<"/about">>).
-define(PAGE_MANAGE_ACCOUNT, <<"/manage_account">>).
-define(PAGE_VALIDATE_LOGIN, <<"/validate_login">>).

-endif.
