%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains common macros and records for
%% web pages.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_GUI_COMMON_HRL).
-define(ONEPANEL_GUI_COMMON_HRL, 1).

-include("registered_names.hrl").
-include_lib("ctool/include/gui/common.hrl").

-record(custom_checkbox, {
    ?ELEMENT_BASE(element_custom_checkbox),
    autofocus,
    checked = false,
    disabled,
    name,
    value,
    postback
}).

%% Macros used as ids of errors that can appear on GUI pages
-define(AUTHENTICATION_ERROR, <<"authentication_error">>).
-define(INTERNAL_SERVER_ERROR, <<"internal_server_error">>).

%% Delay in miliseconds after which comet process will reload it's code
-define(COMET_PROCESS_RELOAD_DELAY, 5000).

%% Root page
-define(PAGE_ROOT, <<"/">>).

%% Installation pages
-define(PAGE_INSTALLATION, <<"/software/installation">>).
-define(PAGE_HOST_SELECTION, <<"/software/installation/hosts_selection">>).
-define(PAGE_PRIMARY_CCM_SELECTION, <<"/software/installation/primary_ccm_selection">>).
-define(PAGE_SYSTEM_LIMITS, <<"/software/installation/system_limits">>).
-define(PAGE_STORAGE, <<"/software/installation/storage">>).
-define(PAGE_INSTALLATION_SUMMARY, <<"/software/installation/summary">>).
-define(PAGE_INSTALLATION_SUCCESS, <<"/software/installation/success">>).

-endif.
