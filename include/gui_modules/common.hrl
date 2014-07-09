%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains common macros and records for
%% gui modules.
%% @end
%% ===================================================================

-ifndef(COMMON_HRL).
-define(COMMON_HRL, 1).

-include_lib("ctool/include/gui/common.hrl").
-include("registered_names.hrl").

% Custom checkbox element
-record(custom_checkbox, {?ELEMENT_BASE(element_custom_checkbox),
    checked = false,
    value = "on",
    postback,
    disabled,
    name}).

%% Macros used as ids of errors that can appear on GUI pages
-define(AUTHENTICATION_ERROR, authentication_error).
-define(INTERNAL_SERVER_ERROR, internal_server_error).

%% Current installation step saved in user session
-define(INSTALL_PAGE, install_page).

%% Current registration step saved in user session
-define(REGISTER_PAGE, register_page).

%% Number of added storage paths
-define(STORAGE_PATHS_SIZE, storage_paths_size).

-endif.
