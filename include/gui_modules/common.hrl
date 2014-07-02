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
-include("custom_elements.hrl").

%% Macros used as ids of errors that can appear on GUI pages
-define(AUTHENTICATION_ERROR, authentication_error).
-define(INTERNAL_SERVER_ERROR, internal_server_error).

-define(INSTALL_STATE, installation_state).
-record(?INSTALL_STATE, {page, main_ccm, ccms, workers, dbs, storage_paths}).

-endif.
