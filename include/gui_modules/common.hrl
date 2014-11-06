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

-include("pages.hrl").
-include("errors.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/gui/common.hrl").

%% Delay in miliseconds after which comet process will reload it's code
-define(COMET_PROCESS_RELOAD_DELAY, 5000).

%% Page content starting height
-define(SUBMENU_HEIGHT, 107).

-endif.
