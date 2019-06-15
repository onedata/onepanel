%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains macros defining paths to dynamic GUI pages.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(GUI_PATHS_HRL).
-define(GUI_PATHS_HRL, 1).

% Endpoint to get Onepanel configuration
-define(CONFIGURATION_PATH, "/configuration").
-define(LOGIN_PATH, "/login").
-define(LOGOUT_PATH, "/logout").
-define(GUI_CONTEXT_PATH, "/gui-context").
-define(GUI_PREAUTHORIZE_PATH, "/gui-preauthorize").

-endif.
