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
-define(ONEZONE_LOGIN_PATH, "/onezone-login").
-define(LOGIN_PATH, "/login").
-define(LOGOUT_PATH, "/logout").
-define(GUI_TOKEN_PATH, "/gui-token").

-endif.
