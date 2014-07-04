%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Updater internal-only definitions
%% @end
%% ===================================================================
-author("Rafal Slota").

-ifndef(UPDATER_INTERNALS_HRL).
-define(UPDATER_INTERNALS_HRL, 1).

-include("onepanel_modules/updater/state.hrl").


%% Erlang libs that need node reboot in order to (safely) reload.
-define(REBOOT_ONLY_MODULES, ["kernel", "stdlib", "crypto"]).

%% Is update 'abort' action available?
-define(ABORT_AVAILABLE, true).

%% Timeout for node startup
-define(NODE_STARTUP_TIMEOUT,  10 * 1000).

-endif.