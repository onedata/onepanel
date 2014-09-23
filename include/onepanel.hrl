%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains common macros and records for
%% onepanel module.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_HRL).
-define(ONEPANEL_HRL, 1).

-include("registered_names.hrl").

%% onepanel gen_server state
-record(state, {socket, address, port, passwords = []}).

-ifdef(provider).

%% Messages that will be sent to onepanel server after initialization
%% Format {Message :: term(), DelayInMiliseconds :: integer()}
-define(INIT_MESSAGES, [{start_updater, application:get_env(?APP_NAME, updater_start_delay, 5000)}]).

-endif.

-ifdef(globalregistry).

%% Messages that will be sent to onepanel server after initialization
%% Format {Message :: term(), DelayInMiliseconds :: integer()}
-define(INIT_MESSAGES, []).

-endif.

-endif.