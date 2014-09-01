%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains common macros and records for
%% onepanel module.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_HRL).
-define(ONEPANEL_HRL, 1).

-include("registered_names.hrl").

%% Timeout for gen_server calls (5 seconds)
-define(GEN_SERVER_TIMEOUT, 5000).

%% Onepanel gen_server state
-record(state, {status, socket, address, port, passwords = []}).

-endif.