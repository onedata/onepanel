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

-define(IP_ADDRESS_REGEX, "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}").

%% Onepanel gen_server state
-record(state, {socket, address, port, regex, passwords = []}).

-endif.