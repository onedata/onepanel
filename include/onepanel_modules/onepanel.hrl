%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains common macros and records for
%% Onepanel module.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_HRL).
-define(ONEPANEL_HRL, 1).

-define(SERVER, ?MODULE).

%% Onepanel gen_server state
-record(state, {status, socket, address, port}).

-endif.