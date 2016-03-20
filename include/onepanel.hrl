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

-ifdef(oneprovider).

%% Message prefix together with host ip address is sent on multicast address
-define(MULTICAST_MESSAGE_PREFIX, <<"oneprovider_">>).

-endif.

-ifdef(onezone).

%% Message prefix together with host ip address is sent on multicast address
-define(MULTICAST_MESSAGE_PREFIX, <<"onezone_">>).

-endif.

-endif.