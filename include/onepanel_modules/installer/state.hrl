%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains installer state record and definition.
%% @end
%% ===================================================================

-ifndef(INSTALLER_STATE_HRL).
-define(INSTALLER_STATE_HRL, 1).

%% Callback events
-define(EVENT_ERROR, error).
-define(EVENT_STATE_CHANGED, state_change).

-define(i_state, i_state).

%% Installer state where
%% * job - currently executing job
%% * stage - currently executing stage
%% * callback - function called each time installer state changes
%% * error - error message sent via callback before terminating
-record(?i_state, {job, stage, error, callback}).

-endif.