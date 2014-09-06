%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module defines the behaviour of all installable components.
%% @end
%% ===================================================================

-module(installer_behaviour).

%% install/2
%% ====================================================================
%% Installs component on specified hosts. Returns ok if operation
%% completes successfully on all hosts or an error message if operation
%% fails on any host.
%% ====================================================================
-callback install(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.


%% uninstall/2
%% ====================================================================
%% Uninstalls component on specified hosts. Returns ok if operation
%% completes successfully on all hosts or an error message if operation
%% fails on any host.
%% ====================================================================
-callback uninstall(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.


%% start/2
%% ====================================================================
%% Starts component on specified hosts. Returns ok if operation
%% completes successfully on all hosts or an error message if operation
%% fails on any host.
%% ====================================================================
-callback start(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.


%% stop/2
%% ====================================================================
%% Stops component on specified hosts. Returns ok if operation
%% completes successfully on all hosts or an error message if operation
%% fails on any host.
%% ====================================================================
-callback stop(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.


%% restart/2
%% ====================================================================
%% Restarts component on specified hosts. Returns ok if operation
%% completes successfully on all hosts or an error message if operation
%% fails on any host.
%% ====================================================================
-callback restart(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.