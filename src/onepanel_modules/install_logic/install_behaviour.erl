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

-module(install_behaviour).

%% install/2
%% ====================================================================
%% @doc Installs component on specified hosts. Returns ok if operation
%% completes successfully on all hosts or an error if operation failes
%% on any host. Moreover it saves list of successful hosts in 
%% configuration file and database.
%% @end
%% ====================================================================
-callback install(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].


%% uninstall/2
%% ====================================================================
%% @doc Uninstalls component on specified hosts. Returns ok if operation
%% completes successfully on all hosts or an error if operation failes
%% on any host. Moreover it saves list of successful hosts in 
%% configuration file and database.
%% @end
%% ====================================================================
-callback uninstall(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].


%% start/2
%% ====================================================================
%% @doc Starts component on specified hosts. Returns ok if operation
%% completes successfully on all hosts or an error if operation failes
%% on any host. Moreover it saves list of successful hosts in
%% configuration file and database.
%% @end
%% ====================================================================
-callback start(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].


%% stop/2
%% ====================================================================
%% @doc Stops component on specified hosts. Returns ok if operation
%% completes successfully on all hosts or an error if operation failes
%% on any host. Moreover it saves list of successful hosts in
%% configuration file and database.
%% @end
%% ====================================================================
-callback stop(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].


%% restart/2
%% ====================================================================
%% @doc Restarts component on specified hosts. Returns ok if operation
%% completes successfully on all hosts or an error if operation failes
%% on any host. Moreover it saves list of successful hosts in
%% configuration file and database.
%% @end
%% ====================================================================
-callback restart(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].