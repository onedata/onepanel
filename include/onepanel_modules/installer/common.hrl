%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains common macros and records for
%% installation modules.
%% @end
%% ===================================================================

-ifndef(INSTALLER_COMMON_HRL).
-define(INSTALLER_COMMON_HRL, 1).

%% Timeout for RPC calls (1 minute)
-define(RPC_TIMEOUT, 60000).

%% Default cookie used for communication with cluster
-define(DEFAULT_COOKIE, veil_cluster_node).

%% Installation directory of veil RPM
-define(PREFIX, filename:join([filename:absname("/"), "opt", "veil"])).

%% Location of configured_nodes.cfg
-define(CONFIGURED_NODES_PATH, filename:join([?PREFIX, "scripts", "configured_nodes.cfg"])).

%% System limit values
-define(ULIMITS_CONFIG_PATH, filename:join([?PREFIX, "scripts", "ulimits.cfg"])).
-define(DEFAULT_OPEN_FILES, "65535").
-define(DEFAULT_PROCESSES, "65535").

%% Default names of installable components
-define(DEFAULT_DB_NAME, "db").
-define(DEFAULT_CCM_NAME, "ccm").
-define(DEFAULT_WORKER_NAME, "worker").

-endif.