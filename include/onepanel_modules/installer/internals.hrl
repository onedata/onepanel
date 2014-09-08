%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains common definitions for installer modules.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_INSTALLER_INTERNALS_HRL).
-define(ONEPANEL_INSTALLER_INTERNALS_HRL, 1).

%% ====================================================================
%% Common macros
%% ====================================================================

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

%% ====================================================================
%% Database components macros
%% ====================================================================

%% Default bigcouch port
-define(DEFAULT_PORT, "5986").

%% Timeout for database nodes addition to cluster
-define(CONNECTION_TIMEOUT, 5000).

%%Paths relative to database_node release
-define(DB_START_COMMAND_SUFFIX, filename:join(["bin", "bigcouch"])).
-define(NOHUP_OUTPUT, filename:join(["var", "log", "nohup.out"])).

%% Location of release packages
-define(DB_RELEASE, filename:join([?PREFIX, "files", "database_node"])).

%% Install path for database nodes, should not be changed, unless you've
%% configured bigcouch realease properly (the one from files/database_node)
-define(DEFAULT_DB_INSTALL_PATH, filename:join([filename:absname("/"), "opt", "bigcouch"])).

%% ====================================================================
%% Software components macros
%% ====================================================================

%% Location of release packages
-define(VEIL_RELEASE, filename:join([?PREFIX, "files", "veil_cluster_node"])).

%% Install path for nodes
-define(DEFAULT_NODES_INSTALL_PATH, filename:join([?PREFIX, "nodes"])).

%% Paths relative to veil_cluster_node release
-define(CONFIG_ARGS_PATH, filename:join(["bin", "config.args"])).
-define(VEIL_CLUSTER_SCRIPT_PATH, filename:join(["bin", "veil_cluster"])).
-define(START_COMMAND_SUFFIX, filename:join(["bin", "veil_cluster_node start"])).

%% Relative path to storage configuration file
-define(STORAGE_CONFIG_PATH, filename:join(["bin", "storage_info.cfg"])).

%% ====================================================================
%% Storage macros
%% ====================================================================

%% Storage test file prefix
-define(STORAGE_TEST_FILE_PREFIX, "storage_test_").

%% Size of storage test file in bytes
-define(STORAGE_TEST_FILE_SIZE, 20).

-endif.