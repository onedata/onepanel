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

%% System limit values
-define(DEFAULT_OPEN_FILES, 65535).
-define(DEFAULT_PROCESSES, 65535).

%% Default names database component
-define(DEFAULT_DB_NAME, "db").

%% ====================================================================
%% Provider specific macros
%% ====================================================================

-ifdef(provider).

%% Default cookie used for communication with application
-define(DEFAULT_COOKIE, veil_cluster_node).

%% Installation directory of RPM package
-define(PREFIX, filename:join([filename:absname("/"), "opt", "veil"])).

%% Location of configured_nodes.cfg file
-define(CONFIGURED_NODES_PATH, filename:join([?PREFIX, "scripts", "configured_nodes.cfg"])).

%% Location of ulimits.cfg file
-define(ULIMITS_CONFIG_PATH, filename:join([?PREFIX, "scripts", "ulimits.cfg"])).

%% Default names of installable components
-define(DEFAULT_CCM_NAME, "ccm").
-define(DEFAULT_WORKER_NAME, "worker").

%% Location of release packages
-define(DB_RELEASE, filename:join([?PREFIX, "files", "database_node"])).

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

%% Storage test file prefix
-define(STORAGE_TEST_FILE_PREFIX, "storage_test_").

%% Size of storage test file in bytes
-define(STORAGE_TEST_FILE_SIZE, 20).

-endif.

%% ====================================================================
%% Global Registry specific macros
%% ====================================================================

-ifdef(globalregistry).

%% Location of release packages
-define(DB_RELEASE, filename:join(["var", "lib", "globalregistry", "bigcouchdb", "database_node"])).

%% Location of configured_nodes.cfg file
-define(CONFIGURED_NODES_PATH, filename:join(["var", "lib", "globalregistry", "configured_nodes.cfg"])).

%% Location of ulimits.cfg file
-define(ULIMITS_CONFIG_PATH, filename:join(["var", "lib", "globalregistry", "ulimits.cfg"])).

-endif.

%% ====================================================================
%% Database components macros
%% ====================================================================

%% Default bigcouch port
-define(DEFAULT_PORT, 5986).

%% Timeout for database nodes addition to cluster
-define(CONNECTION_TIMEOUT, 5000).

%%Paths relative to database_node release
-define(DB_START_COMMAND_SUFFIX, filename:join(["bin", "bigcouch"])).
-define(NOHUP_OUTPUT, filename:join(["var", "log", "nohup.out"])).

%% Install path for database nodes, should not be changed, unless you've
%% configured bigcouch realease properly (the one from files/database_node)
-define(DEFAULT_DB_INSTALL_PATH, filename:join([filename:absname("/"), "opt", "bigcouch"])).


-endif.