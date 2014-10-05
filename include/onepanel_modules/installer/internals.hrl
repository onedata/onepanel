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
%% Provider specific macros
%% ====================================================================

-ifdef(oneprovider).

%% Default cookie used for communication with application
-define(DEFAULT_COOKIE, oneprovider_node).

%% Installation directory of RPM package
-define(PREFIX, filename:join([filename:absname("/"), "opt", "oneprovider"])).

%% Default names of installable components
-define(DEFAULT_CCM_NAME, "ccm").
-define(DEFAULT_WORKER_NAME, "worker").

%% Location of oneprovider release
-define(ONEPROVIDER_RELEASE, filename:join([?PREFIX, "files", "oneprovider_node"])).

%% Location of config files relative to oneprovider release
-define(CONFIG_ARGS_PATH, filename:join(["bin", "config.args"])).
-define(ONEPROVIDER_SCRIPT_PATH, filename:join(["bin", "oneprovider"])).
-define(START_COMMAND_SUFFIX, filename:join(["bin", "oneprovider_node start"])).

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

%% Default cookie used for communication with application
-define(DEFAULT_COOKIE, globalregistry).

%% Installation directory of RPM package
-define(PREFIX, filename:join([filename:absname("/"), "opt", "globalregistry"])).

%% Default names of installable components
-define(DEFAULT_GLOBALREGISTRY_NAME, "globalregistry").

%% Location of globalregistry release
-define(GLOBALREGISTRY_RELEASE, filename:join([?PREFIX, "files", "globalregistry_node"])).

%% Location of config files
-define(CONFIG_PREFIX, filename:join([?PREFIX, "nodes", ?DEFAULT_GLOBALREGISTRY_NAME, "etc"])).
-define(APP_CONFIG_PATH, filename:join([?CONFIG_PREFIX, "app.config"])).
-define(VM_CONFIG_PATH, filename:join([?CONFIG_PREFIX, "vm.args"])).

-endif.

%% ====================================================================
%% Database components macros
%% ====================================================================

%% Default bigcouch port
-define(DEFAULT_PORT, 5986).

%% Timeout for database nodes addition to cluster
-define(CONNECTION_TIMEOUT, 5000).

%% Default names database component
-define(DEFAULT_DB_NAME, "db").

%% Location of database release
-define(DB_RELEASE, filename:join([?PREFIX, "files", "database_node"])).

%%Paths relative to database_node release
-define(DB_START_COMMAND_SUFFIX, filename:join(["bin", "bigcouch"])).
-define(NOHUP_OUTPUT, filename:join(["var", "log", "nohup.out"])).

%% Install path for database nodes, should not be changed, unless you've
%% configured bigcouch realease properly (the one from files/database_node)
-define(DEFAULT_DB_INSTALL_PATH, filename:join([filename:absname("/"), "opt", "bigcouch"])).

%% ====================================================================
%% Common macros
%% ====================================================================

%% Timeout for RPC calls (1 minute)
-define(RPC_TIMEOUT, 60000).

%% System limit values
-define(DEFAULT_OPEN_FILES, 65535).
-define(DEFAULT_PROCESSES, 65535).

%% Location of configured_nodes.cfg file
-define(CONFIGURED_NODES_PATH, filename:join([?PREFIX, "scripts", "configured_nodes.cfg"])).

%% Location of ulimits.cfg file
-define(ULIMITS_CONFIG_PATH, filename:join([?PREFIX, "scripts", "ulimits.cfg"])).

%% Install path for nodes
-define(DEFAULT_NODES_INSTALL_PATH, filename:join([?PREFIX, "nodes"])).

-endif.