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
-define(COOKIE, oneprovider_node).

%% Installation directory of RPM package
-define(PREFIX, filename:join([filename:absname("/"), "opt", "oneprovider"])).

%% Default names of installable components
-define(CCM_NAME, "ccm").
-define(WORKER_NAME, "worker").

%% Location of oneprovider release
-define(ONEPROVIDER_RELEASE, filename:join([?PREFIX, "files", "oneprovider_node"])).

%% Name of oneprovider service
-define(ONEPROVIDER_SERVICE, "oneprovider").

%% Name of database service
-define(DB_SERVICE, ?ONEPROVIDER_SERVICE).

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
-define(COOKIE, globalregistry).

%% Installation directory of RPM package
-define(PREFIX, filename:join([filename:absname("/"), "opt", "globalregistry"])).

%% Default names of installable components
-define(GLOBALREGISTRY_NAME, "globalregistry").

%% Location of globalregistry release
-define(GLOBALREGISTRY_RELEASE, filename:join([?PREFIX, "files", "globalregistry_node"])).

%% Name of Global Registry service
-define(GLOBALREGISTRY_SERVICE, "globalregistry").

%% Name of database service
-define(DB_SERVICE, ?GLOBALREGISTRY_SERVICE).

%% Location of config files
-define(CONFIG_PREFIX, filename:join([?PREFIX, "nodes", ?GLOBALREGISTRY_NAME, "etc"])).
-define(GLOBALREGISTRY_APP_CONFIG, filename:join([?CONFIG_PREFIX, "app.config"])).
-define(GLOBALREGISTRY_VM_ARGS, filename:join([?CONFIG_PREFIX, "vm.args"])).

-endif.

%% ====================================================================
%% Database components macros
%% ====================================================================

%% Default bigcouch port
-define(DB_PORT, 5986).

%% Install path for database nodes, should not be changed, unless you've
%% configured bigcouch realease properly (the one from files/database_node)
-define(DB_PREFIX, filename:join([filename:absname("/"), "opt", "bigcouch"])).

%% Default names database component
-define(DB_NAME, "db").

%% Location of database release
-define(DB_RELEASE, filename:join([?PREFIX, "files", "database_node"])).

%% Location of config files
-define(DB_VM_ARGS, filename:join([?DB_PREFIX, "etc", "vm.args"])).

%% Timeout request using database REST API
-define(DB_CONNECTION_TIMEOUT, 5000).

%% ====================================================================
%% Common macros
%% ====================================================================

%% Timeout for RPC calls (1 minute)
-define(RPC_TIMEOUT, 60000).

%% Default system limit values
-define(OPEN_FILES, 65535).
-define(PROCESSES, 65535).

%% Location of configured_nodes.cfg file
-define(CONFIGURED_NODES_PATH, filename:join([?PREFIX, "scripts", "configured_nodes.cfg"])).

%% Location of ulimits.cfg file
-define(ULIMITS_CONFIG_PATH, filename:join([?PREFIX, "scripts", "ulimits.cfg"])).

%% Install path for nodes
-define(NODES_INSTALL_PATH, filename:join([?PREFIX, "nodes"])).

-endif.