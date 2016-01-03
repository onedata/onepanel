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

%% Default names of installable components
-define(CCM_NAME, "cm").
-define(WORKER_NAME, "worker").

%% Storage test file prefix
-define(STORAGE_TEST_FILE_PREFIX, "storage_test_").

%% Size of storage test file in bytes
-define(STORAGE_TEST_FILE_SIZE, 20).

%% ====================================================================
%% Database components macros
%% ====================================================================

%% Default names database component
-define(DB_NAME, "couchbase").

%% Default Riak database port
-define(DB_PORT, 11211).

-endif.

%% ====================================================================
%% Global Registry specific macros
%% ====================================================================

-ifdef(globalregistry).

%% Installation directory of RPM package
-define(PREFIX, filename:join([filename:absname("/"), "opt", "globalregistry"])).

%% Install path for nodes
-define(NODES_INSTALL_PATH, filename:join([?PREFIX, "nodes"])).

%% Default names of installable components
-define(GLOBALREGISTRY_NAME, "globalregistry").

%% Name of Global Registry service
-define(GLOBALREGISTRY_SERVICE, "globalregistry").

%% Name of database service
-define(DB_SERVICE, ?GLOBALREGISTRY_SERVICE).

%% Location of config files
-define(CONFIG_PREFIX, filename:join([?PREFIX, "nodes", ?GLOBALREGISTRY_NAME, "etc"])).
-define(GLOBALREGISTRY_APP_CONFIG, filename:join([?CONFIG_PREFIX, "app.config"])).
-define(GLOBALREGISTRY_VM_ARGS, filename:join([?CONFIG_PREFIX, "vm.args"])).
-define(GLOBALREGISTRY_DAEMON, filename:join(["bin", "globalregistry"])).

%% Domain name for Global Registry certificate
-define(GLOBALREGISTRY_CERT_DOMAIN, "onedata.org").

%% ====================================================================
%% Database components macros
%% ====================================================================

%% Default names database component
-define(DB_NAME, "db").

%% Default BigCouch database port
-define(DB_PORT, 5986).

%% Install path for database nodes, should not be changed, unless you've
%% configured bigcouch realease properly (the one from files/database_node)
-define(DB_PREFIX, filename:join([filename:absname("/"), "opt", "bigcouch"])).

%% Location of database release
-define(DB_RELEASE, filename:join([?PREFIX, "files", "database_node"])).
-define(DB_DAEMON, filename:join(["bin", "bigcouch"])).

%% Location of config file
-define(DB_CONFIG, filename:join([?DB_PREFIX, "etc", "vm.args"])).

%% Timeout request using database REST API
-define(DB_CONNECTION_TIMEOUT, 5000).

-endif.

%% ====================================================================
%% Common macros
%% ====================================================================

%% Timeout for RPC calls (1 minute)
-define(RPC_TIMEOUT, 60000).

%% Default system limit values
-define(OPEN_FILES, 65535).
-define(PROCESSES, 65535).

-endif.