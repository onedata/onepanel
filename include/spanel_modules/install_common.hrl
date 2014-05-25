%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains common macros and records for dao module
%% @end
%% ===================================================================

-ifndef(INSTALL_LOGIC_HRL).
-define(INSTALL_LOGIC_HRL, 1).

-define(INSTALL_LOGIC, install_logic).
-define(RPC_TIMEOUT, 30000).
-define(GEN_SERVER_TIMEOUT, 5000).

% Default cookie used for communication with cluster
-define(DEFAULT_COOKIE, veil_cluster_node).

% Default bigcouch port
-define(DEFAULT_PORT, "5986").

% Curl options
-define(CURL_OPTS, [{connect_timeout, 5000}, {basic_auth, {"admin", "password"}}]).

% Installation directory of veil RPM
-define(PREFIX, "/opt/veil/").

% Location of configured_nodes.cfg
-define(CONFIGURED_NODES_PATH, ?PREFIX ++ "scripts/configured_nodes.cfg").

% System limit values
-define(ULIMITS_CONFIG_PATH, ?PREFIX ++ "scripts/ulimits.cfg").
-define(DEFAULT_OPEN_FILES, "65535").
-define(DEFAULT_PROCESSES, "65535").

% Location of init.d script
-define(INIT_D_SCRIPT_PATH, "/etc/init.d/veil").

% Location of release packages
-define(VEIL_RELEASE, ?PREFIX ++ "files/veil_cluster_node").
-define(DB_RELEASE, ?PREFIX ++ "files/database_node").

% Location of erl_launcher
-define(ERL_LUNCHER_SCRIPT_PATH, ?PREFIX ++ "scripts/erl_launcher").

% Install path for nodes
-define(DEFAULT_NODES_INSTALL_PATH, ?PREFIX ++ "nodes/").
-define(DEFAULT_BIGCOUCH_INSTALL_PATH, "/opt/bigcouch"). %should not be changed, unless you've configured bigcouch realease properly (the one from files/database_node)
-define(DEFAULT_CCM_NAME, "ccm").
-define(DEFAULT_WORKER_NAME, "worker").
-define(DEFAULT_DB_NAME, "db").

% Paths relative to veil_cluster_node release
-define(CONFIG_ARGS_PATH, "bin/config.args").
-define(VEIL_CLUSTER_SCRIPT_PATH, "bin/veil_cluster").
-define(STORAGE_CONFIG_PATH, "bin/storage_info.cfg").

-endif.