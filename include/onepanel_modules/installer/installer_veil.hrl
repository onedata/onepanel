%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains macros and records for
%% VeilCluster nodes installation modules.
%% @end
%% ===================================================================

-ifndef(INSTALLER_VEIL_HRL).
-define(INSTALLER_VEIL_HRL, 1).

-include("common.hrl").

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

-endif.