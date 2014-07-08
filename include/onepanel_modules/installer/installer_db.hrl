%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains macros and records for
%% database nodes installation module.
%% @end
%% ===================================================================

-ifndef(INSTALLER_DB_HRL).
-define(INSTALLER_DB_HRL, 1).

-include("common.hrl").

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

-endif.