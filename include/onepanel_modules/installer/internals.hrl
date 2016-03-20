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

%% Storage test file prefix
-define(STORAGE_TEST_FILE_PREFIX, "storage_test_").

%% Size of storage test file in bytes
-define(STORAGE_TEST_FILE_SIZE, 20).

-endif.

%% ====================================================================
%% Database components macros
%% ====================================================================

%% Default names database component
-define(DB_NAME, "couchbase").

%% Default Riak database port
-define(DB_PORT, 11211).

%% ====================================================================
%% Common macros
%% ====================================================================

%% Default names of installable components
-define(CM_NAME, "cm").
-define(WORKER_NAME, "worker").

%% Timeout for RPC calls (5 minutes)
-define(RPC_TIMEOUT, timer:minutes(5)).

%% Default system limit values
-define(OPEN_FILES, 65535).
-define(PROCESSES, 65535).

-endif.