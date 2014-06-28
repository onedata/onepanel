%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains common macros and records for
%% db_logic module.
%% @end
%% ===================================================================

-ifndef(DB_LOGIC_HRL).
-define(DB_LOGIC_HRL, 1).

%% Names of database records
-define(USER_RECORD, user).
-define(GLOBAL_CONFIG_RECORD, global_configuration).
-define(LOCAL_CONFIG_RECORD, local_configuration).

%% Names of database tables
-define(USER_TABLE, users).
-define(GLOBAL_CONFIG_TABLE, global_configurations).
-define(LOCAL_CONFIG_TABLE, local_configurations).

%% Id of overall installation state saved in database
-define(CONFIG_ID, current).

%% User table contains name and hashed password of each user
-record(?USER_RECORD, {username, password}).

%% Global config record describes installation configuration that is:
%% - ID which equals CONFIG_ID
%% - hostname of machine where main CCM node is running
%% - list of hostnames of machines where CCM nodes are running
%% - list of hostnames of machines where worker nodes are running
%% - list of hostnames of machines where database nodes are running
%% - list of paths to storages on every worker node
%% - provider ID returned from Global Registry
-record(?GLOBAL_CONFIG_RECORD, {id, main_ccm, opt_ccms = [], workers = [], dbs = [], storage_paths = [], providerId}).

%% Local config record describes host configuration that is:
%% - hostname
%% - GUI port that is visible by Global Registry
%% - REST port that is visible by Global Registry
%% - limit for open files for Bigcouch database
%% - limit for running processes for Bigcouch database
-record(?LOCAL_CONFIG_RECORD, {host, gui_port, rest_port, open_files_limit, processes_limit}).

-endif.