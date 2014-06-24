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

%% Names of database tables
-define(USER_TABLE, user).
-define(CONFIG_TABLE, configuration).
-define(PORT_TABLE, port).

%% Id of current installation state saved in database
-define(CONFIG_ID, current).

%% User table contains name and hashed password of each user
-record(?USER_TABLE, {username, password}).

%% Config table describes current installation state, id equals CONFIG_ID
-record(?CONFIG_TABLE, {id, main_ccm, opt_ccms = [], workers = [], dbs = [], storage_paths = [], ulimits, providerId}).

%% Port table contains gui and rest port of VeilCluster on given host, that is accessible for outer world
-record(?PORT_TABLE, {host, gui, rest}).

-endif.