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

-ifndef(DB_HRL).
-define(DB_HRL, 1).

%% Names of database tables
-define(USER_TABLE, users).
-define(CONFIG_TABLE, configurations).
-define(PORT_TABLE, ports).

%% Id of current installation state saved in database
-define(CONFIG_ID, current).

%% User table contains name and hashed password of each user
-record(?USER_TABLE, {username, password}).

%% Config table describes current installation state, id equals 'last'
-record(?CONFIG_TABLE, {id, main_ccm, opt_ccms = [], workers = [], dbs = [], storage_paths = [], ulimits, providerId}).

%% Port table contains gui and rest port of VeilCluster on given host, that is accessible for outer world
-record(?PORT_TABLE, {host, gui, rest}).

-endif.