%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains common macros and records for
%% users management module.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_USER_LOGIC_HRL).
-define(ONEPANEL_USER_LOGIC_HRL, 1).

%% Names of database records
-define(USER_RECORD, user).

%% Names of database tables
-define(USER_TABLE, users).

%% User record contains following fields:
%% * username       - name of user as a primary key in database
%% * hash           - SHA 512 password hash
-record(?USER_RECORD, {username, password_hash}).

%% Minimum length of user's password
-define(MIN_PASSWORD_LENGTH, 8).

-endif.