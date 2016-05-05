%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains installer state record and definition.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_INSTALLER_STATE_HRL).
-define(ONEPANEL_INSTALLER_STATE_HRL, 1).

%% Names of database records
-define(GLOBAL_CONFIG_RECORD, global_configuration).
-define(LOCAL_CONFIG_RECORD, local_configuration).

%% Names of database tables
-define(GLOBAL_CONFIG_TABLE, global_configurations).
-define(LOCAL_CONFIG_TABLE, local_configurations).

%% Id of current installation configuration saved in database
-define(CONFIG_ID, current).

%% Name of installer state
-define(I_STATE, i_state).

%% Installer state where
%% * job                - currently executing job
%% * stage              - currently executing stage
%% * callback           - function called each time installer state changes
%% * error              - error message sent via callback before terminating
-record(?I_STATE, {job, stage, config, error, callback}).

-ifdef(oneprovider).

%% Global config record contains following fields:
%% * id                 - ID which equals CONFIG_ID as a primary key in database
%% * main_cm            - hostname of machine where main CM node is configured
%% * cms                - list of hostnames of machines where CM nodes are configured
%% * workers            - list of hostnames of machines where worker nodes are configured
%% * dbs                - list of hostnames of machines where database nodes are configured
%% * storage_paths      - list of paths to storages on every worker node
%% * timestamp          - table creation timestamp as elapsed microseconds since epoch
-record(?GLOBAL_CONFIG_RECORD, {id, main_cm, cms = [], workers = [], dbs = [], storage_paths = [], timestamp = 0, args = []}).

%% Local config record describes host configuration that is:
%% * host               - machine hostname as a primary key in database
%% * gui_port           - GUI port that is available for Global Registry
%% * rest_port          - REST port that is available for Global Registry
%% * open_files         - limit of open files for Bigcouch database
%% * process_limit      - limit of processes for Bigcouch database
%% * ip_address         - machine IP address that is visible for Global Registry
-record(?LOCAL_CONFIG_RECORD, {host, gui_port, rest_port, open_files, process_limit, ip_address}).

-endif.

-ifdef(onezone).

%% Global config record contains following fields:
%% * id                 - ID which equals CONFIG_ID as a primary key in database
%% * dbs                - list of hostnames of machines where database nodes are configured
%% * timestamp          - table creation timestamp as elapsed microseconds since epoch
-record(?GLOBAL_CONFIG_RECORD, {id, main_cm, cms = [], workers = [], dbs = [], timestamp = 0, args = []}).

%% Local config record describes host configuration that is:
%% * host               - machine hostname as a primary key in database
%% * open_files         - limit of open files for Bigcouch database
%% * process_limit      - limit of processes for Bigcouch database
-record(?LOCAL_CONFIG_RECORD, {host, open_files, process_limit}).

-endif.

-endif.