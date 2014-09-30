%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains definitions of names used to
%% identify different parts of application (or whole application).
%% @end
%% ===================================================================

-ifndef(ONEPANEL_REGISTERED_NAMES_HRL).
-define(ONEPANEL_REGISTERED_NAMES_HRL, 1).

%% Name of the application.
-define(APP_NAME, onepanel).

%% String version of applicaton name
-define(APP_STR, atom_to_list(?APP_NAME)).

%% Name of installed software application.
-define(SOFTWARE_NAME, oneprovider_node).

%% Global name of gen_server that provides CCM functionality of installed software
-define(CCM, central_cluster_manager).

%% Local name (name and node is used to identify it) of gen_server that
%% coordinates node life cycle of installed software
-define(NODE_MANAGER_NAME, node_manager).

%% Local name (name and node is used to identify it) of gen_server that
%% works as a dispatcher in installed software
-define(DISPATCHER_NAME, request_dispatcher).

%% Local name (name and node is used to identify it) of gen_server that 
%% coordinates node life cycle.
-define(ONEPANEL_SERVER, onepanel).

%% Application's supervisor name
-define(ONEPANEL_SUP, onepanel_sup).

%% Installer gen_server's name
-define(INSTALL_SERVICE, install_service).

-ifdef(provider).

%% Updater gen_server's name
-define(UPDATE_SERVICE, update_service).

-endif.

-endif.