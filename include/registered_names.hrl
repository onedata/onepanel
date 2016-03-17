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

-include_lib("ctool/include/global_definitions.hrl").

%% Name of the application.
-define(APP_NAME, onepanel).

%% String version of applicaton name
-define(APP_STR, atom_to_list(?APP_NAME)).

%% Local name (name and node is used to identify it) of gen_server that 
%% coordinates node life cycle.
-define(ONEPANEL_SERVER, onepanel).

%% Application's supervisor name
-define(ONEPANEL_SUP, onepanel_sup).

%% Installer gen_server's name
-define(INSTALL_SERVICE, install_service).

%% Cm application name
-define(CM_APP_NAME, cluster_manager).

-ifdef(oneprovider).

%% Name of installed software application.
-define(SOFTWARE_NAME, op_worker).

-endif.

-ifdef(onezone).

%% Name of installed software application.
-define(SOFTWARE_NAME, oz_worker).

-endif.


-endif.