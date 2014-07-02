%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains definitions of names used to
%% identify different parts of application (or whole application).
%% @end
%% ===================================================================

-ifndef(REGISTERED_NAMES_HRL).
-define(REGISTERED_NAMES_HRL, 1).

%% Name of the application.
-define(APP_NAME, onepanel).

%% String version of applicaton name
-define(APP_STR, atom_to_list(?APP_NAME)).

%% Local name (name and node is used to identify it) of gen_server that 
%% coordinates node life cycle.
-define(GEN_SERVER_NAME, onepanel).

-endif.