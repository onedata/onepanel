%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Updater common defines.
%% @end
%% ===================================================================
-author("Rafal Slota").

-ifndef(ONEPANEL_UPDATER_COMMON_HRL).
-define(ONEPANEL_UPDATER_COMMON_HRL, 1).

%% Common version holder
-record(version, {major = 0, minor = 0, patch = 0}).

%% In-memory VeilCluster package
-record(package, {type = rpm, binary = <<>>}).

%% Types
-type updater_error() :: {{Stage :: atom(), Job :: atom(), ActionType :: install | rollback}, Object :: any(), Reason :: any()}.

-endif.