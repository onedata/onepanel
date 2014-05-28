%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Write me !
%% @end
%% ===================================================================
-module(updater_files).
-author("Rafal Slota").

-include("spanel_modules/install.hrl").

%% API
-export([backup_instalation/1, revert_instalation/1, move_file/2]).

%% ====================================================================
%% API functions
%% ====================================================================

backup_instalation(Node) ->
    ok.

revert_instalation(Node) ->
    ok.

move_file(Nodes, File) when is_list(Nodes) ->
    lists:map(fun(Node) -> move_file(Node, File) end, Nodes);
move_file(Node, File) ->
    ok.


%% ====================================================================
%% Internal functions
%% ====================================================================
