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
-export([backup_instalation/1, revert_instalation/1, move_file/2, move_all_files/1]).

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
    rpc:call(Node, erlang, apply, [
        fun() ->
            From = os:cmd("find " ++ ?VEIL_RELEASE ++ " -name \"" ++ File  ++ "\"") -- [10],
            Targets = string:tokens( os:cmd("find " ++ ?DEFAULT_NODES_INSTALL_PATH ++ " -name \"" ++ File  ++ "\""), [10]),
            lists:foreach(
                fun(Target) ->
                    lager:info("[Node ~p] Coping file ~p to ~p", [Node, From, Target]),
                    "" = os:cmd("cp -f " ++ From ++ " " ++ Target)
                end, Targets)
        end, []]).


move_all_files(Node) ->
    ok.


%% ====================================================================
%% Internal functions
%% ====================================================================
