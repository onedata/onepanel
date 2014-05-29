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
-module(updater_views).
-author("Rafal Slota").

%% API
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================


install_views(Node) ->
    rpc:call(Node, erlang, apply, [
        fun() ->
            Struct = dao_lib:apply(update, get_db_structure, [], 1),
            Views = dao_lib:apply(update, get_all_views, [], 1),
            case dao_lib:apply(update, setup_views, [Struct], 1) of
                ok -> {ok, Views};
                {ok, _} -> {ok, Views};
                {error, Reason} -> Reason
            end
        end, []]).


refresh_view(Node, View) ->
    rpc:call(Node, erlang, apply, [
        fun() ->
             dao_lib:apply(update, update_view, [View], 1)
        end, []]).

%% ====================================================================
%% Internal functions
%% ====================================================================
