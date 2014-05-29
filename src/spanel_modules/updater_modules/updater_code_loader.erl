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
-module(updater_code_loader).
-author("Rafal Slota").

%% API
-export([force_load/2, soft_load_all/1]).

%% ====================================================================
%% API functions
%% ====================================================================

force_load(Node, Module) ->
    ok = purge(Node, Module),
    Ret =
        rpc:call(Node, erlang, apply, [
            fun() ->
                code:load_file(Module)
            end, []]),
    ok = purge(Node, Module),
    case Ret of
        {error, Reason} -> {error, Reason};
        _ -> ok
    end.

purge(Node) ->
    rpc:call(Node, erlang, apply, [
        fun() ->
            Modules = [Module || {Module, _} <- code:all_loaded()],
            lists:foreach(fun(Module) -> code:purge(Module) end, Modules)
        end, []]).

purge(Node, Module) ->
    rpc:call(Node, erlang, apply, [
        fun() ->
            Modules = [Module],
            lists:foreach(fun(Module) -> code:purge(Module) end, Modules)
        end, []]).

soft_load_all(Node) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
