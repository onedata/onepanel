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
-module(updater_export).
-author("Rafal Slota").
-include("spanel_modules/updater_module/common.hrl").
-include("spanel_modules/install.hrl").

%% API
-export([install_package/1]).
-export([backup_instalation/0, revert_instalation/0, move_file/1, move_all_files/0]).
-export([force_reload_module/1, soft_reload_all_modules/0, force_reload_all_modules/0]).
-export([install_views/0, refresh_view/1, install_view_sources/0, run_pre_update/1]).

%% ====================================================================
%% API functions
%% ====================================================================

install_package(#package{type = rpm, binary = Bin}) ->
    file:write_file("/tmp/veil.rpm", Bin),
    "" = os:cmd("rpm -ivh /tmp/veil.rpm --force --quiet &> /dev/null"),
    ok;
install_package(#package{type = Type}) ->
    {error, unsupported_package}.

run_pre_update(Version) ->
    dao_update:pre_update(Version),
    Modules = dao_update:pre_reload_modules(Version),
    lager:info("TEST ~p", [Modules]),
    lists:foreach(
        fun(Module) ->
            move_file(atom_to_list(Module) ++ ".beam"),
            force_reload_module(Module)
        end, Modules),
    ok.


%% ====================================================================
%% API functions
%% ====================================================================

backup_instalation() ->
    ok.

revert_instalation() ->
    ok.

move_file(File) ->
    RelPrivPath = filename:join([?VEIL_RELEASE, "lib", get_release_name()]),
    WorkerTargetDir = filename:join([?DEFAULT_NODES_INSTALL_PATH, get_node_subpath(), "lib", get_release_name()]),
    WorkerTargetDir1 = filename:join(WorkerTargetDir, "ebin"),

    file:make_dir(WorkerTargetDir),
    file:make_dir(WorkerTargetDir1),

    From = os:cmd("find " ++ RelPrivPath ++ " -name \"" ++ File  ++ "\" ") -- [10],

    lager:info("Moving file ~p to ~p", [From, WorkerTargetDir1]),

    "" = os:cmd("cp -f " ++ From ++ " " ++ WorkerTargetDir1),
    ok.


move_all_files() ->
    Targets = string:tokens( os:cmd("cd " ++ ?VEIL_RELEASE ++ "; find . -type f | grep -v vm.args | grep -v config.args | grep -v storage_info.cfg"), [10] ),
    lists:foreach(
        fun(File) ->
            Target = filename:join([?DEFAULT_NODES_INSTALL_PATH, get_node_subpath(), File]),
            TargetDir = filename:dirname(Target),
            lists:foldl(
                fun(Elem, Acc) ->
                    NewDir = filename:join(Acc, Elem),
                    file:make_dir(NewDir),
                    NewDir
                end,[], filename:split(TargetDir)),
            "" = os:cmd("cp -fr " ++ filename:join([?VEIL_RELEASE, File]) ++ " " ++ Target )
        end, Targets),
    ok.

force_reload_module(Module) ->
    ok = fix_code_path(),
    ok = purge(Module),
    lager:info("Reload: ~p", [Module]),
    code:load_file(Module),
    ok = purge(Module).


fix_code_path() ->
    Paths = string:tokens( os:cmd("find " ++ filename:join([?DEFAULT_NODES_INSTALL_PATH, get_node_subpath(), "lib"]) ++ " -name ebin -type d") ,[10]),
    NewReleasePath = filename:join([?DEFAULT_NODES_INSTALL_PATH, get_node_subpath(), "lib", get_release_name(), "ebin"]),

    code:add_paths(Paths),
    code:add_patha(NewReleasePath),

    lager:info("New code path ~p", [code:get_path()]),
    ok.

get_node_subpath() ->
    {ok, Type} = application:get_env(veil_cluster_node, node_type),
    atom_to_list(Type).

purge() ->
    Modules = [Module || {Module, _} <- code:all_loaded()],
    lists:foreach(fun(Module) -> code:purge(Module) end, Modules).

purge(Module) ->
    Modules = [Module],
    lists:foreach(fun(Module) -> code:purge(Module) end, Modules).

soft_reload_all_modules() ->
    ok = fix_code_path(),
    Modules = [Module || {Module, _} <- code:all_loaded()],
    ModMap =
        lists:map(
            fun(Mod) ->
                purge(Mod),
                code:load_file(Mod),
                {Mod, code:soft_purge(Mod)}
            end, Modules),
    {ok, ModMap}.

force_reload_all_modules() ->
    ok = fix_code_path(),
    Modules = [Module || {Module, _} <- code:all_loaded()],
    ModMap =
        lists:map(
            fun(Mod) ->
                purge(Mod),
                code:load_file(Mod),
                purge(Mod),
                {Mod, code:soft_purge(Mod)}
            end, Modules),
    {ok, ModMap}.


install_views() ->
    Struct = dao_update:get_db_structure(),
    Views = dao_update:get_all_views(),
    dao_lib:apply(update, remove_broken_views, [], 1),
    case dao_lib:apply(update, setup_views, [Struct], 1) of
        ok -> {ok, Views};
        {ok, _} -> {ok, Views};
        {error, Reason} ->
            {error, Reason}
    end.


install_view_sources() ->
    "" = os:cmd("cp -rf " ++ filename:join(?VEIL_RELEASE, "views") ++ " " ++ filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_WORKER_NAME])),
    ok.


refresh_view(View) ->
    Res = dao_lib:apply(update, update_view, [View], 1),
    lager:info("Fail: ~p", [Res]),
    Res.

get_release_name() ->
    os:cmd("basename `find " ++ filename:join(?VEIL_RELEASE, "lib") ++ " -name 'veil_cluster_node*' -type d -printf '%T@ %p\n' | sort -nr | cut -d ' ' -f 2- | head -1`") -- [10].

%% ====================================================================
%% Internal functions
%% ====================================================================
