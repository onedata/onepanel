%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Remote module used to execute/inject code onto oneprovider node.
%% @end
%% ===================================================================
-module(updater_export).
-author("Rafal Slota").
-include("registered_names.hrl").
-include("onepanel_modules/updater/common.hrl").
-include("onepanel_modules/updater/internals.hrl").
-include("onepanel_modules/installer/internals.hrl").

%% API
-export([install_package/1]).
-export([backup_instalation/0, revert_instalation/0, move_file/1, move_all_files/0]).
-export([force_reload_module/1, soft_reload_all_modules/0, force_reload_modules/2]).
-export([install_views/0, refresh_view/1, install_view_sources/0, run_pre_update/1, remove_outdated_views/0]).
-export([runner/3]).


%% ====================================================================
%% API functions
%% ====================================================================


%% runner/3
%% ====================================================================
%% @doc This function is meant to be used for newly spawned processes,
%%      as wrapper that executes Fun from this module and sends result back to given pid - RespondTo.
%% @end
-spec runner(RespondTo :: pid(), Fun :: atom(), Args :: list()) -> {pid(), any()}.
%% ====================================================================
runner(RespondTo, Fun, Args) ->
    Response =
        try apply(?MODULE, Fun, Args) of
            ok -> ok;
            {ok, Data} -> {ok, Data};
            {error, Reason} -> {error, Reason};
            Other -> {error, Other}
        catch
            Type:Error ->
                {error, {Type, Error, erlang:get_stacktrace()}}
        end,
    RespondTo ! {self(), Response}.


%% install_package/1
%% ====================================================================
%% @doc Installs given package in OS (currently only RPM packages are supported).
%% @end
-spec install_package(Package :: #package{}) -> ok | {error, any()}.
%% ====================================================================
install_package(#package{type = rpm, binary = Bin}) ->
    file:write_file("/tmp/oneprovider.rpm", Bin),
    case os:cmd("rpm -i /tmp/oneprovider.rpm --force --quiet") of
        "" -> ok;
        Reason -> {error, {stdout, Reason}}
    end;
install_package(#package{type = _Type}) ->
    {error, unsupported_package}.


%% run_pre_update/1
%% ====================================================================
%% @doc Runs dao_update:pre_update function and reloads modules that DAO wants to be reloaded.
%% @end
-spec run_pre_update(Version :: #version{}) -> ok | {error, any()}.
%% ====================================================================
run_pre_update(Version) ->
    PreloadRes = dao_update:pre_update(Version),
    Modules = dao_update:pre_reload_modules(Version),
    lists:foreach(
        fun(Module) ->
            move_file(atom_to_list(Module) ++ ".beam"),
            force_reload_module(Module)
        end, Modules),
    PreloadRes.


%% backup_instalation/0
%% ====================================================================
%% @doc Backups current node installation (files only).
%% @end
-spec backup_instalation() -> ok | {error, any()}.
%% ====================================================================
backup_instalation() ->
    NodeRoot = filename:join([?NODES_INSTALL_PATH, get_node_subpath()]),
    case os:cmd("cp -rf " ++ NodeRoot ++ " " ++ NodeRoot ++ ".bak") of
        "" -> ok;
        Reason -> {error, {stdout, Reason}}
    end.


%% revert_instalation/0
%% ====================================================================
%% @doc Reverts current node installation (files only) from backup created with revert_instalation/0.
%% @end
-spec revert_instalation() -> ok | {error, any()}.
%% ====================================================================
revert_instalation() ->
    NodeRoot = filename:join([?NODES_INSTALL_PATH, get_node_subpath()]),
    case os:cmd("cp -rf " ++ NodeRoot ++ ".bak" ++ " " ++ NodeRoot) of
        "" -> ok;
        Reason -> {error, Reason}
    end.


%% move_file/1
%% ====================================================================
%% @doc Copies file (given filename) from rpm install path to node install path.
%% @end
-spec move_file(File :: string()) -> ok | {error, any()}.
%% ====================================================================
move_file(File) ->
    {ok, ReleasePath} = application:get_env(?APP_NAME, application_release_path),
    RelPrivPath = filename:join([ReleasePath, "lib"]),
    WorkerTargetDir = filename:join([?NODES_INSTALL_PATH, get_node_subpath(), "lib"]),

    From = os:cmd("find " ++ RelPrivPath ++ " -name \"" ++ File ++ "\" | head -1") -- [10],

    case From of
        "" -> ignore;
        _ ->
            From1 = filename:split(From),
            [_ | [Rel | _]] = lists:dropwhile(fun(Elem) -> Elem =/= "lib" end, From1),

            WorkerTargetDir1 = filename:join([WorkerTargetDir, Rel]),
            WorkerTargetDir2 = filename:join([WorkerTargetDir1, "ebin"]),
            file:make_dir(WorkerTargetDir1),
            file:make_dir(WorkerTargetDir2),

            "" = os:cmd("cp -f " ++ From ++ " " ++ WorkerTargetDir2)
    end,
    ok.


%% move_all_files/0
%% ====================================================================
%% @doc Same as move_file/1 only that works for all files with exception for configuration files.
%% @end
-spec move_all_files() -> ok | {error, any()}.
%% ====================================================================
move_all_files() ->
    {ok, ReleasePath} = application:get_env(?APP_NAME, application_release_path),
    Targets = string:tokens(os:cmd("cd " ++ ReleasePath ++ "; find . -type f | grep -v sys.config | grep -v vm.args | grep -v config.args | grep -v storage_info.cfg"), [10]),
    IsRebootRequired =
        lists:foldl(
            fun(File, RebootRequired) ->
                Target = filename:join([?NODES_INSTALL_PATH, get_node_subpath(), File]),
                TargetDir = filename:dirname(Target),
                lists:foldl(
                    fun(Elem, Acc) ->
                        NewDir = filename:join(Acc, Elem),
                        file:make_dir(NewDir),
                        NewDir
                    end, [], filename:split(TargetDir)),
                Source = filename:join([ReleasePath, File]),
                {ok, SourceBin} = file:read_file(Source),
                TargetBin =
                    case file:read_file(Source) of
                        {ok, Bin} -> Bin;
                        _ -> <<>>
                    end,
                SourceMD5 = crypto:hash(md5, SourceBin),
                TargetMD5 = crypto:hash(md5, TargetBin),

                case SourceMD5 =:= TargetMD5 of
                    true ->
                        false;
                    false ->
                        "" = os:cmd("cp -fr " ++ Source ++ " " ++ Target),
                        RebootRequired orelse is_reboot_only_lib(Target)
                end
            end, false, Targets),
    {ok, IsRebootRequired}.


%% force_reload_module/1
%% ====================================================================
%% @doc Force reloads given module (full code purge and reload).
%% @end
-spec force_reload_module(Module :: atom()) -> ok | {error, any()}.
%% ====================================================================
force_reload_module(Module) ->
    ok = fix_code_path(),
    code:purge(Module),
    code:load_file(Module),
    code:purge(Module),
    ok.


%% fix_code_path/0
%% ====================================================================
%% @doc Rewrites code path using new release numbers.
%% @end
-spec fix_code_path() -> ok | {error, any()}.
%% ====================================================================
fix_code_path() ->
    Paths = string:tokens(os:cmd("find " ++ filename:join([?NODES_INSTALL_PATH, get_node_subpath(), "lib"]) ++ " -name ebin -type d | sort -bdfr"), [10]),
    NewReleasePath = filename:join([?NODES_INSTALL_PATH, get_node_subpath(), "lib", get_release_name(), "ebin"]),

    code:add_paths(Paths),
    code:add_patha(NewReleasePath),

    ok.


%% soft_reload_all_modules/0
%% ====================================================================
%% @doc Softly reloads all modules and returns tuple list that says which module needs force realod.
%% @end
-spec soft_reload_all_modules() -> {ok, [{Module :: atom(), IsReloaded :: boolean()}]} | {error, any()}.
%% ====================================================================
soft_reload_all_modules() ->
    ok = fix_code_path(),
    Modules = [Module || {Module, _} <- get_all_loaded(), Module =/= crypto, Module =/= asn1rt_nif],

    lists:foreach(fun(Mod) -> code:purge(Mod) end, Modules),
    lists:foreach(fun(Mod) -> code:load_file(Mod) end, Modules),

    ModMap =
        lists:map(
            fun(Mod) ->
                {Mod, code:soft_purge(Mod)}
            end, Modules),

    {ok, ModMap}.


%% force_reload_modules/2
%% ====================================================================
%% @doc Hardly reloads given modules and returns tuple list that says which module triggered process kill.
%%      WaitFor specifies delay before executing this function.
%% @end
-spec force_reload_modules(Modules :: [atom()], WaitFor :: non_neg_integer()) -> {ok, [{Module :: atom(), WasKilled :: boolean()}]} | {error, any()}.
%% ====================================================================
force_reload_modules(Modules, WaitFor) ->
    timer:sleep(WaitFor),
    ok = fix_code_path(),

    lists:foreach(fun(Mod) -> code:load_file(Mod) end, Modules),

    ModMap =
        lists:map(
            fun(Mod) ->
                {Mod, code:purge(Mod)}
            end, Modules),
    {ok, ModMap}.


%% install_views/2
%% ====================================================================
%% @doc Install/updates views code in DB (based on code from files).
%% @end
-spec install_views() -> ok | {error, any()}.
%% ====================================================================
install_views() ->
    Struct = dao_update:get_db_structure(),
    Views = dao_update:get_all_views(),
    case dao_lib:apply(update, setup_views, [Struct], 1) of
        ok -> {ok, Views};
        {ok, _} -> {ok, Views};
        {error, Reason} ->
            {error, Reason}
    end.


%% get_all_loaded/0
%% ====================================================================
%% @doc Returns all loaded modules without modules that need node restart after reload.
%% @end
-spec get_all_loaded() -> [Mod :: atom()].
%% ====================================================================
get_all_loaded() ->
    lists:foldl(
        fun({_Module, Path}, Acc) when is_atom(Path) ->
            Acc;
            ({Module, Path}, Acc) ->
                case is_reboot_only_lib(Path) of
                    false -> [{Module, Path} | Acc];
                    true -> Acc
                end
        end, [], code:all_loaded()).


%% install_view_sources/0
%% ====================================================================
%% @doc Installs new view sources (rpm -> node install dir)
%% @end
-spec install_view_sources() -> ok | {error, any()}.
%% ====================================================================
install_view_sources() ->
    {ok, ReleasePath} = application:get_env(?APP_NAME, application_release_path),
    case os:cmd("cp -rf " ++ filename:join(ReleasePath, "views") ++ " " ++ filename:join([?NODES_INSTALL_PATH, ?WORKER_NAME])) of
        "" -> ok;
        Reason -> {error, {stdout, Reason}}
    end.


%% remove_outdated_views/0
%% ====================================================================
%% @doc Removes outdated views.
%% @end
-spec remove_outdated_views() -> ok | {error, any()}.
%% ====================================================================
remove_outdated_views() ->
    dao_lib:apply(update, remove_outdated_views, [], 1).


%% refresh_view/1
%% ====================================================================
%% @doc Refreshes view index. 'View' type shall match the one currently used by DAO.
%% @end
-spec refresh_view(View :: tuple()) -> ok | {error, any()}.
%% ====================================================================
refresh_view(View) ->
    dao_lib:apply(update, update_view, [View], 1).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% get_release_name/0
%% ====================================================================
%% @doc Returns oneprovider_node's release lib directory name.
%% @end
-spec get_release_name() -> ReleaseName :: string().
%% ====================================================================
get_release_name() ->
    {ok, ReleasePath} = application:get_env(?APP_NAME, application_release_path),
    os:cmd("basename `find " ++ filename:join(ReleasePath, "lib") ++ " -name 'oneprovider_node*' -type d -printf '%T@ %p\n' | sort -nr | cut -d ' ' -f 2- | head -1`") -- [10].


%% is_reboot_only_lib/1
%% ====================================================================
%% @doc For given module's file name, checks if the module requires node restart in order to reload.
%% @end
-spec is_reboot_only_lib(FileName :: string()) -> boolean().
%% ====================================================================
is_reboot_only_lib(FilePath) ->
    lists:foldl(fun(Package, Acc) -> Acc + string:str(FilePath, Package) end, 0, ?REBOOT_ONLY_MODULES) =/= 0.


%% get_node_subpath/0
%% ====================================================================
%% @doc Returns node root directory (relative to node install path).
%% @end
-spec get_node_subpath() -> ok | {error, any()}.
%% ====================================================================
get_node_subpath() ->
    {ok, Type} = application:get_env(oneprovider_node, node_type),
    atom_to_list(Type).