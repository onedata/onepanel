%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains storage management functions.
%% @end
%% ===================================================================
-module(installer_storage).

-include("onepanel_modules/db/common.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([add_storage_paths_to_db/1, remove_storage_paths_from_db/1, add_storage_paths_on_host/1, remove_storage_paths_on_host/1]).
-export([check_storage_path_on_hosts/2, check_storage_path_on_host/2, create_storage_test_file/1, remove_storage_test_file/1]).


%% ====================================================================
%% API functions
%% ====================================================================

%% add_storage_paths_to_db/1
%% ====================================================================
%% @doc Adds storage paths to database.
%% @end
-spec add_storage_paths_to_db(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
add_storage_paths_to_db(Args) ->
    try
        Paths = case proplists:get_value(storage_paths, Args, []) of
                    [] -> throw(nothing_to_add);
                    PathsToAdd -> PathsToAdd
                end,

        ConfiguredStoragePaths =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{storage_paths = StoragePaths}} -> StoragePaths;
                _ -> throw("Cannot get configured storage paths")
            end,

        lists:foreach(fun(Path) ->
            case lists:member(Path, ConfiguredStoragePaths) of
                true -> throw("Path: " ++ Path ++ " is already added");
                _ -> ok
            end
        end, Paths),

        case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{storage_paths, ConfiguredStoragePaths ++ Paths}]) of
            ok -> ok;
            Other ->
                ?error("Cannot update storage path configuration: ~p", [Other]),
                {error, Other}
        end
    catch
        _:nothing_to_add -> ok;
        _:Reason ->
            ?error("Cannot add storage path: ~p", [Reason]),
            {error, Reason}
    end.


%% remove_storage_paths_from_db/1
%% ====================================================================
%% @doc Removes storage paths from database.
%% @end
-spec remove_storage_paths_from_db(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove_storage_paths_from_db(Args) ->
    try
        Paths = case proplists:get_value(storage_paths, Args, []) of
                    [] -> throw(nothing_to_remove);
                    PathsToRemove -> PathsToRemove
                end,

        ConfiguredStoragePaths =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{storage_paths = StoragePaths}} -> StoragePaths;
                _ -> throw("Cannot get configured storage paths")
            end,

        lists:foreach(fun(Path) ->
            case lists:member(Path, ConfiguredStoragePaths) of
                false -> throw("Path: " ++ Path ++ " is not added");
                _ -> ok
            end
        end, Paths),

        case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{storage_paths, ConfiguredStoragePaths -- Paths}]) of
            ok -> ok;
            Other ->
                ?error("Cannot update storage path configuration: ~p", [Other]),
                {error, Other}
        end
    catch
        _:nothing_to_remove -> ok;
        _:Reason ->
            ?error("Cannot remove storage path: ~p", [Reason]),
            {error, Reason}
    end.


%% add_storage_paths_on_host/1
%% ====================================================================
%% @doc Adds storage paths on local host.
%% @end
-spec add_storage_paths_on_host(Paths :: [string()]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
add_storage_paths_on_host(Paths) ->
    try
        ?debug("Adding storage paths ~p", [Paths]),
        StorageConfigPath = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_WORKER_NAME, ?STORAGE_CONFIG_PATH]),

        {ok, Fd} = file:open(StorageConfigPath, [append]),
        lists:foreach(fun(Path) ->
            file:write(Fd, lists:flatten(io_lib:format("~p.~n", [[[{name, cluster_fuse_id}, {root, Path}]]])))
        end, Paths),
        ok = file:close(Fd),

        ok
    catch
        _:Reason ->
            ?error("Cannot add storage paths ~p: ~p", [Paths, Reason]),
            {error, Reason}
    end.


%% remove_storage_paths_on_host/1
%% ====================================================================
%% @doc Removes configured storage path on local host.
%% @end
-spec remove_storage_paths_on_host(Paths :: [string()]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove_storage_paths_on_host(Paths) ->
    try
        ?debug("Removing storage path ~p", [Paths]),
        StorageConfigPath = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_WORKER_NAME, ?STORAGE_CONFIG_PATH]),

        {ok, StorageInfo} = file:consult(StorageConfigPath),

        NewPaths = lists:foldl(fun([[{name, cluster_fuse_id}, {root, Path}]], Acc) ->
            case lists:member(Path, Paths) of
                true -> Acc;
                _ -> [Path | Acc]
            end
        end, [], StorageInfo),

        ok = file:delete(StorageConfigPath),
        ok = add_storage_paths_on_host(NewPaths)
    catch
        _:Reason ->
            ?error("Cannot remove storage paths ~p: ~p", [Paths, Reason]),
            {error, Reason}
    end.


%% check_storage_path_on_hosts/1
%% ====================================================================
%% @doc Checks storage availability on hosts. Returns ok or first host for which
%% storage is not available.
%% @end
-spec check_storage_path_on_hosts(Hosts :: [string()], Path :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
check_storage_path_on_hosts([], _) ->
    ok;
check_storage_path_on_hosts([Host | Hosts], Path) ->
    Node = installer_utils:get_node(Host),
    case rpc:call(Node, ?MODULE, create_storage_test_file, [Path], ?RPC_TIMEOUT) of
        {ok, FilePath, Content} ->
            try
                Answer = lists:foldl(fun
                    (H, {NewContent, ErrorHosts}) ->
                        case rpc:call(installer_utils:get_node(H), ?MODULE, check_storage_path_on_host, [FilePath, NewContent], ?RPC_TIMEOUT) of
                            {ok, NextContent} -> {NextContent, ErrorHosts};
                            {error, ErrorHost} -> {NewContent, [ErrorHost | ErrorHosts]}
                        end
                end, {Content, []}, [Host | Hosts]),
                rpc:call(Node, ?MODULE, remove_storage_test_file, [FilePath], ?RPC_TIMEOUT),
                case Answer of
                    {_, []} -> ok;
                    {_, EHosts} -> {error, {hosts, EHosts}}
                end
            catch
                _:Reason ->
                    ?error("Cannot check storage ~p availability on hosts ~p: ~p", [Path, [Host | Hosts], Reason]),
                    rpc:call(Node, ?MODULE, remove_storage_test_file, [FilePath], ?RPC_TIMEOUT),
                    {error, Reason}
            end;
        Other ->
            ?error("Cannot create test file for storage path ~s: ~p", [Path, Other]),
            {error, {hosts, [Host]}}
    end.


%% check_storage_path_on_host/2
%% ====================================================================
%% @doc Checks storage availability on node.
%% @end
-spec check_storage_path_on_host(FilePath :: string(), Content :: string()) -> Result when
    Result :: {ok, NewContent :: string()} | {error, Host :: string()}.
%% ====================================================================
check_storage_path_on_host(FilePath, Content) ->
    try
        {ok, FdRead} = file:open(FilePath, [read]),
        {ok, Content} = file:read_line(FdRead),
        ok = file:close(FdRead),
        {ok, FdWrite} = file:open(FilePath, [write]),
        NewContent = installer_utils:random_ascii_lowercase_sequence(?STORAGE_TEST_FILE_SIZE),
        ok = file:write(FdWrite, NewContent),
        ok = file:close(FdWrite),
        {ok, NewContent}
    catch
        _:Reason ->
            Host = installer_utils:get_host(node()),
            ?error("Storage ~s is not available on host ~p: ~p", [FilePath, Host, Reason]),
            {error, Host}
    end.


%% ====================================================================
%% @doc Creates storage test file. If file already exists new one is generated.
-spec create_storage_test_file(Path :: string()) -> Result when
    Result :: {ok, FilePath :: string(), Content :: string} | {error, Reason :: term()}.
%% ====================================================================
create_storage_test_file(Path) ->
    create_storage_test_file(Path, 20).

create_storage_test_file(_, 0) ->
    ?error("Cannot create storage test file: attempts limit exceeded"),
    {error, "Attempts limit exceeded"};
create_storage_test_file(Path, Attempts) ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    Filename = installer_utils:random_ascii_lowercase_sequence(8),
    FilePath = filename:join([Path, ?STORAGE_TEST_FILE_PREFIX ++ Filename]),
    try
        {ok, Fd} = file:open(FilePath, [write, exclusive]),
        Content = installer_utils:random_ascii_lowercase_sequence(?STORAGE_TEST_FILE_SIZE),
        ok = file:write(Fd, Content),
        ok = file:close(Fd),
        {ok, FilePath, Content}
    catch
        _:_ ->
            create_storage_test_file(Path, Attempts - 1)
    end.


%% ====================================================================
%% @doc Removes storage test file.
-spec remove_storage_test_file(FilePath :: string()) -> Result when
    Result :: ok | {error, Error :: term()}.
%% ====================================================================
remove_storage_test_file(FilePath) ->
    case file:delete(FilePath) of
        ok -> ok;
        {error, Reason} ->
            ?error("Cannot remove storage test file: ~p", [Reason]),
            {error, Reason}
    end.




