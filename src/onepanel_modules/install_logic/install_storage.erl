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
-module(install_storage).

-include("registered_names.hrl").
-include("onepanel_modules/install_logic.hrl").
-include("onepanel_modules/db_logic.hrl").


%% API
-export([add_storage_path/2, add_storage_path/1, remove_storage_path/2, remove_storage_path/1]).
-export([check_storage_path_on_hosts/2, check_storage_path_on_host/2, create_storage_test_file/1, remove_storage_test_file/1]).

-define(STORAGE_TEST_FILE_PREFIX, "storage_test_").
-define(STORAGE_TEST_FILE_LENGTH, 20).

%% ====================================================================
%% API functions
%% ====================================================================

%% add_storage_path/2
%% ====================================================================
%% @doc Adds configured storage path on given hosts.
%% @end
-spec add_storage_path(Hosts :: [string()], Path :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
add_storage_path(Hosts, Path) ->
    try
        StoragePaths =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{storage_paths = Paths}} -> Paths;
                _ -> throw("Cannot get configured storage paths.")
            end,

        case lists:member(Path, StoragePaths) of
            true -> throw("Path: " ++ Path ++ " is already added.");
            _ -> ok
        end,

        {HostsOk, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, add_storage_path, [Path], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{storage_paths, [Path | StoragePaths]}]) of
                    ok -> ok;
                    Other ->
                        lager:error("Cannot update storage path configuration: ~p", [Other]),
                        install_utils:apply_on_hosts(Hosts, ?MODULE, remove_storage_path, [Path], ?RPC_TIMEOUT),
                        {error, {hosts, Hosts}}
                end;
            _ ->
                lager:error("Cannot add storage path on following hosts: ~p", [HostsError]),
                install_utils:apply_on_hosts(HostsOk, ?MODULE, remove_storage_path, [Path], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:Reason ->
            lager:error("Cannot add storage path: ~p", [Reason]),
            {error, Reason}
    end.


%% remove_storage_path/2
%% ====================================================================
%% @doc Removes configured storage path on given hosts.
%% @end
-spec remove_storage_path(Hosts :: [string()], Path :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove_storage_path(Hosts, Path) ->
    try
        StoragePaths = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                           {ok, #?GLOBAL_CONFIG_RECORD{storage_paths = Paths}} -> Paths;
                           _ -> throw("Cannot get configured storage paths.")
                       end,

        case lists:member(Path, StoragePaths) of
            true -> ok;
            _ -> throw("Path: " ++ Path ++ " is not configured.")
        end,

        {HostsOk, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, remove_storage_path, [Path], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                NewStoragePaths = lists:filter(fun(StoragePath) ->
                    StoragePath =/= Path
                end, StoragePaths),
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{storage_paths, NewStoragePaths}]) of
                    ok -> ok;
                    Other ->
                        lager:error("Cannot update storage path configuration: ~p", [Other]),
                        install_utils:apply_on_hosts(Hosts, ?MODULE, add_storage_path, [Path], ?RPC_TIMEOUT),
                        {error, {hosts, Hosts}}
                end;
            _ ->
                lager:error("Cannot remove storage path on following hosts: ~p", [HostsError]),
                install_utils:apply_on_hosts(HostsOk, ?MODULE, add_storage_path, [Path], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:Reason ->
            lager:error("Cannot remove storage path: ~p", [Reason]),
            {error, Reason}
    end.


%% add_storage_path/1
%% ====================================================================
%% @doc Adds configured storage path on local host.
%% @end
-spec add_storage_path(Path :: string()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
add_storage_path(Path) ->
    Host = install_utils:get_host(node()),
    try
        lager:debug("Adding storage path ~s.", [Path]),
        StorageConfigPath = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_WORKER_NAME, ?STORAGE_CONFIG_PATH]),

        {ok, Fd} = file:open(StorageConfigPath, [append]),
        file:write(Fd, lists:flatten(io_lib:format("~p.~n", [[[{name, cluster_fuse_id}, {root, Path}]]]))),
        ok = file:close(Fd),
        {ok, Host}
    catch
        _:Reason ->
            lager:error("Cannot add storage path ~s: ~p", [Path, Reason]),
            {error, Host}
    end.


%% remove_storage_path/1
%% ====================================================================
%% @doc Removes configured storage path on local host.
%% @end
-spec remove_storage_path(Path :: string()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
remove_storage_path(Path) ->
    Host = install_utils:get_host(node()),
    try
        lager:debug("Removing storage path ~s.", [Path]),
        StorageConfigPath = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_WORKER_NAME, ?STORAGE_CONFIG_PATH]),

        {ok, StorageInfo} = file:consult(StorageConfigPath),
        NewPaths = lists:foldl(fun([[{name, cluster_fuse_id}, {root, StoragePath}]], StoragePaths) ->
            case StoragePath of
                Path -> StoragePaths;
                _ -> [StoragePath | StoragePaths]
            end
        end, [], StorageInfo),
        ok = file:delete(StorageConfigPath),
        lists:foreach(fun(NewPath) ->
            ok = add_storage_path(NewPath)
        end, NewPaths),
        {ok, Host}
    catch
        _:Reason ->
            lager:error("Cannot remove storage paths on host ~p: ~p", [install_utils:get_host(node()), Reason]),
            {error, Host}
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
    Node = install_utils:get_node(Host),
    case rpc:call(Node, ?MODULE, create_storage_test_file, [Path], ?RPC_TIMEOUT) of
        {ok, FilePath, Content} ->
            try
                Answer = lists:foldl(fun
                    (H, {NewContent, ErrorHosts}) ->
                        case rpc:call(install_utils:get_node(H), ?MODULE, check_storage_path_on_host, [FilePath, NewContent], ?RPC_TIMEOUT) of
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
                    lager:error("Cannot check storage ~p availability on hosts ~p: ~p", [Path, [Host | Hosts], Reason]),
                    rpc:call(Node, ?MODULE, remove_storage_test_file, [FilePath], ?RPC_TIMEOUT),
                    {error, Reason}
            end;
        Other ->
            lager:error("Cannot create test file for storage path ~s: ~p", [Path, Other]),
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
        NewContent = install_utils:random_ascii_lowercase_sequence(?STORAGE_TEST_FILE_LENGTH),
        ok = file:write(FdWrite, NewContent),
        ok = file:close(FdWrite),
        {ok, NewContent}
    catch
        _:Reason ->
            Host = install_utils:get_host(node()),
            lager:error("Storage ~s is not available on host ~p: ~p", [FilePath, Host, Reason]),
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
    lager:error("Cannot create storage test file: attempts limit exceeded."),
    {error, "Attempts limit exceeded."};
create_storage_test_file(Path, Attempts) ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    Filename = install_utils:random_ascii_lowercase_sequence(8),
    FilePath = filename:join([Path, ?STORAGE_TEST_FILE_PREFIX ++ Filename]),
    try
        {ok, Fd} = file:open(FilePath, [write, exclusive]),
        Content = install_utils:random_ascii_lowercase_sequence(?STORAGE_TEST_FILE_LENGTH),
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
            lager:error("Cannot remove storage test file: ~p", [Reason]),
            {error, Reason}
    end.




