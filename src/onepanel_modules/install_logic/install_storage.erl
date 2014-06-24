%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains storage installation functions.
%% @end
%% ===================================================================
-module(install_storage).
-behaviour(install_behaviour).

-include("registered_names.hrl").
-include("onepanel_modules/install_logic.hrl").
-include("onepanel_modules/db_logic.hrl").

% install_behaviour callbacks
-export([install/2, uninstall/2, start/2, stop/2, restart/2]).

%% API
-export([install/1, uninstall/1, create_storage_test_file/1, delete_storage_test_file/1, check_storage_on_host/2]).

-define(STORAGE_TEST_FILE_PREFIX, "storage_test_").
-define(STORAGE_TEST_FILE_LENGTH, 20).

%% ====================================================================
%% Behaviour callback functions
%% ====================================================================

%% install/2
%% ====================================================================
%% @doc Adds configured storage paths on hosts.
%% @end
-spec install(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
install(Hosts, Args) ->
    try
        Paths = proplists:get_value(paths, Args),

        case Paths of
            undefined -> throw("Storage paths not found in arguments list.");
            _ when is_list(Paths) -> ok;
            _ -> throw("Storage paths should be a list.")
        end,

        InstalledStoragePaths = case dao:get_record(?CONFIG_TABLE, ?CONFIG_ID) of
                                    {ok, #?CONFIG_TABLE{storage_paths = StoragePaths}} -> StoragePaths;
                                    Other ->
                                        lager:error("Cannot get configured storage paths: ~p", [Other]),
                                        throw("Cannot get configured storage paths.")
                                end,

        lists:foreach(fun(Path) ->
            case lists:member(Path, InstalledStoragePaths) of
                true -> throw("Path: " ++ Path ++ " is already installed.");
                _ -> ok
            end
        end, Paths),

        ok = check_storage_on_hosts(Hosts, Paths),

        {_, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, install, [Paths], ?RPC_TIMEOUT),

        case HostsError of
            [] -> ok;
            _ -> throw(HostsError)
        end,

        case dao:update_record(?CONFIG_TABLE, ?CONFIG_ID, [{storage_paths, InstalledStoragePaths ++ Paths}]) of
            ok -> ok;
            UpdateError ->
                lager:error("Cannot update storage paths configuration: ~p", UpdateError),
                rpc:multicall(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),
                {error, Hosts}
        end
    catch
        _:Reason ->
            lager:error("Cannot add storage paths: ~p", [Reason]),
            {error, Reason}
    end.


%% uninstall/2
%% ====================================================================
%% @doc Removes configured storage paths on hosts.
%% @end
-spec uninstall(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
uninstall(Hosts, Args) ->
    try
        Paths = proplists:get_value(paths, Args),

        case Paths of
            undefined -> throw("Storage paths not found in arguments list.");
            _ when is_list(Paths) -> ok;
            _ -> throw("Storage paths should be a list.")
        end,

        InstalledStoragePaths = case dao:get_record(?CONFIG_TABLE, ?CONFIG_ID) of
                                    {ok, #?CONFIG_TABLE{storage_paths = StoragePaths}} -> StoragePaths;
                                    Other ->
                                        lager:error("Cannot get configured storage paths: ~p", [Other]),
                                        throw("Cannot get configured storage paths.")
                                end,

        lists:foreach(fun(Path) ->
            case lists:member(Path, InstalledStoragePaths) of
                true -> ok;
                _ -> throw("Path: " ++ Path ++ " is not addded.")
            end
        end, Paths),

        {_, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, uninstall, [Paths], ?RPC_TIMEOUT),

        case HostsError of
            [] -> ok;
            _ ->
                lager:error("Cannot remove storage paths on following hosts: ~p", [HostsError]),
                throw(HostsError)
        end,

        NewStoragePaths = lists:filter(fun(StoragePath) ->
            not lists:member(StoragePath, Paths)
        end, InstalledStoragePaths),

        case dao:update_record(?CONFIG_TABLE, ?CONFIG_ID, [{storage_paths, NewStoragePaths}]) of
            ok -> ok;
            UpdateError ->
                lager:error("Cannot update storage paths configuration: ~p", UpdateError),
                rpc:multicall(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),
                {error, Hosts}
        end
    catch
        _:Reason ->
            lager:error("Cannot remove storage paths: ~p", [Reason]),
            {error, Reason}
    end.


%% start/2
%% ====================================================================
%% @doc install_behaviour not applicable for this module.
%% Returns ok for any arguments.
%% @end
-spec start(Hosts :: [string()], Args) -> Result when
    Result :: ok,
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
start(_, _) ->
    ok.


%% stop/2
%% ====================================================================
%% @doc install_behaviour not applicable for this module.
%% Returns ok for any arguments.
%% @end
-spec stop(Hosts :: [string()], Args) -> Result when
    Result :: ok,
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
stop(_, _) ->
    ok.


%% restart/2
%% ====================================================================
%% @doc install_behaviour not applicable for this module.
%% Returns ok for any arguments.
%% @end
-spec restart(Hosts :: [string()], Args) -> Result when
    Result :: ok,
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
restart(_, _) ->
    ok.


%% ====================================================================
%% API functions
%% ====================================================================

%% install/1
%% ====================================================================
%% @doc Adds configured storage paths on host.
%% @end
-spec install(Paths :: [string()]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(Paths) ->
    lager:info("Adding storage paths..."),
    StorageConfig = ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/" ++ ?STORAGE_CONFIG_PATH,
    try
        {ok, Fd} = file:open(StorageConfig, [append]),
        lists:foreach(fun(Path) ->
            file:write(Fd, "[[{name, cluster_fuse_id}, {root, \"" ++ Path ++ "\"}]].\n")
        end, Paths),
        ok = file:close(Fd),
        ok
    catch
        _:Reason ->
            lager:error("Cannot add storage paths on host ~p: ~p", [install_utils:get_host(node()), Reason]),
            {error, Reason}
    end.


%% uninstall/1
%% ====================================================================
%% @doc Removes configured storage paths on host.
%% @end
-spec uninstall(Paths :: [string()]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
uninstall(Paths) ->
    lager:info("Removing storage paths..."),
    StorageConfig = ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/" ++ ?STORAGE_CONFIG_PATH,
    try
        {ok, StorageInfo} = file:consult(StorageConfig),
        NewPaths = lists:foldl(fun([[{name, cluster_fuse_id}, {root, Path}]], Acc) ->
            case lists:member(Path, Paths) of
                true -> Acc;
                false -> [Path | Acc]
            end
        end, [], StorageInfo),
        ok = file:delete(StorageConfig),
        ok = install(NewPaths),
        ok
    catch
        _:Reason ->
            lager:error("Cannot remove storage paths on host ~p: ~p", [install_utils:get_host(node()), Reason]),
            {error, Reason}
    end.


%% check_storage_on_host/2
%% ====================================================================
%% @doc Checks storage availability on node.
%% @end
-spec check_storage_on_host(FilePath :: string(), Content :: string()) -> Result when
    Result :: {ok, NewContent :: string()} | {error, Host :: string()}.
%% ====================================================================
check_storage_on_host(FilePath, Content) ->
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
%% @doc Creates new path for storage test file.
%% If path already exists new one is generated.
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
    FilePath = Path ++ "/" ++ ?STORAGE_TEST_FILE_PREFIX ++ Filename,
    try
        {ok, Fd} = file:open(FilePath, [write, exclusive]),
        Content = install_utils:random_ascii_lowercase_sequence(?STORAGE_TEST_FILE_LENGTH),
        ok = file:write(Fd, Content),
        ok = file:close(Fd),
        {ok, FilePath, Content}
    catch
        _:_ -> create_storage_test_file(Path, Attempts - 1)
    end.


%% ====================================================================
%% @doc Deletes storage test file.
-spec delete_storage_test_file(FilePath :: string()) -> Result when
    Result :: ok | {error, Error :: term()}.
%% ====================================================================
delete_storage_test_file(FilePath) ->
    case file:delete(FilePath) of
        ok -> ok;
        {error, Reason} ->
            lager:error("Cannot delete storage test file: ~p", [Reason]),
            {error, Reason}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% check_storage_on_hosts/1
%% ====================================================================
%% @doc Checks storage availability on hosts. Returns ok or first host for which
%% storage is not available.
%% @end
-spec check_storage_on_hosts(Hosts :: [string()], Path :: string()) -> Result when
    Result :: ok | {error, ErrorHosts :: string()}.
%% ====================================================================
check_storage_on_hosts([], _) ->
    ok;
check_storage_on_hosts([Host | Hosts], Path) ->
    Node = install_utils:get_node(Host),
    case rpc:call(Node, ?MODULE, create_storage_test_file, [Path], ?RPC_TIMEOUT) of
        {ok, FilePath, Content} ->
            try
                Answer = lists:foldl(fun
                    (H, {NewContent, ErrorHosts}) ->
                        case rpc:call(install_utils:get_node(H), ?MODULE, check_storage_on_host, [FilePath, NewContent], ?RPC_TIMEOUT) of
                            {ok, NextContent} -> {NextContent, ErrorHosts};
                            {error, ErrorHost} -> {NewContent, [ErrorHost | ErrorHosts]}
                        end
                end, {Content, []}, [Host | Hosts]),
                rpc:call(Node, ?MODULE, delete_storage_test_file, [FilePath], ?RPC_TIMEOUT),
                case Answer of
                    {_, []} -> ok;
                    {_, EHosts} -> {error, {not_available, EHosts}}
                end
            catch
                _:Reason ->
                    lager:error("Cannot check storage ~p availability on hosts ~p: ~p", [Path, [Host | Hosts], Reason]),
                    rpc:call(Node, ?MODULE, delete_storage_test_file, [FilePath], ?RPC_TIMEOUT),
                    error
            end;
        Other -> Other
    end.





