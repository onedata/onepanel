%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains storage management functions.
%% @end
%% ===================================================================
-module(installer_storage).

-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([add_ceph_storage/5, add_dio_storage/3]).
-export([check_storage_path_on_hosts/2, check_storage_path_on_host/2,
    create_storage_test_file/1, remove_storage_test_file/1]).


%% ====================================================================
%% API functions
%% ====================================================================

add_ceph_storage(Workers, StorageName, MonHost, ClusterName, PoolName) ->
    try
        Helper = onepanel_utils:dropwhile_failure(Workers, fslogic_storage,
            new_helper_init, [<<"Ceph">>, #{<<"mon_host">> => MonHost,
                <<"cluster_name">> => ClusterName, <<"pool_name">> => PoolName}],
            ?RPC_TIMEOUT),
        Storage = onepanel_utils:dropwhile_failure(Workers, fslogic_storage,
            new_storage, [StorageName, [Helper]], ?RPC_TIMEOUT),
        {ok, _} = onepanel_utils:dropwhile_failure(Workers, storage, create,
            [Storage], ?RPC_TIMEOUT),
        ok
    catch
        Error:Reason ->
            ?error_stacktrace("Cannot add Ceph storage ~p due to: ~p",
                [{StorageName, MonHost, ClusterName, PoolName}, {Error, Reason}]),
            {error, Reason}
    end.

add_dio_storage(Workers, StorageName, MountPoint) ->
    try
        Hosts = [onepanel_utils:get_host(Worker) || Worker <- Workers],
        ok = check_storage_path_on_hosts(Hosts, binary_to_list(MountPoint)),
        Helper = onepanel_utils:dropwhile_failure(Workers, fslogic_storage, new_helper_init,
            [<<"DirectIO">>, #{<<"root_path">> => MountPoint}], ?RPC_TIMEOUT),
        Storage = onepanel_utils:dropwhile_failure(Workers, fslogic_storage,
            new_storage, [StorageName, [Helper]], ?RPC_TIMEOUT),
        {ok, _} = onepanel_utils:dropwhile_failure(Workers, storage, create,
            [Storage], ?RPC_TIMEOUT),
        ok
    catch
        error:{badmatch, {error, {hosts, EHosts}}} ->
            {error, {hosts, EHosts}};
        Error:Reason ->
            ?error_stacktrace("Cannot add direct IO storage ~p due to: ~p",
                [{StorageName, MountPoint}, {Error, Reason}]),
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
    Node = onepanel_utils:get_node(Host),
    case rpc:call(Node, ?MODULE, create_storage_test_file, [Path], ?RPC_TIMEOUT) of
        {ok, FilePath, Content} ->
            try
                Answer = lists:foldl(fun
                    (H, {NewContent, ErrorHosts}) ->
                        case rpc:call(onepanel_utils:get_node(H), ?MODULE, check_storage_path_on_host, [FilePath, NewContent], ?RPC_TIMEOUT) of
                            {ok, NextContent} -> {NextContent, ErrorHosts};
                            {error, ErrorHost} ->
                                {NewContent, [ErrorHost | ErrorHosts]}
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
        NewContent = onepanel_utils:random_ascii_lowercase_sequence(?STORAGE_TEST_FILE_SIZE),
        ok = file:write(FdWrite, NewContent),
        ok = file:close(FdWrite),
        {ok, NewContent}
    catch
        _:Reason ->
            Host = onepanel_utils:get_host(node()),
            ?error("Storage ~s is not available on host ~p: ~p", [FilePath, Host, Reason]),
            {error, Host}
    end.


%% ====================================================================
%% @doc Creates storage test file. If file already exists new one is generated.
%% @end
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
    Filename = onepanel_utils:random_ascii_lowercase_sequence(8),
    FilePath = filename:join([Path, ?STORAGE_TEST_FILE_PREFIX ++ Filename]),
    try
        {ok, Fd} = file:open(FilePath, [write, exclusive]),
        Content = onepanel_utils:random_ascii_lowercase_sequence(?STORAGE_TEST_FILE_SIZE),
        ok = file:write(Fd, Content),
        ok = file:close(Fd),
        {ok, FilePath, Content}
    catch
        _:_ ->
            create_storage_test_file(Path, Attempts - 1)
    end.


%% ====================================================================
%% @doc Removes storage test file.
%% @end
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




