%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains storage installation functions
%% @end
%% ===================================================================
-module(install_storage).

-include("registered_names.hrl").
-include("spanel_modules/install.hrl").
-include("spanel_modules/db.hrl").

%% API
-export([create_storage_test_file/1, delete_storage_test_file/1, check_storage_on_host/2, check_storage_on_hosts/1]).
-export([add_storage_paths/1, remove_storage_paths/1, add_storage_paths_on_hosts/1]).

-define(STORAGE_TEST_FILE_PREFIX, "storage_test_").
-define(STORAGE_TEST_FILE_LENGTH, 20).

%% ====================================================================
%% @doc Creates new path for storage test file. If path already exists new one is generated.
-spec create_storage_test_file(Path :: string()) -> {ok, FilePath :: string(), Content :: string} | error.
%% ====================================================================
create_storage_test_file(Path) ->
  create_storage_test_file(Path, 20).

create_storage_test_file(_, 0) ->
  error;
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
-spec delete_storage_test_file(FilePath :: string()) -> ok | {error, Error :: term()}.
%% ====================================================================
delete_storage_test_file(FilePath) ->
  case file:delete(FilePath) of
    ok -> ok;
    {error, Error} ->
      lager:error("Error while deleting storage test file: ~p", [Error]),
      {error, Error}
  end.

%% check_storage_on_hosts/1
%% ====================================================================
%% @doc Checks storage availability on all nodes
%% @end
-spec check_storage_on_hosts(Path :: string()) -> ok | error.
%% ====================================================================
check_storage_on_hosts(Path) ->
  case create_storage_test_file(Path) of
    {ok, FilePath, Content} ->
      try
        {ok, NextContent} = check_storage_on_host(FilePath, Content),
        {ok, _} = lists:foldl(fun
          (Node, {ok, NewContent}) ->
            gen_server:call({?SPANEL_NAME, Node}, {check_storage, FilePath, NewContent}, ?GEN_SERVER_TIMEOUT);
          (_, Other) -> Other
        end, {ok, NextContent}, nodes(hidden)),
        delete_storage_test_file(FilePath),
        ok
      catch
        _:_ ->
          delete_storage_test_file(FilePath),
          error
      end;
    _ -> error
  end.

%% check_storage_on_host/2
%% ====================================================================
%% @doc Checks storage availability on node
%% @end
-spec check_storage_on_host(FilePath :: string(), Content :: string()) -> {ok, NewContent :: string()} | error.
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
    _:_ -> error
  end.

%% add_storage_paths/1
%% ====================================================================
%% @doc Removes storage configuration on host
%% @end
-spec remove_storage_paths(Paths :: [string()]) -> ok | error.
%% ====================================================================
remove_storage_paths(Paths) ->
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
    ok = add_storage_paths(NewPaths),
    ok
  catch
    _:_ -> error
  end.

%% add_storage_paths/1
%% ====================================================================
%% @doc Adds storage configuration on host
%% @end
-spec add_storage_paths(Paths :: [string()]) -> ok | error.
%% ====================================================================
add_storage_paths(Paths) ->
  StorageConfig = ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/" ++ ?STORAGE_CONFIG_PATH,
  try
    {ok, Fd} = file:open(StorageConfig, [append]),
    lists:foreach(fun(Path) -> file:write(Fd, "[[{name,cluster_fuse_id},{root,\"" ++ Path ++ "\"}]].\n") end, Paths),
    ok = file:close(Fd),
    ok
  catch
    _:_ -> error
  end.

%% add_storage_paths_on_hosts/2
%% ====================================================================
%% @doc Adds storage configuration on hosts
%% @end
-spec add_storage_paths_on_hosts(Paths :: [string()]) -> ok | {error, ErrorHosts :: [string()]}.
%% ====================================================================
add_storage_paths_on_hosts(Paths) ->
  Hosts = install_utils:get_hosts(),
  {HostsOk, HostsFailed} = install_utils:apply_on_hosts(Hosts, ?MODULE, add_storage_paths, [Paths], ?RPC_TIMEOUT),
  StoragePaths = case dao:get_record(configurations, last) of
                   #configuration{storage_paths = InstalledStoragePaths} -> InstalledStoragePaths;
                   _ -> []
                 end,
  case dao:update_record(configurations, #configuration{id = last, storage_paths = StoragePaths ++ Paths}) of
    ok ->
      case HostsFailed of
        [] -> ok;
        _ -> {error, HostsFailed}
      end;
    _ ->
      lager:error("Error while updating storage configuration."),
      rpc:multicall(HostsOk, ?MODULE, remove_storage_paths, [Paths], ?RPC_TIMEOUT),
      {error, Hosts}
  end.
