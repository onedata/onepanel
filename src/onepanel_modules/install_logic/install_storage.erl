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
-include("onepanel_modules/install_logic.hrl").
-include("onepanel_modules/db_logic.hrl").

%% API
-export([create_storage_test_file/1, delete_storage_test_file/1, check_storage_on_host/2, check_storage_on_hosts/2]).
-export([add_storage_paths/1, remove_storage_paths/1, add_storage_paths_on_hosts/2]).

-define(STORAGE_TEST_FILE_PREFIX, "storage_test_").
-define(STORAGE_TEST_FILE_LENGTH, 20).

%% ====================================================================
%% API functions
%% ====================================================================

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


%% check_storage_on_hosts/1
%% ====================================================================
%% @doc Checks storage availability on hosts. Returns ok or first host for which
%% storage is not available.
%% @end
-spec check_storage_on_hosts(Hosts :: [string()], Path :: string()) -> Result when
  Result :: ok | {error, ErrorHosts :: [string()]}.
%% ====================================================================
check_storage_on_hosts([], _) ->
  ok;
check_storage_on_hosts([Host | Hosts], Path) ->
  Node = install_utils:get_node(Host),
  case gen_server:call({?GEN_SERVER_NAME, Node}, {create_storage_test_file, Path}, ?GEN_SERVER_TIMEOUT) of
    {ok, FilePath, Content} ->
      try
        Answer = lists:foldl(fun
          (H, {NewContent, ErrorHosts}) ->
            case gen_server:call({?GEN_SERVER_NAME, install_utils:get_node(H)}, {check_storage, FilePath, NewContent}, ?GEN_SERVER_TIMEOUT) of
              {ok, NextContent} -> {NextContent, ErrorHosts};
              {error, ErrorHost} -> {NewContent, [ErrorHost | ErrorHosts]}
            end
        end, {Content, []}, [Host | Hosts]),
        gen_server:cast({?GEN_SERVER_NAME, Node}, {delete_storage_test_file, FilePath}),
        case Answer of
          {_, []} -> ok;
          {_, EHosts} -> {error, EHosts}
        end
      catch
        _:Reason ->
          lager:error("Cannot check storage ~p availability on hosts ~p: ~p", [Path, [Host | Hosts], Reason]),
          gen_server:cast({?GEN_SERVER_NAME, Node}, {delete_storage_test_file, FilePath}),
          error
      end;
    Other -> Other
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
      lager:error("Storage is not available on host ~p: ~p", [Host, Reason]),
      {error, Host}
  end.


%% add_storage_paths/1
%% ====================================================================
%% @doc Removes storage configuration on host.
%% @end
-spec remove_storage_paths(Paths :: [string()]) -> Result when
  Result :: ok | {error, Reason :: term()}.
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
    _:Reason ->
      lager:error("Cannot remove storage paths on host ~p: ~p", [install_utils:get_host(node()), Reason]),
      {error, Reason}
  end.


%% add_storage_paths/1
%% ====================================================================
%% @doc Adds storage configuration on host.
%% @end
-spec add_storage_paths(Paths :: [string()]) -> Result when
  Result :: ok | {error, Reason :: term()}.
%% ====================================================================
add_storage_paths(Paths) ->
  lager:info("Adding storage paths..."),
  StorageConfig = ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/" ++ ?STORAGE_CONFIG_PATH,
  try
    {ok, Fd} = file:open(StorageConfig, [append]),
    lists:foreach(fun(Path) -> file:write(Fd, "[[{name, cluster_fuse_id}, {root, \"" ++ Path ++ "\"}]].\n") end, Paths),
    ok = file:close(Fd),
    ok
  catch
    _:Reason ->
      lager:error("Cannot add storage paths on host ~p: ~p", [install_utils:get_host(node()), Reason]),
      {error, Reason}
  end.


%% add_storage_paths_on_hosts/2
%% ====================================================================
%% @doc Adds storage configuration on hosts.
%% @end
-spec add_storage_paths_on_hosts(Hosts :: [string()], Paths :: [string()]) -> ok | {error, ErrorHosts :: [string()]}.
%% ====================================================================
add_storage_paths_on_hosts(Hosts, Paths) ->
  {HostsOk, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, add_storage_paths, [Paths], ?RPC_TIMEOUT),
  StoragePaths = case dao:get_record(?CONFIG_TABLE, ?CONFIG_ID) of
                   #?CONFIG_TABLE{storage_paths = InstalledStoragePaths} -> InstalledStoragePaths;
                   _ -> []
                 end,

  case dao:update_record(?CONFIG_TABLE, ?CONFIG_ID, [{storage_paths, StoragePaths ++ Paths}]) of
    ok ->
      case HostsError of
        [] -> ok;
        _ ->
          lager:error("Cannot add storage paths on following hosts: ~p", [HostsError]),
          {error, HostsError}
      end;
    _ ->
      lager:error("Cannot update storage paths configuration."),
      rpc:multicall(HostsOk, ?MODULE, remove_storage_paths, [Paths], ?RPC_TIMEOUT),
      {error, Hosts}
  end.
