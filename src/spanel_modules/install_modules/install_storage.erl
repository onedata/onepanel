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
-include("spanel_modules/install_common.hrl").

%% API
-export([create_storage_test_file/1, delete_storage_test_file/1, check_storage_on_node/2, check_storage_on_nodes/1]).
-export([set_ulimits_on_node/2, set_ulimits_on_nodes/1, set_ulimits_on_nodes/3]).

-define(STORAGE_TEST_FILE_PREFIX, "storage_test_").

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
    Content = atom_to_list(node()),
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

%% check_storage_on_nodes/1
%% ====================================================================
%% @doc Checks storage availability on all nodes
%% @end
-spec check_storage_on_nodes(Path :: string()) -> ok | error.
%% ====================================================================
check_storage_on_nodes(Path) ->
  case create_storage_test_file(Path) of
    {ok, FilePath, Content} ->
      try
        {ok, Content} = check_storage_on_node(FilePath, Content),
        {ok, _} = lists:foldl(fun
          (Node, {ok, NewContent}) ->
            gen_server:call({?SPANEL_NAME, Node}, {check_storage, FilePath, NewContent}, ?GEN_SERVER_TIMEOUT);
          (_, Other) -> Other
        end, {ok, Content}, nodes(hidden)),
        delete_storage_test_file(FilePath),
        ok
      catch
        _:_ ->
          delete_storage_test_file(FilePath),
          error
      end;
    _ -> error
  end.

%% check_storage_on_node/2
%% ====================================================================
%% @doc Checks storage availability on node
%% @end
-spec check_storage_on_node(FilePath :: string(), Content :: string()) -> {ok, NewContent :: string()} | error.
%% ====================================================================
check_storage_on_node(FilePath, Content) ->
  try
    {ok, FdRead} = file:open(FilePath, [read]),
    {ok, Content} = file:read_line(FdRead),
    ok = file:close(FdRead),
    {ok, FdWrite} = file:open(FilePath, [write]),
    NewContent = install_utils:random_ascii_lowercase_sequence(20),
    ok = file:write(FdWrite, NewContent),
    ok = file:close(FdWrite),
    {ok, NewContent}
  catch
    _:_ -> error
  end.

%% set_ulimit_on_nodes/1
%% ====================================================================
%% @doc Sets default system limits on nodes
%% @end
-spec set_ulimits_on_nodes(Nodes :: [node()]) -> ok | error.
%% ====================================================================
set_ulimits_on_nodes(Nodes) ->
  set_ulimits_on_nodes(Nodes, ?DEFAULT_OPEN_FILES, ?DEFAULT_PROCESSES).

%% set_ulimit_on_nodes/3
%% ====================================================================
%% @doc Sets system limits on nodes
%% @end
-spec set_ulimits_on_nodes(Nodes :: [node()], OpenFiles :: integer(), Processes :: integer()) -> ok | error.
%% ====================================================================
set_ulimits_on_nodes(Nodes, OpenFiles, Processes) ->
  {_, FailedNodes} = install_utils:apply_on_nodes(Nodes, ?MODULE, set_ulimits_on_node, [OpenFiles, Processes], ?RPC_TIMEOUT),
  case FailedNodes of
    [] -> ok;
    _ -> {error, FailedNodes}
  end.

%% set_ulimit_on_node/2
%% ====================================================================
%% @doc Sets system limits on node
%% @end
-spec set_ulimits_on_node(OpenFiles :: integer(), Processes :: integer()) -> ok | error.
%% ====================================================================
set_ulimits_on_node(OpenFiles, Processes) ->
  case file:consult(?ULIMITS_CONFIG_PATH) of
    {ok, []} ->
      file:write_file(?ULIMITS_CONFIG_PATH, io_lib:fwrite("~p.\n~p.\n", [{open_files, OpenFiles}, {process_limit, Processes}]), [append]),
      {node(), ok};
    {ok, _} ->
      {node(), ok};
    Error ->
      lager:error("Cannot parse file ~p, error: ~p", [?ULIMITS_CONFIG_PATH, Error]),
      {node(), error}
  end.