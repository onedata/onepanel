%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains installation logic functions
%% @end
%% ===================================================================
-module(install_logic).

-include("registered_names.hrl").
-include("spanel_modules/install_logic.hrl").

%% API
-export([create_storage_test_file/1, delete_storage_test_file/1, check_storage_on_node/2, check_storage_on_nodes/1]).

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
  Filename = random_ascii_lowercase_sequence(8),
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
    NewContent = atom_to_list(node()),
    ok = file:write(FdWrite, NewContent),
    ok = file:close(FdWrite),
    {ok, NewContent}
  catch
    _:_ -> error
  end.

%% random_ascii_lowercase_sequence
%% ====================================================================
%% @doc Create random sequence consisting of lowercase ASCII letters.
-spec random_ascii_lowercase_sequence(Length :: integer()) -> list().
%% ====================================================================
random_ascii_lowercase_sequence(Length) ->
  lists:foldl(fun(_, Acc) -> [random:uniform(26) + 96 | Acc] end, [], lists:seq(1, Length)).
