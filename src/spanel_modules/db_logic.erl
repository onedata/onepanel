%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains database management functions
%% @end
%% ===================================================================
-module(db_logic).

-include("registered_names.hrl").
-include("spanel_modules/install_common.hrl").
-include("spanel_modules/db_logic.hrl").

%% API
-export([initialize_table/1, create_database/0, delete_database/0, add_database_node/1, get_database_nodes/0]).
-export([save_record/2, update_record/2, get_record/2, exist_record/2]).

%% init/0
%% ====================================================================
%% @doc Initialize users table with default user (admin, password)
%% @end
-spec initialize_table(Table :: atom()) -> ok | error.
%% ====================================================================
initialize_table(users) ->
  try
    {ok, Username} = application:get_env(?APP_NAME, default_username),
    {ok, Password} = application:get_env(?APP_NAME, default_password),
    PasswordHash = user_logic:hash_password(Password),
    ok = save_record(users, #user{username = Username, password = PasswordHash})
  catch
    _:_ -> error
  end;
initialize_table(configurations) ->
  ok = save_record(configurations, #configuration{id = last, ulimits = {?DEFAULT_OPEN_FILES, ?DEFAULT_PROCESSES}}).

%% create_database/0
%% ====================================================================
%% @doc Creates new database schema and initializes tabels on single node
%% @end
-spec create_database() -> ok | error.
%% ====================================================================
create_database() ->
  try
    Node = node(),
    case mnesia:create_schema([node()]) of
      ok -> ok;
      {error, {Node, {already_exists, Node}}} -> ok;
      {error, Reason} -> throw(Reason)
    end,
    ok = application:start(mnesia),
    case mnesia:create_table(users, [
      {attributes, record_info(fields, user)},
      {record_name, user},
      {disc_copies, [node()]}
    ]) of
      {atomic, ok} -> initialize_table(users);
      {aborted, {already_exists, users}} -> ok;
      {aborted, UsersError} -> throw(UsersError)
    end,
    case mnesia:create_table(configurations, [
      {attributes, record_info(fields, configuration)},
      {record_name, configuration},
      {disc_copies, [node()]}
    ]) of
      {atomic, ok} -> initialize_table(configurations);
      {aborted, {already_exists, configurations}} -> ok;
      {aborted, ConfigurationError} -> throw(ConfigurationError)
    end,
    ok
  catch
    _:_ -> error
  end.

%% delete_database/0
%% ====================================================================
%% @doc Deletes database schema and tabels on single node
%% @end
-spec delete_database() -> ok | error.
%% ====================================================================
delete_database() ->
  try
    Tables = lists:filter(fun(Table) -> Table =/= schema end, mnesia:system_info(tables)),
    lists:foreach(fun(Table) ->
      {atomic, ok} = mnesia:delete_table(Table)
    end, Tables),
    ok = application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = application:start(mnesia),
    ok
  catch
    _:_ -> error
  end.

%% add_database_node/1
%% ====================================================================
%% @doc Deletes database schema and tabels on single node
%% @end
-spec add_database_node(Node :: node()) -> ok | error.
%% ====================================================================
add_database_node(Node) ->
  try
    {ok, [Node]} = mnesia:change_config(extra_db_nodes, [Node]),
    {atomic, ok} = mnesia:change_table_copy_type(schema, Node, disc_copies),
    Tables = lists:filter(fun(Table) -> Table =/= schema end, mnesia:system_info(tables)),
    lists:foreach(fun(Table) ->
      Type = mnesia:table_info(Table, storage_type),
      {atomic, ok} = mnesia:add_table_copy(Table, Node, Type)
    end, Tables),
    ok
  catch
    _:_ -> error
  end.

%% get_database_nodes/0
%% ====================================================================
%% @doc Returns list of database nodes
%% @end
-spec get_database_nodes() -> [node()].
%% ====================================================================
get_database_nodes() ->
  mnesia:system_info(db_nodes).

%% save_record/2
%% ====================================================================
%% @doc Saves record in database table
%% @end
-spec save_record(Table :: atom(), Record :: record()) -> ok | error.
%% ====================================================================
save_record(Table, Record) ->
  Transaction = fun() ->
    case mnesia:write(Table, Record, write) of
      ok -> ok;
      _ -> error
    end
  end,
  mnesia:activity(transaction, Transaction).

%% update_record/2
%% ====================================================================
%% @doc Updates record in database table
%% @end
-spec update_record(Table :: atom(), Record :: record()) -> ok | error.
%% ====================================================================
update_record(Table, NewRecord) ->
  case get_record(Table, element(2, NewRecord)) of
    {ok, OldRecord} ->
      List = lists:map(fun
        ({Old, undefined}) -> Old;
        ({_, New}) -> New
      end, lists:zip(tuple_to_list(OldRecord), tuple_to_list(NewRecord))),
      save_record(Table, list_to_tuple(List));
    _ -> save_record(Table, NewRecord)
  end.

%% get_record/2
%% ====================================================================
%% @doc Gets record from database table
%% @end
-spec get_record(Table :: atom(), Key :: term()) -> {ok, Record :: record()} | not_found | error.
%% ====================================================================
get_record(Table, Key) ->
  Transaction = fun() ->
    case mnesia:read(Table, Key) of
      [Record] -> {ok, Record};
      [] -> not_found;
      _ -> error
    end
  end,
  mnesia:activity(transaction, Transaction).

%% exist_record/2
%% ====================================================================
%% @doc Checks whether record exist in database table
%% @end
-spec exist_record(Table :: atom(), Key :: term()) -> {ok, Record :: record()} | not_found | error.
%% ====================================================================
exist_record(Table, Key) ->
  Transaction = fun() ->
    case mnesia:read(Table, Key) of
      [_] -> true;
      _ -> false
    end
  end,
  mnesia:activity(transaction, Transaction).