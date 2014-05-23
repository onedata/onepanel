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

-include("spanel_modules/db_logic.hrl").

%% API
-export([create_database/0, delete_database/0, add_database_node/1, save_record/2, get_record/2]).

%% create_database/0
%% ====================================================================
%% @doc Creates new database schema and initializes tabels on single node
%% @end
-spec create_database() -> ok | error.
%% ====================================================================
create_database() ->
  try
    mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = application:start(mnesia),
    {atomic, ok} = mnesia:create_table(users, [
      {attributes, record_info(fields, user)},
      {record_name, user},
      {disc_copies, [node()]}
    ]),
    {atomic, ok} = mnesia:create_table(configurations, [
      {attributes, record_info(fields, configuration)},
      {record_name, configuration},
      {disc_copies, [node()]}
    ]),
    ok
  catch
    _:_ -> error
  end.

%% create_database/0
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
    {ok, DbNodes} = mnesia:change_config(extra_db_nodes, [Node]),
    true = lists:any(fun(DbNode) -> DbNode =:= Node end, DbNodes),
    {atomic, ok} = mnesia:change_table_copy_type(schema, Node, disc_copies),
    [{atomic, ok} = mnesia:add_table_copy(TableCopy, Node, Type)
      || {TableCopy, [{_, Type}]} <- [{Table, mnesia:table_info(Table, where_to_commit)}
      || Table <- mnesia:system_info(tables)]],
    ok
  catch
    _:_ -> error
  end.

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

%% get_record/2
%% ====================================================================
%% @doc Gets record from database table
%% @end
-spec get_record(Table :: atom(), Key :: term()) -> {ok, Record :: record()} | not_found | error.
%% ====================================================================
get_record(Table, Key) ->
  Transaction = fun() ->
    case mnesia:read(Table, Key) of
      [Record] -> Record;
      [] -> not_found;
      _ -> error
    end
  end,
  mnesia:activity(transaction, Transaction).