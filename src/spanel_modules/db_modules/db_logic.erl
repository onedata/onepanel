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
-include("spanel_modules/install.hrl").
-include("spanel_modules/db.hrl").

%% API
-export([create/0, delete/0, add_node/1, get_nodes/0]).

%% initialize/0
%% ====================================================================
%% @doc Initialize table with default values
%% @end
-spec initialize(Table :: atom()) -> ok | error.
%% ====================================================================
initialize(users) ->
  try
    {ok, Username} = application:get_env(?APP_NAME, default_username),
    {ok, Password} = application:get_env(?APP_NAME, default_password),
    PasswordHash = user_logic:hash_password(Password),
    ok = dao:save_record(users, #user{username = Username, password = PasswordHash})
  catch
    _:_ -> error
  end;
initialize(configurations) ->
  try
    ok = dao:save_record(configurations, #configuration{id = last, ulimits = {?DEFAULT_OPEN_FILES, ?DEFAULT_PROCESSES}})
  catch
    _:_ -> error
  end.

%% create/0
%% ====================================================================
%% @doc Creates new database schema and initializes tabels on single node
%% @end
-spec create() -> ok | error.
%% ====================================================================
create() ->
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
      {atomic, ok} -> initialize(users);
      {aborted, {already_exists, users}} -> ok;
      {aborted, UsersError} -> throw(UsersError)
    end,
    case mnesia:create_table(configurations, [
      {attributes, record_info(fields, configuration)},
      {record_name, configuration},
      {disc_copies, [node()]}
    ]) of
      {atomic, ok} -> initialize(configurations);
      {aborted, {already_exists, configurations}} -> ok;
      {aborted, ConfigurationError} -> throw(ConfigurationError)
    end,
    ok
  catch
    _:_ -> error
  end.

%% delete/0
%% ====================================================================
%% @doc Deletes database schema and tabels on single node
%% @end
-spec delete() -> ok | error.
%% ====================================================================
delete() ->
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

%% add_node/1
%% ====================================================================
%% @doc Add node to database cluster
%% @end
-spec add_node(Node :: node()) -> ok | error.
%% ====================================================================
add_node(Node) ->
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

%% get_nodes/0
%% ====================================================================
%% @doc Returns list of nodes database cluster
%% @end
-spec get_nodes() -> [node()].
%% ====================================================================
get_nodes() ->
  mnesia:system_info(db_nodes).
