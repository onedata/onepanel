%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains database management functions. It allows
%% to create, initialize and delete database tables. Moreover it allows
%% to add node to database cluster and list available nodes.
%% @end
%% ===================================================================
-module(db_logic).

-include("onepanel_modules/logic/db_logic.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([create/0, initialize/1, delete/0, add_node/1, get_nodes/0]).

%% ====================================================================
%% API functions
%% ====================================================================

%% initialize/1
%% ====================================================================
%% @doc Initializes table with default values.
%% @end
-spec initialize(Table :: atom()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
initialize(?USER_TABLE) ->
    try
        {ok, DefaultUsername} = application:get_env(?APP_NAME, default_username),
        {ok, DefaultPassword} = application:get_env(?APP_NAME, default_password),
        ok = user_logic:create_user(DefaultUsername, DefaultPassword)
    catch
        _:Reason ->
            ?error("Cannot initialize user table: ~p", [Reason]),
            {error, Reason}
    end;
initialize(?GLOBAL_CONFIG_TABLE) ->
    try
        ok = dao:save_record(?GLOBAL_CONFIG_TABLE, #?GLOBAL_CONFIG_RECORD{id = ?CONFIG_ID, timestamp = 0})
    catch
        _:Reason ->
            ?error("Cannot initialize global configuration table: ~p", [Reason]),
            {error, Reason}
    end;
initialize(_) ->
    ok.

%% create/0
%% ====================================================================
%% @doc Creates new database schema and initializes tabels on single node.
%% @end
-spec create() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
create() ->
    application:stop(mnesia),
    try
        Node = node(),
        case mnesia:create_schema([Node]) of
            ok -> ok;
            {error, {Node, {already_exists, Node}}} -> ok;
            {error, CreateError} -> throw(CreateError)
        end,
        ok = application:start(mnesia),

        lists:foreach(fun({Table, Record, Fields}) ->
            case mnesia:create_table(Table, [
                {attributes, Fields},
                {record_name, Record},
                {disc_copies, [Node]}
            ]) of
                {atomic, ok} -> initialize(Table);
                {aborted, {already_exists, Table}} -> ok;
                {aborted, Error} -> throw(Error)
            end
        end, ?TABLES),

        ok
    catch
        _:Reason ->
            ?error("Cannot create database tables: ~p", [Reason]),
            {error, Reason}
    end.


%% delete/0
%% ====================================================================
%% @doc Deletes database schema and tabels on single node.
%% @end
-spec delete() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
delete() ->
    try
        Node = node(),
        Tables = lists:map(fun({Table, _, _}) -> Table end, ?TABLES),
        lists:foreach(fun(Table) ->
            {atomic, ok} = mnesia:del_table_copy(Table, Node)
        end, Tables),
        ok = application:stop(mnesia),
        ok = mnesia:delete_schema([Node]),
        ok = application:start(mnesia),
        ok
    catch
        _:Reason ->
            ?error("Cannot delete database tables: ~p", [Reason]),
            {error, Reason}
    end.


%% add_node/1
%% ====================================================================
%% @doc Adds node to database cluster.
%% @end
-spec add_node(Node :: node()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
add_node(Node) ->
    try
        {ok, [Node]} = mnesia:change_config(extra_db_nodes, [Node]),
        case mnesia:change_table_copy_type(schema, Node, disc_copies) of
            {atomic, ok} -> ok;
            {aborted, {already_exists, schema, Node, disc_copies}} -> ok;
            ChangeTypeError -> throw(ChangeTypeError)
        end,
        Tables = lists:map(fun({Table, _, _}) -> Table end, ?TABLES),
        lists:foreach(fun(Table) ->
            Type = mnesia:table_info(Table, storage_type),
            {atomic, ok} = mnesia:add_table_copy(Table, Node, Type)
        end, Tables),
        ok
    catch
        _:Reason ->
            ?error("Cannot add node ~p to database cluster: ~p", [Node, Reason]),
            {error, Reason}
    end.


%% get_nodes/0
%% ====================================================================
%% @doc Returns list of nodes database cluster.
%% @end
-spec get_nodes() -> Result when
    Result :: [node()].
%% ====================================================================
get_nodes() ->
    mnesia:system_info(db_nodes).