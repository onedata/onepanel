%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains database management functions. It allows
%% to create, initialize and delete database tables. Moreover it allows
%% to add node to database cluster and list available nodes.
%% @end
%% ===================================================================
-module(db_logic).

-include("registered_names.hrl").
-include("onepanel_modules/user_logic.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include("onepanel_modules/updater/state.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([create/0, initialize/1, delete/0, add_node/1, get_nodes/0]).
-export([get_password/1, change_password/3]).

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
        {ok, Username} = application:get_env(?APP_NAME, default_username),
        {ok, UserPassword} = application:get_env(?APP_NAME, default_user_password),
        {ok, DbPassword} = application:get_env(?APP_NAME, default_db_password),
        ok = user_logic:create_user(Username, UserPassword),
        ok = set_password(Username, DbPassword)
    catch
        _:Reason ->
            ?error("Cannot initialize user table: ~p", [Reason]),
            {error, Reason}
    end;
initialize(?GLOBAL_CONFIG_TABLE) ->
    try
        ok = dao:save_record(?GLOBAL_CONFIG_TABLE, #?GLOBAL_CONFIG_RECORD{id = ?CONFIG_ID})
    catch
        _:Reason ->
            ?error("Cannot initialize global configuration table: ~p", [Reason]),
            {error, Reason}
    end;
initialize(?UPDATER_STATE_TABLE) ->
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
        case mnesia:create_table(?USER_TABLE, [
            {attributes, record_info(fields, ?USER_RECORD)},
            {record_name, ?USER_RECORD},
            {disc_copies, [Node]}
        ]) of
            {atomic, ok} -> initialize(?USER_TABLE);
            {aborted, {already_exists, ?USER_TABLE}} -> ok;
            {aborted, UserError} -> throw(UserError)
        end,
        case mnesia:create_table(?LOCAL_CONFIG_TABLE, [
            {attributes, record_info(fields, ?LOCAL_CONFIG_RECORD)},
            {record_name, ?LOCAL_CONFIG_RECORD},
            {disc_copies, [Node]}
        ]) of
            {atomic, ok} -> ok;
            {aborted, {already_exists, ?LOCAL_CONFIG_TABLE}} -> ok;
            {aborted, LocalConfigError} -> throw(LocalConfigError)
        end,
        case mnesia:create_table(?GLOBAL_CONFIG_TABLE, [
            {attributes, record_info(fields, ?GLOBAL_CONFIG_RECORD)},
            {record_name, ?GLOBAL_CONFIG_RECORD},
            {disc_copies, [Node]}
        ]) of
            {atomic, ok} -> initialize(?GLOBAL_CONFIG_TABLE);
            {aborted, {already_exists, ?GLOBAL_CONFIG_TABLE}} -> ok;
            {aborted, GlobalConfigError} -> throw(GlobalConfigError)
        end,
        case mnesia:create_table(?UPDATER_STATE_TABLE, [
            {attributes, record_info(fields, ?u_state)},
            {record_name, ?u_state},
            {disc_copies, [Node]}
        ]) of
            {atomic, ok} -> initialize(?UPDATER_STATE_TABLE);
            {aborted, {already_exists, ?UPDATER_STATE_TABLE}} -> ok;
            {aborted, UpdaterError} -> throw(UpdaterError)
        end,
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
        Tables = lists:filter(fun(Table) -> Table =/= schema end, mnesia:system_info(tables)),
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
        Tables = lists:filter(fun(Table) -> Table =/= schema end, mnesia:system_info(tables)),
        lists:foreach(fun(Table) ->
            Type = mnesia:table_info(Table, storage_type),
            {atomic, ok} = mnesia:add_table_copy(Table, Node, Type)
        end, Tables),
        ok
    catch
        _:Reason ->
            ?error("Cannot add database node to cluster: ~p", [Reason]),
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


%% get_password/1
%% ====================================================================
%% @doc Returns current database nodes password.
%% @end
-spec get_password(Username :: binary()) -> Result when
    Result :: {ok, Password :: binary()} | {error, Reason :: term()}.
%% ====================================================================
get_password(Username) ->
    case dao:get_record(?USER_TABLE, Username) of
        {ok, #?USER_RECORD{db_password = Password}} -> {ok, Password};
        Other -> {error, Other}
    end.


%% change_password/3
%% ====================================================================
%% @doc Changes password for all database nodes. Returns 'ok' in case of
%% successful operation on all nodes or host for which password could not
%% be changed.
%% @end
-spec change_password(Username :: binary(), OldPassword :: binary(), NewPassword :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
change_password(Username, OldPassword, NewPassword) ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{dbs = Dbs}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        case Dbs of
            [] -> throw("Database nodes not configured");
            _ -> ok
        end,
        {SuccessfulChange, ErrorHost} = lists:foldl(fun
            (Db, {Acc, undefined}) ->
                case change_password(Db, Username, OldPassword, NewPassword) of
                    ok -> {[Db | Acc], undefined};
                    _ -> {Acc, Db}
                end;
            (_, {Acc, Host}) ->
                {Acc, Host}
        end, {[], undefined}, Dbs),
        case ErrorHost of
            undefined ->
                set_password(Username, NewPassword);
            _ ->
                lists:foreach(fun(Db) ->
                    change_password(Db, Username, NewPassword, OldPassword)
                end, SuccessfulChange),
                {error, {host, ErrorHost}}
        end
    catch
        _:Reason ->
            ?error("Cannot change database password: ~p", [Reason]),
            {error, Reason}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% change_password/4
%% ====================================================================
%% @doc Changes password for database node on given host.
%% @end
-spec change_password(Db :: string(), Username :: binary(), OldPassword :: binary(), NewPassword :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
change_password(Db, Username, OldPassword, NewPassword) ->
    Url = "http://" ++ Db ++ ":" ++ ?DEFAULT_PORT ++ "/_config/admins/" ++ binary_to_list(Username),
    Headers = [{content_type, "application/json"}],
    Body = mochijson2:encode(NewPassword),
    Options = [{basic_auth, {binary_to_list(Username), binary_to_list(OldPassword)}}],
    case ibrowse:send_req(Url, Headers, put, Body, Options) of
        {ok, "200", _ResHeaders, _ResBody} -> ok;
        Other ->
            ?error("Cannot change password for database node at host ~s: ~p", [Db, Other]),
            {error, Other}
    end.


%% set_password/2
%% ====================================================================
%% @doc Set database nodes passwords in database.
%% @end
-spec set_password(Username :: binary(), Password :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
set_password(Username, Password) ->
    dao:update_record(?USER_TABLE, Username, [{db_password, Password}]).