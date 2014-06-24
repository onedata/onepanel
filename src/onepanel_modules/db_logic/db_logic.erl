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
-include("onepanel_modules/install_logic.hrl").
-include("onepanel_modules/db_logic.hrl").

%% API
-export([create/0, initialize/1, delete/0, add_node/1, get_nodes/0]).

%% ====================================================================
%% API functions
%% ====================================================================

%% initialize/0
%% ====================================================================
%% @doc Initializes table with default values.
%% @end
-spec initialize(Table :: atom()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
initialize(?USER_TABLE) ->
    try
        {ok, Username} = application:get_env(?APP_NAME, default_username),
        {ok, Password} = application:get_env(?APP_NAME, default_password),
        PasswordHash = user_logic:hash_password(Password),
        ok = dao:save_record(?USER_TABLE, #?USER_TABLE{username = Username, password = PasswordHash})
    catch
        _:Reason -> {error, Reason}
    end;
initialize(?CONFIG_TABLE) ->
    try
        ok = dao:save_record(?CONFIG_TABLE, #?CONFIG_TABLE{id = ?CONFIG_ID, ulimits = {?DEFAULT_OPEN_FILES, ?DEFAULT_PROCESSES}})
    catch
        _:Reason -> {error, Reason}
    end;
initialize(?PORT_TABLE) ->
    try
        {ok, #?CONFIG_TABLE{main_ccm = MainCCM}} = dao:get_record(?CONFIG_TABLE, ?CONFIG_ID),
        {ok, Hosts} = install_utils:get_control_panel_hosts(MainCCM),
        {ok, [{_, GuiPort}, {_, RestPort}]} = install_utils:get_ports_to_check(MainCCM),
        lists:foreach(fun(Host) ->
            ok = dao:save_record(ports, #?PORT_TABLE{host = Host, gui = GuiPort, rest = RestPort})
        end, Hosts),
        ok
    catch
        _:Reason ->

            {error, Reason}
    end.


%% create/0
%% ====================================================================
%% @doc Creates new database schema and initializes tabels on single node.
%% @end
-spec create() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
create() ->
    try
        Node = node(),
        case mnesia:create_schema([Node]) of
            ok -> ok;
            {error, {Node, {already_exists, Node}}} -> ok;
            {error, CreateError} -> throw(CreateError)
        end,
        ok = application:start(mnesia),
        case mnesia:create_table(?USER_TABLE, [
            {attributes, record_info(fields, ?USER_TABLE)},
            {record_name, ?USER_TABLE},
            {disc_copies, [Node]}
        ]) of
            {atomic, ok} -> initialize(?USER_TABLE);
            {aborted, {already_exists, ?USER_TABLE}} -> ok;
            {aborted, UserError} -> throw(UserError)
        end,
        case mnesia:create_table(?CONFIG_TABLE, [
            {attributes, record_info(fields, ?CONFIG_TABLE)},
            {record_name, ?CONFIG_TABLE},
            {disc_copies, [Node]}
        ]) of
            {atomic, ok} -> initialize(?CONFIG_TABLE);
            {aborted, {already_exists, ?CONFIG_TABLE}} -> ok;
            {aborted, ConfigurationError} -> throw(ConfigurationError)
        end,
        case mnesia:create_table(?PORT_TABLE, [
            {attributes, record_info(fields, ?PORT_TABLE)},
            {record_name, ?PORT_TABLE},
            {disc_copies, [Node]}
        ]) of
            {atomic, ok} -> ok;
            {aborted, {already_exists, ?PORT_TABLE}} -> ok;
            {aborted, PortError} -> throw(PortError)
        end,
        ok
    catch
        _:Reason ->
            lager:error("Cannot create database tables: ~p", [Reason]),
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
        Tables = lists:filter(fun(Table) -> Table =/= schema end, mnesia:system_info(tables)),
        lists:foreach(fun(Table) ->
            {atomic, ok} = mnesia:delete_table(Table)
        end, Tables),
        ok = application:stop(mnesia),
        ok = mnesia:delete_schema([node()]),
        ok = application:start(mnesia),
        ok
    catch
        _:Reason ->
            lager:error("Cannot delete database tables: ~p", [Reason]),
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
            {aborted, {already_exists, schema, Node, _}} -> ok;
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
            lager:error("Cannot add database node to cluster: ~p", [Reason]),
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
