%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains cluster database management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_db).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("names.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([init/0, destroy/0]).
-export([wait_for_tables/0, global_wait_for_tables/0]).
-export([create_tables/0, copy_tables/0, delete_tables/0, upgrade_tables/0]).
-export([add_node/1, remove_node/1, get_nodes/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initializes database cluster by ensuring that database schema is present.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | no_return().
init() ->
    Node = node(),
    mnesia:stop(),
    case mnesia:create_schema([Node]) of
        ok -> ok;
        {error, {Node, {already_exists, Node}}} -> ok;
        {error, Reason} -> ?throw_error(Reason, [])
    end,
    ok = mnesia:start().


%%--------------------------------------------------------------------
%% @doc Removes all the user and configuration data from this host.
%% Removes host from the mnesia database cluster.
%% @end
%%--------------------------------------------------------------------
-spec destroy() -> ok.
destroy() ->
    mnesia:stop(),
    ok = mnesia:delete_schema([node()]).


%%--------------------------------------------------------------------
%% @doc Creates database tables
%% @end
%%--------------------------------------------------------------------
-spec create_tables() -> ok.
create_tables() ->
    lists:foreach(fun(Model) ->
        Table = model:get_table_name(Model),
        case mnesia:create_table(Table, [
            {attributes, model:get_fields()},
            {record_name, ?WRAPPER_RECORD},
            {disc_copies, get_nodes()}
        ]) of
            {atomic, ok} ->
                Model:seed(),
                ok;
            {aborted, {already_exists, Table}} -> ok;
            {aborted, Reason} -> error(?make_error(Reason))
        end
    end, model:get_models()).


%%--------------------------------------------------------------------
%% @doc Waits for all tables known to mnesia to be available.
%% In multinode setup this requires other nodes to be online
%% unless current node was the last one to be stopped.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_tables() -> ok.
wait_for_tables() ->
    Timeout = infinity,
    Tables = mnesia:system_info(tables),
    ok = mnesia:wait_for_tables(Tables, Timeout).


%%--------------------------------------------------------------------
%% @doc Waits for all known nodes to be available and for mnesia
%% on them to be ready.
%% This is different from local wait_for_tables, which does not wait
%% for other nodes if the current node has the newest state.
%% Always returns ok in a new cluster (before service_onepanel:init_cluster/1)
%% as the mnesia nodes list is empty.
%% @end
%%--------------------------------------------------------------------
-spec global_wait_for_tables() -> ok | no_return().
global_wait_for_tables() ->
    Nodes = get_nodes(),
    Attempts = application:get_env(?APP_NAME, wait_for_cluster_attempts, 600),
    Delay = application:get_env(?APP_NAME, wait_for_cluster_delay, 1000),
    onepanel_utils:wait_until(onepanel_rpc, call,
        [Nodes, onepanel_db, wait_for_tables, []],
        {validator, fun(Results) ->
            {_GoodResults, BadResults} = service_utils:partition_results(Results),
            case BadResults of
                [] -> ok;
                _ ->
                    {BadNodes, _} = lists:unzip(BadResults),
                    ?info("Missing/not ready nodes: ", [onepanel_utils:join(BadNodes, <<" ">>)]),
                    error(cluster_incomplete)
            end
        end}, Attempts, Delay).


-spec upgrade_tables() -> ok.
upgrade_tables() ->
    lists:foreach(fun(Model) ->
        ?debug("Upgrading model ~p", [Model]),
        Table = model:get_table_name(Model),
        upgrade_table(Table, Model),
        ?debug("Model ~p upgraded", [Model])
    end, model:get_models()).


%%--------------------------------------------------------------------
%% @doc Copies content of the database tables from remote node.
%% @end
%%--------------------------------------------------------------------
-spec copy_tables() -> ok.
copy_tables() ->
    {atomic, ok} = mnesia:change_table_copy_type(schema, node(), disc_copies),
    Tables = lists:map(fun(Model) ->
        Table = model:get_table_name(Model),
        {atomic, ok} = mnesia:add_table_copy(Table, node(), disc_copies),
        Table
    end, model:get_models()),
    Timeout = onepanel_env:get(copy_tables_timeout),
    ok = mnesia:wait_for_tables(Tables, Timeout).


%%--------------------------------------------------------------------
%% @doc Removes database tables and their contents from this host.
%% @end
%%--------------------------------------------------------------------
-spec delete_tables() -> ok | no_return().
delete_tables() ->
    lists:foreach(fun(Model) ->
        Table = model:get_table_name(Model),
        case mnesia:del_table_copy(Table, node()) of
            {atomic, ok} -> ok;
            {aborted, {no_exists, _}} -> ok;
            {aborted, Reason} -> ?throw_error(Reason, [])
        end
    end, model:get_models()).


%%--------------------------------------------------------------------
%% @doc Adds node to the cluster.
%% @end
%%--------------------------------------------------------------------
-spec add_node(Node :: node()) -> ok.
add_node(Node) ->
    {ok, [Node]} = mnesia:change_config(extra_db_nodes, [Node]),
    ok.


%%--------------------------------------------------------------------
%% @doc Removes node from the cluster.
%% @end
%%--------------------------------------------------------------------
-spec remove_node(Node :: node()) -> ok.
remove_node(Node) ->
    {atomic, ok} = mnesia:del_table_copy(schema, Node),
    ok.


%%--------------------------------------------------------------------
%% @doc Returns list of the onepanel cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    mnesia:table_info(schema, disc_copies).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Invokes upgrade function on all elements in the table
%% to ensure they have up-to-date schema.
%% On the first run after upgrading from 18.02
%% handles migration to records being wrapped in #document{}.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_table(Table :: atom(), Model :: model:model()) ->
    ok | no_return().
upgrade_table(Table, Model) ->
    case mnesia:transform_table(
        Table,
        fun(Record) -> model:upgrade(Model, Record) end,
        model:get_fields(),
        ?WRAPPER_RECORD
    ) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> ?throw_error(Reason)
    end.
