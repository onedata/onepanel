%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(db_manager).
-author("Krzysztof Trzepla").

-include("onepanel.hrl").
-include("db/models.hrl").

%% API
-export([create_db/0, delete_db/0, is_db_empty/0, is_table_empty/1, empty_db/0]).
-export([add_node/2, commit_node/0, get_nodes/0, copy_tables/0]).

-type table() :: atom().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec create_db() -> ok | no_return().
create_db() ->
    Node = node(),

    application:stop(mnesia),
    case mnesia:create_schema([Node]) of
        ok -> ok;
        {error, {Node, {already_exists, Node}}} -> ok;
        {error, Reason} -> throw(Reason)
    end,
    ok = application:start(mnesia),

    copy_tables(),
    create_tables(),
    create_meta().


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec delete_db() -> ok.
delete_db() ->
    delete_tables(),
    ok = application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = application:start(mnesia).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec empty_db() -> ok.
empty_db() ->
    lists:foreach(fun(Model) ->
        Table = model_logic:table_name(Model),
        {atomic, ok} = mnesia:clear_table(Table)
    end, ?MODELS ++ ?INTERNAL_MODELS).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec is_table_empty(Table :: table()) -> boolean().
is_table_empty(Table) ->
    0 == count_records(Table).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec is_db_empty() -> boolean().
is_db_empty() ->
    Tables = [model_logic:table_name(Model) || Model <- ?MODELS],
    0 == count_records(Tables).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec add_node(Node :: node(), Force :: boolean()) ->
    ok | ignore | try_again | {error, Reason :: term()}.
add_node(Node, Force) ->
    Server = {?ONEPANEL_SERVER, Node},
    Timestamp = db_meta:get_timestamp(),
    try gen_server:call(Server, {join_db_cluster, Timestamp, Force}) of
        ok ->
            {ok, [Node]} = mnesia:change_config(extra_db_nodes, [Node]),
            gen_server:cast(Server, {join_db_cluster_ack, node()});
        ignore -> ignore;
        try_again -> try_again
    catch
        _:Reason -> {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec commit_node() -> ok.
commit_node() ->
    {atomic, ok} = mnesia:change_table_copy_type(schema, node(), disc_copies),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    mnesia:system_info(db_nodes).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec copy_tables() -> ok | no_return().
copy_tables() ->
    Node = node(),

    lists:foreach(fun(Model) ->
        Table = model_logic:table_name(Model),
        case mnesia:add_table_copy(Table, Node, disc_copies) of
            {atomic, ok} -> ok;
            {aborted, {already_exists, Table, Node}} -> ok;
            {aborted, {no_exists, _}} -> ok;
            {aborted, Reason} -> throw(Reason)
        end
    end, ?MODELS ++ ?INTERNAL_MODELS).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec count_records(Table :: table() | [table()]) -> Count :: integer().
count_records(Table) when is_atom(Table) ->
    mnesia:table_info(Table, size);

count_records(Tables) when is_list(Tables) ->
    lists:foldl(fun(Table, Count) ->
        Count + count_records(Table)
    end, 0, Tables).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec create_tables() -> ok | no_return().
create_tables() ->
    Tables = lists:map(fun(Model) ->
        Table = model_logic:table_name(Model),
        case mnesia:create_table(Table, [
            {attributes, Model:fields()},
            {record_name, Model},
            {disc_copies, [node()]}
        ]) of
            {atomic, ok} -> ok;
            {aborted, {already_exists, Table}} -> ok;
            {aborted, Reason} -> throw(Reason)
        end,
        Table
    end, ?MODELS ++ ?INTERNAL_MODELS),
    Timeout = onepanel:get_env(create_tables_timeout),
    ok = mnesia:wait_for_tables(Tables, Timeout).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec create_meta() -> ok | no_return().
create_meta() ->
    case db_meta:set_timestamp() of
        ok -> ok;
        {error, already_exists} -> ok;
        {error, Reason} -> throw(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec delete_tables() -> ok | no_return().
delete_tables() ->
    lists:foreach(fun(Model) ->
        Table = model_logic:table_name(Model),
        case mnesia:del_table_copy(Table, node()) of
            {atomic, ok} -> ok;
            {aborted, {no_exists, _}} -> ok;
            {aborted, Reason} -> throw(Reason)
        end
    end, ?MODELS ++ ?INTERNAL_MODELS).