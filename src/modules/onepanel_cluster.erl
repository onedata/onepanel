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
-module(onepanel_cluster).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("names.hrl").

%% API
-export([init/0, join/1, leave/1, add_node/1, remove_node/1, clear_node/0,
    get_nodes/0]).
-export([node_to_host/0, node_to_host/1, nodes_to_hosts/0, nodes_to_hosts/1,
    host_to_node/1, host_to_node/2, hosts_to_nodes/1, hosts_to_nodes/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | no_return().
init() ->
    Node = node(),
    Nodes = get_nodes(),

    case lists:delete(Node, Nodes) of
        [] -> clear_node();
        [ClusterNode | _] -> leave(ClusterNode)
    end,

    ok = mnesia:create_schema([Node]),
    ok = mnesia:start(),
    create_tables().


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec join(ClusterNode :: node()) -> ok | no_return().
join(ClusterNode) ->
    Node = node(),
    clear_node(),
    ok = mnesia:start(),
    onepanel_rpc:check_call([ClusterNode], ?MODULE, add_node, [Node]),
    {atomic, ok} = mnesia:change_table_copy_type(schema, Node, disc_copies),
    copy_tables().


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec leave(ClusterNode :: node()) -> ok | no_return().
leave(ClusterNode) ->
    Node = node(),
    clear_node(),
    onepanel_rpc:check_call([ClusterNode], ?MODULE, remove_node, [Node]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec add_node(Node :: node()) -> ok.
add_node(Node) ->
    {ok, [Node]} = mnesia:change_config(extra_db_nodes, [Node]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec remove_node(Node :: node()) -> ok.
remove_node(Node) ->
    {atomic, ok} = mnesia:del_table_copy(schema, Node),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec clear_node() -> ok | no_return().
clear_node() ->
    ok = mnesia:start(),
    delete_tables(),
    mnesia:stop(),
    ok = mnesia:delete_schema([node()]).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    mnesia:system_info(db_nodes).


%%--------------------------------------------------------------------
%% @doc @equiv node_to_host(node())
%%--------------------------------------------------------------------
-spec node_to_host() -> Host :: service:host().
node_to_host() ->
    node_to_host(node()).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec node_to_host(Node :: node()) -> Host :: service:host().
node_to_host(Node) ->
    BinNode= erlang:atom_to_binary(Node, utf8),
    [_Name, Host] = binary:split(BinNode, <<"@">>),
    Host.


%%--------------------------------------------------------------------
%% @doc @equiv nodes_to_hosts(onepanel:nodes())
%%--------------------------------------------------------------------
-spec nodes_to_hosts() -> Hosts :: [service:host()].
nodes_to_hosts() ->
    nodes_to_hosts(onepanel_cluster:get_nodes()).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec nodes_to_hosts(Nodes :: [node()]) -> Hosts :: [service:host()].
nodes_to_hosts(Nodes) ->
    lists:map(fun(Node) -> node_to_host(Node) end, Nodes).


%%--------------------------------------------------------------------
%% @doc @equiv host_to_node(?APP_NAME)
%%--------------------------------------------------------------------
-spec host_to_node(Host :: service:host()) -> Node :: node().
host_to_node(Host) ->
    Name = erlang:atom_to_list(?APP_NAME),
    host_to_node(Name, Host).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec host_to_node(Name :: atom() | string(), Host :: service:host()) ->
    Node :: node().
host_to_node(Name, Host) when is_atom(Name) ->
    host_to_node(erlang:atom_to_list(Name), Host);

host_to_node(Name, Host) ->
    erlang:binary_to_atom(onepanel_utils:join([Name, Host], <<"@">>), utf8).


%%--------------------------------------------------------------------
%% @doc @equiv host_to_node(?APP_NAME, Hosts)
%%--------------------------------------------------------------------
-spec hosts_to_nodes(Hosts :: [service:host()]) -> Nodes :: [node()].
hosts_to_nodes(Hosts) ->
    Name = erlang:atom_to_list(?APP_NAME),
    hosts_to_nodes(Name, Hosts).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec hosts_to_nodes(Name :: atom() | string(), Hosts :: [service:host()]) ->
    Nodes :: [node()].
hosts_to_nodes(Name, Hosts) ->
    lists:map(fun(Host) -> host_to_node(Name, Host) end, Hosts).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec create_tables() -> ok.
create_tables() ->
    Tables = lists:map(fun(Model) ->
        Table = model:table_name(Model),
        {atomic, ok} = mnesia:create_table(Table, [
            {attributes, Model:get_fields()},
            {record_name, Model},
            {disc_copies, [node()]}
        ]),
        Table
    end, model:get_models()),
    Timeout = onepanel_env:get(create_tables_timeout),
    ok = mnesia:wait_for_tables(Tables, Timeout).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec copy_tables() -> ok.
copy_tables() ->
    Tables = lists:map(fun(Model) ->
        Table = model:table_name(Model),
        {atomic, ok} = mnesia:add_table_copy(Table, node(), disc_copies),
        Table
    end, model:get_models()),
    Timeout = onepanel_env:get(copy_tables_timeout),
    ok = mnesia:wait_for_tables(Tables, Timeout).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec delete_tables() -> ok | no_return().
delete_tables() ->
    lists:foreach(fun(Model) ->
        Table = model:table_name(Model),
        case mnesia:del_table_copy(Table, node()) of
            {atomic, ok} -> ok;
            {aborted, {no_exists, _}} -> ok;
            {aborted, Reason} -> ?throw(Reason)
        end
    end, model:get_models()).