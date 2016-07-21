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
-module(service_onepanel).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("names.hrl").
-include("service.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([purge_node/1, create_tables/1, copy_tables/1, add_nodes/1,
    remove_nodes/1]).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see service_behaviour:name/0
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    ?APP_NAME.


%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_hosts/0
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    onepanel_cluster:nodes_to_hosts(get_nodes()).


%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_hosts/0
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    mnesia:system_info(db_nodes).


%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_steps/2
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := Hosts} = Ctx) ->
    [
        #steps{action = init_cluster},
        #steps{action = join_cluster,
            ctx = Ctx#{cluster_hosts => [hd(Hosts)], hosts => tl(Hosts)}}
    ];

get_steps(init_cluster, _Ctx) ->
    [
        #step{function = purge_node, selection = first},
        #step{function = create_tables, selection = first}
    ];

get_steps(join_cluster, #{hosts := Hosts, cluster_hosts := ClusterHosts} = Ctx) ->
    NewHosts = lists:filter(fun(Host) ->
        not lists:member(Host, ClusterHosts)
    end, Hosts),
    [
        #step{hosts = NewHosts, function = purge_node},
        #step{hosts = NewHosts, module = mnesia, function = start, args = []},
        #step{hosts = ClusterHosts, function = add_nodes, selection = first,
            ctx = Ctx#{hosts => NewHosts}},
        #step{hosts = NewHosts, function = copy_tables}
    ];

get_steps(join_cluster, Ctx) ->
    get_steps(join_cluster, Ctx#{cluster_hosts => get_hosts()});

get_steps(leave_cluster, #{hosts := Hosts, cluster_hosts := ClusterHosts} = Ctx) ->
    NewHosts = lists:filter(fun(Host) ->
        lists:member(Host, ClusterHosts)
    end, Hosts),
    [
        #step{hosts = NewHosts, function = purge_node},
        #step{hosts = ClusterHosts -- NewHosts, function = remove_nodes,
            selection = first, ctx = Ctx#{hosts => NewHosts}}
    ];

get_steps(leave_cluster, Ctx) ->
    get_steps(leave_cluster, Ctx#{cluster_hosts => get_hosts()});

get_steps(Action, _Ctx) ->
    ?throw({action_not_supported, Action}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec purge_node(Ctx :: service:ctx()) -> ok | no_return().
purge_node(_Ctx) ->
    Host = onepanel_cluster:node_to_host(),
    ok = mnesia:start(),
    delete_tables(),
    mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    onepanel_rpc:call_any(lists:delete(node(), get_nodes()), ?MODULE,
        remove_nodes, [#{hosts => [Host]}]).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec create_tables(Ctx :: service:ctx()) -> ok.
create_tables(_Ctx) ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),

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
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec copy_tables(Ctx :: service:ctx()) -> ok.
copy_tables(_Ctx) ->
    {atomic, ok} = mnesia:change_table_copy_type(schema, node(), disc_copies),
    Tables = lists:map(fun(Model) ->
        Table = model:table_name(Model),
        {atomic, ok} = mnesia:add_table_copy(Table, node(), disc_copies),
        Table
    end, model:get_models()),
    Timeout = onepanel_env:get(copy_tables_timeout),
    ok = mnesia:wait_for_tables(Tables, Timeout).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec add_nodes(Ctx :: service:ctx()) -> ok.
add_nodes(#{hosts := Hosts}) ->
    lists:foreach(fun(Host) ->
        Node = onepanel_cluster:host_to_node(Host),
        {ok, [Node]} = mnesia:change_config(extra_db_nodes, [Node])
    end, Hosts).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec remove_nodes(Ctx :: service:ctx()) -> ok.
remove_nodes(#{hosts := Hosts}) ->
    lists:foreach(fun(Host) ->
        Node = onepanel_cluster:host_to_node(Host),
        {atomic, ok} = mnesia:del_table_copy(schema, Node)
    end, Hosts).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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