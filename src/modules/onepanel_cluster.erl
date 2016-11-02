%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains cluster management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_cluster).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("names.hrl").

%% API
-export([node_to_host/0, node_to_host/1, nodes_to_hosts/0, nodes_to_hosts/1,
    host_to_node/1, host_to_node/2, hosts_to_nodes/1, hosts_to_nodes/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv node_to_host(node())
%% @end
%%--------------------------------------------------------------------
-spec node_to_host() -> Host :: service:host().
node_to_host() ->
    node_to_host(node()).


%%--------------------------------------------------------------------
%% @doc Returns a hostname of a node.
%% @end
%%--------------------------------------------------------------------
-spec node_to_host(Node :: node()) -> Host :: service:host().
node_to_host(Node) ->
    StrNode = erlang:atom_to_list(Node),
    [_Name, Host] = string:tokens(StrNode, "@"),
    Host.


%%--------------------------------------------------------------------
%% @doc @equiv nodes_to_hosts(onepanel:nodes())
%% @end
%%--------------------------------------------------------------------
-spec nodes_to_hosts() -> Hosts :: [service:host()].
nodes_to_hosts() ->
    nodes_to_hosts(service_onepanel:get_nodes()).


%%--------------------------------------------------------------------
%% @doc Converts a list of nodes to a list of hostnames.
%% @end
%%--------------------------------------------------------------------
-spec nodes_to_hosts(Nodes :: [node()]) -> Hosts :: [service:host()].
nodes_to_hosts(Nodes) ->
    lists:map(fun node_to_host/1, Nodes).


%%--------------------------------------------------------------------
%% @doc @equiv host_to_node(?APP_NAME, Host)
%% @end
%%--------------------------------------------------------------------
-spec host_to_node(Host :: service:host()) -> Node :: node().
host_to_node(Host) ->
    host_to_node(?APP_NAME, Host).


%%--------------------------------------------------------------------
%% @doc Creates a node name from a name and a hostname.
%% @end
%%--------------------------------------------------------------------
-spec host_to_node(Name :: atom() | string(), Host :: service:host()) ->
    Node :: node().
host_to_node(Name, Host) when is_atom(Name) ->
    host_to_node(erlang:atom_to_list(Name), Host);

host_to_node(Name, Host) ->
    erlang:list_to_atom(Name ++ "@" ++ Host).


%%--------------------------------------------------------------------
%% @doc @equiv hosts_to_node(?APP_NAME, Hosts)
%% @end
%%--------------------------------------------------------------------
-spec hosts_to_nodes(Hosts :: [service:host()]) -> Nodes :: [node()].
hosts_to_nodes(Hosts) ->
    hosts_to_nodes(?APP_NAME, Hosts).


%%--------------------------------------------------------------------
%% @doc Converts a list of hostnames to a list of nodes with a given node name.
%% @end
%%--------------------------------------------------------------------
-spec hosts_to_nodes(Name :: atom() | string(), Hosts :: [service:host()]) ->
    Nodes :: [node()].
hosts_to_nodes(Name, Hosts) ->
    lists:map(fun(Host) -> host_to_node(Name, Host) end, Hosts).