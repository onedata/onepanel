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
-export([node_to_host/0, node_to_host/1, nodes_to_hosts/1]).
-export([service_to_node/1, service_to_node/2,  service_to_nodes/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv service_to_node(Host, node())
%% @end
%%--------------------------------------------------------------------
-spec service_to_node(Name :: atom() | string()) -> Node :: node().
service_to_node(Name) ->
    service_to_node(Name, node()).


%%--------------------------------------------------------------------
%% @doc Creates a node name from a service Name and a hostname.
%% @end
%%--------------------------------------------------------------------
-spec service_to_node(Name :: atom() | string(), HostOrNode :: service:host() | node()) ->
    Node :: node().
service_to_node(Name, Node) when is_atom(Node) ->
    service_to_node(Name, node_to_host(Node));

service_to_node(Name, Host) when is_atom(Name) ->
    service_to_node(atom_to_list(Name), Host);

service_to_node(Name, Host) ->
    erlang:list_to_atom(Name ++ "@" ++ Host).


%%--------------------------------------------------------------------
%% @doc Converts a list of hostnames to a list of nodes with given node name.
%% @end
%%--------------------------------------------------------------------
-spec service_to_nodes(Name :: atom() | string(), Hosts :: [service:host()]) ->
    Nodes :: [node()].
service_to_nodes(Name, HostsOrNodes) ->
    lists:map(fun(HostOrNode) -> service_to_node(Name, HostOrNode) end, HostsOrNodes).


%%--------------------------------------------------------------------
%% @doc @equiv node_to_host(node())
%% @end
%%--------------------------------------------------------------------
-spec node_to_host() -> Host :: service:host().
node_to_host() ->
    node_to_host(node()).


%%--------------------------------------------------------------------
%% @doc Returns the hostname of a node.
%% @end
%%--------------------------------------------------------------------
-spec node_to_host(Node :: node()) -> Host :: service:host().
node_to_host(Node) ->
    StrNode = erlang:atom_to_list(Node),
    [_Name, Host] = string:tokens(StrNode, "@"),
    Host.


%%--------------------------------------------------------------------
%% @doc Converts a list of nodes to a list of hostnames.
%% @end
%%--------------------------------------------------------------------
-spec nodes_to_hosts(Nodes :: [node()]) -> Hosts :: [service:host()].
nodes_to_hosts(Nodes) ->
    lists:map(fun node_to_host/1, Nodes).
