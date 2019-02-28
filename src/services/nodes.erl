%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Utilities for selecting service nodes.
%%% @end
%%%--------------------------------------------------------------------
-module(nodes).
-author("Wojciech Geisler").

-include("names.hrl").
-include("service.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").

% @formatter:off
-type opts() :: #{
    service := service:name(),
    hosts => [service:host()]
}.
% @formatter:on

-export_type([opts/0]).

%% API
-export([local/1, any/1, all/1]).
-export([any_with/1, all_with/1]).
-export([service_to_node/2, service_to_nodes/2]).


%%--------------------------------------------------------------------
%% @doc Returns node name for given service on current host.
%% @end
%%--------------------------------------------------------------------
-spec local(ServiceName :: atom() | string()) -> Node :: node().
local(ServiceName) ->
    service_to_node(ServiceName, node()).


%%--------------------------------------------------------------------
%% @doc Returns one of given service's nodes.
%% Node living on current host is preferred.
%% If Opts contain 'hosts' key, nodes are selected from those hosts.
%% @end
%%--------------------------------------------------------------------
-spec any(ServiceNameOrOpts :: service:name() | opts()) -> {ok, node()} | #error{}.
any(ServiceName) when ?IS_SERVICE_NAME(ServiceName) ->
    any(#{service => ServiceName});

any(#{service := ServiceName} = Opts) ->
    case hosts:all(Opts) of
        [] -> ?make_error(?ERR_NO_SERVICE_HOSTS(ServiceName));
        Hosts ->
            Self = hosts:self(),
            Host = case lists:member(Self, Hosts) of
                true -> Self;
                false -> utils:random_element(Hosts)
            end,
            {ok, service_to_node(ServiceName, Host)}
    end.


%%--------------------------------------------------------------------
%% @doc Returns all nodes of given service
%% If Opts contain 'hosts' key, returned nodes are created from their
%% hostnames.
%% @end
%%--------------------------------------------------------------------
-spec all(ServiceNameOrOpts :: service:name() | opts()) -> [node()].
all(ServiceName) when ?IS_SERVICE_NAME(ServiceName) ->
    all(#{service => ServiceName});

all(Opts = #{service := ServiceName}) ->
    Hosts = hosts:all(Opts),
    service_to_nodes(ServiceName, Hosts).


%%--------------------------------------------------------------------
%% @doc Returns Onepanel node from a host with given service's node present.
%% Returns current node if the service has none.
%% @end
%%--------------------------------------------------------------------
-spec any_with(ServiceName :: service:name()) ->
    {ok, node()} | {fallback, node()}.
any_with(ServiceName) ->
    case any(ServiceName) of
        {ok, Node} -> {ok, service_to_node(?SERVICE_PANEL, Node)};
        _ -> {fallback, node()}
    end.


%%--------------------------------------------------------------------
%% @doc Returns all Onepanel nodes cohabiting hosts with given Service nodes.
%% @end
%%--------------------------------------------------------------------
-spec all_with(ServiceName :: service:name()) ->
    [node()].
all_with(ServiceName) ->
    service_to_nodes(?SERVICE_PANEL, all(ServiceName)).


%%--------------------------------------------------------------------
%% @doc Creates a node name from a service Name and a hostname.
%% @end
%%--------------------------------------------------------------------
-spec service_to_node(Name :: atom() | string(), HostOrNode :: service:host() | node()) ->
    Node :: node().
service_to_node(Name, Node) when is_atom(Node) ->
    service_to_node(Name, hosts:from_node(Node));

service_to_node(Name, Host) when is_atom(Name) ->
    service_to_node(atom_to_list(Name), Host);

service_to_node(Name, Host) ->
    erlang:list_to_atom(Name ++ "@" ++ Host).


%%--------------------------------------------------------------------
%% @doc Converts a list of hostnames to a list of nodes with given node name.
%% @end
%%--------------------------------------------------------------------
-spec service_to_nodes(Name :: atom() | string(), Hosts :: [service:host() | node()]) ->
    Nodes :: [node()].
service_to_nodes(Name, HostsOrNodes) ->
    [service_to_node(Name, HostOrNode) || HostOrNode <- HostsOrNodes].
