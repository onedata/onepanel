%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Utilities for selecting service hosts.
%%% @end
%%%--------------------------------------------------------------------
-module(hosts).
-author("Wojciech Geisler").

-include("service.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").

%% API
-export([self/0, all/1]).
-export([from_node/1, from_nodes/1]).


%%--------------------------------------------------------------------
%% @doc Returns hostname of this node.
%% @end
%%--------------------------------------------------------------------
-spec self() -> service:host().
self() ->
    from_node(node()).


%%--------------------------------------------------------------------
%% @doc Returns hosts for given service. Opts:
%% 'service' - name of the service
%% 'hosts' - overrides returned hosts list
%% @end
%%--------------------------------------------------------------------
-spec all(ServiceNameOrOpts :: service:name() | Opts) -> [service:host()] when
    Opts :: #{
    service := service:name(),
    hosts => [service:host()]
    }.
all(ServiceName) when ?IS_SERVICE_NAME(ServiceName) ->
    all(#{service => ServiceName});

all(#{service := _ServiceName, hosts := Hosts}) ->
    Hosts;

all(#{service := ServiceName}) ->
    Module = service:get_module(ServiceName),
    Module:get_hosts().


%%--------------------------------------------------------------------
%% @doc Returns the hostname of a node.
%% @end
%%--------------------------------------------------------------------
-spec from_node(Node :: node()) -> Host :: service:host().
from_node(Node) ->
    StrNode = erlang:atom_to_list(Node),
    [_Name, Host] = string:tokens(StrNode, "@"),
    Host.


%%--------------------------------------------------------------------
%% @doc Converts a list of nodes to a list of hostnames.
%% @end
%%--------------------------------------------------------------------
-spec from_nodes(Nodes :: [node()]) -> Hosts :: [service:host()].
from_nodes(Nodes) ->
    lists:map(fun from_node/1, Nodes).