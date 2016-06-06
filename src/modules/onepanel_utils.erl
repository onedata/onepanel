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
-module(onepanel_utils).
-author("Krzysztof Trzepla").

-include("onepanel.hrl").

%% API
-export([node_to_host/0, node_to_host/1, nodes_to_hosts/0, nodes_to_hosts/1,
    host_to_node/1, host_to_node/2, hosts_to_nodes/1, hosts_to_nodes/2]).
-export([wait_until/5, wait_until/6]).

-type expectation() :: {equal, Expected :: term()} | {validator,
    Validator :: fun((term()) -> term() | no_return())}.

%%%===================================================================
%%% API functions
%%%===================================================================

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
    NodeStr = erlang:atom_to_list(Node),
    [_Name, Host] = string:tokens(NodeStr, "@"),
    Host.


%%--------------------------------------------------------------------
%% @doc @equiv nodes_to_hosts(onepanel:nodes())
%%--------------------------------------------------------------------
-spec nodes_to_hosts() -> Hosts :: [service:host()].
nodes_to_hosts() ->
    nodes_to_hosts(onepanel:nodes()).


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
-spec host_to_node(Name :: string(), Host :: service:host()) -> Node :: node().
host_to_node(Name, Host) ->
    erlang:list_to_atom(string:join([Name, Host], "@")).


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
-spec hosts_to_nodes(Name :: string(), Hosts :: [service:host()]) ->
    Nodes :: [node()].
hosts_to_nodes(Name, Hosts) ->
    lists:map(fun(Host) -> host_to_node(Name, Host) end, Hosts).


%%--------------------------------------------------------------------
%% @doc @equiv wait_until(Module, Function, Args, Expectation, Attempts,
%% timer:seconds(1))
%% @end
%%--------------------------------------------------------------------
-spec wait_until(Module :: module(), Function :: atom(), Args :: list(),
    Expectation :: expectation(), Attempts :: integer()) -> ok | no_return().
wait_until(Module, Function, Args, Expectation, Attempts) ->
    wait_until(Module, Function, Args, Expectation, Attempts, timer:seconds(1)).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec wait_until(Module :: module(), Function :: atom(), Args :: list(),
    Expectation :: expectation(), Attempts :: integer(), Delay :: integer()) ->
    ok | no_return().
wait_until(_Module, _Function, _Args, _Expectation, Attempts, _Delay) when
    Attempts =< 0 ->
    throw(attempts_limit_exceeded);

wait_until(Module, Function, Args, {equal, Expected}, Attempts, Delay) ->
    wait_until(Module, Function, Args,
        {validator, fun(Result) -> Expected = Result end}, Attempts, Delay);

wait_until(Module, Function, Args, {validator, Validator}, Attempts, Delay) ->
    try
        Result = erlang:apply(Module, Function, Args),
        Validator(Result),
        ok
    catch
        _:_ ->
            timer:sleep(Delay),
            wait_until(Module, Function, Args, {validator, Validator},
                Attempts - 1, Delay)
    end;

wait_until(Module, Function, Args, Expected, Attempts, Delay) ->
    wait_until(Module, Function, Args, {equal, Expected}, Attempts, Delay).
