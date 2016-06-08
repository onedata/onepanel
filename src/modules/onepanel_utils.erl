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
-include_lib("ctool/include/logging.hrl").

%% API
-export([node_to_host/0, node_to_host/1, nodes_to_hosts/0, nodes_to_hosts/1,
    host_to_node/1, host_to_node/2, hosts_to_nodes/1, hosts_to_nodes/2]).
-export([get_basic_auth_header/2]).
-export([wait_until/5, wait_until/6]).
-export([gen_uuid/0]).

-type expectation() :: {equal, Expected :: term()} | {validator,
    Validator :: fun((term()) -> term() | no_return())}.

-define(UUID_LEN, 32).

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
-spec host_to_node(Name :: atom() | string(), Host :: service:host()) ->
    Node :: node().
host_to_node(Name, Host) when is_atom(Name) ->
    host_to_node(erlang:atom_to_list(Name), Host);

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
-spec hosts_to_nodes(Name :: atom() | string(), Hosts :: [service:host()]) ->
    Nodes :: [node()].
hosts_to_nodes(Name, Hosts) ->
    lists:map(fun(Host) -> host_to_node(Name, Host) end, Hosts).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_basic_auth_header(Username :: string() | binary(),
    Password :: string() | binary()) -> {Key :: binary(), Value :: binary()}.
get_basic_auth_header(Username, Password) when is_list(Username)->
    get_basic_auth_header(erlang:list_to_binary(Username), Password);

get_basic_auth_header(Username, Password) when is_list(Password)->
    get_basic_auth_header(Username, erlang:list_to_binary(Password));

get_basic_auth_header(Username, Password) ->
    Hash = base64:encode(<<Username/binary, ":", Password/binary>>),
    {<<"Authorization">>, <<"Basic ", Hash/binary>>}.


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
        _:Reason ->
            timer:sleep(Delay),
            ?debug_stacktrace("Call ~p:~p(~p) returned unexpected result: ~p."
            " Retrying...", [Module, Function, Args, Reason]),
            wait_until(Module, Function, Args, {validator, Validator},
                Attempts - 1, Delay)
    end;

wait_until(Module, Function, Args, Expected, Attempts, Delay) ->
    wait_until(Module, Function, Args, {equal, Expected}, Attempts, Delay).


%%--------------------------------------------------------------------
%% @doc Generates random UUID.
%%--------------------------------------------------------------------
-spec gen_uuid() -> binary().
gen_uuid() ->
    http_utils:base64url_encode(crypto:rand_bytes(?UUID_LEN)).

