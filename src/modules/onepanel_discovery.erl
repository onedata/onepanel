%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module is responsible discovery of a cluster nodes using multicast
%%% advertisement.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_discovery).
-author("Krzysztof Trzepla").

-behaviour(gen_server).

-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0]).
-export([get_nodes/0, get_hosts/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {
    ip :: inet:ip_address(),
    port :: inet:port_number(),
    socket :: inet:socket(),
    nodes = gb_sets:new() :: gb_sets:set(node())
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc Returns a list of discovered nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    gen_server:call(?MODULE, get_nodes).


%%--------------------------------------------------------------------
%% @doc Returns a list of discovered hosts.
%% @end
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    onepanel_cluster:nodes_to_hosts(get_nodes()).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Initializes the server.
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    try
        Address = onepanel_env:get(advertise_address),
        Port = onepanel_env:get(advertise_port),
        {ok, Ip} = inet:getaddr(Address, inet),
        {ok, Socket} = gen_udp:open(Port, [binary, {reuseaddr, true}, {ip, Ip},
            {multicast_loop, false}, {add_membership, {Ip, {0, 0, 0, 0}}}]),

        self() ! advertise,

        Nodes = gb_sets:from_list([node()]),
        {ok, #state{nodes = Nodes, ip = Ip, port = Port, socket = Socket}}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize onepanel discovery due to: ~p",
                [Reason]),
            {stop, Reason}
    end.


%%--------------------------------------------------------------------
%% @private @doc Handles call messages.
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_call(get_nodes, _From, #state{nodes = Nodes} = State) ->
    {reply, gb_sets:to_list(Nodes), State};

handle_call(Request, _From, State) ->
    ?log_bad_request(Request),
    {reply, {error, {invalid_request, Request}}, State}.


%%--------------------------------------------------------------------
%% @private @doc Handles cast messages.
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(Request, State) ->
    ?log_bad_request(Request),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private @doc Handles all non call/cast messages.
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info({udp, _Socket, _Ip, _Port, Msg}, #state{nodes = Nodes} = State) ->
    NewState = case verify_node(Msg, Nodes) of
        {ok, Node} ->
            ?info("Discovered node ~p", [Node]),
            State#state{nodes = gb_sets:add_element(Node, Nodes)};
        {error, already_discovered} ->
            State;
        {error, invalid_node} ->
            ?debug("Cannot parse node name: ~p", [Msg]),
            State;
        {error, {resolve, Hostname, Reason}} ->
            ?warning("Cannot resolve hostname ~p due to: ~p", [Hostname, Reason]),
            State
    end,
    {noreply, NewState};

handle_info(advertise, #state{ip = Ip, port = Port, socket = Socket} = State) ->
    gen_udp:send(Socket, Ip, Port, erlang:atom_to_binary(node(), utf8)),
    Delay = onepanel_env:get(advertise_max_delay),
    erlang:send_after(random:uniform(Delay), self(), advertise),
    {noreply, State};

handle_info(Info, State) ->
    ?log_bad_request(Info),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term().
terminate(Reason, State) ->
    ?log_terminate(Reason, State).


%%--------------------------------------------------------------------
%% @private @doc Converts process state when code is changed
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Checks whether received message is a valid name of a node,
%% which yet has not been discovered.
%% @end
%%--------------------------------------------------------------------
-spec verify_node(Msg :: binary(), Nodes :: gb_sets:set(node())) ->
    {ok, Node :: node()} | {error, Reason :: term()}.
verify_node(Msg, Nodes) ->
    case binary:split(Msg, <<"@">>, [global]) of
        [_, Hostname] ->
            case inet:getaddr(erlang:binary_to_list(Hostname), inet) of
                {ok, _} ->
                    Node = erlang:binary_to_atom(Msg, utf8),
                    case gb_sets:is_element(Node, Nodes) of
                        true -> {error, already_discovered};
                        false -> {ok, Node}
                    end;
                {error, Reason} ->
                    {error, {resolve, Hostname, Reason}}
            end;
        _ ->
            {error, invalid_node}
    end.
