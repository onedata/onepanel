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
-module(onepanel_discovery).
-author("Krzysztof Trzepla").

-behaviour(gen_server).

-include("modules/logger.hrl").

%% API
-export([start_link/0]).
-export([get_nodes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {
    ip :: inet:ip_address(),
    port :: inet:port_number(),
    socket :: inet:socket(),
    nodes = sets:new() :: sets:set(node())
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    gen_server:call(?MODULE, get_nodes).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    try
        process_flag(trap_exit, true),

        Address = onepanel_env:get(advertise_address),
        Port = onepanel_env:get(advertise_port),
        {ok, Ip} = inet:getaddr(Address, inet),
        {ok, Socket} = gen_udp:open(Port, [binary, {reuseaddr, true}, {ip, Ip},
            {multicast_loop, false}, {add_membership, {Ip, {0, 0, 0, 0}}}]),
        ok = gen_udp:controlling_process(Socket, self()),

        self() ! advertise,

        {ok, #state{ip = Ip, port = Port, socket = Socket}}
    catch
        _:Reason ->
            ?log_error("Cannot initialize onepanel discovery due to: ~p",
                [Reason], true),
            {stop, Reason}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
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
    {reply, sets:to_list(Nodes), State};

handle_call(Request, _From, State) ->
    ?log_bad_request(Request),
    {reply, {invalid_request, Request}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(Request, State) ->
    ?log_bad_request(Request),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info({udp, _Socket, _Ip, _Port, Msg}, #state{nodes = Nodes} = State) ->
    NewState = case verify_node(Msg, Nodes) of
        {ok, Node} ->
            ?log_info("Discovered node ~p", [Node]),
            State#state{nodes = sets:add_element(Node, Nodes)};
        {error, already_discovered} ->
            State;
        {error, invalid_node} ->
            ?log_debug("Cannot parse node name: ~p", [Msg]),
            State;
        {error, {resolve, Hostname, Reason}} ->
            ?log_warning("Cannot resolve hostname ~p due to: ~p", [Hostname, Reason]),
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
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
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
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
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
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec verify_node(Msg :: binary(), Nodes :: sets:set(node())) ->
    {ok, Node :: node()} | {error, Reason :: term()}.
verify_node(Msg, Nodes) ->
    case binary:split(Msg, <<"@">>, [global]) of
        [_, Hostname] ->
            case inet:getaddr(erlang:binary_to_list(Hostname), inet) of
                {ok, _} ->
                    Node = erlang:binary_to_atom(Msg, utf8),
                    case sets:is_element(Node, Nodes) of
                        true -> {error, already_discovered};
                        false -> {ok, Node}
                    end;
                {error, Reason} ->
                    {error, {resolve, Hostname, Reason}}
            end;
        _ ->
            {error, invalid_node}
    end.
