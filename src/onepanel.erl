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
-module(onepanel).
-author("Krzysztof Trzepla").

-behaviour(gen_server).

-include("onepanel.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0, nodes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {
    ip :: inet:ip_address(),
    port :: inet:port_number(),
    socket :: inet:socket()
}).

%%%===================================================================
%%% API
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
-spec nodes() -> [node()].
nodes() ->
    [node() | erlang:nodes()].

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
    {ok, Address} = application:get_env(?APP_NAME, multicast_address),
    {ok, Port} = application:get_env(?APP_NAME, multicast_port),
    {ok, Ip} = inet:getaddr(Address, inet),
    {ok, Socket} = gen_udp:open(Port, [binary, {reuseaddr, true}, {ip, Ip},
        {multicast_loop, false}, {add_membership, {Ip, {0, 0, 0, 0}}}]),
    ok = gen_udp:controlling_process(Socket, self()),

    self() ! multicast_ping,

    {ok, #state{ip = Ip, port = Port, socket = Socket}}.

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
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_cast(_Request, State) ->
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
handle_info({udp, _Socket, _Ip, _Port, Msg}, State) ->
    case binary:split(Msg, <<"@">>, [global]) of
        [_, Hostname] ->
            case inet:getaddr(binary_to_list(Hostname), inet) of
                {ok, _} ->
                    Node = binary_to_atom(Msg, latin1),
                    case connect_node(Node) of
                        ok ->
                            ?info("Node ~p successfully connected.", [Node]);
                        {error, already_connected} ->
                            ok;
                        {error, Reason} ->
                            ?error("Cannot connect node ~p due to ~p.",
                                [Node, Reason])
                    end;
                {error, Reason} ->
                    ?warning("Cannot resolve host ~p due to: ~p",
                        [Hostname, Reason])
            end;
        _ ->
            ok
    end,
    {noreply, State};

handle_info(multicast_ping, #state{ip = Ip, port = Port, socket = Socket} =
    State) ->
    {ok, Delay} = application:get_env(?APP_NAME, multicast_ping_delay_seconds),
    gen_udp:send(Socket, Ip, Port, erlang:atom_to_binary(node(), latin1)),
    erlang:send_after(timer:seconds(Delay), self(), multicast_ping),
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
terminate(_Reason, _State) ->
    ok.

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
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec connect_node(Node :: node()) -> ok | {error, Reason :: term()}.
connect_node(Node) ->
    case lists:member(Node, onepanel:nodes()) of
        true ->
            {error, already_connected};
        false ->
            case {net_kernel:connect_node(Node), net_adm:ping(Node)} of
                {true, pong} -> ok;
                {false, pong} -> {error, connection_failed};
                {_, pang} -> {error, not_responding_to_pings}
            end
    end.
