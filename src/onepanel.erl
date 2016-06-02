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
-export([start_link/0]).
-export([connect_node/1, add_node/2, health_check/1, nodes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {
    ip :: inet:ip_address(),
    port :: inet:port_number(),
    socket :: inet:socket(),
    status :: idle | {connecting, reference()},
    ignored_nodes :: list(node())
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


add_node(Node, Force) ->
    case connect_node(Node) of
        ok -> db_manager:add_node(Node, Force);
        {error, Reason} -> {error, Reason}
    end.



health_check(Nodes) ->
    MissingNodes = Nodes -- onepanel:nodes(),
    case MissingNodes of
        [] -> ok;
        _ -> {error, {missing_nodes, MissingNodes}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec nodes() -> [node()].
nodes() ->
    db_manager:get_nodes().

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
        {ok, Address} = application:get_env(?APP_NAME, advertise_address),
        {ok, Port} = application:get_env(?APP_NAME, advertise_port),
        {ok, Ip} = inet:getaddr(Address, inet),
        {ok, Socket} = gen_udp:open(Port, [binary, {reuseaddr, true}, {ip, Ip},
            {multicast_loop, false}, {add_membership, {Ip, {0, 0, 0, 0}}}]),
        ok = gen_udp:controlling_process(Socket, self()),

        db_manager:create_db(),
        self() ! advertise,

        {ok, #state{ip = Ip, port = Port, socket = Socket, status = idle,
            ignored_nodes = []}}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize onepanel due to: ~p",
                [Reason]),
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
handle_call({join_db_cluster, Timestamp, Force}, _From, #state{status = idle} =
    State) ->

    case ignore_request(Timestamp, Force) of
        true ->
            ?info("Received join_db_cluster ~p and ignored",
                [{Timestamp, Force}]),
            {reply, ignore, State};
        false ->
            ?info("Received join_db_cluster ~p and accepted",
                [{Timestamp, Force}]),
            db_manager:delete_db(),
            Ref = schedule_join_db_cluster_timeout(),
            {reply, ok, State#state{status = {connecting, Ref}}}
    end;

handle_call({join_db_cluster, _Timestamp, _Force}, _From, #state{} = State) ->
    {reply, try_again, State};

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
handle_cast(join_db_cluster_ack, State) ->
    db_manager:commit_node(),
    db_manager:copy_tables(),
    {noreply, State#state{status = idle}};

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
handle_info({udp, _Socket, _Ip, _Port, Msg},
    #state{ignored_nodes = IgnoredNodes} = State) ->

    NewState = case verify_node(Msg, IgnoredNodes) of
        {ok, Node} ->
            ?info("Received advertiesment from node ~p", [Node]),
            case add_node(Node, false) of
                ok -> ?info("Node ~p successfully added.", [Node]), State;
                try_again ->
                    ?info("Try again adding node ~p", [Node]),
                    State;
                ignore ->
                    ?info("Ignore adding node ~p", [Node]),
                    State#state{ignored_nodes = [Node | IgnoredNodes]};
                {error, already_connected} -> State;
                {error, Reason} ->
                    ?error("Cannot add node ~p due to ~p.", [Node, Reason]),
                    State
            end;
        {error, Reason} ->
            case Reason of
                ignored_node -> ok;
                invalid_node -> ?debug("Cannot parse node name: ~p", [Msg]);
                {resolve, Hostname, Reason2} ->
                    ?warning("Cannot resolve hostname ~p due to: ~p",
                        [Hostname, Reason2])
            end,
            State
    end,
    {noreply, NewState};

handle_info(advertise, #state{ip = Ip, port = Port, socket = Socket} = State) ->
    gen_udp:send(Socket, Ip, Port, erlang:atom_to_binary(node(), utf8)),
    {ok, Delay} = application:get_env(?APP_NAME, advertise_max_delay),
    erlang:send_after(random:uniform(Delay), self(), advertise),
    {noreply, State};

handle_info({join_db_cluster_timeout, Ref}, #state{status = {connecting, Ref}} =
    State) ->
    db_manager:create_db(),
    {noreply, State#state{status = idle}};

handle_info({join_db_cluster_timeout, _Ref}, #state{} = State) ->
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

verify_node(Msg, IgnoredNodes) ->
    case binary:split(Msg, <<"@">>, [global]) of
        [_, Hostname] ->
            case inet:getaddr(binary_to_list(Hostname), inet) of
                {ok, _} ->
                    Node = binary_to_atom(Msg, utf8),
                    case lists:member(Node, IgnoredNodes) of
                        true -> {error, ignored_node};
                        false -> {ok, Node}
                    end;
                {error, Reason} ->
                    {error, {resolve, Hostname, Reason}}
            end;
        _ ->
            {error, invalid_node}
    end.

ignore_request(_Timestamp, true) ->
    false;

ignore_request(Timestamp, _Force) ->
    not db_manager:is_db_empty() orelse Timestamp > db_meta:get_timestamp().

schedule_join_db_cluster_timeout() ->
    Ref = erlang:make_ref(),
    {ok, Delay} = application:get_env(?APP_NAME, join_db_cluster_timeout),
    erlang:send_after(Delay, self(), {join_db_cluster_timeout, Ref}),
    Ref.
