%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module is a gen_server that connects separate Onepanel
%% nodes to create a cluster.
%% @end
%% ===================================================================
-module(onepanel).

-behaviour(gen_server).

-include("onepanel.hrl").
-include("onepanel_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% start_link/0
%% ====================================================================
%% @doc Starts the server
%% @end
-spec start_link() -> Result when
    Result :: {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?ONEPANEL_SERVER}, ?MODULE, [], []).


%% ====================================================================
%% gen_server callbacks
%% ====================================================================

%% init/0
%% ====================================================================
%% @doc Initializes the server
%% @end
-spec init(Args :: term()) -> Result when
    Result :: {ok, State :: #state{}} |
    {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} |
    ignore.
%% ====================================================================
init([]) ->
    try
        ok = db_logic:create(),
        {ok, Period} = application:get_env(?APP_NAME, connection_ping_period),
        {ok, Address} = application:get_env(?APP_NAME, multicast_address),
        {ok, Port} = application:get_env(?APP_NAME, onepanel_port),
        {ok, Socket} = gen_udp:open(Port, [binary, {reuseaddr, true}, {ip, Address},
            {multicast_loop, false}, {add_membership, {Address, {0, 0, 0, 0}}}]),
        ok = gen_udp:controlling_process(Socket, self()),
        ok = gen_udp:send(Socket, Address, Port, net_adm:localhost()),

        timer:send_after(1000 * Period, connection_ping),
        timer:send_after(1000, start_updater),

        {ok, #state{status = not_connected, socket = Socket, address = Address, port = Port}}
    catch
        _:_ -> {stop, initialization_error}
    end.


%% handle_call/3
%% ====================================================================
%% @doc Handling call messages
%% @end
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) -> Result when
    Result :: {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
%% ====================================================================
handle_call(get_status, _From, #state{status = Status} = State) ->
    {reply, Status, State};

handle_call(Request, _From, State) ->
    ?error("Wrong call: ~p", [Request]),
    {reply, {error, wrong_request}, State}.


%% handle_cast/2
%% ====================================================================
%% @doc Handling cast messages
%% @end
-spec handle_cast(Request :: term(), State :: #state{}) -> Result when
    Result :: {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
%% ====================================================================
handle_cast({connection_request, Node}, #state{status = not_connected} = State) ->
    ?info("Connection request from node: ~p", [Node]),
    db_logic:delete(),
    gen_server:cast({?ONEPANEL_SERVER, Node}, {connection_response, node()}),
    {noreply, State#state{status = waiting}};

handle_cast({connection_response, Node}, State) ->
    ?info("Connection response from node: ~p", [Node]),
    case db_logic:add_node(Node) of
        ok ->
            gen_server:cast({?ONEPANEL_SERVER, Node}, connection_acknowledgement),
            {noreply, State#state{status = connected}};
        _ ->
            {noreply, State}
    end;

handle_cast(connection_acknowledgement, State) ->
    ?info("Connection acknowledgement."),
    {noreply, State#state{status = connected}};

handle_cast(Request, State) ->
    ?error("Wrong cast: ~p", [Request]),
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc Handling all non call/cast messages
%% @end
-spec handle_info(Info :: timeout() | term(), State :: #state{}) -> Result when
    Result :: {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
%% ====================================================================
handle_info({udp, _Socket, _Address, _Port, HostBinary}, #state{status = Status} = State) ->
    Host = binary_to_list(HostBinary),
    Node = list_to_atom(?APP_STR ++ "@" ++ Host),
    case net_kernel:connect_node(Node) of
        true -> gen_server:cast({?ONEPANEL_SERVER, Node}, {connection_request, node()});
        Other -> ?error("Cannot connect node ~p: ~p", [Node, Other])
    end,
    case Status of
        connected -> {noreply, State};
        _ -> {noreply, State#state{status = waiting}}
    end;

handle_info(connection_ping, #state{status = not_connected, socket = Socket, address = Address, port = Port} = State) ->
    {ok, Period} = application:get_env(?APP_NAME, connection_ping_period),
    gen_udp:send(Socket, Address, Port, net_adm:localhost()),
    erlang:send_after(Period * 1000, self(), connection_ping),
    {noreply, State};

handle_info(connection_ping, State) ->
    {noreply, State};

handle_info(start_updater, State) ->
    updater:start(),
    timer:send_after(60 * 1000, start_updater),
    {noreply, State};

handle_info(Info, State) ->
    ?error("Wrong info: ~p", [Info]),
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> Result when
    Result :: term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc Convert process state when code is changed
%% @end
-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) -> Result when
    Result :: {ok, NewState :: #state{}} | {error, Reason :: term()}.
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
