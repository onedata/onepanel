%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module is a gen_server that connects separate onepanel
%% nodes to create a cluster.
%% @end
%% ===================================================================
-module(onepanel).

-behaviour(gen_server).

-include("onepanel.hrl").
-include("onepanel_modules/installer/state.hrl").
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
        {ok, UpdaterStartDelay} = application:get_env(?APP_NAME, updater_start_delay),
        {ok, Address} = application:get_env(?APP_NAME, multicast_address),
        {ok, Port} = application:get_env(?APP_NAME, onepanel_port),
        {ok, Socket} = gen_udp:open(Port, [binary, {reuseaddr, true}, {ip, Address},
            {multicast_loop, false}, {add_membership, {Address, {0, 0, 0, 0}}}]),
        ok = gen_udp:controlling_process(Socket, self()),

        self() ! connection_ping,
        timer:send_after(UpdaterStartDelay, start_updater),

        {ok, #state{socket = Socket, address = Address, port = Port}}
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
handle_call(get_timestamp, _From, State) ->
    Timestamp = installer_utils:get_timestamp(),
    {reply, Timestamp, State};

handle_call({get_password, Username}, _From, #state{passwords = Passwords} = State) ->
    {reply, {ok, proplists:get_value(Username, Passwords)}, State};

handle_call({set_password, Username, Password}, _From, #state{passwords = Passwords} = State) ->
    {reply, ok, State#state{passwords = [{Username, Password} | proplists:delete(Username, Passwords)]}};

handle_call(Request, _From, State) ->
    ?warning("[onepanel] Wrong call: ~p", [Request]),
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
handle_cast({remove_password, Username}, #state{passwords = Passwords} = State) ->
    {noreply, State#state{passwords = proplists:delete(Username, Passwords)}};

handle_cast({connection_request, Node}, State) ->
    ?info("[onepanel] Connection request from node: ~p", [Node]),
    LocalTimestamp = installer_utils:get_timestamp(),
    RemoteTimestamp = gen_server:call({?ONEPANEL_SERVER, Node}, get_timestamp),
    case RemoteTimestamp >= LocalTimestamp of
        true ->
            db_logic:delete(),
            gen_server:cast({?ONEPANEL_SERVER, Node}, {connection_response, node()});
        _ ->
            ok
    end,
    {noreply, State};

handle_cast({connection_response, Node}, State) ->
    ?info("[onepanel] Connection response from node: ~p", [Node]),
    case db_logic:add_node(Node) of
        ok ->
            ?info("[onepanel] Node ~p successfully added to database cluster", [Node]);
        Other ->
            ?error("[onepanel] Cannot add node ~p to database cluster: ~p", [Node, Other])
    end,
    {noreply, State};

handle_cast(Request, State) ->
    ?warning("[onepanel] Wrong cast: ~p", [Request]),
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
handle_info({udp, _Socket, _Address, _Port, HostBinary}, State) ->
    Host = binary_to_list(HostBinary),
    case inet:getaddr(Host, inet) of
        {ok, _} ->
            Node = onepanel_utils:get_node(Host),
            case lists:member(Node, db_logic:get_nodes()) of
                false ->
                    case net_kernel:connect_node(Node) of
                        true -> gen_server:cast({?ONEPANEL_SERVER, Node}, {connection_request, node()});
                        Other -> ?error("[onepanel] Cannot connect node ~p: ~p", [Node, Other])
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    {noreply, State};

handle_info(connection_ping, #state{socket = Socket, address = Address, port = Port} = State) ->
    {ok, Delay} = application:get_env(?APP_NAME, connection_ping_delay),
    gen_udp:send(Socket, Address, Port, net_adm:localhost()),
    erlang:send_after(Delay, self(), connection_ping),
    {noreply, State};

handle_info(start_updater, State) ->
    updater:start(),
    {ok, Delay} = application:get_env(?APP_NAME, updater_lookup_delay),
    timer:send_after(Delay, start_updater),
    {noreply, State};

handle_info(Info, State) ->
    ?warning("[onepanel] Wrong info: ~p", [Info]),
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
