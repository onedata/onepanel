%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code
%% @end
%% ===================================================================
-module(spanel).

-behaviour(gen_server).

-include("registered_names.hrl").
-include("spanel_modules/db.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {status = connected}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  try
    ok = db_logic:create(),
    {ok, Address} = application:get_env(?APP_NAME, multicast_address),
    {ok, Port} = application:get_env(?APP_NAME, spanel_port),
    {ok, Socket} = gen_udp:open(Port, [binary, {reuseaddr, true}, {ip, Address},
      {multicast_loop, false}, {add_membership, {Address, {0, 0, 0, 0}}}]),
    ok = gen_udp:controlling_process(Socket, self()),
    ok = gen_udp:send(Socket, Address, Port, net_adm:localhost()),
    {ok, #state{}}
  catch
    _:_ -> {stop, initialization_error}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(get_hosts, _From, #state{status = connected} = State) ->
  {reply, install_utils:get_hosts(), State};
handle_call({authenticate, Username, Password}, _From, #state{status = connected} = State) ->
  {reply, user_logic:authenticate(Username, Password), State};
handle_call({change_password, Username, OldPassword, NewPassword}, _From, #state{status = connected} = State) ->
  {reply, user_logic:change_password(Username, OldPassword, NewPassword), State};
handle_call({set_ulimits, Hosts, OpenFiles, Processes}, _From, #state{status = connected} = State) ->
  {reply, install_utils:set_ulimits_on_hosts(Hosts, OpenFiles, Processes), State};
handle_call({create_storage_test_file, Path}, _From, #state{status = connected} = State) ->
  {reply, install_storage:create_storage_test_file(Path), State};
handle_call({check_storage, FilePath, Content}, _From, #state{status = connected} = State) ->
  {reply, install_storage:check_storage_on_host(FilePath, Content), State};
handle_call({add_storage, Hosts, Paths}, _From, #state{status = connected} = State) ->
  {reply, install_storage:add_storage_paths_on_hosts(Hosts, Paths), State};
handle_call({install_ccms, MainCCM, OptCCMs, Dbs}, _From, #state{status = connected} = State) ->
  {reply, install_veil:install_veil_nodes([MainCCM | OptCCMs], ccm, MainCCM, OptCCMs, Dbs), State};
handle_call({start_ccms, Hosts}, _From, #state{status = connected} = State) ->
  {reply, install_veil:start_veil_nodes(Hosts, ccm), State};
handle_call({install_workers, MainCCM, OptCCMs, Workers, Dbs}, _From, #state{status = connected} = State) ->
  {reply, install_veil:install_veil_nodes(Workers, worker, MainCCM, OptCCMs, Dbs), State};
handle_call({start_workers, Hosts}, _From, #state{status = connected} = State) ->
  {reply, install_veil:start_veil_nodes(Hosts, worker), State};
handle_call({install_dbs, Hosts}, _From, State) ->
  {reply, install_db:install_dbs(Hosts), State};
handle_call(_Request, _From, State) ->
  {reply, {error, wrong_request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({connection_request, Node}, #state{status = connected} = State) ->
  lager:info("Connection request from node: ~p", [Node]),
  case db_logic:get_nodes() of
    [_] -> case db_logic:delete() of
             ok ->
               gen_server:cast({?SPANEL_NAME, Node}, {connection_response, node()}),
               {noreply, State#state{status = not_connected}};
             _ ->
               {noreply, State#state{status = banned}}
           end;
    _ -> {noreply, State}
  end;
handle_cast({connection_response, Node}, #state{status = connected} = State) ->
  lager:info("Connection response from node: ~p", [Node]),
  case db_logic:add_node(Node) of
    ok -> gen_server:cast({?SPANEL_NAME, Node}, connection_acknowledgement);
    _ -> ok
  end,
  {noreply, State};
handle_cast(connection_acknowledgement, State) ->
  lager:info("Connection acknowledgement."),
  {noreply, State#state{status = connected}};
handle_cast({delete_storage_test_file, Path}, #state{status = connected} = State) ->
  install_storage:delete_storage_test_file(Path),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({udp, _Socket, _Address, _Port, <<Host/binary>>}, #state{status = connected} = State) ->
  Node = binary_to_atom(<<"spanel@", Host/binary>>, latin1),
  case net_kernel:connect_node(Node) of
    true -> gen_server:cast({?SPANEL_NAME, Node}, {connection_request, node()});
    _ -> lager:error("Can not connect SPanel node: ~p.", [Node])
  end,
  {noreply, State};
handle_info({udp, _Socket, _Address, _Port, <<_Host/binary>>}, State) ->
  {noreply, State};
handle_info(Info, State) ->
  lager:error("Wrong info: ~p", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
