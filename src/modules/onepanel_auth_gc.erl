%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for deleting inactive sessions and
%%% expired authorization nonce.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_auth_gc).
-author("Krzysztof Trzepla").

-behaviour(gen_server).

-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-record(state, {}).

% Cookies are long-lived (a week by default), perform cleaning more often.
-define(CLEANING_INTERVAL, gui_session:cookie_ttl() div 7).
-define(CLEAN_STALE_AUTH_REQ, clean_stale_auth).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link(?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @private @doc Initializes the server.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    schedule_stale_auth_cleaning(),
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @private @doc Handles call messages.
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
handle_call(Request, _From, State) ->
    ?log_bad_request(Request),
    {reply, {error, {invalid_request, Request}}, State}.


%%--------------------------------------------------------------------
%% @private @doc Handles cast messages.
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
%% @private @doc Handles all non call/cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info(?CLEAN_STALE_AUTH_REQ, #state{} = State) ->
    clean_sessions(),
    authorization_nonce:delete_expired_nonces(),
    schedule_stale_auth_cleaning(),
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
terminate(_Reason, _State) ->
    ok.


%%--------------------------------------------------------------------
%% @private @doc Converts process state when code is changed.
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
%% @private @doc Traverses sessions removing expired ones
%% and cleaning stale tokens in the active.
%% @end
%%--------------------------------------------------------------------
-spec clean_sessions() -> ok.
clean_sessions() ->
    Sessions = onepanel_session:list(),
    lists:foreach(fun(Session) ->
        case onepanel_session:is_active(Session) of
            false -> onepanel_session:delete(Session);
            true -> remove_expired_session_tokens(Session)
        end
    end, Sessions).


%% @private
-spec remove_expired_session_tokens(onepanel_session:record()) -> ok.
remove_expired_session_tokens(Session) ->
    Cleaned = onepanel_session:remove_expired_tokens(Session),
    onepanel_session:save(Cleaned).


%% @private
-spec schedule_stale_auth_cleaning() -> ok.
schedule_stale_auth_cleaning() ->
    erlang:send_after(?CLEANING_INTERVAL, self(), ?CLEAN_STALE_AUTH_REQ),
    ok.
