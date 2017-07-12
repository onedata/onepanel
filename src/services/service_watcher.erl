%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module is responsible for restarting registered services when
%%% not running.
%%% @end
%%%--------------------------------------------------------------------
-module(service_watcher).
-author("Krzysztof Trzepla").

-behaviour(gen_server).

-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0, register_service/1, unregister_service/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {
    services :: gb_sets:set(service:name())
}).

-define(TIMEOUT, timer:minutes(5)).

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
    gen_server:start_link({local, ?SERVICE_WATCHER_NAME}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Adds a service to a collection of supervised services.
%% @end
%%--------------------------------------------------------------------
-spec register_service(Service :: service:name()) -> ok.
register_service(Service) ->
    gen_server:call(?SERVICE_WATCHER_NAME, {register, Service}, ?TIMEOUT).

%%--------------------------------------------------------------------
%% @doc Removes a service from a collection of supervised services.
%% @end
%%--------------------------------------------------------------------
-spec unregister_service(Service :: service:name()) -> ok.
unregister_service(Service) ->
    gen_server:call(?SERVICE_WATCHER_NAME, {unregister, Service}, ?TIMEOUT).

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
    schedule_services_check(),
    {ok, #state{
        services = gb_sets:new()
    }}.

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
handle_call({register, Service}, _From, #state{services = Services} = State) ->
    {reply, ok, State#state{
        services = gb_sets:add_element(Service, Services)
    }};

handle_call({unregister, Service}, _From, #state{services = Services} = State) ->
    {reply, ok, State#state{
        services = gb_sets:del_element(Service, Services)
    }};

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
handle_info(check, #state{services = Services} = State) ->
    lists:foreach(fun(Service) ->
        Module = service:get_module(Service),
        case Module:status(#{}) of
            running -> ok;
            _ ->
                ?critical("Service ~p in not running. Restarting...", [Service]),
                try
                    Module:start(#{})
                catch
                    _:Reason ->
                        ?critical("Failed to restart service ~p due to: ~p",
                            [Service, Reason])
                end
        end
    end, gb_sets:to_list(Services)),
    schedule_services_check(),
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
%% @private @doc Schedules services status check.
%% @end
%%--------------------------------------------------------------------
-spec schedule_services_check() -> ok.
schedule_services_check() ->
    Delay = onepanel_env:get(services_check_delay),
    erlang:send_after(Delay, self(), check),
    ok.