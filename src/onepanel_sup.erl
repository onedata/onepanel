%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module is responsible for onepanel workers supervision.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_sup).
-author("Krzysztof Trzepla").

-include_lib("ctool/include/logging.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(),
        [ChildSpec :: supervisor:child_spec()]}} | ignore.
init([]) ->
    % Initialization done here rather than in onepanel_app:start
    % because too long wait before spawning supervisor causes timeout
    % and exit of application

    service_onepanel:init_cluster(#{}),

    ?info("Waiting for distributed database to be ready"),
    onepanel_db:wait_for_tables(),
    onepanel_db:upgrade_tables(),

    https_listener:start(),
    onepanel_utils:wait_until(https_listener, healthcheck, [], {equal, ok},
        onepanel_env:get(rest_listener_status_check_attempts)),

    {ok, {#{strategy => one_for_all, intensity => 3, period => 1}, [
        service_executor_spec(),
        onepanel_cron_spec(),
        onepanel_session_gc_spec()
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Returns a worker child_spec for a service_executor gen_server.
%% @end
%%--------------------------------------------------------------------
-spec service_executor_spec() -> supervisor:child_spec().
service_executor_spec() ->
    #{
        id => service_executor,
        start => {service_executor, start_link, []},
        restart => transient,
        shutdown => timer:seconds(10),
        type => worker,
        modules => [service_executor]
    }.

%%--------------------------------------------------------------------
%% @private @doc Returns a worker child_spec for a onepanel_cron gen_server.
%% @end
%%--------------------------------------------------------------------
-spec onepanel_cron_spec() -> supervisor:child_spec().
onepanel_cron_spec() ->
    #{
        id => onepanel_cron,
        start => {onepanel_cron, start_link, []},
        restart => transient,
        shutdown => timer:seconds(10),
        type => worker,
        modules => [onepanel_cron]
    }.

%%--------------------------------------------------------------------
%% @private @doc Returns a worker child_spec for a onepanel_session_gc
%% gen_server.
%% @end
%%--------------------------------------------------------------------
-spec onepanel_session_gc_spec() -> supervisor:child_spec().
onepanel_session_gc_spec() ->
    #{
        id => onepanel_session_gc,
        start => {onepanel_session_gc, start_link, []},
        restart => permanent,
        shutdown => timer:seconds(10),
        type => worker,
        modules => [onepanel_session_gc]
    }.