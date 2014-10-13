%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements supervisor behaviour. It starts onepanel
%% gen_server as a child.
%% @end
%% ===================================================================
-module(onepanel_sup).

-behaviour(supervisor).

-include("registered_names.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% start_link/0
%% ====================================================================
%% @doc Starts the supervisor
%% @end
-spec start_link() -> Result when
    Result :: {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
%% ====================================================================
start_link() ->
    supervisor:start_link({local, ?ONEPANEL_SUP}, ?MODULE, []).


%% ====================================================================
%% Supervisor callbacks
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
-spec init(Args :: term()) -> Result when
    Result :: {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}.
%% ====================================================================
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Children = [
        {?ONEPANEL_SERVER, {?ONEPANEL_SERVER, start_link, []}, Restart, Shutdown, Type, [?ONEPANEL_SERVER]}
    ],

    {ok, {SupFlags, Children}}.
