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
-module(onepanel_sup).
-author("Krzysztof Trzepla").

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
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(),
        [ChildSpec :: supervisor:child_spec()]}} | ignore.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 3, period => 1}, [
        onepanel_spec(),
        service_executor_spec()
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a worker child_spec for a onepanel gen_server.
%% @end
%%--------------------------------------------------------------------
-spec onepanel_spec() -> supervisor:child_spec().
onepanel_spec() ->
    #{
        id => onepanel,
        start => {onepanel, start_link, []},
        restart => transient,
        shutdown => timer:seconds(10),
        type => worker,
        modules => [onepanel]
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a worker child_spec for a service_executor gen_server.
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
