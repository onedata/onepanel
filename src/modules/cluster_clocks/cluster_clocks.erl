%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module handles synchronizing clocks of nodes in a cluster - it provides
%%% the generic mechanisms for both Onezone and Oneprovider cluster.
%%%
%%% The clocks of all nodes in the cluster are always synchronized with
%%% deterministically chosen master node, which coordinates the process.
%%% The clocks are synchronized in the following moments:
%%%
%%% 1) After the cluster is deployed and initialized (first run):
%%%    * Onezone - after all services are up
%%%    * Oneprovider - after successful registration in a Onezone service.
%%% 2) After the cluster is successfully resumed (next runs):
%%%    * Onezone - after all services are up
%%%    * Oneprovider - upon connection to Onezone (which is awaited infinitely).
%%% 3) Immediately after service startup - for each service apart from onepanel.
%%% 4) Periodically, with configured interval - the periodic sync is started
%%%    upon successful sync of 1) or 2).
%%% @end
%%%--------------------------------------------------------------------
-module(cluster_clocks).
-author("Lukasz Opiola").

-include("names.hrl").
-include("modules/models.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/onedata.hrl").

-export([synchronize_node_upon_start/1]).
-export([restart_periodic_sync/1]).
-export([run_periodic_sync/1]).

% cluster-specific callback run before every periodic clock sync, must return one of:
%   * the list of nodes to be synced
%   * 'skip' atom that causes the current attempt to be skipped
%   * 'abort' atom that causes the whole periodic sync to abort
%     (must be followed by a restart later)
-type prepare_cluster_clock_sync() :: fun(() -> {ok, [node()]} | skip | abort).

-define(CRON_JOB_NAME, ?MODULE).

-define(PERIODIC_SYNC_INTERVAL, timer:seconds(application:get_env(
    ?APP_NAME, clock_synchronization_interval_seconds, 600) % 10 minutes
)).

%%%===================================================================
%%% API
%%%===================================================================

-spec synchronize_node_upon_start(node()) -> ok | no_return().
synchronize_node_upon_start(Node) ->
    true = service_onepanel:run_on_master_node(fun() ->
        synchronize_node_with_self(Node)
    end),
    ?info("Synchronized clock on node ~p with master node upon startup", [Node]).


-spec restart_periodic_sync(prepare_cluster_clock_sync()) -> ok | no_return().
restart_periodic_sync(PrepareClusterClockSync) ->
    true = service_onepanel:run_on_master_node(fun() ->
        ?info("Awaiting initial cluster-wide clock synchronization..."),
        % run the first sync action (with retries) - this must finish with success, otherwise it
        % is better if the whole calling process crashes and fails to deploy / setup cluster
        Attempts = onepanel_env:get(initial_clock_synchronization_attempts),
        Delay = onepanel_env:get(initial_clock_synchronization_retry_delay),
        try
            onepanel_utils:wait_until(
                ?MODULE, run_periodic_sync, [PrepareClusterClockSync],
                {equal, true}, Attempts, Delay
            )
        catch throw:attempts_limit_exceeded ->
            ?critical("Timed out when waiting for clocks in the cluster to be synchronized", []),
            error(clocks_cannot_be_synchronized)
        end,
        ?info("Initial cluster-wide clock synchronization established successfully"),
        % ensure the previous periodic sync job is aborted
        abort_periodic_sync(),
        % schedule further periodic sync actions - they are best effort and
        % may fail, but the first run guarantees that at least one sync has succeeded
        ?info("Scheduling periodic cluster-wide clock synchronization"),
        ok = onepanel_cron:add_job(?CRON_JOB_NAME, fun() ->
            run_periodic_sync(PrepareClusterClockSync)
        end, ?PERIODIC_SYNC_INTERVAL),
        true
    end),
    ok.


-spec run_periodic_sync(prepare_cluster_clock_sync()) -> boolean().
run_periodic_sync(PrepareClusterClockSync) ->
    try
        ?debug("Preparing periodic clock sync..."),
        case PrepareClusterClockSync() of
            skip ->
                ?debug("Skipping periodic clock sync"),
                false;
            abort ->
                abort_periodic_sync(),
                ?info("Periodic clock sync has been aborted"),
                false;
            {ok, Nodes} ->
                ?debug("Running periodic clock sync for nodes: ~p", [Nodes]),
                WasSuccessful = synchronize_all_nodes_with_self(Nodes),
                case WasSuccessful of
                    true -> ?debug("Synchronized all clocks in the cluster with master node");
                    false -> ?warning("Failed to synchronize clocks of some nodes with master node")
                end,
                WasSuccessful
        end
    catch Class:Reason:Stacktrace ->
        ?error_stacktrace("Error while running periodic clock sync: ~w:~p", [Class, Reason], Stacktrace),
        false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec synchronize_all_nodes_with_self([node()]) -> boolean().
synchronize_all_nodes_with_self(Nodes) ->
    OtherNodes = Nodes -- [node()],
    lists:all(fun synchronize_node_with_self/1, OtherNodes).


%% @private
-spec synchronize_node_with_self(node()) -> boolean().
synchronize_node_with_self(Node) ->
    case global_clock:synchronize_remote_with_local(Node) of
        ok -> true;
        error -> false
    end.


%% @private
-spec abort_periodic_sync() -> ok.
abort_periodic_sync() ->
    % remove any previous periodic sync jobs across the cluster
    utils:rpc_multicall(service_onepanel:get_nodes(), onepanel_cron, remove_job, [?CRON_JOB_NAME]),
    ok.
