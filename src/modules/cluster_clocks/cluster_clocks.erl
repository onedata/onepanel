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

-type resolve_nodes_to_sync() :: fun(() -> {ok, [node()]} | skip).

-export([synchronize_node_upon_start/1]).
-export([restart_periodic_sync/2]).
-export([run_on_master/1]).

-define(PERIODIC_SYNC_INTERVAL, timer:seconds(application:get_env(
    ?APP_NAME, clock_synchronization_interval_seconds, 600) % 10 minutes
)).

%%%===================================================================
%%% API
%%%===================================================================

-spec synchronize_node_upon_start(node()) -> ok | no_return().
synchronize_node_upon_start(Node) ->
    true = run_on_master(fun() ->
        synchronize_node_with_self(Node)
    end),
    ?info("Successfully synchronized clock on node ~p with master node", [Node]).


-spec restart_periodic_sync(onepanel_cron:job_name(), resolve_nodes_to_sync()) -> ok | no_return().
restart_periodic_sync(Name, ResolveNodesToSync) ->
    true = run_on_master(fun() ->
        PeriodicAction = fun() ->
            case ResolveNodesToSync() of
                skip ->
                    ?debug("Skipping periodic clock sync"),
                    true;
                {ok, Nodes} ->
                    WasSuccessful = sync_nodes_with_self(Nodes),
                    case WasSuccessful of
                        true -> ?info("Successfully synchronized all clocks in the cluster with master node");
                        false -> ?warning("Failed to synchronize clocks of some nodes with master node")
                    end,
                    WasSuccessful
            end
        end,
        % run the first sync action - this must finish with success
        true = PeriodicAction(),
        % remove any previous periodic sync jobs across the cluster
        rpc:multicall(service_onepanel:get_nodes(), onepanel_cron, remove_job, [?MODULE]),
        % schedule further periodic sync actions - they are best effort and
        % may fail, but the first run guarantees that at least one sync has succeeded
        onepanel_cron:add_job(Name, PeriodicAction, ?PERIODIC_SYNC_INTERVAL),
        true
    end),
    ok.


-spec run_on_master(fun(() -> boolean())) -> boolean() | no_return().
run_on_master(Fun) ->
    case get_master_node() of
        Self when node() =:= Self ->
            try
                Fun()
            catch Class:Reason ->
                ?error_stacktrace("Unexpected error in ~p - ~w:~p", [?MODULE, Class, Reason]),
                false
            end;
        MasterNode ->
            case rpc:call(MasterNode, ?MODULE, ?FUNCTION_NAME, [Fun]) of
                % all internal functions in this module return a boolean -
                % crash in case of any problems with RPC
                Result when is_boolean(Result) -> Result
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec sync_nodes_with_self([node()]) -> boolean().
sync_nodes_with_self(Nodes) ->
    try
        run_for_other_nodes(fun synchronize_node_with_self/1, Nodes)
    catch Class:Reason ->
        ?error_stacktrace("Error while running periodic clock sync: ~w:~p", [Class, Reason]),
        false
    end.


%% @private
-spec synchronize_node_with_self(node()) -> boolean().
synchronize_node_with_self(Node) ->
    case clock:synchronize_remote_with_local(Node) of
        ok -> true;
        error -> false
    end.


%% @private
-spec run_for_other_nodes(fun((node()) -> boolean()), [node()]) -> boolean().
run_for_other_nodes(Callback, Nodes) ->
    OtherNodes = Nodes -- [node()],
    lists:all(Callback, OtherNodes).


%% @private
-spec get_master_node() -> node().
get_master_node() ->
    hd(lists:sort(service_onepanel:get_nodes())).
