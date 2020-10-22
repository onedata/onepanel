%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module handles synchronizing clocks of nodes in Oneprovider cluster.
%%% All nodes are periodically synchronized with the designated master node (in
%%% the context of clocks sync) - see the cluster_clocks module for more info.
%%% Apart from that, the master node is periodically synchronized with the
%%% Onezone service, so that all nodes are synchronized with the global Onezone
%%% time.
%%% @end
%%%--------------------------------------------------------------------
-module(oneprovider_cluster_clocks).
-author("Lukasz Opiola").

-include("names.hrl").
-include("modules/models.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/onedata.hrl").

-export([synchronize_node_upon_start/1]).
-export([restart_periodic_sync/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec synchronize_node_upon_start(node()) -> ok | no_return().
synchronize_node_upon_start(Node) ->
    case is_master_synchronized_with_onezone() of
        true ->
            cluster_clocks:synchronize_node_upon_start(Node);
        false ->
            ?info("Deferring clock sync with node ~p until Onezone connection is established", [Node])
    end.


-spec restart_periodic_sync() -> ok | no_return().
restart_periodic_sync() ->
    cluster_clocks:restart_periodic_sync(?MODULE, fun resolve_nodes_to_sync/0).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Clocks sync is done only when the master node is synchronized with Onezone's
%% global time, which is done after Onezone connection is established.
%% @end
%%--------------------------------------------------------------------
-spec resolve_nodes_to_sync() -> {ok, [node()]} | skip.
resolve_nodes_to_sync() ->
    case ensure_master_synced_with_onezone() of
        true -> {ok, service_nodes_to_sync()};
        false -> skip
    end.


%% @private
-spec ensure_master_synced_with_onezone() -> boolean().
ensure_master_synced_with_onezone() ->
    % Although the periodic sync for Oneprovider is started only after connection
    % to Onezone is established, in come cases the sync attempt itself may fail
    case clock:synchronize_local_with_remote_server(fun fetch_zone_time/0) of
        ok ->
            ?info("Successfully synchronized clock on the master node with Onezone"),
            true;
        error ->
            case is_master_synchronized_with_onezone() of
                false ->
                    ?warning(
                        "Unable to synchronize clock on the master node with Onezone - "
                        "deferring cluster-wide clock synchronization"
                    ),
                    false;
                true ->
                    ?warning(
                        "Unable to synchronize clock on the master node with Onezone - "
                        "relying on current clock sync state"
                    ),
                    true
            end
    end.


%% @private
-spec fetch_zone_time() -> clock:millis().
fetch_zone_time() ->
    {ok, Timestamp} = oz_providers:get_zone_time(none),
    Timestamp.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if at least one clock sync with Onezone was successful, or the
%% node has restarted and restored the previous synchronization.
%% @end
%%--------------------------------------------------------------------
-spec is_master_synchronized_with_onezone() -> boolean().
is_master_synchronized_with_onezone() ->
    cluster_clocks:run_on_master(fun clock:is_synchronized/0).


%% @private
-spec service_nodes_to_sync() -> [node()].
service_nodes_to_sync() ->
    lists:flatten([
        service_onepanel:get_nodes(),
        service_cluster_manager:get_nodes(),
        service_op_worker:get_nodes()
    ]).