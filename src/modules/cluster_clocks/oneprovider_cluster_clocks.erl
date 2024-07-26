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
            ?info("Deferring clock sync with node ~tp until Onezone connection is established", [Node])
    end.


-spec restart_periodic_sync() -> ok | no_return().
restart_periodic_sync() ->
    cluster_clocks:restart_periodic_sync(fun prepare_cluster_clock_sync/0).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the cluster nodes to be synced, but only if the master node is
%% synchronized with Onezone's global time. Otherwise, cluster clock sync is
%% skipped for until the next periodic attempt.
%% @end
%%--------------------------------------------------------------------
-spec prepare_cluster_clock_sync() -> {ok, [node()]} | skip.
prepare_cluster_clock_sync() ->
    case service_oneprovider:is_registered() of
        false ->
            ?warning("Aborting periodic clock sync since the Oneprovider has been deregistered"),
            % the periodic clock sync will be restarted upon the next registration
            abort;
        true ->
            case revise_master_sync_with_onezone() of
                true -> {ok, service_nodes_to_sync()};
                false -> skip
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts clock synchronization with Onezone. In case of failure, tries to
%% reuse the previous synchronization (given that it has succeeded at least
%% once). Returns true if the master node's clock can be perceived as
%% synchronized with Onezone after the process.
%% @end
%%--------------------------------------------------------------------
-spec revise_master_sync_with_onezone() -> boolean().
revise_master_sync_with_onezone() ->
    case global_clock:synchronize_local_with_remote_server(fun fetch_zone_time/0) of
        ok ->
            ?debug("Synchronized clock on the master node with Onezone"),
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
-spec fetch_zone_time() -> {ok, time:millis()} | {error, term()}.
fetch_zone_time() ->
    case oz_endpoint:request(none, "/provider/public/get_current_time", get) of
        {ok, 200, _, ResponseBody} ->
            #{<<"timeMillis">> := Timestamp} = json_utils:decode(ResponseBody),
            {ok, Timestamp};
        {ok, Code, _, ResponseBody} ->
            {error, {bad_http_response, Code, ResponseBody}};
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if at least one clock sync with Onezone was successful, or the
%% node has restarted and restored the previous synchronization.
%% @end
%%--------------------------------------------------------------------
-spec is_master_synchronized_with_onezone() -> boolean().
is_master_synchronized_with_onezone() ->
    service_onepanel:run_on_master_node(fun global_clock:is_synchronized/0).


%% @private
-spec service_nodes_to_sync() -> [node()].
service_nodes_to_sync() ->
    lists:flatten([
        service_onepanel:get_nodes(),
        service_cluster_manager:get_current_primary_node(),
        service_op_worker:get_nodes()
    ]).