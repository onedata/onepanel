%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module handles synchronizing clocks of nodes in Onezone cluster.
%%% All nodes are periodically synchronized with the designated master node (in
%%% the context of clocks sync) - see the cluster_clocks module for more info.
%%% @end
%%%--------------------------------------------------------------------
-module(onezone_cluster_clocks).
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
    cluster_clocks:synchronize_node_upon_start(Node).


-spec restart_periodic_sync() -> ok | no_return().
restart_periodic_sync() ->
    % make sure the master node is using the system time - it will be the
    % reference clock for all nodes on Onezone and all subject Oneproviders
    cluster_clocks:run_on_master(fun() ->
        clock:reset_to_system_time(),
        true
    end),
    cluster_clocks:restart_periodic_sync(?MODULE, fun resolve_nodes_to_sync/0).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec resolve_nodes_to_sync() -> {ok, [node()]}.
resolve_nodes_to_sync() ->
    {ok, lists:flatten([
        service_onepanel:get_nodes(),
        service_cluster_manager:get_nodes(),
        service_oz_worker:get_nodes()
    ])}.
