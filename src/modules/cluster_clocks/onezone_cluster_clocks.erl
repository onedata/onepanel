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
%%%
%%% For testing/debugging purposes, the master node's reference clock may be
%%% started with a fixed bias, offsetting the global clock of the whole
%%% deployment. This is controlled by the undocumented onepanel env key
%%% 'onezone_clock_bias_milliseconds' (default 0 = local system time); see
%%% restart_periodic_sync/0.
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
    % set up the master node's reference clock - it will be the reference
    % clock for all nodes on Onezone and all subject Oneproviders; normally
    % the local system time is used, but a fixed bias may be configured for
    % testing purposes
    service_onepanel:run_on_master_node(fun() ->
        case onepanel_env:get(onezone_clock_bias_milliseconds, ?APP_NAME, 0) of
            0 ->
                global_clock:reset_to_system_time();
            Bias when is_integer(Bias) ->
                ?warning(
                    "Applying a configured clock bias of ~Bms to the Onezone "
                    "master clock (TESTING option onezone_clock_bias_milliseconds)",
                    [Bias]
                ),
                global_clock:store_bias(local_clock, Bias)
        end,
        true
    end),
    cluster_clocks:restart_periodic_sync(fun prepare_cluster_clock_sync/0).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% No specific preparation is required - this simply returns all cluster nodes to be synced.
%% @end
%%--------------------------------------------------------------------
-spec prepare_cluster_clock_sync() -> {ok, [node()]}.
prepare_cluster_clock_sync() ->
    {ok, lists:flatten([
        service_onepanel:get_nodes(),
        service_cluster_manager:get_current_primary_node(),
        service_oz_worker:get_nodes()
    ])}.
