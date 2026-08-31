%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common utilities for panel progress middleware handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(panel_progress_middleware_handler_utils).
-author("Bartosz Walkowicz").

-include("deployment_progress.hrl").
-include("middleware/middleware.hrl").

-export([rest_to_marker_mapping/1]).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Maps between rest 'progress' endpoint fields and atoms
%% used in onepanel_deployment model.
%% @end
%%--------------------------------------------------------------------
-spec rest_to_marker_mapping(onedata:cluster_type() | common) ->
    [{RestField :: atom(), ProgressMark :: onepanel_deployment:marker()}].
rest_to_marker_mapping(?ONEZONE) ->
    rest_to_marker_mapping(common);
rest_to_marker_mapping(?ONEPROVIDER) ->
    [
        {storagesSetup, ?PROGRESS_STORAGE_SETUP}
        | rest_to_marker_mapping(common)
    ];
rest_to_marker_mapping(common) ->
    [
        {clusterNodes, ?PROGRESS_CLUSTER},
        {clusterIps, ?PROGRESS_CLUSTER_IPS},
        {webCertificate, ?PROGRESS_LETSENCRYPT_CONFIG},
        {dnsCheck, ?DNS_CHECK_ACKNOWLEDGED}
    ].
