%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common utilities for cluster middleware handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_middleware_handler_utils).
-author("Bartosz Walkowicz").

-include("middleware/middleware.hrl").
-include("deployment_progress.hrl").

%% API
-export([
    validate_cluster_state/0
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec validate_cluster_state() -> ok | errors:error().
validate_cluster_state() ->
    ClusterType = onepanel_env:get_cluster_type(),
    case can_handle_cluster_requests(ClusterType) of
        true -> ok;
        false -> ?ERROR_NOT_FOUND
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec can_handle_cluster_requests(onedata:cluster_type()) -> boolean().
can_handle_cluster_requests(?ONEPROVIDER) -> service_oneprovider:is_registered();
can_handle_cluster_requests(?ONEZONE) -> onepanel_deployment:is_set(?PROGRESS_CLUSTER).

