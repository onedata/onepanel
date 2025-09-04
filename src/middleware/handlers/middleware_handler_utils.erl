%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common utilities for middleware handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_handler_utils).
-author("Bartosz Walkowicz").

-include("authentication.hrl").
-include("middleware/middleware.hrl").
-include("names.hrl").
-include("deployment_progress.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").

-export([
    if_cluster_type_then/2,
    assert_cluster_deployed/0,
    is_cluster_member/1,
    ok_result/1,
    rest_to_marker_mapping/1
]).

%%%===================================================================
%%% API
%%%===================================================================


-spec if_cluster_type_then(onedata:cluster_type(), X) -> false | {true, X} when X :: term().
if_cluster_type_then(ClusterType, Then) ->
    case onepanel_env:get_cluster_type() of
        ClusterType -> {true, Then};
        _ -> false
    end.


-spec assert_cluster_deployed() -> ok | no_return().
assert_cluster_deployed() ->
    case onepanel_env:get_cluster_type() of
        ?ONEPROVIDER ->
            service_oneprovider:is_registered()
                orelse throw(?ERROR_NOT_FOUND);
        ?ONEZONE ->
            onepanel_deployment:is_set(?PROGRESS_CLUSTER)
                orelse throw(?ERROR_NOT_FOUND)
    end,
    ok.


-spec is_cluster_member(middleware:client()) -> boolean().
is_cluster_member(#client{role = member}) -> true;
is_cluster_member(_) -> false.


-spec ok_result
    (errors:error()) -> errors:error();
    (Result) -> {ok, Result}.
ok_result({error, _} = Error) -> Error;
ok_result(Result) -> {ok, Result}.


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
