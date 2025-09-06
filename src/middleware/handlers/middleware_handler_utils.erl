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
    get_main_service/0,
    get_worker_service/0,
    validate_cluster_deployed/0,
    is_cluster_member/1,
    ok_result/1
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


-spec get_main_service() -> service:name().
get_main_service() ->
    case onepanel_env:get_cluster_type() of
        ?ONEPROVIDER -> ?SERVICE_OP;
        ?ONEZONE -> ?SERVICE_OZ
    end.


-spec get_worker_service() -> service:name().
get_worker_service() ->
    case onepanel_env:get_cluster_type() of
        ?ONEPROVIDER -> ?SERVICE_OPW;
        ?ONEZONE -> ?SERVICE_OZW
    end.


-spec validate_cluster_deployed() -> ok | errors:error().
validate_cluster_deployed() ->
    case onepanel_deployment:is_set(?PROGRESS_CLUSTER) of
        true -> ok;
        false -> ?ERROR_NOT_FOUND
    end.


-spec is_cluster_member(middleware:client()) -> boolean().
is_cluster_member(#client{role = member}) -> true;
is_cluster_member(_) -> false.


-spec ok_result
    (errors:error()) -> errors:error();
    (Result) -> {ok, Result}.
ok_result({error, _} = Error) -> Error;
ok_result({ok, _} = Result) -> Result;
ok_result(Result) -> {ok, Result}.
