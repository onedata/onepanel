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
    service_exec/3,
    service_call/2,
    service_call/3,
    service_call/5,

    if_oz_then/1,
    if_op_then/1,

    get_main_service/0,
    get_worker_service/0,

    is_cluster_member/1,

    validate_cluster_deployed/0,
    validate_op_registered/0
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec service_exec(service:name(), service:action(), service:step_ctx()) ->
    ok | no_return().
service_exec(Service, Action, Ctx) ->
    ActionResults = service:apply_sync(Service, Action, Ctx),
    service_utils:throw_on_error(ActionResults),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @equiv result_from_service_action(Service, Action, #{}).
%% @end
%%--------------------------------------------------------------------
-spec service_call(service:name(), service:action()) ->
    {ok, term()} | errors:error().
service_call(Service, Action) ->
    service_call(Service, Action, #{}).


%%--------------------------------------------------------------------
%% @doc Executes service action. Returns result of a function named
%% like the action, defined in service's main module.
%% @equiv result_from_service_action(Service, Action, Ctx, get_module(Service), Action).
%% @end
%%--------------------------------------------------------------------
-spec service_call(service:name(), service:action(), service:step_ctx()) ->
    {ok, term()} | errors:error().
service_call(Service, Action, Ctx) ->
    service_call(
        Service, Action, Ctx,
        service:get_module(Service), Action
    ).


%%--------------------------------------------------------------------
%% @doc Executes service action. Returns result value
%% of given Module:Function executed as one of the action steps.
%% If the function was executed on multiple hosts, result from
%% one host only is returned.
%% If any step returned an error, the error is thrown.
%% @end
%%--------------------------------------------------------------------
-spec service_call(
    service:name(), service:action(), service:step_ctx(), module(), Function :: atom()
) ->
    {ok, term()} | errors:error().
service_call(Service, Action, Ctx, Module, Function) ->
    ActionResults = service:apply_sync(Service, Action, Ctx),
    case service_utils:results_contain_error(ActionResults) of
        {true, Error} ->
            Error;
        false ->
            {[{_Host, Result} | _], []} = service_utils:select_service_step(
                Module, Function, ActionResults
            ),
            case Result of
                {ok, _} -> Result;
                _ -> {ok, Result}
            end
    end.


-spec if_oz_then(X) -> false | {true, X} when X :: term().
if_oz_then(Then) ->
    if_cluster_type_then(?ONEZONE, Then).


-spec if_op_then(X) -> false | {true, X} when X :: term().
if_op_then(Then) ->
    if_cluster_type_then(?ONEPROVIDER, Then).


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


-spec is_cluster_member(middleware:client()) -> boolean().
is_cluster_member(#client{role = member}) -> true;
is_cluster_member(_) -> false.


-spec validate_cluster_deployed() -> ok | errors:error().
validate_cluster_deployed() ->
    case onepanel_deployment:is_set(?PROGRESS_CLUSTER) of
        true -> ok;
        false -> ?ERROR_NOT_FOUND
    end.


-spec validate_op_registered() -> ok | no_return().
validate_op_registered() ->
    case service_oneprovider:is_registered() of
        true -> ok;
        false -> throw(?ERR_UNREGISTERED_ONEPROVIDER(?err_ctx()))
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec if_cluster_type_then(onedata:cluster_type(), X) -> false | {true, X} when X :: term().
if_cluster_type_then(ClusterType, Then) ->
    case onepanel_env:get_cluster_type() of
        ClusterType -> {true, Then};
        _ -> false
    end.
