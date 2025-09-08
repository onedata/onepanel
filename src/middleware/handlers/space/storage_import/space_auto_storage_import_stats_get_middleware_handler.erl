%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Gets auto storage import stats for a space.
%%% @end
%%%-------------------------------------------------------------------
-module(space_auto_storage_import_stats_get_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

% middleware_handler callbacks
-export([
    supported_interfaces/1,
    service_availability_requirements/1,
    preauthorize/1,
    validate/1,
    process/1,
    translate_output/2
]).

-type t() :: ?MODULE.
-type input() :: map().
-type state() :: #onp_req_state{input :: input()}.
-type output() :: map().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> false | {true, [rest]}.
supported_interfaces(_) ->
    middleware_handler_utils:if_cluster_type_then(?ONEPROVIDER, [rest]).


-spec service_availability_requirements(middleware_handler:req_ctx()) ->
    {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    {true, [?SERVICE_OPW, all_healthy_ignoring_ones3]}.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok.
validate(#onp_req_state{input = Data}) ->
    validate_period(Data),
    validate_metrics(Data).


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{id = SpaceId}}, input = Data}) ->
    Ctx = maps:with([space_id, period, metrics], Data#{space_id => SpaceId}),
    middleware_handler_utils:ok_result(middleware_utils:result_from_service_action(
        ?SERVICE_OP, get_auto_storage_import_stats, Ctx
    )).


-spec translate_output(state(), output()) -> {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Stats) ->
    {ok, ?OK_REPLY(Stats)}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec validate_period(input()) -> ok.
validate_period(Data) ->
    Period = maps:get(period, Data),
    case is_supported_period(Period) of
        true -> ok;
        false -> throw(?ERR_BAD_VALUE_LIST_NOT_ALLOWED(?err_ctx(), <<"period">>, supported_periods()))
    end.


%% @private
-spec is_supported_period(binary()) -> boolean().
is_supported_period(Period) ->
    lists:member(Period, supported_periods()).


%% @private
-spec supported_periods() -> [op_worker_storage_import:period()].
supported_periods() ->
    [<<"minute">>, <<"hour">>, <<"day">>].


%% @private
-spec validate_metrics(input()) -> ok.
validate_metrics(Data) ->
    MetricsJoined = maps:get(metrics, Data),
    lists:foreach(fun(Metric) ->
        case is_supported_metric(Metric) of
            true -> ok;
            false -> throw(?ERR_BAD_VALUE_LIST_NOT_ALLOWED(?err_ctx(), <<"metrics">>, supported_metrics()))
        end
    end, binary:split(MetricsJoined, <<",">>, [global, trim])).


%% @private
-spec is_supported_metric(binary()) -> boolean().
is_supported_metric(Metric) ->
    lists:member(Metric, supported_metrics()).


%% @private
-spec supported_metrics() -> [op_worker_storage_import:metric_type()].
supported_metrics() ->
    [<<"queueLength">>, <<"createdFiles">>, <<"modifiedFiles">>, <<"deletedFiles">>].
