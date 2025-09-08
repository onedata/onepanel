%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Starts auto-cleaning for a space (op_panel only).
%%% @end
%%%-------------------------------------------------------------------
-module(space_start_auto_cleaning_create_middleware_handler).
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
-type input() :: undefined.
-type state() :: #onp_req_state{input :: input()}.
-type output() :: binary().

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
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok.
validate(_) ->
    ok.


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{id = SpaceId}}}) ->
    case middleware_utils:result_from_service_action(
        ?SERVICE_OP, start_auto_cleaning, #{space_id => SpaceId}
    ) of
        {ok, ReportId} -> {ok, ReportId};
        no_need -> ok;
        {error, _} = Error -> Error
    end.


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(
    #onp_req_state{ctx = #onp_req_ctx{interface = rest, gri = #gri{id = SpaceId}}},
    ReportId
) ->
    {ok, #rest_resp{
        code = ?HTTP_202_ACCEPTED,
        headers = rest_translator:make_location_header([
            <<"provider/spaces">>, SpaceId,
            <<"auto-cleaning/reports">>, ReportId
        ]),
        body = #{<<"reportId">> => ReportId}
    }}.
