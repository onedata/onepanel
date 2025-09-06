%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Returns this Oneprovider details (op_panel only).
%%% @end
%%%-------------------------------------------------------------------
-module(provider_instance_get_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

-export([
    supported_interfaces/0,
    service_availability_requirements/1,
    preauthorize/1,
    validate/1,
    process/1,
    translate_output/2
]).

-type t() :: ?MODULE.
-type input() :: undefined.
-type state() :: #onp_req_state{input :: input()}.
-type output() :: map().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces() -> false | {true, [rest]}.
supported_interfaces() ->
    middleware_handler_utils:if_cluster_type_then(?ONEPROVIDER, [rest]).


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok | errors:error().
validate(_) ->
    middleware_handler_utils:validate_op_registered().


-spec process(state()) -> {ok, output()} | errors:error().
process(_) ->
    middleware_handler_utils:ok_result(middleware_utils:result_from_service_action(
        ?SERVICE_OP, get_details
    )).


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Result) ->
    {ok, ?OK_REPLY(Result)}.
