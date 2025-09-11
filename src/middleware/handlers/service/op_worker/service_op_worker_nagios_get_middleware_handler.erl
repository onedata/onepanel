%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Returns Nagios proxy response for op worker service.
%%% TODO VFS-13023 test endpoint?
%%% @end
%%%-------------------------------------------------------------------
-module(service_op_worker_nagios_get_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").
-include_lib("ctool/include/http/headers.hrl").

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
-type output() :: {ok, Code :: integer(), Headers :: map(), Body :: binary()}.

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> false | {true, [rest]}.
supported_interfaces(_) ->
    middleware_handler_utils:if_op_then([rest]).


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok | errors:error().
validate(_) ->
    case service_op_worker:get_hosts() /= [] of
        true -> ok;
        false -> ?ERROR_NOT_FOUND
    end.


-spec process(state()) -> {ok, output()} | errors:error().
process(_) ->
    try
        {ok, _} = middleware_handler_utils:service_call(?SERVICE_OPW, get_nagios_response)
    catch _:_ ->
        ?ERR_SERVICE_UNAVAILABLE(?err_ctx())
    end.


-spec translate_output(state(), output()) -> {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, {ok, Code, Headers, Body}) ->
    {ok, #rest_resp{code = Code, headers = Headers, body = {binary, Body}}}.
