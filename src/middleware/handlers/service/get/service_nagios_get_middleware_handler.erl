%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Returns Nagios proxy response for worker service (op_worker or oz_worker).
%%% @end
%%%-------------------------------------------------------------------
-module(service_nagios_get_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").
-include_lib("ctool/include/http/headers.hrl").

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
-type output() :: #{code := integer(), headers := map(), body := binary()}.

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> false | {true, [rest]}.
supported_interfaces(#onp_req_ctx{gri = #gri{aspect = {nagios, WorkerBin}}}) ->
    ClusterType = onepanel_env:get_cluster_type(),
    case service_middleware_handler_utils:parse_service_name(WorkerBin) of
        {ok, ?SERVICE_OPW} when ClusterType =:= ?ONEPROVIDER -> {true, [rest]};
        {ok, ?SERVICE_OZW} when ClusterType =:= ?ONEZONE -> {true, [rest]};
        _ -> false
    end.


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok | errors:error().
validate(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{aspect = {nagios, WorkerBin}}}}) ->
    Hosts = case service_middleware_handler_utils:parse_service_name(WorkerBin) of
        {ok, ?SERVICE_OPW} -> service_op_worker:get_hosts();
        {ok, ?SERVICE_OZW} -> service_oz_worker:get_hosts()
    end,
    case Hosts /= [] of
        true -> ok;
        false -> ?ERROR_NOT_FOUND
    end.


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{aspect = {nagios, WorkerBin}}}}) ->
    {ok, Worker} = service_middleware_handler_utils:parse_service_name(WorkerBin),
    try
        {ok, Code, Headers, Body} = middleware_utils:result_from_service_action(
            Worker, get_nagios_response
        ),
        {ok, #{code => Code, headers => Headers, body => Body}}
    catch _:_ ->
        ?ERR_SERVICE_UNAVAILABLE(?err_ctx())
    end.


-spec translate_output(state(), output()) -> {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, #{
    code := Code,
    headers := Headers,
    body := Body
}) ->
    {ok, #rest_resp{code = Code, headers = Headers, body = {binary, Body}}}.
