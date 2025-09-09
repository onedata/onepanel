%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Returns service status on a single host
%%% @end
%%%-------------------------------------------------------------------
-module(service_host_status_get_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

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
-type output() :: atom().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> false | {true, [rest]}.
supported_interfaces(#onp_req_ctx{gri = #gri{aspect = {host_status, ServiceBin}}}) ->
    service_middleware_handler_utils:supported_interfaces_for_service(ServiceBin).


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok | errors:error().
validate(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{
    aspect = {host_status, ServiceBin},
    id = <<HostBin/binary>>
}}}) ->
    {ok, Service} = service_middleware_handler_utils:parse_service_name(ServiceBin),
    Host = binary_to_list(HostBin),
    service_middleware_handler_utils:ensure_has_host(Service, Host).


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{aspect = {host_status, ServiceBin}, id = HostBin}}}) ->
    {ok, Service} = service_middleware_handler_utils:parse_service_name(ServiceBin),
    Module = service:get_module(Service),
    HostList = str_utils:binary_to_unicode_list(HostBin),
    middleware_handler_utils:service_call(
        Service, status, #{hosts => [HostList]},
        Module, status
    ).


-spec translate_output(state(), output()) -> {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Status) ->
    {ok, ?OK_REPLY(atom_to_binary(Status, utf8))}.
