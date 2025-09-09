%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Starts or stops a single host for given service.
%%% @end
%%%-------------------------------------------------------------------
-module(service_start_stop_update_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

-export([
    supported_interfaces/1,
    service_availability_requirements/1,
    preauthorize/1,
    validate/1,
    process/1
]).

-type t() :: ?MODULE.
-type input() :: map().
-type state() :: #onp_req_state{input :: input()}.
-type output() :: undefined.

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> false | {true, [rest]}.
supported_interfaces(#onp_req_ctx{gri = #gri{aspect = {start_stop, ServiceBin}}}) ->
    service_middleware_handler_utils:supported_interfaces_for_service(ServiceBin).


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok | errors:error().
validate(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{aspect = {start_stop, ServiceBin}, id = <<HostBin/binary>>}}}) ->
    {ok, Service} = service_middleware_handler_utils:parse_service_name(ServiceBin),
    Host = binary_to_list(HostBin),
    service_middleware_handler_utils:ensure_has_host(Service, Host).


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{
    ctx = #onp_req_ctx{gri = #gri{aspect = {start_stop, ServiceBin}, id = <<HostBin/binary>>}},
    input = Data
}) ->
    {ok, Service} = service_middleware_handler_utils:parse_service_name(ServiceBin),
    Host = binary_to_list(HostBin),
    Action = case maps:get(started, Data) of
        true -> start;
        false -> stop
    end,
    middleware_handler_utils:service_exec(Service, Action, #{hosts => [Host]}).
