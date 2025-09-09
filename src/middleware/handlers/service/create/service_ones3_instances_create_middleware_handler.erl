%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Adds ones3 nodes (OP only).
%%% @end
%%%-------------------------------------------------------------------
-module(service_ones3_instances_create_middleware_handler).
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
-type input() :: map().
-type state() :: #onp_req_state{input :: input()}.
-type output() :: service_executor:task_id().

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
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok | errors:error().
validate(#onp_req_state{input = Data}) ->
    NewHosts = service_middleware_handler_utils:extract_new_hosts(Data),
    service_middleware_handler_utils:validate_hosts_not_existing(?SERVICE_ONES3, NewHosts).


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{input = Data}) ->
    NewHosts = service_middleware_handler_utils:extract_new_hosts(Data),
    Ctx = kv_utils:copy_found([{port, port}], Data, #{new_hosts => NewHosts}),
    {ok, service:apply_async(?SERVICE_ONES3, add_nodes, Ctx)}.


-spec translate_output(state(), output()) -> {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, TaskId) ->
    {ok, ?ASYNC_TASK_REPLY(TaskId)}.
