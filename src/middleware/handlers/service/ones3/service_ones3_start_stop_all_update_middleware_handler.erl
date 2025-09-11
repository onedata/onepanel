%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Starts or stops ones3 on all hosts (OP only).
%%% TODO VFS-13023 test endpoint?
%%% @end
%%%-------------------------------------------------------------------
-module(service_ones3_start_stop_all_update_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

% middleware_handler callbacks
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
%%% Callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> {true, [rest]} | false.
supported_interfaces(_) ->
    middleware_handler_utils:if_op_then([rest]).


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) -> false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok | errors:error().
validate(_) ->
    middleware_handler_utils:validate_op_registered().


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{input = Data}) ->
    service_middleware_handler_utils:set_started_all(?SERVICE_ONES3, maps:get(started, Data)).


