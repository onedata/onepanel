%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for updating Onezone GUI message by id.
%%% @end
%%%-------------------------------------------------------------------
-module(zone_gui_message_update_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("deployment_progress.hrl").
-include("middleware/middleware.hrl").

-export([
    supported_interfaces/0,
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


-spec supported_interfaces() -> false | {true, [rest]}.
supported_interfaces() ->
    middleware_handler_utils:if_cluster_type_then(?ONEZONE, [rest]).


-spec service_availability_requirements(middleware_handler:req_ctx()) -> {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    {true, [?SERVICE_OZW, all_healthy_ignoring_ones3]}.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok | no_return().
validate(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{aspect = {gui_message, Id}}}}) ->
    case onepanel_deployment:is_set(?PROGRESS_READY) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end,
    case service:get_hosts(?SERVICE_OZW) /= [] andalso oz_worker_rpc:gui_message_exists(Id) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end.


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{aspect = {gui_message, Id}}}, input = Data}) ->
    Ctx = maps:put(message_id, Id, maps:remove(id, Data)),
    middleware_utils:execute_service_action(?SERVICE_OZ, update_gui_message, Ctx).
