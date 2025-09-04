%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for getting cluster instance details.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_instance_get_middleware_handler).
-author("Wojciech Geisler").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

%% middleware_handler callbacks
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


-spec supported_interfaces() -> {true, [rest]}.
supported_interfaces() ->
    {true, [rest]}.


-spec service_availability_requirements(middleware_handler:req_ctx()) ->
    false | {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    % fetches from ozw, local services may be down
    middleware_handler_utils:if_cluster_type_then(
        ?ONEZONE, [?SERVICE_OZW, all_healthy_ignoring_ones3]
    ).


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok | no_return().
validate(#onp_req_state{ctx = #onp_req_ctx{client = #client{role = root}}}) ->
    % clusters must be fetched with Onezone user authorization
    throw(?ERROR_NOT_FOUND);
validate(#onp_req_state{ctx = #onp_req_ctx{client = #client{role = member}}}) ->
    ok.


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{id = Id}, client = Client}}) ->
    clusters:get_details(Client#client.zone_credentials, Id).


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Result) ->
    {ok, ?OK_REPLY(Result)}.
