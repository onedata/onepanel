%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for returning current user's cluster ids.
%%% TODO VFS-13023 test endpoint
%%% @end
%%%-------------------------------------------------------------------
-module(user_current_user_clusters_get_middleware_handler).
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
-type state() :: #onp_req_state{ctx :: #onp_req_ctx{}, input :: input()}.
-type output() :: [binary()].

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> {true, [rest]}.
supported_interfaces(_) ->
    {true, [rest]}.


-spec service_availability_requirements(middleware_handler:req_ctx()) ->
    false | {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    % fetches from ozw, local services may be down
    middleware_handler_utils:if_oz_then([?SERVICE_OZW, all_healthy_ignoring_ones3]).


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok | errors:error().
validate(#onp_req_state{ctx = #onp_req_ctx{client = #client{role = member}}}) -> ok;
validate(_) -> ?ERROR_NOT_FOUND.


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{client = #client{zone_credentials = ZoneCreds}}}) ->
    clusters:list_user_clusters(ZoneCreds).


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Ids) ->
    {ok, ?OK_REPLY(#{<<"ids">> => Ids})}.
