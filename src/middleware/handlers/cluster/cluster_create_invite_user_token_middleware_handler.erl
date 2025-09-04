%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for creating invite user token.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_create_invite_user_token_middleware_handler).
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
-type output() :: tokens:serialized().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces() -> {true, [rest]}.
supported_interfaces() -> {true, [rest]}.


-spec service_availability_requirements(middleware_handler:req_ctx()) ->
    false | {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    % fetches from ozw, local services may be down
    middleware_handler_utils:if_cluster_type_then(
        ?ONEZONE, [?SERVICE_OZW, all_healthy_ignoring_ones3]
    ).


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_ADD_USER).


-spec validate(state()) -> ok | no_return().
validate(_) ->
    middleware_handler_utils:assert_cluster_deployed().


-spec process(state()) -> {ok, output()} | errors:error().
process(_) ->
    clusters:create_invite_token_for_admin().


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Token) ->
    {ok, ?OK_REPLY(#{<<"token">> => Token})}.
