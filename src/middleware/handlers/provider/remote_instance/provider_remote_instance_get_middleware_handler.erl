%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Returns a remote provider info available to current user.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_remote_instance_get_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

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
-type output() :: map().

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
    middleware_handler_utils:if_oz_then([all_healthy_ignoring_ones3]).


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok | errors:error().
validate(#onp_req_state{ctx = #onp_req_ctx{client = #client{role = member}}}) ->
    ok;
validate(#onp_req_state{ctx = #onp_req_ctx{client = #client{role = root}}}) ->
    ?ERROR_NOT_FOUND.


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{client = #client{zone_credentials = Auth}, gri = #gri{id = Id}}}) ->
    {ok, clusters:fetch_remote_provider_info(Auth, Id)}.


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Result) ->
    {ok, ?OK_REPLY(Result)}.
