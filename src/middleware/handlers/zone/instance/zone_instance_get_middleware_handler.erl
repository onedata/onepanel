%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for returning remote Onezone instance details.
%%% Available on Oneprovider panels to fetch info about a remote Onezone
%%% either by provided registration token or the zone the provider is registered to.
%%% @end
%%%-------------------------------------------------------------------
-module(zone_instance_get_middleware_handler).
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
-type output() :: map().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> false | {true, [rest]}.
supported_interfaces(_) ->
    middleware_handler_utils:if_cluster_type_then(?ONEPROVIDER, [rest]).


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok | errors:error().
validate(#onp_req_state{input = Data}) ->
    case {service_oneprovider:is_registered(), Data} of
        {true, _} -> ok;
        {false, #{token := _}} -> ok;
        {false, _} -> ?ERR_MISSING_REQUIRED_VALUE(?err_ctx(), <<"token">>)
    end.


-spec process(state()) -> {ok, output()}.
process(#onp_req_state{input = Data}) ->
    Domain = case Data of
        #{token := Token} -> onezone_tokens:read_domain(Token);
        _ -> list_to_binary(service_oneprovider:get_oz_domain())
    end,
    {ok, onezone_client:fetch_zone_info(Domain)}.


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Result) ->
    {ok, ?OK_REPLY(Result)}.
