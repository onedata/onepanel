%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Registers Oneprovider (op_panel only).
%%% @end
%%%-------------------------------------------------------------------
-module(provider_instance_create_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

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

-export_type([t/0, input/0, state/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces() -> false | {true, [rest]}.
supported_interfaces() ->
    middleware_handler_utils:if_cluster_type_then(?ONEPROVIDER, [rest]).


-spec service_availability_requirements(middleware_handler:req_ctx()) ->
    {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    {true, [?SERVICE_OPW, all_healthy_ignoring_ones3]}.


-spec preauthorize(state()) -> false.
preauthorize(_) ->
    % only root can register a provider (kept legacy behaviour here)
    false.


-spec validate(state()) -> ok | errors:error().
validate(#onp_req_state{input = _}) ->
    case service_oneprovider:is_registered() of
        true -> ?ERROR_ALREADY_EXISTS;
        false -> ok
    end.


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{input = Data}) ->
    Ctx = kv_utils:copy_found([
        {tokenProvisionMethod, oneprovider_token_provision_method},
        {token, oneprovider_token},
        {tokenFile, oneprovider_token_file},
        {name, oneprovider_name},
        {subdomainDelegation, oneprovider_subdomain_delegation},
        {domain, oneprovider_domain},
        {subdomain, oneprovider_subdomain},
        {adminEmail, oneprovider_admin_email},
        {geoLatitude, oneprovider_geo_latitude},
        {geoLongitude, oneprovider_geo_longitude}
    ], Data),
    middleware_utils:execute_service_action(?SERVICE_OP, register, Ctx).
