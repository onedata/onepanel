%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for updating external IPs of services in the cluster.
%%% @end
%%%-------------------------------------------------------------------
-module(host_external_ips_update_middleware_handler).
-author("Wojciech Geisler").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

%% middleware_handler callbacks
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


-spec supported_interfaces() -> {true, [rest]}.
supported_interfaces() ->
    {true, [rest]}.


-spec service_availability_requirements(middleware_handler:req_ctx()) ->
    {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    {true, [all_healthy_ignoring_ones3]}.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok.
validate(_) ->
    ok.


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{input = Data}) ->
    ClusterIps = maps:get(hosts, Data),
    Service = case onepanel_env:get_cluster_type() of
        ?ONEPROVIDER -> ?SERVICE_OP;
        ?ONEZONE -> ?SERVICE_OZ
    end,
    Ctx = #{cluster_ips => onepanel_utils:convert(ClusterIps, {keys, list})},
    middleware_utils:execute_service_action(Service, set_cluster_ips, Ctx).
