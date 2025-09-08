%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for updating DNS check configuration.
%%% @end
%%%-------------------------------------------------------------------
-module(panel_dns_check_configuration_update_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

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
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> {true, [rest]}.
supported_interfaces(_) ->
    {true, [rest]}.


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok.
validate(_) ->
    ok.


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{input = Data}) ->
    ClusterWorker = middleware_handler_utils:get_worker_service(),
    Ctx = kv_utils:copy_found([
        {dnsServers, dns_servers},
        {builtInDnsServer, built_in_dns_server},
        {dnsCheckAcknowledged, dns_check_acknowledged}
    ], Data),
    middleware_utils:execute_service_action(ClusterWorker, configure_dns_check, Ctx).
