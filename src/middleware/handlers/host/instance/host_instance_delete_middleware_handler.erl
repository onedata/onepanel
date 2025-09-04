%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for removing a host from the cluster.
%%% @end
%%%-------------------------------------------------------------------
-module(host_instance_delete_middleware_handler).
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
-type input() :: undefined.
-type state() :: #onp_req_state{input :: input()}.
-type output() :: undefined.

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces() -> {true, [rest]}.
supported_interfaces() ->
    {true, [rest]}.


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok | no_return().
validate(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{id = HostBin}}}) ->
    Host = binary_to_list(HostBin),
    host_exists(Host) orelse throw(?ERROR_NOT_FOUND),
    service_onepanel:is_host_used(Host) andalso throw(?ERROR_NOT_SUPPORTED),
    ok.


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{id = Id}}}) ->
    Host = binary_to_list(Id),
    middleware_utils:execute_service_action(
        ?SERVICE_PANEL, leave_cluster, #{hosts => [Host]}
    ).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec host_exists(service:host()) -> boolean().
host_exists(Host) ->
    lists:member(Host, service_onepanel:get_hosts()).
