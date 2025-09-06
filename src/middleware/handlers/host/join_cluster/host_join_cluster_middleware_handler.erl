%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for joining a cluster on a solitary node.
%%% @end
%%%-------------------------------------------------------------------
-module(host_join_cluster_middleware_handler).
-author("Bartosz Walkowicz").

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


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = #client{role = Role}}}) ->
    Role == guest andalso service_onepanel:available_for_clustering().


-spec validate(state()) -> ok | errors:error().
validate(_) ->
    case service_onepanel:available_for_clustering() of
        true -> ok;
        false -> ?ERR_NODE_ALREADY_IN_CLUSTER(?err_ctx(), hosts:self())
    end.


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{input = Data}) ->
    InviteToken = onepanel_utils:get_converted(inviteToken, Data, binary),
    Ctx = #{
        invite_token => InviteToken,
        cluster_host => invite_tokens:get_cluster_host(InviteToken)
    },
    middleware_utils:execute_service_action(?SERVICE_PANEL, join_cluster, Ctx).
