%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for getting host instance information.
%%% @end
%%%-------------------------------------------------------------------
-module(host_instance_get_middleware_handler).
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
-type output() :: #{binary() := term()}.

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
preauthorize(_) ->
    % node information is public
    true.


-spec validate(state()) -> ok | no_return().
validate(_) ->
    ok.


-spec process(state()) -> {ok, output()} | no_return().
process(_) ->
    Hostname = onepanel_utils:convert(hosts:self(), binary),
    ClusterType = onepanel_env:get_cluster_type(),
    {ok, #{<<"hostname">> => Hostname, <<"clusterType">> => ClusterType}}.


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Result) ->
    {ok, ?OK_REPLY(Result)}.
