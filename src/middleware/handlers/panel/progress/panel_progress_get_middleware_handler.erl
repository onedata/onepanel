%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for returning deployment progress flags.
%%% @end
%%%-------------------------------------------------------------------
-module(panel_progress_get_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("authentication.hrl").
-include("middleware/middleware.hrl").
-include("deployment_progress.hrl").

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
-type output() :: #{atom() => boolean()}.

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
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok.
validate(_) ->
    ok.


-spec process(state()) -> {ok, output()} | no_return().
process(_) ->
    {ok, format_deployment_progress()}.


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Progress) ->
    {ok, ?OK_REPLY(Progress)}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


-spec format_deployment_progress() -> #{atom() => boolean()}.
format_deployment_progress() ->
    ClusterType = onepanel_env:get_cluster_type(),
    Fields = middleware_handler_utils:rest_to_marker_mapping(ClusterType),
    Fields2 = case ClusterType of
        ?ONEPROVIDER ->
            [{isRegistered, fun service_oneprovider:is_registered/0} | Fields];
        ?ONEZONE ->
            Fields
    end,
    lists:foldl(fun
        ({Key, Fun}, Acc) when is_function(Fun) -> Acc#{Key => Fun()};
        ({Key, Mark}, Acc) -> Acc#{Key => onepanel_deployment:is_set(Mark)}
    end, #{}, Fields2).
