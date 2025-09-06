%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for updating deployment progress markers.
%%% @end
%%%-------------------------------------------------------------------
-module(panel_progress_update_middleware_handler).
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


-spec validate(state()) -> ok.
validate(_) ->
    ok.


-spec process(state()) -> ok.
process(#onp_req_state{input = Data}) ->
    ClusterType = onepanel_env:get_cluster_type(),
    Mapping = panel_progress_middleware_handler_utils:rest_to_marker_mapping(ClusterType),
    MarksToSet = lists:filtermap(fun({Field, Bool}) ->
        case lists:keyfind(Field, 1, Mapping) of
            {Field, ProgressMark} -> {true, {ProgressMark, Bool}};
            false -> false
        end
    end, maps:to_list(Data)),

    lists:foreach(fun
        ({Marker, true}) -> onepanel_deployment:set_marker(Marker);
        ({Marker, false}) -> onepanel_deployment:unset_marker(Marker)
    end, MarksToSet).
