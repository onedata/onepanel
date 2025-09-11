%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for returning Onepanel health status.
%%% TODO VFS-13023 test endpoint
%%% @end
%%%-------------------------------------------------------------------
-module(panel_health_get_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").
-include("modules/errors.hrl").

%% middleware_handler callbacks
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


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(_) ->
    % Health is public
    true.


-spec validate(state()) -> ok.
validate(_) ->
    ok.


-spec process(state()) -> {ok, output()} | errors:error().
process(_) ->
    Nodes = nodes:all(?SERVICE_PANEL),

    AllHealthyFun = fun(Listener) ->
        lists:all(fun
            ({_Node, ok}) -> true;
            (_) -> false
        end, onepanel_rpc:call_all(Nodes, Listener, healthcheck, []))
    end,

    case AllHealthyFun(http_listener) andalso AllHealthyFun(https_listener) of
        true -> {ok, #{<<"status">> => <<"healthy">>}};
        false -> ?ERR_INTERNAL_SERVER_ERROR(?err_ctx(), undefined)
    end.


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Result) ->
    {ok, ?OK_REPLY(Result)}.
