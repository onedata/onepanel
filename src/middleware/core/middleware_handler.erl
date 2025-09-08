%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour should be implemented by modules that implement specific
%%% middleware operations - serves as a link between
%%%%% API and onepanel internals.
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_handler).
-author("Bartosz Walkowicz").

-include("http/rest.hrl").
-include("middleware/middleware.hrl").

-abc([t, input, state, output]).

-type t() :: middleware_handler_types:t().

-type interface() :: rest.

-type operation() :: create | get | update | delete.

-type rest_input() :: json_utils:json_map().
-type interface_input() :: rest_input().
-type handler_input() :: middleware_handler_types:input().

% Conditions used to describe when a request can be processed by the cluster.
% - Specifying service name indicates that there must exist a node
%   with the service deployed and the service must have status 'healthy'.
% - 'all_healthy_ignoring_ones3' means that all deployed service nodes,
%   except ones3, must have status 'healthy'. Does not enforce presence
%   of all services.
-type availability_level() :: all_healthy_ignoring_ones3 | service:name().

-type req_ctx() :: #onp_req_ctx{}.
-type state() :: middleware_handler_types:state().

-type handler_output() :: middleware_handler_types:output().
-type rest_output() :: #rest_resp{}.
-type interface_output() :: rest_output().

-export_type([
    t/0,
    interface/0, operation/0,
    rest_input/0, interface_input/0, handler_input/0,
    availability_level/0,
    req_ctx/0, state/0,
    handler_output/0, rest_output/0, interface_output/0
]).


%%%===================================================================
%%% Callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Returns list of supported interfaces (if any - e.g. operations may be
%% supported only for specific cluster type) for this handler.
%% NOTE: Currently only 'rest' is supported, but this allows future extensions
%% for GraphSync or other protocols.
%% @end
%%--------------------------------------------------------------------
-callback supported_interfaces(req_ctx()) -> false | {true, [interface()]}.


%%--------------------------------------------------------------------
%% @doc
%% Parses input from the specified interface into structured input data.
%% NOTE: Optional callback. Many handlers do not accept request bodies (e.g., GET).
%% If this callback is not provided, the input is 'undefined' and the handler's
%% input type should reflect that.
%% @end
%%--------------------------------------------------------------------
-callback parse_input(req_ctx(), interface_input()) -> {ok, handler_input()} | errors:error().


%%--------------------------------------------------------------------
%% @doc
%% Returns a list of required availability (if any) checks for the request
%% execution to proceed.
%% @end
%%--------------------------------------------------------------------
-callback service_availability_requirements(req_ctx()) ->
    false | {true, [availability_level()]}.


%%--------------------------------------------------------------------
%% @doc
%% Builds per-request handler state (e.g., prefetch additional documents/models).
%% Optional. If not provided, the default state is #onp_req_state composed of
%% ctx and input returned from parse_input/2.
%% @end
%%--------------------------------------------------------------------
-callback init_state(req_ctx(), handler_input()) -> {ok, state()} | errors:error().


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation.
%% NOTE: This is a panel-side pre-authorization that checks what can be verified
%% locally. Requests may still fail authorization in downstream services
%% (e.g. RPC/REST to Onezone), even if this callback returns true.
%% @end
%%--------------------------------------------------------------------
-callback preauthorize(state()) -> boolean().


%%--------------------------------------------------------------------
%% @doc
%% Determines if given request can be further processed
%% (e.g. checks whether space is supported locally).
%% NOTE: Use primarily to validate input against current model state (some
%% operations may be unavailable in a given state even if the input is
%% semantically correct).
%% @end
%%--------------------------------------------------------------------
-callback validate(state()) -> ok | errors:error().


%%--------------------------------------------------------------------
%% @doc
%% Processes the middleware request.
%% @end
%%--------------------------------------------------------------------
-callback process(state()) -> ok | {ok, handler_output()} | errors:error().


%%--------------------------------------------------------------------
%% @doc
%% Translates handler output for the specific interface.
%% This allows handlers to format their results differently for different
%% protocols (e.g., REST vs GraphSync response format).
%% NOTE: Optional callback. It is invoked only when process/1 returns
%% {ok, Output}. If process/1 returns 'ok' and the handler's output type is
%% 'undefined', this callback is not called and need not be implemented.
%% @end
%%--------------------------------------------------------------------
-callback translate_output(state(), handler_output()) ->
    {ok, interface_output()} | errors:error().


-optional_callbacks([parse_input/2, init_state/2, translate_output/2]).
