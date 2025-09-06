%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler to set/change the emergency passphrase.
%%% @end
%%%-------------------------------------------------------------------
-module(panel_emergency_passphrase_create_middleware_handler).
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
-type input() :: #{newPassphrase := binary(), currentPassphrase => binary() | undefined}.
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
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = #client{role = guest}}}) ->
    not emergency_passphrase:is_set();
preauthorize(_) ->
    false.


-spec validate(state()) -> ok.
validate(_) ->
    % validation is part of the passphrase-changing function
    ok.


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{input = Input}) ->
    New = maps:get(newPassphrase, Input),
    Current = maps:get(currentPassphrase, Input, undefined),
    emergency_passphrase:change(Current, New).
