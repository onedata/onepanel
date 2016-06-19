%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_errors).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/logger.hrl").

%% API
-export([new/7, translate/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec new(Module :: module(), Function :: atom(), Arity :: non_neg_integer(),
    Args :: term(), Reason :: term(), Stacktrace :: term(), Line :: non_neg_integer()) ->
    #error{}.
new(_Module, _Function, _Arity, _Args, #error{} = _Reason, _Stacktrace, _Line) ->
    #error{module = Module, function = Function, arity = Arity, args = Args,
        reason = Reason, stacktrace = Stacktrace, line = Line} = _Reason,
    new(Module, Function, Arity, Args, Reason, Stacktrace, Line);

new(Module, Function, Arity, Args, Reason, Stacktrace, Line) ->
    #error{module = Module, function = Function, arity = Arity, args = Args,
        reason = Reason, stacktrace = Stacktrace, line = Line}.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec translate(Type :: atom(), Reason :: term()) ->
    {Name :: binary(), Description :: binary()} | no_return().
translate(Type, Reason) ->
    log(Type, Reason),
    do_translate(Type, Reason).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec do_translate(Type :: atom(), Reason :: term()) ->
    {Name :: binary(), Description :: binary()} | no_return().
do_translate(_Type, #error{reason = {?ERR_MISSING_REQUIRED_KEY, Key}}) ->
    {<<"Invalid Request">>, <<"Missing required key: '", Key/binary, "'.">>};

do_translate(_Type, #error{reason = {?ERR_INVALID_VALUE_TYPE, Key, Type}}) ->
    {<<"Invalid Request">>, <<"Invalid '", Key/binary, "' type, expected: '",
        Type/binary, "'.">>};

do_translate(_Type, #error{reason = ?ERR_INVALID_USERNAME}) ->
    Len = erlang:integer_to_binary(onepanel_env:get(min_username_length)),
    {<<"Invalid Request">>, <<"Username must be at least ", Len/binary,
        " characters long and contain only alphanumeric characters [a-zA-Z0-9].">>};

do_translate(_Type, #error{reason = ?ERR_INVALID_PASSWORD}) ->
    Len = erlang:integer_to_binary(onepanel_env:get(min_password_length)),
    {<<"Invalid Request">>, <<"Password must be at least ", Len/binary,
        " characters long and contain a minimum of 1 lower case letter [a-z]"
        " and a minimum of 1 upper case letter [A-Z] and a minimum of 1 numeric"
        " character [0-9]. Password must not contain a colon character [:].">>};

do_translate(_Type, #error{reason = ?ERR_INVALID_ROLE}) ->
    {<<"Invalid Request">>, <<"User role must be one of 'admin' or 'regular'.">>};

do_translate(_Type, #error{reason = ?ERR_USERNAME_NOT_AVAILABLE}) ->
    {<<"Operation Error">>, <<"Username is not available.">>};

do_translate(_Type, #error{reason = ?ERR_INVALID_USERNAME_OR_PASSWORD}) ->
    {<<"Authentication Error">>, <<"Invalid username or password.">>};

do_translate(_Type, #error{}) ->
    {<<"Internal Error">>, <<"Server encountered an unexpected error.">>};

do_translate(Type, Reason) ->
    erlang:raise(Type, Reason, []).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec log(Type :: atom(), Reason :: #error{} | term()) -> ok.
log(_Type, #error{module = Module, function = Function, arity = Arity,
    args = Args, reason = Reason, stacktrace = Stacktrace, line = Line}) ->
    ?log_error("Function: ~p:~p/~p~nArgs: ~p~nReason: ~p~nStacktrace: ~p~n"
    "Line: ~p~n", [Module, Function, Arity, Args, Reason, Stacktrace, Line]);
log(Type, Reason) ->
    ?log_error("Type ~p~nReason: ~p~n", [Type, Reason]).