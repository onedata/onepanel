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
new(_Module, _Function, _Arity, _Args, #error{} =
    _Reason, _Stacktrace, _Line) ->
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
translate(_Type, #error{reason = ?ERR_INVALID_REQUEST}) ->
    {<<"Invalid Request">>, <<>>};

translate(_Type, #error{reason = {?ERR_MISSING_KEY, Keys}}) ->
    Key = get_key(Keys),
    {<<"Invalid Request">>, <<"Missing required key: '", Key/binary, "'.">>};

translate(_Type, #error{reason = {?ERR_INVALID_VALUE, Keys, ValueSpec}}) ->
    Key = get_key(Keys),
    Expectation = get_expectation(ValueSpec, <<>>),
    {<<"Invalid Request">>, <<"Invalid '", Key/binary, "' value, expected: ",
        Expectation/binary, ".">>};

translate(_Type, #error{reason = {?ERR_MISSING_PARAM, Keys}}) ->
    Key = get_key(Keys),
    {<<"Invalid Request">>, <<"Missing required parameter: '", Key/binary, "'.">>};

translate(_Type, #error{reason = ?ERR_INVALID_USERNAME}) ->
    Len = erlang:integer_to_binary(onepanel_env:get(min_username_length)),
    {<<"Invalid Request">>, <<"Username must be at least ", Len/binary,
        " characters long and contain only alphanumeric characters [a-zA-Z0-9].">>};

translate(_Type, #error{reason = ?ERR_INVALID_PASSWORD}) ->
    Len = erlang:integer_to_binary(onepanel_env:get(min_password_length)),
    {<<"Invalid Request">>, <<"Password must be at least ", Len/binary,
        " characters long and contain a minimum of 1 lower case letter [a-z]"
        " and a minimum of 1 upper case letter [A-Z] and a minimum of 1 numeric"
        " character [0-9]. Password must not contain a colon character [:].">>};

translate(_Type, #error{reason = ?ERR_INVALID_ROLE}) ->
    {<<"Invalid Request">>, <<"User role must be one of 'admin' or 'regular'.">>};

translate(_Type, #error{reason = ?ERR_USERNAME_NOT_AVAILABLE}) ->
    {<<"Operation Error">>, <<"Username is not available.">>};

translate(_Type, #error{reason = ?ERR_INVALID_USERNAME_OR_PASSWORD}) ->
    {<<"Authentication Error">>, <<"Invalid username or password.">>};

translate(_Type, #error{module = Module, function = Function, arity = Arity,
    args = Args, reason = Reason, stacktrace = Stacktrace, line = Line}) ->
    ?log_error("Function: ~p:~p/~p~nArgs: ~p~nReason: ~p~nStacktrace: ~p~n"
    "Line: ~p~n", [Module, Function, Arity, Args, Reason, Stacktrace, Line]),
    {<<"Internal Error">>, <<"Server encountered an unexpected error.">>};

translate(Type, Reason) ->
    ?log_error("Type: ~p~nReason: ~p~n", [Type, Reason]),
    erlang:raise(Type, Reason, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_key(Keys :: onepanel_parser:keys()) -> Key :: binary().
get_key(Keys) ->
    Tokens = lists:map(fun(Key) ->
        erlang:atom_to_binary(Key, utf8)
    end, Keys),
    onepanel_utils:join(Tokens, <<".">>).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_expectation(ValueSpec :: onepanel_parser:value_spec(), Acc :: binary()) ->
    Expectation :: binary().
get_expectation(atom, Acc) ->
    <<Acc/binary, "'string'">>;

get_expectation(ValueSpec, Acc) when is_atom(ValueSpec) ->
    <<Acc/binary, "'", (erlang:atom_to_binary(ValueSpec, utf8))/binary, "'">>;

get_expectation({equal, {string, Value}}, Acc) ->
    <<Acc/binary, "'", Value/binary, "'">>;

get_expectation({equal, {atom, Value}}, Acc) ->
    <<Acc/binary, "'", (erlang:atom_to_binary(Value, utf8))/binary, "'">>;

get_expectation({equal, {integer, Value}}, Acc) ->
    <<Acc/binary, "'", (erlang:integer_to_binary(Value))/binary, "'">>;

get_expectation({equal, {float, Value}}, Acc) ->
    <<Acc/binary, "'", (erlang:float_to_binary(Value))/binary, "'">>;

get_expectation({oneof, ValueSpecs}, Acc) ->
    Tokens = lists:map(fun(ValueSpec) ->
        get_expectation(ValueSpec, <<>>)
    end, ValueSpecs),
    <<Acc/binary, "[", (onepanel_utils:join(Tokens, <<" | ">>))/binary, "]">>;

get_expectation(ValueSpecs, Acc) when is_list(ValueSpecs) ->
    Tokens = lists:map(fun(ValueSpec) ->
        get_expectation(ValueSpec, <<>>)
    end, ValueSpecs),
    <<Acc/binary, "[", (onepanel_utils:join(Tokens, <<", ">>))/binary, "]">>;

get_expectation(ValueSpec, Acc) when is_map(ValueSpec) ->
    Tokens = lists:map(fun({Key, Value}) ->
        BinKey = erlang:atom_to_binary(Key, utf8),
        <<"'", (BinKey)/binary, "': ", (get_expectation(Value, <<>>))/binary>>
    end, maps:to_list(ValueSpec)),
    <<Acc/binary, "{", (onepanel_utils:join(Tokens, <<", ">>))/binary, "}">>.
