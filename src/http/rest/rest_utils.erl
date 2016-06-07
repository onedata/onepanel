%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains helper functions for modules implementing
%%% rest_module_behavior.
%%% @see rest_module_behavior
%%% @end
%%%-------------------------------------------------------------------
-module(rest_utils).
-author("Konrad Zemek").

-include_lib("ctool/include/logging.hrl").

%% API
-export([report_missing_key/1, report_invalid_type/2, report_error/1,
    report_error/2, set_error_response/2]).
-export([assert_key/2, assert_type/3, assert_key_type/3, assert_key_value/4]).

-type key() :: binary().
-type value() :: term().
-type type() :: base64 | binary | atom.
-type error() :: invalid_request | invalid_value | missing_key.

-export_type([error/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec assert_key(Key :: binary(), Data :: rest_handler:data()) ->
    Value :: term() | no_return().
assert_key(Key, Data) ->
    case lists:keyfind(Key, 1, Data) of
        {Key, Value} -> Value;
        false -> report_missing_key(Key)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec assert_type(Key :: key(), Value :: value(), Type :: type()) ->
    Value :: value() | no_return().
assert_type(Key, Value, base64 = Type) when is_binary(Value) ->
    try base64:decode(Value) catch _:_ -> report_invalid_type(Key, Type) end;

assert_type(_Key, Value, binary) when is_binary(Value) ->
    Value;

assert_type(_Key, Value, atom) when is_binary(Value) ->
    erlang:binary_to_atom(Value, utf8);

assert_type(Key, _Value, Type) ->
    report_invalid_type(Key, Type).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec assert_key_type(Key :: key(), Data :: rest_handler:data(), Type :: type()) ->
    Value :: value() | no_return().
assert_key_type(Key, Data, Type) ->
    assert_type(Key, assert_key(Key, Data), Type).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec assert_key_value(Key :: key(), Data :: rest_handler:data(), Type :: type(),
    ExpectedValues :: [value()]) -> Value :: value() | no_return().
assert_key_value(Key, Data, Type, ExpectedValues) ->
    Value = assert_key_type(Key, Data, Type),
    case lists:member(Value, ExpectedValues) of
        true -> Value;
        false -> report_invalid_value(Key, Type, ExpectedValues)
    end.


%%--------------------------------------------------------------------
%% @doc Throws an exception to report an missing key.
%%--------------------------------------------------------------------
-spec report_missing_key(Key :: key()) -> no_return().
report_missing_key(Key) ->
    Description = <<"missing required key: '", Key/binary, "'">>,
    report_error(missing_key, Description).


%%--------------------------------------------------------------------
%% @doc Throws an exception to report an invalid type.
%%--------------------------------------------------------------------
-spec report_invalid_type(Key :: key(), ExpectedType :: type()) ->
    no_return().
report_invalid_type(Key, ExpectedType) ->
    Description = <<"invalid '", Key/binary, "' type, expected: '",
        (get_type_name(ExpectedType))/binary, "'">>,
    report_error(invalid_value, Description).


%%--------------------------------------------------------------------
%% @doc Throws an exception to report an invalid value.
%%--------------------------------------------------------------------
-spec report_invalid_value(Key :: key(), Type :: type(),
    ExpectedValues :: [value()]) -> no_return().
report_invalid_value(Key, atom, ExpectedValues) ->
    Values = [erlang:atom_to_binary(V, utf8) || V <- ExpectedValues],
    report_invalid_value(Key, binary, Values);

report_invalid_value(Key, binary, [Value | Values]) ->
    Expectation = lists:foldl(fun(V, Acc) ->
        <<"'", V/binary, "', ", Acc/binary>>
    end, <<"'", Value/binary, "'">>, Values),
    Description = <<"invalid '", Key/binary, "' value, expected one of: ",
        Expectation/binary, "">>,
    report_error(invalid_value, Description).


%%--------------------------------------------------------------------
%% @doc Throws an exception to report a generic REST error.
%%--------------------------------------------------------------------
-spec report_error(Type :: error()) -> no_return().
report_error(Type) ->
    throw({rest_error, Type}).


%%--------------------------------------------------------------------
%% @doc Throws an exception to report a generic REST error with a description.
%%--------------------------------------------------------------------
-spec report_error(Type :: error(), Description :: binary()) -> no_return().
report_error(Type, Description) ->
    throw({rest_error, {Type, Description}}).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec set_error_response(Type :: error() | {Type :: error(),
    Description :: binary()}, Req :: cowboy_req:req()) -> Req :: cowboy_req:req().
set_error_response({Type, Description}, Req) ->
    Body = json_utils:encode([
        {<<"error">>, Type},
        {<<"description">>, Description}
    ]),
    cowboy_req:set_resp_body(Body, Req);

set_error_response(Type, Req) ->
    Body = json_utils:encode([{<<"error">>, Type}]),
    cowboy_req:set_resp_body(Body, Req).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_type_name(Type :: type()) -> Name :: binary().
get_type_name(base64) -> <<"base64 encoded string">>.