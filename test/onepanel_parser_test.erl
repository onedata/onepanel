%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for onepanel_parser module.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_parser_test).
-author("Krzysztof Trzepla").

-ifdef(TEST).

-include("modules/errors.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test functions
%%%===================================================================

parse_integer_key_test() ->
    Data1 = [{<<"key">>, 1}],
    Data2 = [{<<"key">>, <<"1">>}],
    Data3 = [{<<"key">>, <<"not_an_integer">>}],
    ArgsSpec = #{key => integer},
    ?assertEqual(#{key => 1}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertEqual(#{key => 1}, onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertThrow(#error{reason = {?ERR_INVALID_VALUE, [key], integer}},
        onepanel_parser:parse(Data3, ArgsSpec)).


parse_float_key_test() ->
    Data1 = [{<<"key">>, 1.0}],
    Data2 = [{<<"key">>, <<"1.0">>}],
    Data3 = [{<<"key">>, 1}],
    Data4 = [{<<"key">>, <<"1">>}],
    Data5 = [{<<"key">>, <<"not_a_float">>}],
    ArgsSpec = #{key => float},
    ?assertEqual(#{key => 1.0}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertEqual(#{key => 1.0}, onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertEqual(#{key => 1.0}, onepanel_parser:parse(Data3, ArgsSpec)),
    ?assertEqual(#{key => 1.0}, onepanel_parser:parse(Data4, ArgsSpec)),
    ?assertThrow(#error{reason = {?ERR_INVALID_VALUE, [key], float}},
        onepanel_parser:parse(Data5, ArgsSpec)).


parse_atom_key_test() ->
    Data1 = [{<<"key">>, value}],
    Data2 = [{<<"key">>, <<"value">>}],
    Data3 = [{<<"key">>, 1}],
    ArgsSpec = #{key => atom},
    ?assertEqual(#{key => value}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertEqual(#{key => value}, onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertThrow(#error{reason = {?ERR_INVALID_VALUE, [key], atom}},
        onepanel_parser:parse(Data3, ArgsSpec)).


parse_boolean_key_test() ->
    Data1 = [{<<"key">>, true}],
    Data2 = [{<<"key">>, false}],
    Data3 = [{<<"key">>, <<"true">>}],
    Data4 = [{<<"key">>, <<"false">>}],
    Data5 = [{<<"key">>, not_a_boolean}],
    Data6 = [{<<"key">>, <<"not_a_boolean">>}],
    ArgsSpec = #{key => boolean},
    ?assertEqual(#{key => true}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertEqual(#{key => false}, onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertEqual(#{key => true}, onepanel_parser:parse(Data3, ArgsSpec)),
    ?assertEqual(#{key => false}, onepanel_parser:parse(Data4, ArgsSpec)),
    ?assertThrow(#error{reason = {?ERR_INVALID_VALUE, [key], boolean}},
        onepanel_parser:parse(Data5, ArgsSpec)),
    ?assertThrow(#error{reason = {?ERR_INVALID_VALUE, [key], boolean}},
        onepanel_parser:parse(Data6, ArgsSpec)).


parse_string_key_test() ->
    Data1 = [{<<"key">>, <<"value">>}],
    Data2 = [{<<"key">>, 1}],
    ArgsSpec = #{key => string},
    ?assertEqual(#{key => <<"value">>}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertThrow(#error{reason = {?ERR_INVALID_VALUE, [key], string}},
        onepanel_parser:parse(Data2, ArgsSpec)).


parse_required_key_test() ->
    Data = [],
    ArgsSpec = #{key => string},
    ?assertThrow(#error{reason = {?ERR_MISSING_KEY, [key]}},
        onepanel_parser:parse(Data, ArgsSpec)).


parse_optional_key_test() ->
    Data = [],
    ArgsSpec = #{key => {string, optional}},
    ?assertEqual(#{}, onepanel_parser:parse(Data, ArgsSpec)).


parse_optional_default_key_test() ->
    Data = [],
    ArgsSpec = #{key => {string, {optional, <<"value">>}}},
    ?assertEqual(#{key => <<"value">>}, onepanel_parser:parse(Data, ArgsSpec)).


parse_nested_key_test() ->
    Data1 = [{<<"key1">>, [
        {<<"key2">>, [
            {<<"key3">>, <<"value">>}
        ]}
    ]}],
    Data2 = [{<<"key1">>, [
        {<<"key2">>, []}
    ]}],
    Data3 = [{<<"key1">>, []}],
    Data4 = [],
    ArgsSpec = #{key1 => #{
        key2 => #{
            key3 => string
        }
    }},
    ?assertEqual(#{key1 => #{
        key2 => #{
            key3 => <<"value">>
        }
    }}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertThrow(#error{reason = {?ERR_MISSING_KEY, [key1, key2, key3]}},
        onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertThrow(#error{reason = {?ERR_MISSING_KEY, [key1, key2]}},
        onepanel_parser:parse(Data3, ArgsSpec)),
    ?assertThrow(#error{reason = {?ERR_MISSING_KEY, [key1]}},
        onepanel_parser:parse(Data4, ArgsSpec)).


parse_oneof_key_test() ->
    Data1 = [{<<"oneof_key">>, [
        {<<"key1">>, <<"value1">>}
    ]}],
    Data2 = [{<<"oneof_key">>, [
        {<<"key2">>, <<"2">>},
        {<<"key3">>, <<"value3">>}
    ]}],
    ArgsSpec = #{oneof_key => {oneof, [
        #{key1 => string},
        #{
            key2 => integer,
            key3 => string
        }
    ]}},
    ?assertEqual(#{oneof_key => #{
        key1 => <<"value1">>
    }}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertEqual(#{oneof_key => #{
        key2 => 2,
        key3 => <<"value3">>
    }}, onepanel_parser:parse(Data2, ArgsSpec)).


parse_wildcard_key_test() ->
    Data = [
        {<<"key1">>, [
            {<<"key">>, <<"value1">>}
        ]},
        {<<"key2">>, [
            {<<"key">>, <<"value2">>}
        ]},
        {<<"key3">>, [
            {<<"key">>, <<"value3">>}
        ]}
    ],
    ArgsSpec = #{'_' => #{key => string}},
    ?assertEqual(#{
        <<"key1">> => #{key => <<"value1">>},
        <<"key2">> => #{key => <<"value2">>},
        <<"key3">> => #{key => <<"value3">>}
    }, onepanel_parser:parse(Data, ArgsSpec)).


parse_standard_and_wildcard_key_test() ->
    Data = [
        {<<"key1">>, [
            {<<"key">>, <<"value1">>}
        ]},
        {<<"key2">>, [
            {<<"key">>, <<"value2">>}
        ]},
        {<<"key3">>, [
            {<<"key">>, <<"value3">>}
        ]}
    ],
    ArgsSpec1 = #{
        key2 => #{key => string},
        '_' => #{key => string}
    },
    ArgsSpec2 = #{
        '_' => #{key => string},
        key2 => #{key => string}
    },
    ?assertEqual(#{
        key2 => #{key => <<"value2">>},
        <<"key1">> => #{key => <<"value1">>},
        <<"key3">> => #{key => <<"value3">>}
    }, onepanel_parser:parse(Data, ArgsSpec1)),
    ?assertEqual(#{
        key2 => #{key => <<"value2">>},
        <<"key1">> => #{key => <<"value1">>},
        <<"key3">> => #{key => <<"value3">>}
    }, onepanel_parser:parse(Data, ArgsSpec2)).

-endif.