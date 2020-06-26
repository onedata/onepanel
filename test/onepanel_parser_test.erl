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
    Data1 = #{<<"key">> => 1},
    Data2 = #{<<"key">> => <<"1">>},
    Data3 = #{<<"key">> => <<"not_an_integer">>},
    ArgsSpec = #{key => integer},
    ?assertEqual(#{key => 1}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertEqual(#{key => 1}, onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertThrow(?ERROR_BAD_VALUE_INTEGER(<<"key">>),
        onepanel_parser:parse(Data3, ArgsSpec)).


parse_float_key_test() ->
    Data1 = #{<<"key">> => 1.0},
    Data2 = #{<<"key">> => <<"1.0">>},
    Data3 = #{<<"key">> => 1},
    Data4 = #{<<"key">> => <<"1">>},
    Data5 = #{<<"key">> => <<"not_a_float">>},
    ArgsSpec = #{key => float},
    ?assertEqual(#{key => 1.0}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertEqual(#{key => 1.0}, onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertEqual(#{key => 1.0}, onepanel_parser:parse(Data3, ArgsSpec)),
    ?assertEqual(#{key => 1.0}, onepanel_parser:parse(Data4, ArgsSpec)),
    ?assertThrow(?ERROR_BAD_VALUE_FLOAT(<<"key">>),
        onepanel_parser:parse(Data5, ArgsSpec)).


parse_atom_key_test() ->
    Data1 = #{<<"key">> => value},
    Data2 = #{<<"key">> => <<"value">>},
    Data3 = #{<<"key">> => 1},
    ArgsSpec = #{key => atom},
    ?assertEqual(#{key => value}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertEqual(#{key => value}, onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertThrow(?ERROR_BAD_VALUE_ATOM(<<"key">>),
        onepanel_parser:parse(Data3, ArgsSpec)).


parse_boolean_key_test() ->
    Data1 = #{<<"key">> => true},
    Data2 = #{<<"key">> => false},
    Data3 = #{<<"key">> => <<"true">>},
    Data4 = #{<<"key">> => <<"false">>},
    Data5 = #{<<"key">> => not_a_boolean},
    Data6 = #{<<"key">> => <<"not_a_boolean">>},
    ArgsSpec = #{key => boolean},
    ?assertEqual(#{key => true}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertEqual(#{key => false}, onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertEqual(#{key => true}, onepanel_parser:parse(Data3, ArgsSpec)),
    ?assertEqual(#{key => false}, onepanel_parser:parse(Data4, ArgsSpec)),
    ?assertThrow(?ERROR_BAD_VALUE_BOOLEAN(<<"key">>),
        onepanel_parser:parse(Data5, ArgsSpec)),
    ?assertThrow(?ERROR_BAD_VALUE_BOOLEAN(<<"key">>),
        onepanel_parser:parse(Data6, ArgsSpec)).


parse_list_key_test() ->
    Data1 = #{<<"key">> => [<<"some_string">>, <<"string2">>]},
    Data2 = #{<<"key">> => []},
    Data3 = #{<<"key">> => [1]},
    Data4 = #{<<"key">> => [#{<<"subkey">> => <<"a">>}, #{<<"subkey">> => <<"b">>}]},
    Data5 = #{<<"key">> => [3, <<"not-an-integer">>]},
    Data6 = #{<<"key">> => <<"not-a-list">>},
    ArgsSpec1 = #{key => [string]},
    ArgsSpec2 = #{key => [#{subkey => string}]},
    ArgsSpec3 = #{key => [integer]},
    ArgsSpec4 = #{key => [atom]},
    ?assertEqual(#{key => [<<"some_string">>, <<"string2">>]},
        onepanel_parser:parse(Data1, ArgsSpec1)),
    ?assertEqual(#{key => []}, onepanel_parser:parse(Data2, ArgsSpec1)),
    ?assertThrow(?ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"key">>),
        onepanel_parser:parse(Data3, ArgsSpec1)),
    ?assertEqual(#{key => [#{subkey => <<"a">>}, #{subkey => <<"b">>}]},
        onepanel_parser:parse(Data4, ArgsSpec2)),
    ?assertThrow(?ERROR_BAD_VALUE_INTEGER(<<"key.2">>),
        onepanel_parser:parse(Data5, ArgsSpec3)),
    ?assertThrow(?ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"key">>),
        onepanel_parser:parse(Data6, ArgsSpec1)),
    ?assertThrow(?ERROR_BAD_VALUE_LIST_OF_ATOMS(<<"key">>),
        onepanel_parser:parse(Data6, ArgsSpec4)),
    ?assertThrow(?ERROR_BAD_DATA(<<"key">>),
        onepanel_parser:parse(Data6, ArgsSpec3)).


parse_string_key_test() ->
    Data1 = #{<<"key">> => <<"value">>},
    Data2 = #{<<"key">> => 1},
    ArgsSpec = #{key => string},
    ?assertEqual(#{key => <<"value">>}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertThrow(?ERROR_BAD_VALUE_BINARY(<<"key">>),
        onepanel_parser:parse(Data2, ArgsSpec)).


parse_ip_key_test() ->
    Data1 = #{<<"key">> => <<"1.1.1.1">>},
    Data2 = #{<<"key">> => <<"1.1.1.256">>},
    Data3 = #{<<"key">> => <<"abc">>},
    ArgsSpec = #{key => ip4},
    ?assertEqual(#{key => {1, 1, 1, 1}}, onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertThrow(?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"key">>),
        onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertThrow(?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"key">>),
        onepanel_parser:parse(Data3, ArgsSpec)).


parse_ip_list_key_test() ->
    Data1 = #{<<"key">> => [<<"1.1.1.1">>, <<"1.2.3.4">>]},
    Data2 = #{<<"key">> => [<<"abc">>]},
    Data3 = #{<<"key">> => <<"abc">>},
    ArgsSpec = #{key => [ip4]},
    ?assertEqual(#{key => [{1,1,1,1}, {1,2,3,4}]},
        onepanel_parser:parse(Data1, ArgsSpec)),
    ?assertThrow(?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"key">>),
        onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertThrow(?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"key">>),
        onepanel_parser:parse(Data3, ArgsSpec)).


parse_required_key_test() ->
    Data = #{},
    ArgsSpec = #{key => string},
    ?assertThrow(?ERROR_MISSING_REQUIRED_VALUE(<<"key">>),
        onepanel_parser:parse(Data, ArgsSpec)).


parse_optional_key_test() ->
    Data = #{},
    ArgsSpec = #{key => {string, optional}},
    ?assertEqual(#{}, onepanel_parser:parse(Data, ArgsSpec)).


parse_optional_default_key_test() ->
    Data = #{},
    ArgsSpec1 = #{key => {string, {optional, <<"value">>}}},
    ArgsSpec2 = #{key => {[string], {optional, [<<"value">>]}}},
    ArgsSpec3 = #{key => {[string], {optional, []}}},
    ?assertEqual(#{key => <<"value">>}, onepanel_parser:parse(Data, ArgsSpec1)),
    ?assertEqual(#{key => [<<"value">>]}, onepanel_parser:parse(Data, ArgsSpec2)),
    ?assertEqual(#{key => []}, onepanel_parser:parse(Data, ArgsSpec3)).


parse_nested_key_test() ->
    Data1 = #{<<"key1">> => #{
        <<"key2">> =>
            #{<<"key3">> => <<"value">>}
        }
    },
    Data2 = #{<<"key1">> => #{
        <<"key2">> => #{}}
    },
    Data3 = #{<<"key1">> => #{}},
    Data4 = #{},
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
    ?assertThrow(?ERROR_MISSING_REQUIRED_VALUE(<<"key1.key2.key3">>),
        onepanel_parser:parse(Data2, ArgsSpec)),
    ?assertThrow(?ERROR_MISSING_REQUIRED_VALUE(<<"key1.key2">>),
        onepanel_parser:parse(Data3, ArgsSpec)),
    ?assertThrow(?ERROR_MISSING_REQUIRED_VALUE(<<"key1">>),
        onepanel_parser:parse(Data4, ArgsSpec)).


parse_enum_test_() ->
    Data1 = #{<<"key1">> => <<"val1">>},
    Data2 = #{<<"key1">> => <<"val2">>},
    Data3 = #{<<"key1">> => <<"other">>},
    Data4 = #{<<"key1">> => <<"val1">>, <<"key2">> => <<"val3">>},
    Data5 = #{<<"key1">> => <<"val1">>, <<"key2">> => <<"badoptional">>},
    Data6 = #{<<"key1">> => <<"val1">>, <<"key3">> => 3},
    Data7 = #{<<"key1">> => <<"val1">>, <<"key3">> => <<"string">>},
    ArgsSpec = #{
        key1 => {enum, string, [<<"val1">>, <<"val2">>]},
        key2 => {{enum, string, [<<"val3">>, <<"val4">>]}, optional},
        key3 => {{enum, integer, [3, 4]}, optional}
    }, [
        ?_assertEqual(#{key1 => <<"val1">>}, onepanel_parser:parse(Data1, ArgsSpec)),
        ?_assertEqual(#{key1 => <<"val2">>}, onepanel_parser:parse(Data2, ArgsSpec)),
        ?_assertThrow(?ERROR_BAD_VALUE_NOT_ALLOWED(<<"key1">>, [<<"val1">>, <<"val2">>]),
            onepanel_parser:parse(Data3, ArgsSpec)),
        ?_assertEqual(#{key1 => <<"val1">>, key2 => <<"val3">>},
            onepanel_parser:parse(Data4, ArgsSpec)),
        ?_assertThrow(?ERROR_BAD_VALUE_NOT_ALLOWED(<<"key2">>, [<<"val3">>, <<"val4">>]),
            onepanel_parser:parse(Data5, ArgsSpec)),
        ?_assertEqual(#{key1 => <<"val1">>, key3 => 3},
            onepanel_parser:parse(Data6, ArgsSpec)),
        ?_assertThrow(?ERROR_BAD_VALUE_INTEGER(<<"key3">>),
            onepanel_parser:parse(Data7, ArgsSpec))
    ].


parse_subclasses_test_() ->
    Data1 = #{<<"storage">> => #{<<"type">> => <<"s3">>, <<"hostname">> => <<"hostname.com">>}},
    Data2 = #{<<"storage">> => #{<<"type">> => <<"posix">>, <<"user">> => 3}},
    Data3 = #{<<"storage">> => #{<<"type">> => <<"badtype">>, <<"user">> => 3}},
    Data4 = #{<<"storage">> => #{<<"type">> => <<"s3">>, <<"hostname">> => 3}},
    Data5 = #{<<"storage">> => #{}},
    Subclasses = [
        #{type => {discriminator, <<"s3">>}, hostname => string},
        #{type => {discriminator, <<"posix">>}, user => integer}
    ],
    ArgsSpec = #{storage => {subclasses, onepanel_parser:prepare_subclasses(Subclasses)}},
    [
        {"prepare_subclasses",
            ?_assertEqual(#{storage => {subclasses, {type, #{
                <<"s3">> => #{type => {equal, <<"s3">>}, hostname => string},
                <<"posix">> => #{type => {equal, <<"posix">>}, user => integer}
            }}}}, ArgsSpec)},
        ?_assertEqual(#{storage => #{type => <<"s3">>, hostname => <<"hostname.com">>}},
            onepanel_parser:parse(Data1, ArgsSpec)),
        ?_assertEqual(#{storage => #{type => <<"posix">>, user => 3}},
            onepanel_parser:parse(Data2, ArgsSpec)),
        ?_assertThrow(?ERROR_BAD_VALUE_NOT_ALLOWED(<<"storage.type">>, [<<"posix">>, <<"s3">>]),
            onepanel_parser:parse(Data3, ArgsSpec)),
        ?_assertThrow(?ERROR_BAD_VALUE_BINARY(<<"storage.hostname">>),
            onepanel_parser:parse(Data4, ArgsSpec)),
        ?_assertThrow(?ERROR_MISSING_REQUIRED_VALUE(<<"storage.type">>),
            onepanel_parser:parse(Data5, ArgsSpec))
].

parse_top_level_subclasses_test_() ->
    Data1 = #{<<"type">> => <<"s3">>, <<"hostname">> => <<"hostname.com">>},
    Data2 = #{<<"type">> => <<"posix">>, <<"user">> => 3},
    Data3 = #{<<"type">> => <<"badtype">>, <<"user">> => 3},
    Data4 = #{<<"type">> => <<"s3">>, <<"hostname">> => 3},
    Data5 = #{},
    Subclasses = [
        #{type => {discriminator, <<"s3">>}, hostname => string},
        #{type => {discriminator, <<"posix">>}, user => integer}
    ],
    ArgsSpec = {subclasses, onepanel_parser:prepare_subclasses(Subclasses)},
    [
        {"prepare_top_level_subclasses",
            ?_assertEqual({subclasses, {type, #{
                <<"s3">> => #{type => {equal, <<"s3">>}, hostname => string},
                <<"posix">> => #{type => {equal, <<"posix">>}, user => integer}
            }}}, ArgsSpec)},
        ?_assertEqual(#{type => <<"s3">>, hostname => <<"hostname.com">>},
            onepanel_parser:parse(Data1, ArgsSpec)),
        ?_assertEqual(#{type => <<"posix">>, user => 3},
            onepanel_parser:parse(Data2, ArgsSpec)),
        ?_assertThrow(?ERROR_BAD_VALUE_NOT_ALLOWED(<<"type">>, [<<"posix">>, <<"s3">>]),
            onepanel_parser:parse(Data3, ArgsSpec)),
        ?_assertThrow(?ERROR_BAD_VALUE_BINARY(<<"hostname">>),
            onepanel_parser:parse(Data4, ArgsSpec)),
        ?_assertThrow(?ERROR_MISSING_REQUIRED_VALUE(<<"type">>),
            onepanel_parser:parse(Data5, ArgsSpec))
    ].


parse_wildcard_key_test() ->
    Data = #{
        <<"key1">> => #{
            <<"key">> => <<"value1">>
        },
        <<"key2">> => #{
            <<"key">> => <<"value2">>
        },
        <<"key3">> => #{
            <<"key">> => <<"value3">>
        }
    },
    ArgsSpec = #{'_' => #{key => string}},
    ?assertEqual(#{
        <<"key1">> => #{key => <<"value1">>},
        <<"key2">> => #{key => <<"value2">>},
        <<"key3">> => #{key => <<"value3">>}
    }, onepanel_parser:parse(Data, ArgsSpec)).


parse_standard_and_wildcard_key_test() ->
    Data = #{
        <<"key1">> => #{
            <<"key">> => <<"value1">>
        },
        <<"key2">> => #{
            <<"key">> => <<"value2">>
        },
        <<"key3">> => #{
            <<"key">> => <<"value3">>
        }
    },
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
