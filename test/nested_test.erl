%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for nested module.
%%% @end
%%%--------------------------------------------------------------------
-module(nested_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-include("modules/errors.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(MAP, #{
    other1 => something1,
    key1 => value1,
    key2 => #{key3 => value3, other3 => something3},
    bad1 => atom,
    bad2 => [{'subkey', 'not', '2', 'tuple'}]
}).
-define(LIST, [
    {other1, something1},
    {key1, value1},
    {key2, [{key3, value3}, {other3, something3}]},
    {bad1, atom},
    {bad2, [{'subkey', 'not', '2', 'tuple'}]}
]).
-define(MIXED1, [
    {other1, something1},
    {key1, value1},
    {key2, #{key3 => value3, other3 => something3}},
    {bad1, atom},
    {bad2, [{'subkey', 'not', '2', 'tuple'}]}
]).
-define(MIXED2, #{
    other1 => something1,
    key1 => value1,
    key2 => [{key3, value3}, {other3, something3}],
    bad1 => atom,
    bad2 => [{'subkey', 'not', '2', 'tuple'}]
}).
-define(ALL, [?MAP, ?LIST, ?MIXED1, ?MIXED2]).

%%%===================================================================
%%% Test functions
%%%===================================================================

get_test_() -> [[
    {"empty path returns whole term",
        ?_assertEqual('not-a-container', nested:get([], 'not-a-container'))},
    {"get single key",
        ?_assertEqual(value1, nested:get(key1, Container))},
    {"get single key path",
        ?_assertEqual(value1, nested:get([key1], Container))},
    {"get nested path",
        ?_assertEqual(value3, nested:get([key2, key3], Container))},
    {"fail on missing key", [
        ?_assertError({badkeys, [missing]}, nested:get(missing, Container)),
        ?_assertError({badkeys, [missing, key3]}, nested:get([missing, key3], Container)),
        ?_assertError({badkeys, [key2, missing]}, nested:get([key2, missing], Container))
    ]},
    {"return default on missing key", [
        ?_assertEqual(default, nested:get(missing, Container, default)),
        ?_assertEqual(default, nested:get([missing, key3], Container, default)),
        ?_assertEqual(default, nested:get([key2, missing], Container, default))

    ]}
] || Container <- ?ALL].

find_test_() -> [[
    {"empty path returns whole term",
        ?_assertEqual({ok, 'not-a-container'}, nested:find([], 'not-a-container'))},
    {"find single key",
        ?_assertEqual({ok, value1}, nested:find(key1, Container))},
    {"find single key path",
        ?_assertEqual({ok, value1}, nested:find([key1], Container))},
    {"find nested path",
        ?_assertEqual({ok, value3}, nested:find([key2, key3], Container))},
    {"not find key",
        ?_assertEqual(error, nested:find(missing, Container))},
    {"not find nested key 1",
        ?_assertEqual(error, nested:find([missing, key3], Container))},
    {"not find nested key 2",
        ?_assertEqual(error, nested:find([key2, missing], Container))}
] || Container <- ?ALL].

get_find_test_() -> [[
    {"fail on bad container",
        ?_assertError({badnested, 'not-a-container'}, nested:Fun(key, 'not-a-container'))},
    {"fail on bad subcontainer",
        ?_assertError({badnested, _}, nested:Fun([bad1, subkey], Container))},
    {"fail on bad subcontainer",
        ?_assertError({badnested, _}, nested:Fun([bad2, subkey], Container))}
] || Container <- ?ALL, Fun <- [get, find]].


converted_test_() -> [[
    ?_assertEqual(<<"value1">>, nested:get_converted([key1], Container, binary)),
    ?_assertEqual(<<"value3">>, nested:get_converted([key2, key3], Container, binary)),
    ?_assertEqual(default, nested:get_converted([missing], Container, binary, default)),
    ?_assertEqual(<<"value1">>, nested:get_converted([key1], Container, binary, default)),
    ?_assertEqual({ok, <<"value1">>}, nested:find_converted([key1], Container, binary)),
    ?_assertEqual({ok, <<"value3">>}, nested:find_converted([key2, key3], Container, binary)),
    ?_assertError({badkeys, [missing]}, nested:get_converted([missing], Container, binary)),
    ?_assertEqual(error, nested:find_converted([missing], Container, binary))
] || Container <- ?ALL].


put_test_() -> [
    {"fail on bad container",
        ?_assertError({badnested, 'not-a-container'}, nested:put(key, value, 'not-a-container'))},
    {"put new single key",
        ?_assertEqual(#{key1 => value1, key2 => value2},
            nested:put(key2, value2, #{key1 => value1}))},
    {"put new single key path",
        ?_assertEqual(#{key1 => value1, key2 => value2},
            nested:put([key2], value2, #{key1 => value1}))},
    {"put new nested path, inherit container type", [
        ?_assertEqual(#{key1 => value1, key2 => #{subkey1 => value2}},
            nested:put([key2, subkey1], value2, #{key1 => value1})),
        ?_assertEqual([{key2, [{subkey1, value2}]}, {key1, value1}],
            nested:put([key2, subkey1], value2, [{key1, value1}])),
        ?_assertEqual([{key1, value1}, {key2, #{subkey1 => #{subkey2 => value2}}}],
            nested:put([key2, subkey1, subkey2], value2, [{key1, value1}, {key2, #{}}]))
    ]},
    {"override old value", [
        ?_assertEqual(#{key1 => #{key2 => new}},
            nested:put([key1, key2], new, #{key1 => #{key2 => old}})),
        ?_assertEqual([{key1, [{key2, new}]}],
            nested:put([key1, key2], new, [{key1, [{key2, old}]}])),
        ?_assertEqual([{key1, #{key2 => new}}],
            nested:put([key1, key2], new, [{key1, #{key2 => old}}])),
        ?_assertEqual(#{key1 => [{key2, new}]},
            nested:put([key1, key2], new, #{key1 => [{key2, old}]}))
    ]}
] ++ [
    [
        {"fail on bad subcontainer",
            ?_assertError({badnested, _}, nested:put([bad1, subkey], value, Container))},
        {"fail on bad subcontainer",
            ?_assertError({badnested, _}, nested:put([bad2, subkey], value, Container))}
    ] || Container <- ?ALL].


remove_test_() -> [
    ?_assertEqual(#{k1 => v1}, nested:remove(k2, #{k1 => v1, k2 => v2})),
    ?_assertEqual(#{k1 => v1}, nested:remove(k2, #{k1 => v1})),
    ?_assertEqual(#{k1 => v1}, nested:remove([k2], #{k1 => v1, k2 => v2})),
    ?_assertEqual(#{k1 => v1}, nested:remove([k2], #{k1 => v1})),
    ?_assertEqual([{k1, v1}], nested:remove(k2, [{k1, v1}, {k2, v2}])),
    ?_assertEqual([{k1, v1}], nested:remove(k2, [{k1, v1}])),
    ?_assertEqual([{k1, v1}], nested:remove([k2], [{k1, v1}, {k2, v2}])),
    ?_assertEqual([{k1, v1}], nested:remove([k2], [{k1, v1}])),
    ?_assertEqual(#{k1 => v1, k2 => #{k3 => #{k4 => v4}}},
        nested:remove([k2, k3, k5], #{k1 => v1, k2 => #{k3 => #{k4 => v4, k5 => v5}}})),
    ?_assertEqual([{k1, v1}, {k2, [{k4, v4}]}],
        nested:remove([k2, k3], [{k1, v1}, {k2, [{k4, v4}, {k3, v3}]}])),
    {"delete tuple of any arity",
        ?_assertEqual([{k1, v1}], nested:remove([k2], [{k1, v1}, {k2, a, b}]))}
] ++ [
    [
        {"skip bad subcontainer", [
            ?_assertEqual(Container, nested:remove([bad2, subkey, subkey3, subkey4], Container)),
            ?_assertEqual(Container, nested:remove([bad1, subkey1], Container))
        ]}
    ] || Container <- ?ALL
].


copy_and_copy_found_test_() -> [[
    {"replace value", [
        ?_assertEqual(#{<<"key1">> => value1},
            nested:Fun([{key1, <<"key1">>}], Container, #{<<"key1">> => old})),
        ?_assertEqual(#{<<"key1">> => value1, key2 => v},
            nested:Fun([{key1, <<"key1">>}], Container, #{<<"key1">> => old, key2 => v}))
    ]},
    {"nested from",
        ?_assertEqual(#{<<"key1">> => value3},
            nested:Fun([{[key2, key3], <<"key1">>}], Container, #{<<"key1">> => old}))},
    {"nested to",
        ?_assertEqual(#{<<"key1">> => #{<<"subkey1">> => #{<<"subkey2">> => value1}}},
            nested:Fun([
                {key1, [<<"key1">>, <<"subkey1">>, <<"subkey2">>]}
            ], Container, #{}))},
    {"nested, from and to",
        ?_assertEqual(#{key => #{subkey => value3}},
            nested:Fun([{[key2, key3], [key, subkey]}], Container,
                #{key => #{subkey => old}}))},
    {"nested from and to with mixed target",
        ?_assertEqual(#{key => [{subkey, value3}, other]},
            nested:Fun([{[key2, key3], [key, subkey]}], Container,
                #{key => [other]}))},
    {"do nothing with empty mappings", [
        ?_assertEqual(Container, nested:Fun([], Container, Container)),
        ?_assertEqual(#{}, nested:Fun([], Container, #{})),
        ?_assertEqual([], nested:Fun([], Container, []))
    ]},
    {"use default mappings",
        ?_assertEqual(#{{target} => value1, key2 => default2, key3 => value3},
            nested:Fun([
                {key1, {target}, default1},
                {missing, key2, default2},
                {[key2, key3], key3}
            ], Container, #{}))},
    {"fail on bad subcontainer", [
        ?_assertError({badnested, _}, nested:Fun([{[bad1, subkey], key}], Container, #{})),
        ?_assertError({badnested, _}, nested:Fun([{[bad2, subkey, subkey2], key}], Container, #{})),
        ?_assertError({badnested, _}, nested:Fun([{key1, [bad1, subkey]}], #{key1 => value}, Container))
    ]}
] || Container <- ?ALL, Fun <- [copy, copy_found]].


copy_test_() -> [[
    {"fail on missing",[
        ?_assertError({badkeys, [missing]},
            nested:copy([{missing, key1}], Container, #{key1 => old})),
        ?_assertError({badkeys, [missing, subkey]},
            nested:copy([{[missing, subkey], key1}], Container, #{key1 => old})),
        ?_assertError({badkeys, [missing]},
            nested:copy([
                {key1, key1},
                {missing, key2}
            ], Container, Container))
    ]},
    {"multiple mappings",
        ?_assertEqual(#{{target} => value1, key3 => value3},
            nested:copy([
                {key1, {target}},
                {[key2, key3], key3}
            ], Container, #{}))}
] || Container <- ?ALL].


copy_found_test_() -> [[
    {"skip missing",
        ?_assertEqual(#{key1 => old},
            nested:copy_found([{missing, key1}], Container, #{key1 => old}))},
    {"multiple mappings",
        ?_assertEqual(#{{target} => value1, key3 => value3},
            nested:copy_found([
                {key1, {target}},
                {missing, key2},
                {[key2, key3], key3}
            ], Container, #{}))}
] || Container <- ?ALL].

-endif.
