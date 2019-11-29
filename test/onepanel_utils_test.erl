%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for onepanel_utils module.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_utils_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


-define(MAP, #{
    other1 => something1,
    key1 => 3,
    key2 => #{key3 => value3, other3 => something3},
    key4 => #{key5 => [<<"1">>, <<"2">>]},
    bad1 => atom,
    bad2 => [{'subkey', 'not', '2', 'tuple'}]
}).
-define(LIST, [
    {other1, something1},
    {key1, 3},
    {key2, [{key3, value3}, {other3, something3}]},
    {key4, #{key5 => [<<"1">>, <<"2">>]}},
    {bad1, atom},
    {bad2, [{'subkey', 'not', '2', 'tuple'}]}
]).
-define(MIXED1, [
    {other1, something1},
    {key1, 3},
    {key2, #{key3 => value3, other3 => something3}},
    {key4, #{key5 => [<<"1">>, <<"2">>]}},
    {bad1, atom},
    {bad2, [{'subkey', 'not', '2', 'tuple'}]}
]).
-define(MIXED2, #{
    other1 => something1,
    key1 => 3,
    key2 => [{key3, value3}, {other3, something3}],
    key4 => [{key5, [<<"1">>, <<"2">>]}],
    bad1 => atom,
    bad2 => [{'subkey', 'not', '2', 'tuple'}]
}).
-define(ALL, [?MAP, ?LIST, ?MIXED1, ?MIXED2]).

%%%===================================================================
%%% Test functions
%%%===================================================================

converted_test_() -> [[
    ?_assertEqual(<<"3">>, onepanel_utils:get_converted([key1], Container, binary)),
    ?_assertEqual(3, onepanel_utils:get_converted([key1], Container, integer)),
    ?_assertEqual(3.0, onepanel_utils:get_converted([key1], Container, float)),
    ?_assertEqual(<<"value3">>, onepanel_utils:get_converted([key2, key3], Container, binary)),
    ?_assertEqual(default, onepanel_utils:get_converted([missing], Container, binary, default)),
    ?_assertEqual(<<"3">>, onepanel_utils:get_converted([key1], Container, binary, default)),
    ?_assertEqual([1, 2], onepanel_utils:get_converted([key4, key5], Container, {seq, integer})),
    ?_assertEqual({ok, <<"3">>}, onepanel_utils:find_converted([key1], Container, binary)),
    ?_assertEqual({ok, <<"value3">>}, onepanel_utils:find_converted([key2, key3], Container, binary)),
    ?_assertEqual({ok, [1, 2]}, onepanel_utils:find_converted([key4, key5], Container, {seq, integer})),
    ?_assertError({badkeys, [missing]}, onepanel_utils:get_converted([missing], Container, binary)),
    ?_assertEqual(error, onepanel_utils:find_converted([missing], Container, binary))
] || Container <- ?ALL].

-endif.
