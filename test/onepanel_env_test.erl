%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for onepanel_env module.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_env_test).
-author("Krzysztof Trzepla").

-ifdef(TEST).

-include("modules/errors.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(APP_CONFIGS, [
    {a1, ?APP_CONFIG_1},
    {a2, ?APP_CONFIG_2}
]).
-define(APP_CONFIG_1, [
    {k1, v1},
    {k2, v2},
    {k3, v3}
]).
-define(APP_CONFIG_2, [
    {k4, v4},
    {k5, ?VALUE_5}
]).
-define(VALUE_5, [
    {k6, v6},
    {k7, ?VALUE_7}
]).
-define(VALUE_7, [
    {k8, v8}
]).
-define(FILE_CONTENT(X), lists:flatten(io_lib:format("~p.", [X]))).

%%%===================================================================
%%% Test generators
%%%===================================================================

onepanel_env_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun read_should_return_value/1,
            fun read_should_report_missing_key/1,
            fun read_should_pass_errors/1,
            fun write_should_append_value/1,
            fun write_should_replace_value/1,
            fun write_should_pass_errors/1
        ]
    }.

%%%===================================================================
%%% Test functions
%%%===================================================================

read_should_return_value(_) ->
    ?assertEqual({ok, ?APP_CONFIGS}, onepanel_env:read([], "p1")),
    ?assertEqual({ok, ?APP_CONFIG_1}, onepanel_env:read([a1], "p1")),
    ?assertEqual({ok, ?APP_CONFIG_1}, onepanel_env:read(a1, "p1")),
    ?assertEqual({ok, ?APP_CONFIG_2}, onepanel_env:read([a2], "p1")),
    ?assertEqual({ok, ?APP_CONFIG_2}, onepanel_env:read(a2, "p1")),
    ?assertEqual({ok, v1}, onepanel_env:read([a1, k1], "p1")),
    ?assertEqual({ok, v2}, onepanel_env:read([a1, k2], "p1")),
    ?assertEqual({ok, v3}, onepanel_env:read([a1, k3], "p1")),
    ?assertEqual({ok, v4}, onepanel_env:read([a2, k4], "p1")),
    ?assertEqual({ok, ?VALUE_5}, onepanel_env:read([a2, k5], "p1")),
    ?assertEqual({ok, v6}, onepanel_env:read([a2, k5, k6], "p1")),
    ?assertEqual({ok, ?VALUE_7}, onepanel_env:read([a2, k5, k7], "p1")),
    ?_assertEqual({ok, v8}, onepanel_env:read([a2, k5, k7, k8], "p1")).


read_should_report_missing_key(_) ->
    ?assertMatch(#error{reason = ?ERR_NOT_FOUND},
        onepanel_env:read([a3], "p1")),
    ?assertMatch(#error{reason = ?ERR_NOT_FOUND},
        onepanel_env:read([a1, k4], "p1")),
    ?assertMatch(#error{reason = ?ERR_NOT_FOUND},
        onepanel_env:read([a2, k5, k8], "p1")),
    ?_assertMatch(#error{reason = ?ERR_NOT_FOUND},
        onepanel_env:read([a2, k5, k7, k9], "p1")).


read_should_pass_errors(_) ->
    ?_assertThrow(#error{reason = enoent}, onepanel_env:read([a1], "p2")).


write_should_append_value(_) ->
    ?assertEqual(ok, onepanel_env:write([a3], [{k9, v9}], "p1")),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a1, ?APP_CONFIG_1},
        {a2, ?APP_CONFIG_2},
        {a3, [{k9, v9}]}
    ])}, pop_msg()),
    ?assertEqual(ok, onepanel_env:write([a1, k9], v9, "p1")),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a1, [{k1, v1}, {k2, v2}, {k3, v3}, {k9, v9}]},
        {a2, ?APP_CONFIG_2}
    ])}, pop_msg()),
    ?assertEqual(ok, onepanel_env:write([a2, k5, k9], v9, "p1")),
    Msg = pop_msg(),
    ?_assertEqual({ok, ?FILE_CONTENT([
        {a1, ?APP_CONFIG_1},
        {a2, [
            {k4, v4},
            {k5, ?VALUE_5 ++ [{k9, v9}]}
        ]}
    ])}, Msg).


write_should_replace_value(_) ->
    ?assertEqual(ok, onepanel_env:write([a1], [{k9, v9}], "p1")),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a1, [{k9, v9}]},
        {a2, ?APP_CONFIG_2}
    ])}, pop_msg()),
    ?assertEqual(ok, onepanel_env:write([a1, k1], v9, "p1")),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a1, [{k1, v9}, {k2, v2}, {k3, v3}]},
        {a2, ?APP_CONFIG_2}
    ])}, pop_msg()),
    ?assertEqual(ok, onepanel_env:write([a2, k5], v9, "p1")),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a1, ?APP_CONFIG_1},
        {a2, [
            {k4, v4},
            {k5, v9}
        ]}
    ])}, pop_msg()),
    ?assertEqual(ok, onepanel_env:write([a2, k5, k7], v9, "p1")),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a1, ?APP_CONFIG_1},
        {a2, [
            {k4, v4},
            {k5, [
                {k6, v6},
                {k7, v9}
            ]}
        ]}
    ])}, pop_msg()),
    ?assertEqual(ok, onepanel_env:write([a2, k5, k7, k8], v9, "p1")),
    Msg = pop_msg(),
    ?_assertEqual({ok, ?FILE_CONTENT([
        {a1, ?APP_CONFIG_1},
        {a2, [
            {k4, v4},
            {k5, [
                {k6, v6},
                {k7, [
                    {k8, v9}
                ]}
            ]}
        ]}
    ])}, Msg).


write_should_pass_errors(_) ->
    ?_assertThrow(#error{reason = enoent},
        onepanel_env:write([a1, k1], v9, "p2")).

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start() ->
    meck:new([file], [unstick, passthrough]),

    meck:expect(file, consult, fun
        ("p1") ->
            {ok, [?APP_CONFIGS]};
        (Path) ->
            meck:passthrough([Path])
    end),

    meck:expect(file, write_file, fun
        ("p1", Content) ->
            self() ! {ok, lists:flatten(Content)},
            ok;
        (Path, Content) ->
            meck:passthrough([Path, Content])
    end),

    ok.


stop(_) ->
    remove_msgs(),
    ?assert(meck:validate([file])),
    meck:unload().

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv pop_msg(timer:seconds(5))
%%--------------------------------------------------------------------
-spec pop_msg() -> Any :: term() | timeout.
pop_msg() ->
    pop_msg(timer:seconds(5)).


%%--------------------------------------------------------------------
%% @doc Returns first message from process message queue.
%%--------------------------------------------------------------------
-spec pop_msg(Timeout :: timeout()) -> Any :: term() | timeout.
pop_msg(Timeout) ->
    receive
        Any -> Any
    after
        Timeout -> timeout
    end.


%%--------------------------------------------------------------------
%% @doc Removes all message from porcess message queue.
%%--------------------------------------------------------------------
-spec remove_msgs() -> ok.
remove_msgs() ->
    case pop_msg(0) of
        timeout -> ok;
        _ -> pop_msg(0)
    end.

-endif.