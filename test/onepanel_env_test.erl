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
    {k3, v3},
    {k4, v4}
]).
-define(IGNORED, [
    {a1, [{k1, i1}]}
]).
-define(CUSTOM_1, [
    {a1, [{k1, c1}, {k2, c2}, {k4, c4}, {k10, c10}]}
]).
-define(CUSTOM_2, [
    {a1, [{k1, d1}, {k2, d2}]}
]).
-define(OVERLAY, [
    {a1, [{k1, o1}]}
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
-define(FILE_CONTENT(X), lists:flatten(io_lib:format(
    "% MACHINE GENERATED FILE. DO NOT MODIFY.~n"
    "% Use overlay.config for custom configuration.~n~n"
    "~tp.", [X]))).

%%%===================================================================
%%% Test generators
%%%===================================================================

onepanel_env_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            {"read_should_return_value", fun read_should_return_value/0},
            {"read_should_report_missing_key", fun read_should_report_missing_key/0},
            {"read_should_pass_errors", fun read_should_pass_errors/0},
            {"write_should_append_value", fun write_should_prepend_value/0},
            {"write_should_replace_value", fun write_should_replace_value/0},
            {"rename_should_rename_key", fun rename_should_rename_key/0},
            {"rename_should_ignore_missing", fun rename_should_ignore_missing/0},
            {"write_should_pass_errors", fun write_should_pass_errors/0},
            {"read_effective_does_not_read_whole_config", fun read_effective_does_not_read_whole_config/0},
            {"read_effective_follows_priority", fun read_effective_follows_priority/0}
        ]
    }.

%%%===================================================================
%%% Test functions
%%%===================================================================

read_should_return_value() ->
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
    ?assertEqual({ok, v8}, onepanel_env:read([a2, k5, k7, k8], "p1")).


read_should_report_missing_key() ->
    ?assertMatch(error,
        onepanel_env:read([a3], "p1")),
    ?assertMatch(error,
        onepanel_env:read([a1, k5], "p1")),
    ?assertMatch(error,
        onepanel_env:read([a2, k6, k8], "p1")),
    ?assertMatch(error,
        onepanel_env:read([a2, k6, k7, k9], "p1")).


read_should_pass_errors() ->
    ?assertThrow(?ERROR_FILE_ACCESS("p2", enoent), onepanel_env:read([a1], "p2")).


write_should_prepend_value() ->
    ?assertEqual(ok, onepanel_env:write([a3], [{k9, v9}], service1)),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a3, [{k9, v9}]},
        {a1, ?APP_CONFIG_1},
        {a2, ?APP_CONFIG_2}
    ])}, pop_msg()),
    ?assertEqual(ok, onepanel_env:write([a1, k9], v9, service1)),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a1, [{k9, v9}, {k1, v1}, {k2, v2}, {k3, v3}, {k4, v4}]},
        {a2, ?APP_CONFIG_2}
    ])}, pop_msg()),
    ?assertEqual(ok, onepanel_env:write([a2, k5, k9], v9, service1)),
    Msg = pop_msg(),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a1, ?APP_CONFIG_1},
        {a2, [
            {k4, v4},
            {k5, [{k9, v9} | ?VALUE_5]}
        ]}
    ])}, Msg).


write_should_replace_value() ->
    ?assertEqual(ok, onepanel_env:write([a1], [{k9, v9}], service1)),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a1, [{k9, v9}]},
        {a2, ?APP_CONFIG_2}
    ])}, pop_msg()),
    ?assertEqual(ok, onepanel_env:write([a1, k1], v9, service1)),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a1, [{k1, v9}, {k2, v2}, {k3, v3}, {k4, v4}]},
        {a2, ?APP_CONFIG_2}
    ])}, pop_msg()),
    ?assertEqual(ok, onepanel_env:write([a2, k5], v9, service1)),
    ?assertEqual({ok, ?FILE_CONTENT([
        {a1, ?APP_CONFIG_1},
        {a2, [
            {k4, v4},
            {k5, v9}
        ]}
    ])}, pop_msg()),
    ?assertEqual(ok, onepanel_env:write([a2, k5, k7], v9, service1)),
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
    ?assertEqual(ok, onepanel_env:write([a2, k5, k7, k8], v9, service1)),
    Msg = pop_msg(),
    ?assertEqual({ok, ?FILE_CONTENT([
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


write_should_pass_errors() ->
    ?assertThrow(?ERROR_FILE_ACCESS("/nonexistent/p3", enoent),
        onepanel_env:write([a1, k1], v9, service3)).


rename_should_rename_key() ->
    ?assertEqual(true, onepanel_env:rename(service1, [a1, k2], [a1, k9])),
    Expected = {ok, ?FILE_CONTENT([
        {a1, [{k9, v2}, {k1, v1}, {k3, v3}, {k4, v4}]},
        {a2, ?APP_CONFIG_2}
    ])},
    Result = pop_msg(),
    ?assertEqual(Expected, Result).


rename_should_ignore_missing() ->
    ?assertEqual(false, onepanel_env:rename(service1, [a1, missing], [a1, k9])),
    % no write happens when there is no change
    Msg = pop_msg(),
    ?assertEqual(timeout, Msg).


read_effective_does_not_read_whole_config() ->
    ?assertError(badarg, onepanel_env:read_effective([], s1)),
    ?assertError(badarg, onepanel_env:read_effective([a1], s1)).


read_effective_follows_priority() ->
    % not overridden
    ?assertEqual({ok, v3}, onepanel_env:read_effective([a1, k3], service1)),
    ?assertEqual({ok, v4}, onepanel_env:read_effective([a2, k4], service1)),
    % set in custom 1
    ?assertEqual({ok, c4}, onepanel_env:read_effective([a1, k4], service1)),
    ?assertEqual({ok, c10}, onepanel_env:read_effective([a1, k10], service1)),

    % set in custom 2
    ?assertEqual({ok, d2}, onepanel_env:read_effective([a1, k2], service1)),

    % set in overlay - top priority
    ?assertEqual({ok, o1}, onepanel_env:read_effective([a1, k1], service1)).


%%%===================================================================
%%% Test fixtures
%%%===================================================================

start() ->
    meck:unload(),
    meck:new([file], [unstick, passthrough]),

    meck:expect(file, consult, fun(Path) ->
        case onepanel_utils:convert(filename:flatten(Path), list) of
            "p1" -> {ok, [?APP_CONFIGS]};
            "config.d/01-custom.config" -> {ok, [?CUSTOM_1]};
            "config.d/ignored.bad-suffix" -> {ok, [?IGNORED]};
            "config.d/10-graphana.config" -> {ok, [?CUSTOM_2]};
            "overlay.config" -> {ok, [?OVERLAY]};
            Path -> meck:passthrough([Path])
        end
    end),

    meck:expect(file, list_dir, fun
        ("config.d") -> {ok, ["10-graphana.config", "01-custom.config", "ignored.bad-suffix"]};
        ("config.d/") -> {ok, ["10-graphana.config", "01-custom.config", "ignored.bad-suffix"]};
        (Path) -> error({unexpected_dir, Path})
    end),

    meck:expect(file, write_file, fun
        ("p1", Content) ->
            self() ! {ok, lists:flatten(Content)},
            ok;
        (Path, Content) ->
            meck:passthrough([Path, Content])
    end),


    meck:expect(onepanel_env, get, fun
        (service1_app_config_file) -> "p1";
        (service1_generated_config_file) -> "p1";
        (service1_overlay_config_file) -> "overlay.config";
        (service1_custom_config_dir) -> "config.d";
        (service2_generated_config_file) -> "p1";
        (service3_generated_config_file) -> "/nonexistent/p3";
        (Variable) -> meck:passthrough([Variable])
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
%% @end
%%--------------------------------------------------------------------
-spec pop_msg() -> Any :: term() | timeout.
pop_msg() ->
    pop_msg(timer:seconds(1)).


%%--------------------------------------------------------------------
%% @doc Returns first message from process message queue.
%% @end
%%--------------------------------------------------------------------
-spec pop_msg(timeout()) -> Any :: term() | timeout.
pop_msg(Timeout) ->
    receive
        Any -> Any
    after
        Timeout -> timeout
    end.


%%--------------------------------------------------------------------
%% @doc Removes all message from process message queue.
%% @end
%%--------------------------------------------------------------------
-spec remove_msgs() -> ok.
remove_msgs() ->
    case pop_msg(0) of
        timeout -> ok;
        _ -> pop_msg(0)
    end.

-endif.