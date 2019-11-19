%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for onepanel_vm module.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_vm_test).
-author("Krzysztof Trzepla").

-ifdef(TEST).

-include_lib("ctool/include/errors.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test generators
%%%===================================================================

onepanel_vm_test_() ->
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
    ?assertEqual({ok, <<"v1">>}, onepanel_vm:read(<<"k1">>, "p1")),
    ?assertEqual({ok, <<"v2">>}, onepanel_vm:read(<<"k2">>, "p1")),
    ?_assertEqual({ok, <<"v3">>}, onepanel_vm:read(<<"k3">>, "p1")).


read_should_report_missing_key(_) ->
    ?_assertMatch(error, onepanel_vm:read(<<"k4">>, "p1")).


read_should_pass_errors(_) ->
    ?_assertThrow(?ERROR_FILE_ACCESS("p2", enoent), onepanel_vm:read(<<"k1">>, "p2")).


write_should_append_value(_) ->
    onepanel_vm:write(<<"k4">>, <<"v4">>, "p1"),
    Msg = pop_msg(),
    ?_assertEqual({ok, <<
        "-k1 v1\n"
        "-k2 v2\n"
        "-k3 v3\n"
        "-k4 v4"
    >>}, Msg).


write_should_replace_value(_) ->
    onepanel_vm:write(<<"k1">>, <<"v4">>, "p1"),
    ?assertEqual({ok, <<
        "-k1 v4\n"
        "-k2 v2\n"
        "-k3 v3"
    >>}, pop_msg()),
    onepanel_vm:write(<<"k2">>, <<"v4">>, "p1"),
    ?assertEqual({ok, <<
        "-k1 v1\n"
        "-k2 v4\n"
        "-k3 v3"
    >>}, pop_msg()),
    onepanel_vm:write(<<"k3">>, <<"v4">>, "p1"),
    Msg = pop_msg(),
    ?_assertEqual({ok, <<
        "-k1 v1\n"
        "-k2 v2\n"
        "-k3 v4"
    >>}, Msg).


write_should_pass_errors(_) ->
    ?_assertThrow(?ERROR_FILE_ACCESS("p2", enoent),
        onepanel_vm:write(<<"k1">>, <<"v1">>, "p2")).

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start() ->
    meck:new([file], [unstick, passthrough]),

    meck:expect(file, read_file, fun
        ("p1") ->
            {ok, <<
                "-k1 v1\n"
                "-k2 v2\n"
                "-k3 v3"
            >>};
        (Path) ->
            meck:passthrough([Path])
    end),

    meck:expect(file, write_file, fun
        ("p1", Content) ->
            self() ! {ok, Content},
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
%% @end
%%--------------------------------------------------------------------
-spec pop_msg() -> Any :: term() | timeout.
pop_msg() ->
    pop_msg(timer:seconds(5)).


%%--------------------------------------------------------------------
%% @doc Returns first message from process message queue.
%% @end
%%--------------------------------------------------------------------
-spec pop_msg(Timeout :: timeout()) -> Any :: term() | timeout.
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