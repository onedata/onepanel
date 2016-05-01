%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for vm_config_editor module.
%%% @end
%%%--------------------------------------------------------------------
-module(vm_config_editor_test).
-author("Krzysztof Trzepla").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test generators
%%%===================================================================

vm_config_editor_test_() ->
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
    ?assertEqual({ok, <<"v1">>}, vm_config_editor:read(<<"k1">>, "p1")),
    ?assertEqual({ok, <<"v2">>}, vm_config_editor:read(<<"k2">>, "p1")),
    ?_assertEqual({ok, <<"v3">>}, vm_config_editor:read(<<"k3">>, "p1")).


read_should_report_missing_key(_) ->
    ?_assertEqual({error, not_found}, vm_config_editor:read(<<"k4">>, "p1")).


read_should_pass_errors(_) ->
    ?_assertEqual({error, enoent}, vm_config_editor:read(<<"k1">>, "p2")).


write_should_append_value(_) ->
    ?_assertEqual({ok, <<
        "-k1 v1\n"
        "-k2 v2\n"
        "-k3 v3\n"
        "-k4 v4"
    >>}, vm_config_editor:write(<<"k4">>, <<"v4">>, "p1")).


write_should_replace_value(_) ->
    ?assertEqual({ok, <<
        "-k1 v4\n"
        "-k2 v2\n"
        "-k3 v3"
    >>}, vm_config_editor:write(<<"k1">>, <<"v4">>, "p1")),
    ?assertEqual({ok, <<
        "-k1 v1\n"
        "-k2 v4\n"
        "-k3 v3"
    >>}, vm_config_editor:write(<<"k2">>, <<"v4">>, "p1")),
    ?_assertEqual({ok, <<
        "-k1 v1\n"
        "-k2 v2\n"
        "-k3 v4"
    >>}, vm_config_editor:write(<<"k3">>, <<"v4">>, "p1")).


write_should_pass_errors(_) ->
    ?_assertEqual({error, enoent},
        vm_config_editor:write(<<"k1">>, <<"v1">>, "p2")).

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
            {ok, Content};
        (Path, Content) ->
            meck:passthrough([Path, Content])
    end),

    ok.


stop(_) ->
    ?assert(meck:validate([file])),
    meck:unload().

-endif.