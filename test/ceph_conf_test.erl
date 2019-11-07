%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for ceph_conf module.
%%% @end
%%%--------------------------------------------------------------------
-module(ceph_conf_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-include("modules/errors.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(FILE1, "p1").
-define(IP1, <<"1.1.1.1">>).
-define(IP2, <<"1.2.3.4">>).
-define(INPUT, <<
    "[global]\n",
    "# a comment\n",
    "auth client required = cephx\n",
    "mon health   = false # with comment\n",
    " mon host = ", ?IP1/binary, ", ", ?IP2/binary, "\n",
    "[osd]\n",
    "osd journal size = 1000 ; other comment\n",
    "[mon.a]\n",
    "auth service required = cephx\n"
>>).

-define(PARSED, #{
    global => #{
        <<"auth client required">> => <<"cephx">>,
        <<"mon health">> => <<"false">>,
        <<"mon host">> => [?IP1, ?IP2]
    },
    osd => #{
        <<"osd journal size">> => <<"1000">>
    },
    <<"mon.a">> => #{
        <<"auth service required">> => <<"cephx">>
    }
}).

%%%===================================================================
%%% Test generators
%%%===================================================================

ceph_conf_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun parse_should_return_whole_config/0,
            fun write_should_format_config_/1,
            fun update_test_/1,
            fun put_test_/1,
            fun append_test_/1,
            fun find_test_/1
        ]
    }.

%%%===================================================================
%%% Test functions
%%%===================================================================


parse_should_return_whole_config() ->
    ?assertEqual(?PARSED, ceph_conf:parse(?INPUT)).


write_should_format_config_(_) ->
    lists:map(fun({Name, Config, Expected}) ->
        {Name, fun() ->
            ?assertEqual(ok, ceph_conf:write(Config, ?FILE1)),
            {ok, Written} = pop_msg(),
            ?assertEqual(Expected, Written)
        end}
    end, [
        {"format empty config",
            #{},
            <<>>},
        {"format single scope",
            #{
                osd => #{
                    <<"mon health">> => <<"false">>,
                    <<"mon host">> => [<<"1.1.1.1">>, <<"1.2.3.4">>]
                }
            },
            <<
                "[osd]\n"
                "mon health = false\n"
                "mon host = 1.1.1.1, 1.2.3.4\n"
            >>
        },
        {"format multiple scopes",
            #{
                osd => #{
                    <<"mon health">> => <<"false">>
                },
                <<"mon.a">> => #{
                    <<"some int">> => 300
                }
            },
            <<
                "[osd]\n",
                "mon health = false\n",
                "[mon.a]\n",
                "some int = 300\n"
            >>
        }
    ]).


update_test_(_) ->
    Base = #{
        global => Global = #{
            <<"key">> => <<"value">>,
            <<"key2">> => <<"value2">>
        },
        <<"mon.a">> => #{
            <<"key">> => <<"value">>
        }
    },

    lists:map(fun({Name, {Scope, Key, Updater}, Expected}) ->
        {Name,
            ?_assertEqual(Expected, ceph_conf:update(Scope, Key, Updater, Base))}
    end, [
        {"change value",
            {global, <<"key">>, fun(Old) ->
                ?assertEqual(<<"value">>, Old),
                <<"newvalue">>
            end},
            Base#{global => Global#{<<"key">> => <<"newvalue">>}}},
        {"use binary scope",
            {<<"global">>, <<"key">>, fun(Old) ->
                ?assertEqual(<<"value">>, Old),
                <<"newvalue">>
            end},
            Base#{global => Global#{<<"key">> => <<"newvalue">>}}},
        {"create key",
            {global, <<"newkey">>, fun(Old) ->
                ?assertEqual(undefined, Old),
                <<"newvalue">>
            end},
            Base#{global => Global#{<<"newkey">> => <<"newvalue">>}}},
        {"create scope",
            {<<"osd.1">>, <<"newkey">>, fun(Old) ->
                ?assertEqual(undefined, Old),
                <<"newvalue">>
            end},
            Base#{global => Global, <<"osd.1">> => #{<<"newkey">> => <<"newvalue">>}}},
        {"remove key",
            {global, <<"key">>, fun(_) -> undefined end},
            Base#{global => #{<<"key2">> => <<"value2">>}}},
        {"remove scope after last key removed",
            {<<"mon.a">>, <<"key">>, fun(_) -> undefined end},
            maps:remove(<<"mon.a">>, Base)}
    ]).


put_test_(_) ->
    Base = #{global => Global = #{
        <<"key">> => <<"value">>
    }},
    lists:map(fun({Name, {Scope, Key, Value}, Expected}) ->
        {Name, ?_assertEqual(Expected, ceph_conf:put(Scope, Key, Value, Base))}
    end, [
        {"change value",
            {global, <<"key">>, <<"newvalue">>},
            Base#{global => Global#{<<"key">> => <<"newvalue">>}}},
        {"use binary scope",
            {<<"global">>, <<"key">>, <<"newvalue">>},
            Base#{global => Global#{<<"key">> => <<"newvalue">>}}},
        {"create key",
            {global, <<"newkey">>, <<"newvalue">>},
            Base#{global => Global#{<<"newkey">> => <<"newvalue">>}}},
        {"create scope",
            {<<"osd.1">>, <<"newkey">>, <<"newvalue">>},
            Base#{global => Global, <<"osd.1">> => #{<<"newkey">> => <<"newvalue">>}}}
    ]).



append_test_(_) ->
    Base = #{global => Global = #{
        <<"key">> => <<"v1">>,
        <<"list">> => [<<"v1">>, <<"v2">>]
    }},
    lists:map(fun({Name, {Scope, Key, Values}, Expected}) ->
        {Name, ?_assertEqual(Expected, ceph_conf:append(Scope, Key, Values, Base))}
    end, [
        {"append single value to single value",
            {global, <<"key">>, <<"v2">>},
            Base#{global => Global#{<<"key">> => [<<"v1">>, <<"v2">>]}}},
        {"use binary scope",
            {<<"global">>, <<"key">>, <<"v2">>},
            Base#{global => Global#{<<"key">> => [<<"v1">>, <<"v2">>]}}},
        {"append single value to list",
            {global, <<"list">>, <<"v3">>},
            Base#{global => Global#{<<"list">> => [<<"v1">>, <<"v2">>, <<"v3">>]}}},
        {"append list to single value",
            {global, <<"key">>, [<<"v2">>, <<"v3">>]},
            Base#{global => Global#{<<"key">> => [<<"v1">>, <<"v2">>, <<"v3">>]}}},
        {"append list to list",
            {global, <<"list">>, [<<"v3">>, <<"v4">>]},
            Base#{global => Global#{<<"list">> => [<<"v1">>, <<"v2">>, <<"v3">>, <<"v4">>]}}},
        {"create key",
            {global, <<"newkey">>, <<"newvalue">>},
            Base#{global => Global#{<<"newkey">> => [<<"newvalue">>]}}},
        {"create scope",
            {<<"osd.1">>, <<"newkey">>, <<"newvalue">>},
            Base#{global => Global, <<"osd.1">> => #{<<"newkey">> => [<<"newvalue">>]}}}
    ]).


find_test_(_) ->
    Base = #{
        global => Global = #{
            <<"key">> => <<"v1">>,
            <<"list">> => [<<"v1">>, <<"v2">>]
        },
        <<"mon.a">> => #{
            <<"key">> => <<"value">>
        }
    },

    lists:map(fun({Name, {Scope, Key}, Expected}) ->
        {Name, ?_assertEqual(Expected, ceph_conf:find(Scope, Key, Base))}
    end, [
        {"find value",
            {global, <<"key">>},
            {ok, <<"v1">>}},
        {"find value usign binary scope",
            {<<"global">>, <<"key">>},
            {ok, <<"v1">>}}
    ])
    ++ lists:map(fun({Name, {Scope, Key}}) ->
        {Name, ?_assertMatch({error, ?ERR_NOT_FOUND},
            ceph_conf:find(Scope, Key, Base))}
    end, [
        {"missing key",
            {<<"global">>, <<"nonexistent">>}},
        {"missing scope",
            {osd, <<"nonexistend">>}}
    ]).


%===================================================================
% Test fixtures
%===================================================================

-spec start() -> ok.
start() ->
    meck:new([file], [unstick, passthrough]),

    meck:expect(file, read_file, fun
        ("p1") ->
            {ok, ?INPUT};
        (Path) ->
            meck:passthrough([Path])
    end),

    meck:expect(file, write_file, fun
        ("p1", Content) ->
            self() ! {ok, Content},
            ok;
        (Path, Content) ->
            meck:passthrough([Path, Content])
    end).


-spec stop(term()) -> term().
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

