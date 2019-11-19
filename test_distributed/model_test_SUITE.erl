%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of 'model' module.
%%% @end
%%%--------------------------------------------------------------------
-module(model_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

%% tests
-export([
    create_test/1,
    get_test/1,
    save_test/1,
    update_test/1,
    exists_test/1,
    delete_test/1,
    list_test/1,
    select_test/1,
    size_test/1,
    clear_test/1,
    wrapper_test/1,
    upgrade_test/1,
    upgrade_loop_is_detected_test/1
]).

-define(MODEL, example_model).
-record(?MODEL, {
    field1 :: integer(),
    field2 :: binary(),
    field3 :: atom()
}).

all() ->
    ?ALL([
        create_test,
        get_test,
        save_test,
        update_test,
        exists_test,
        delete_test,
        list_test,
        select_test,
        size_test,
        clear_test,
        wrapper_test,
        upgrade_test,
        upgrade_loop_is_detected_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertEqual({ok, 1}, ?callAny(Config, model, create, [?MODEL, Record])),
    ?assertMatch({error, already_exists},
        ?callAny(Config, model, create, [?MODEL, Record])).


get_test(Config) ->
    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertMatch(?ERR_DOC_NOT_FOUND, ?callAny(Config, model, get, [?MODEL, 1])),
    ?assertEqual({ok, 1}, ?callAny(Config, model, create, [?MODEL, Record])),
    ?assertEqual({ok, Record}, ?callAny(Config, model, get, [?MODEL, 1])).


save_test(Config) ->
    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertEqual(ok, ?callAny(Config, model, save, [?MODEL, Record])),
    ?assertEqual({ok, Record}, ?callAny(Config, model, get, [?MODEL, 1])),

    Record2 = Record#?MODEL{field2 = <<"field2_new">>},
    ?assertEqual(ok, ?callAny(Config, model, save, [?MODEL, Record2])),
    ?assertEqual({ok, Record2}, ?callAny(Config, model, get, [?MODEL, 1])).


update_test(Config) ->
    ?assertMatch(?ERR_DOC_NOT_FOUND,
        ?callAny(Config, model, update, [?MODEL, 1, #{}])),

    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertEqual(ok, ?callAny(Config, model, save, [?MODEL, Record])),

    Record2 = Record#?MODEL{field2 = <<"field2_new">>},
    ?assertEqual(ok, ?callAny(Config, model, update,
        [?MODEL, 1, #{field2 => <<"field2_new">>}])),
    ?assertEqual({ok, Record2}, ?callAny(Config, model, get, [?MODEL, 1])),

    Record3 = Record2#?MODEL{field3 = field3_new},
    ?assertEqual(ok, ?callAny(Config, model, update,
        [?MODEL, 1, fun(R) -> R#?MODEL{field3 = field3_new} end])),
    ?assertEqual({ok, Record3}, ?callAny(Config, model, get, [?MODEL, 1])).


exists_test(Config) ->
    ?assertEqual(false, ?callAny(Config, model, exists, [?MODEL, 1])),
    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertEqual(ok, ?callAny(Config, model, save, [?MODEL, Record])),
    ?assertEqual(true, ?callAny(Config, model, exists, [?MODEL, 1])).


delete_test(Config) ->
    ?assertEqual(ok, ?callAny(Config, model, delete, [?MODEL, 1])),
    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertEqual(ok, ?callAny(Config, model, save, [?MODEL, Record])),
    ?assertEqual(ok, ?callAny(Config, model, delete, [?MODEL, 1])),
    ?assertEqual(false, ?callAny(Config, model, exists, [?MODEL, 1])).


list_test(Config) ->
    Records = ?config(records, Config),
    ?assertEqual(Records, lists:sort(?callAny(Config, model, list, [?MODEL]))).


select_test(Config) ->
    [Record1, Record2, Record3 | _] = ?config(records, Config),
    ?assertEqual([Record1], ?callAny(Config, model, select, [
        ?MODEL, [{#?MODEL{field1 = 1, _ = '_'}, []}]
    ])),
    ?assertEqual([Record2], ?callAny(Config, model, select, [
        ?MODEL, [{#?MODEL{field1 = 2, _ = '_'}, []}]
    ])),
    ?assertEqual([Record3], ?callAny(Config, model, select, [
        ?MODEL, [{#?MODEL{field1 = 3, _ = '_'}, []}]
    ])),
    ?assertEqual([Record1, Record2], ?callAny(Config, model, select, [
        ?MODEL, [{#?MODEL{field2 = <<"field1">>, _ = '_'}, []}]
    ])),
    ?assertEqual([Record1, Record3, Record2], ?callAny(Config, model, select, [
        ?MODEL, [{#?MODEL{field3 = field1, _ = '_'}, []}]
    ])).


size_test(Config) ->
    Length = erlang:length(?config(records, Config)),
    ?assertEqual(Length, ?callAny(Config, model, size, [?MODEL])).


clear_test(Config) ->
    ?assertEqual(ok, ?callAny(Config, model, clear, [?MODEL])),
    ?assertEqual(0, ?callAny(Config, model, size, [?MODEL])).


% Model record stored directly in a table
% should be wrapped in #document{} record when tables are upgraded
wrapper_test(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Key = 3,
    Record = #?MODEL{field1 = Key, field2 = <<"binary">>, field3 = atom},
    ?assertEqual(ok, rpc:call(Node, model, transaction, [fun() ->
        mnesia:write(?MODEL, Record, write)
    end])),

    ok = rpc:call(Node, onepanel_db, upgrade_tables, []),

    ?assertMatch([#document{key = Key, value = Record}],
        rpc:call(Node, model, transaction, [fun() ->
            mnesia:read(?MODEL, Key)
        end])),
    ?assertMatch({ok, Record},
        rpc:call(Node, model, get, [?MODEL, Key])).


upgrade_test(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Key = 123,
    OldRecord = {?MODEL, Key, <<"to-be-upgraded">>},
    OldDoc = #document{key = Key, version = 1, value = OldRecord},

    ExpectedRecord = {?MODEL, Key, <<"upgraded">>, undefined},
    ExpectedDoc = OldDoc#document{value = ExpectedRecord, version = 2},

    ?assertEqual(ok, rpc:call(Node, model, transaction, [fun() ->
        mnesia:write(?MODEL, OldDoc, write)
    end])),
    ?assertEqual({ok, OldRecord},
        rpc:call(Node, model, get, [?MODEL, Key])),

    ?assertEqual(ok, rpc:call(Node, onepanel_db, upgrade_tables, [])),

    ?assertMatch([ExpectedDoc],
        rpc:call(Node, model, transaction, [fun() ->
            mnesia:read(?MODEL, Key)
        end])),
    ?assertEqual({ok, ExpectedRecord},
        rpc:call(Node, model, get, [?MODEL, Key])).


% upgrade function returning old version number as new should not cause infinite loop
upgrade_loop_is_detected_test(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Key = 123,
    OldRecord = {?MODEL, Key, <<"to-be-upgraded">>},
    OldDoc = #document{key = Key, version = 1, value = OldRecord},

    ?assertEqual(ok, rpc:call(Node, model, transaction, [fun() ->
        mnesia:write(?MODEL, OldDoc, write)
    end])),

    ?assertMatch({error, _}, rpc:call(Node, onepanel_db, upgrade_tables, [])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_testcase(Case, Config) when
    Case =:= list_test;
    Case =:= select_test;
    Case =:= size_test;
    Case =:= clear_test ->
    NewConfig = init_per_testcase(default, Config),
    Records = lists:sort([
        #?MODEL{field1 = 1, field2 = <<"field1">>, field3 = field1},
        #?MODEL{field1 = 2, field2 = <<"field1">>, field3 = field1},
        #?MODEL{field1 = 3, field2 = <<"field2">>, field3 = field1}
    ]),
    lists:foreach(fun(Record) ->
        ?assertEqual(ok, ?callAny(NewConfig, model, save, [?MODEL, Record]))
    end, Records),
    [{records, Records} | NewConfig];

init_per_testcase(wrapper_test, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    NewConfig = init_per_testcase(default, Config),
    % revert to older table schema to test its upgrade
    {_, []} = rpc:multicall(Nodes, mnesia, transform_table, [
        ?MODEL,
        fun(_Record) -> throw(table_not_empty) end,
        record_info(fields, ?MODEL),
        ?MODEL
    ]),
    NewConfig;

init_per_testcase(upgrade_test, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    NewConfig = init_per_testcase(default, Config),
    test_utils:mock_expect(Nodes, ?MODEL, upgrade, fun(1 = _CurrentVsn, Record) ->
        {?MODEL, Key, <<"to-be-upgraded">>} = Record,
        {2, {?MODEL, Key, <<"upgraded">>, undefined}}
    end),
    test_utils:mock_expect(Nodes, ?MODEL, get_record_version, fun() -> 2 end),
    NewConfig;

init_per_testcase(upgrade_loop_is_detected_test, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    NewConfig = init_per_testcase(default, Config),
    test_utils:mock_expect(Nodes, ?MODEL, upgrade, fun(CurrentVsn, Record) ->
        {?MODEL, Key, <<"to-be-upgraded">>} = Record,
        % simulate incorrect new version number
        {CurrentVsn, {?MODEL, Key, <<"upgraded">>, undefined}}
    end),
    test_utils:mock_expect(Nodes, ?MODEL, get_record_version, fun() -> 2 end),
    NewConfig;

init_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    onepanel_test_utils:ensure_started(Config),
    test_utils:mock_new(Nodes, [model, onepanel_deployment, ?MODEL],
        [passthrough, non_strict]),

    % 'service' model required for detecting unregistered provider
    % in https_listener:response_headers/0
    test_utils:mock_expect(Nodes, model, get_models, fun() -> [service, ?MODEL] end),

    % required for successful deployment
    test_utils:mock_expect(Nodes, onepanel_deployment, is_set,
        fun(_) -> false end),
    test_utils:mock_expect(Nodes, ?MODEL, get_fields, fun() ->
        record_info(fields, ?MODEL)
    end),
    test_utils:mock_expect(Nodes, ?MODEL, get_record_version, fun() ->
        1
    end),
    test_utils:mock_expect(Nodes, ?MODEL, seed, fun() -> ok end),
    ok = rpc:call(hd(Nodes), service_onepanel, reset_node, [#{}]),
    ok = rpc:call(hd(Nodes), service_onepanel, init_cluster, [#{}]),
    onepanel_test_utils:init(Config).


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes).