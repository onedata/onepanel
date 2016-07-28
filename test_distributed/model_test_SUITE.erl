%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(model_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
    end_per_testcase/2]).

%% tests
-export([
    create_test/1,
    get_test/1,
    save_test/1,
    update_test/1,
    exists_test/1,
    delete_test/1
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
        delete_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    Nodes = ?config(onepanel_nodes, Config),

    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertEqual(ok, call(Nodes, create, [Record])),
    ?assertMatch(#error{reason = ?ERR_ALREADY_EXISTS},
        call(Nodes, create, [Record])).


get_test(Config) ->
    Nodes = ?config(onepanel_nodes, Config),

    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertMatch(#error{reason = ?ERR_NOT_FOUND}, call(Nodes, get, [1])),
    ?assertEqual(ok, call(Nodes, create, [Record])),
    ?assertEqual({ok, Record}, call(Nodes, get, [1])).


save_test(Config) ->
    Nodes = ?config(onepanel_nodes, Config),

    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertEqual(ok, call(Nodes, save, [Record])),
    ?assertEqual({ok, Record}, call(Nodes, get, [1])),

    Record2 = Record#?MODEL{field2 = <<"field2_new">>},
    ?assertEqual(ok, call(Nodes, save, [Record2])),
    ?assertEqual({ok, Record2}, call(Nodes, get, [1])).


update_test(Config) ->
    Nodes = ?config(onepanel_nodes, Config),

    ?assertMatch(#error{reason = ?ERR_NOT_FOUND},
        call(Nodes, update, [1, #{}])),

    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertEqual(ok, call(Nodes, save, [Record])),

    Record2 = Record#?MODEL{field2 = <<"field2_new">>},
    ?assertEqual(ok, call(Nodes, update, [1, #{field2 => <<"field2_new">>}])),
    ?assertEqual({ok, Record2}, call(Nodes, get, [1])),

    Record3 = Record2#?MODEL{field3 = field3_new},
    ?assertEqual(ok, call(Nodes, update,
        [1, fun(R) -> R#?MODEL{field3 = field3_new} end])),
    ?assertEqual({ok, Record3}, call(Nodes, get, [1])).


exists_test(Config) ->
    Nodes = ?config(onepanel_nodes, Config),

    ?assertEqual(false, call(Nodes, exists, [1])),
    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertEqual(ok, call(Nodes, save, [Record])),
    ?assertEqual(true, call(Nodes, exists, [1])).


delete_test(Config) ->
    Nodes = ?config(onepanel_nodes, Config),

    ?assertEqual(ok, call(Nodes, delete, [1])),
    Record = #?MODEL{field1 = 1, field2 = <<"field2">>, field3 = field3},
    ?assertEqual(ok, call(Nodes, save, [Record])),
    ?assertEqual(ok, call(Nodes, delete, [1])),
    ?assertEqual(false, call(Nodes, exists, [1])).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


init_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_new(Nodes, [model, ?MODEL], [passthrough, non_strict]),
    test_utils:mock_expect(Nodes, model, get_models, fun() -> [?MODEL] end),
    test_utils:mock_expect(Nodes, ?MODEL, get_fields, fun() ->
        record_info(fields, ?MODEL)
    end),
    onepanel_test_utils:init(Config).


end_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_unload(Nodes).

%%%===================================================================
%%% Internal functions
%%%===================================================================

call(Node, Function, Args) when is_atom(Node) ->
    rpc:call(Node, model, Function, [?MODEL | Args]);


call(Nodes, Function, Args) when is_list(Nodes) ->
    Node = utils:random_element(Nodes),
    call(Node, Function, Args).