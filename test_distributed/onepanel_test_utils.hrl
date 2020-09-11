%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This header contains common test macros.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ONEPANEL_TEST_UTILS_HRL).
-define(ONEPANEL_TEST_UTILS_HRL, 1).

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/performance.hrl").

-define(API_VERSION, 3).

-define(call(Config, Module, Function, Args),
    ?call(Config, all_nodes, Module, Function, Args)).

-define(call(Config, NodesType, Module, Function, Args),
    rpc:call(hd(?config(NodesType, Config)), Module, Function, Args)).

-define(callAny(Config, Module, Function, Args),
    ?callAny(Config, all_nodes, Module, Function, Args)).

-define(callAny(Config, NodesType, Module, Function, Args),
    rpc:call(lists_utils:random_element(?config(NodesType, Config)),
        Module, Function, Args)).

-define(callAll(Config, Module, Function, Args),
    ?callAll(Config, all_nodes, Module, Function, Args)).

-define(callAll(Config, NodesType, Module, Function, Args),
    lists:map(fun(Node) ->
        rpc:call(Node, Module, Function, Args)
    end, ?config(NodesType, Config))).

-define(assertAllEqual(Expect, ExprList),
    lists:foreach(fun(Expr) ->
        ?assertEqual(Expect, Expr)
    end, ExprList)).

-define(assertAllMatch(Expect, ExprList),
    lists:foreach(fun(Expr) ->
        ?assertMatch(Expect, Expr)
    end, ExprList)).

-endif.