%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This suite is to be removed in 18.07.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_onepanel_session_test_SUITE).
-author("Krzysztof Trzepla").

-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% tests
-export([dummy_test/1]).


all() ->
    ?ALL([dummy_test]).

%%%===================================================================
%%% Test functions
%%%===================================================================

dummy_test(_Config) ->
    ok.

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    Posthook = fun(NewConfig) -> onepanel_test_utils:init(NewConfig) end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].


end_per_suite(_Config) ->
    ok.
