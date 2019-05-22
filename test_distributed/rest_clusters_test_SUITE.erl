%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Empty test SUITE to be implemented in version 19.02.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_clusters_test_SUITE).
-author("Wojciech Geisler").

-include_lib("eunit/include/eunit.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([dummy_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([dummy_test]).

dummy_test(_Config) ->
    ok.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    Posthook = fun(NewConfig) ->
        onepanel_test_utils:init(NewConfig)
    end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].


end_per_suite(_Config) ->
    ok.