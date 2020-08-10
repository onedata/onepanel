%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Example test SUITE using onenv
%%% @end
%%%-------------------------------------------------------------------
-module(example_onenv_test_SUITE).
-author("Michal Stanisz").

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    example_test/1
]).

all() -> [
    example_test
].


%%%===================================================================
%%% API
%%%===================================================================

example_test(Config) ->
    [P1 | _] = test_config:get_providers(Config),
    [Worker1P1 | _] = WorkersP1 = test_config:get_provider_nodes(Config, P1),
    [SpaceId | _] = test_config:get_provider_spaces(Config, P1),
    Workers = test_config:get_all_op_worker_nodes(Config),
    
    ct:print("P1: ~p~nSpaceId: ~p~nWorkers: ~p~n", [P1, SpaceId, Workers]),
    
    ok.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    Posthook = fun(NewConfig) ->
        onenv_test_utils:prepare_base_test_config(NewConfig)
    end,
    test_config:set_many(Config, [
        {add_envs, [op_panel, onepanel, [{key, value}]]},
        {add_envs, [oz_panel, onepanel, [{another_key, another_value}]]},
        {set_onenv_scenario, ["1op"]}, % name of yaml file in test_distributed/onenv_scenarios
        {set_posthook, Posthook}
    ]).

end_per_suite(_Config) ->
    ok.
