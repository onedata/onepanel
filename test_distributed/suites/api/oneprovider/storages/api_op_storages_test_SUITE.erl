%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider storages API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_op_storages_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").
-include_lib("onenv_ct/include/chart_values.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    get_storage_ids_test/1
]).

all() -> [
    get_storage_ids_test
].


%%%===================================================================
%%% API
%%%===================================================================


get_storage_ids_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    StoragesIds = opw_test_rpc:get_storages(krakow),

    ExpectedData = #{
        <<"ids">> => StoragesIds
    },

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get storage ids using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                ProviderId
            ),
            prepare_args_fun = fun(_) ->
                #rest_args{method = get, path = <<"provider/storages">>}
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ?assertEqual(ExpectedData, Body)
            end)
        }
    ])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op_s3"
    }).

end_per_suite(_Config) ->
    oct_background:end_per_suite().
