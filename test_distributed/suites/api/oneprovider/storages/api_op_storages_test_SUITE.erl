%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider storages API (REST): the
%%% operations' answers that don't depend on a storage's type. The details
%%% that do are checked by each type's suite.
%%% @end
%%%-------------------------------------------------------------------
-module(api_op_storages_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").
-include_lib("onenv_ct/include/chart_values.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    get_storage_ids_test/1,
    get_storage_test/1,
    modify_storage_test/1
]).

all() -> [
    get_storage_ids_test,
    get_storage_test,
    modify_storage_test
].

-define(INEXISTENT_STORAGE_ID, <<"inexistentStorageId">>).


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


get_storage_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    [StorageId | _] = opw_test_rpc:get_storages(krakow),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get storage details using /provider/storages/{storage_id} rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(ProviderId),
            prepare_args_fun = fun(#api_test_ctx{data = Data}) ->
                {Id, _} = api_test_utils:maybe_substitute_bad_id(StorageId, Data),
                #rest_args{
                    method = get,
                    path = <<"provider/storages/", Id/binary>>
                }
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertMatch(#{<<"id">> := StorageId}, RespBody)
            end),
            data_spec = #data_spec{
                bad_values = [{bad_id, ?INEXISTENT_STORAGE_ID, ?ERROR_NOT_FOUND}]
            }
        }
    ])).


%% The correct request changes nothing, so that the storage stays as the other
%% tests of the suite find it.
modify_storage_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    [StorageId | _] = opw_test_rpc:get_storages(krakow),
    #{<<"name">> := StorageName, <<"type">> := StorageType} = api_test_utils:describe_storage(krakow, StorageId),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Modify storage using /provider/storages/{id} rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                ProviderId, [?CLUSTER_UPDATE]
            ),
            prepare_args_fun = fun(#api_test_ctx{data = Data}) ->
                {Id, _} = api_test_utils:maybe_substitute_bad_id(StorageId, Data),
                #rest_args{
                    method = patch,
                    path = <<"provider/storages/", Id/binary>>,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(#{StorageName => #{<<"type">> => StorageType}})
                }
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertMatch(#{<<"id">> := StorageId, <<"verificationPassed">> := true}, RespBody)
            end),
            data_spec = #data_spec{
                bad_values = [{bad_id, ?INEXISTENT_STORAGE_ID, ?ERROR_NOT_FOUND}]
            }
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
