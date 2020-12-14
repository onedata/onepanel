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
-module(api_oneprovider_storages_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).
-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_storages_ids/1,
    add_s3_storage/1,
    get_s3_storage/1,
    delete_s3_storage/1
]).

-define(S3_STORAGE_NAME, <<"s3Storage-1">>).
-define(S3_STORAGE_SPEC, #{
    ?S3_STORAGE_NAME => #{
        <<"type">> => <<"s3">>,
        <<"bucketName">> => <<"bucket2.iam.example.com">>,
        <<"hostname">> => <<"s3.amazonaws.com:80/">>,
        <<"skipStorageDetection">> => <<"true">>
    }
}).

all() -> [
    get_storages_ids, add_s3_storage, get_s3_storage, delete_s3_storage
].


%%%===================================================================
%%% API
%%%===================================================================


get_storages_ids(Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    StoragesIds = op_worker_test_rpc:get_storage_ids(Config),

    ExpectedData = #{
        <<"ids">> => StoragesIds
    },

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get storage ids using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, []}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) ->
                #rest_args{method = get, path = <<"provider/storages">>}
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ?assertEqual(ExpectedData, Body)
            end)
        }
    ])).


add_s3_storage(Config) ->
    % todo: VFS-6717 delete s3 storage after test
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    StorageIdsBeforeAdd = op_worker_test_rpc:get_storage_ids(Config),

    RequestBody = ?S3_STORAGE_SPEC,

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Add s3 storage using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root
                    % todo: VFS-6717 uncomment when there will be no dependencies in test
                    %{member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) -> #rest_args{
                method = post,
                path = <<"provider/storages">>,
                headers = #{<<"content-type">> => <<"application/json">>},
                body = json_utils:encode(RequestBody)}
            end,
            verify_fun = build_add_s3_storage_verify_fun(Config, StorageIdsBeforeAdd, RequestBody),
            validate_result_fun = api_test_validate:http_204_no_content()
        }
    ])).


%% @private
build_add_s3_storage_verify_fun(Config, StorageIdsBeforeAdd, RequestBody) ->
    fun
        (expected_success, _) ->
            StorageIdsAfterAdd = op_worker_test_rpc:get_storage_ids(Config),

            % todo: VFS-6716 get NewStorageId from HTTP response
            [NewStorageID] = ?assertMatch([_], lists:subtract(StorageIdsAfterAdd, StorageIdsBeforeAdd)),
            StorageDetails = op_worker_test_rpc:describe_storage(Config, NewStorageID),
            ?assert(add_request_match_response(RequestBody, StorageDetails)),
            true;
        (expected_failure, _) ->
            StorageIdsAfterAdd = op_worker_test_rpc:get_storage_ids(Config),
            ?assertEqual(StorageIdsBeforeAdd, StorageIdsAfterAdd),
            true
    end.


%% @private
add_request_match_response(RequestBody, StorageDetails) ->
    RequestMap = maps:get(?S3_STORAGE_NAME, RequestBody),
    maps_utils:is_submap(RequestMap, StorageDetails) and (maps:get(<<"name">>, StorageDetails) =:= ?S3_STORAGE_NAME).


get_s3_storage(Config) ->
    % todo: VFS-6717 add s3 storage before, and delete after test
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    [StorageName | _] = maps:keys(?S3_STORAGE_SPEC),
    StorageId = api_test_utils:get_storage_id_by_name(Config, StorageName),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get s3 storage details using /provider/storages/{storage_id} rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, []}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = <<"provider/storages/", StorageId/binary>>}
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                StorageDetails = op_worker_test_rpc:describe_storage(Config, StorageId),
                StorageDetailsBinary = onepanel_utils:convert_recursive(StorageDetails, {map, binary}),
                RespBodyBinary = onepanel_utils:convert_recursive(RespBody, {map, binary}),
                ?assertEqual(StorageDetailsBinary, RespBodyBinary)
            end)
        }
    ])).


delete_s3_storage(Config) ->
    % todo: VFS-6717 add s3 storage before test
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    StorageId = api_test_utils:get_storage_id_by_name(Config, ?S3_STORAGE_NAME),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Delete s3 storage using /provider/storages/{storage_id} rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root
                    % todo: VFS-6717 uncomment when there will be no dependencies in test
                    %{member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = delete,
                    path = <<"provider/storages/", StorageId/binary>>}
            end,
            verify_fun = build_delete_s3_storage_verify_fun(Config, StorageId),
            validate_result_fun = api_test_validate:http_204_no_content()
        }
    ])).


%% @private
build_delete_s3_storage_verify_fun(Config, StorageId) ->
    fun
        (ExpectedResult, _) ->
            StorageIdsAfterDelete = op_worker_test_rpc:get_storage_ids(Config),
            case ExpectedResult of
                expected_success -> ?assertNot(lists:member(StorageId, StorageIdsAfterDelete));
                expected_failure -> ?assert(lists:member(StorageId, StorageIdsAfterDelete))
            end,
            true
    end.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    application:start(ssl),
    hackney:start(),
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op"
    }).

end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(ssl).

