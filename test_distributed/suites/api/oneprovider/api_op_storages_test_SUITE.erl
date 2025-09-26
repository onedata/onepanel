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
-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_storages_ids_test/1,
    add_s3_storage_test/1,
    delete_s3_storage_test/1
]).

-define(S3_STORAGE_NAME, <<"s3Storage-1">>).
-define(S3_STORAGE_SPEC, #{
    ?S3_STORAGE_NAME => #{
        <<"type">> => <<"s3">>,
        <<"bucketName">> => ?S3_BUCKET_NAME,
        <<"hostname">> => <<"http://", (?S3_HOSTNAME)/binary>>,
        <<"accessKey">> => ?S3_KEY_ID,
        <<"secretKey">> => ?S3_ACCESS_KEY
    }
}).

all() -> [
    get_storages_ids_test,
    add_s3_storage_test,
    delete_s3_storage_test
].


%%%===================================================================
%%% API
%%%===================================================================


get_storages_ids_test(_Config) ->
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


add_s3_storage_test(_Config) ->
    % todo: VFS-6717 delete s3 storage after test
    MemRef = api_test_memory:init(),
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    StorageIdsBeforeAdd = opw_test_rpc:get_storages(krakow),

    RequestBody = ?S3_STORAGE_SPEC,

    ?assert(api_test_runner:run_tests([
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
                    {user, ?ERR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) -> #rest_args{
                method = post,
                path = <<"provider/storages">>,
                headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                body = json_utils:encode(RequestBody)}
            end,
            verify_fun = build_add_s3_storage_verify_fun(MemRef, StorageIdsBeforeAdd, RequestBody),
            validate_result_fun = api_test_validate:http_200_ok(
                fun(Body) ->
                    AddedStorageId = kv_utils:get([?S3_STORAGE_NAME, <<"id">>], Body),
                    api_test_memory:set(MemRef, storage_id, AddedStorageId)
                end
            )
        }
    ])).


%% @private
build_add_s3_storage_verify_fun(MemRef, StorageIdsBeforeAdd, RequestBody) ->
    fun
        (expected_success, _) ->
            StorageIdsAfterAdd = opw_test_rpc:get_storages(krakow),

            NewStorageID = api_test_memory:get(MemRef, storage_id),
            StorageDetails = opw_test_rpc:storage_describe(krakow, NewStorageID),
            ExpectedScheme = maps:get(<<"scheme">>, StorageDetails),
            ExpectedHostname = maps:get(<<"hostname">>, StorageDetails),
            ExtendedHostname =  str_utils:join_binary([ExpectedScheme, <<"://">>, ExpectedHostname]),
            StorageDetails2 = maps:put(<<"hostname">>,ExtendedHostname, maps:remove(<<"scheme">>, StorageDetails)),

            ?assert(add_request_match_response(RequestBody, StorageDetails2)),
            true;
        (expected_failure, _) ->
            StorageIdsAfterAdd = opw_test_rpc:get_storages(krakow),
            ?assertEqual(StorageIdsBeforeAdd, StorageIdsAfterAdd),
            true
    end.


%% @private
add_request_match_response(RequestBody, StorageDetails) ->
    RequestMap = maps:get(?S3_STORAGE_NAME, RequestBody),
    % secretKey is redacted in StorageDetails so it won't match
    maps_utils:is_submap(maps:remove(<<"secretKey">>, RequestMap), StorageDetails) and
        (maps:get(<<"name">>, StorageDetails) =:= ?S3_STORAGE_NAME).


delete_s3_storage_test(_Config) ->
    % todo: VFS-6717 add s3 storage before test
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    StorageId = api_test_utils:get_storage_id_by_name(krakow, ?S3_STORAGE_NAME),

    ?assert(api_test_runner:run_tests([
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
                    {user, ?ERR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = delete,
                    path = <<"provider/storages/", StorageId/binary>>}
            end,
            verify_fun = build_delete_s3_storage_verify_fun(StorageId),
            validate_result_fun = api_test_validate:http_204_no_content()
        }
    ])).


%% @private
build_delete_s3_storage_verify_fun(StorageId) ->
    fun
        (ExpectedResult, _) ->
            StorageIdsAfterDelete = opw_test_rpc:get_storages(krakow),
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
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op_s3"
    }).

end_per_suite(_Config) ->
    oct_background:end_per_suite().

