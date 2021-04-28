%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider s3 storage API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_oneprovider_s3_test_SUITE).
-author("Piotr Duleba").

-include("api_test_storages.hrl").
-include("api_test_runner.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").
-include_lib("onenv_ct/include/chart_values.hrl").

%% API
-export([all/0]).
-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    add_correct_storage_and_perform_detection_test/1,
    add_correct_storage_and_skip_detection_test/1,
    add_bad_storage_and_perform_detection_test/1,
    add_bad_storage_and_skip_detection_test/1
]).

all() -> [
    add_correct_storage_and_perform_detection_test,
    add_correct_storage_and_skip_detection_test,
    add_bad_storage_and_perform_detection_test,
    add_bad_storage_and_skip_detection_test
].


%%%===================================================================
%%% API
%%%===================================================================


add_correct_storage_and_perform_detection_test(Config) ->
    api_oneprovider_storages_test_base:add_storage_test_base(Config, s3, correct_args, false,
        fun build_add_s3_storage_setup_fun/1,
        fun build_add_s3_storage_data_spec/3,
        fun build_add_s3_storage_prepare_args_fun/2,
        fun build_add_s3_storage_validate_result_fun/3,
        fun build_add_s3_storage_verify_fun/3
    ).


add_correct_storage_and_skip_detection_test(Config) ->
    api_oneprovider_storages_test_base:add_storage_test_base(Config, s3, correct_args, true,
        fun build_add_s3_storage_setup_fun/1,
        fun build_add_s3_storage_data_spec/3,
        fun build_add_s3_storage_prepare_args_fun/2,
        fun build_add_s3_storage_validate_result_fun/3,
        fun build_add_s3_storage_verify_fun/3
    ).


add_bad_storage_and_perform_detection_test(Config) ->
    api_oneprovider_storages_test_base:add_storage_test_base(Config, s3, bad_args, false,
        fun build_add_s3_storage_setup_fun/1,
        fun build_add_s3_storage_data_spec/3,
        fun build_add_s3_storage_prepare_args_fun/2,
        fun build_add_s3_storage_validate_result_fun/3,
        fun build_add_s3_storage_verify_fun/3
    ).


add_bad_storage_and_skip_detection_test(Config) ->
    api_oneprovider_storages_test_base:add_storage_test_base(Config, s3, bad_args, true,
        fun build_add_s3_storage_setup_fun/1,
        fun build_add_s3_storage_data_spec/3,
        fun build_add_s3_storage_prepare_args_fun/2,
        fun build_add_s3_storage_validate_result_fun/3,
        fun build_add_s3_storage_verify_fun/3
    ).


%% @private
-spec build_add_s3_storage_setup_fun(api_test_memory:env_ref()) ->
    api_test_runner:setup_fun().
build_add_s3_storage_setup_fun(MemRef) ->
    fun() ->
        ExistingStorages = opw_test_rpc:get_storages(krakow),
        api_test_memory:set(MemRef, existing_storages, ExistingStorages)
    end.


%% @private
-spec build_add_s3_storage_data_spec(api_test_memory:env_ref(), api_oneprovider_storages_test_base:storage_type(),
    api_oneprovider_storages_test_base:args_correctness()) -> api_test_runner:data_spec().
build_add_s3_storage_data_spec(MemRef, s3, correct_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),
    #data_spec{
        required = [
            {<<"type">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"type">>))},
            {<<"hostname">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"hostname">>))},
            {<<"bucketName">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"bucketName">>))}
        ],
        optional = [
            <<"accessKey">>,
            <<"secretKey">>,
            <<"timeout">>,
            <<"qosParameters">>,
            <<"storagePathType">>
        ],
        correct_values = #{
            <<"bucketName">> => [?S3_BUCKET_NAME],
            <<"hostname">> => [?S3_HOSTNAME],
            <<"accessKey">> => [?S3_KEY_ID],
            <<"secretKey">> => [?S3_ACCESS_KEY],
            <<"type">> => [<<"s3">>],
            <<"timeout">> => [?STORAGE_TIMEOUT],
            <<"qosParameters">> => [?STORAGE_QOS_PARAMETERS],
            %% TODO: VFS-7621 add flat path type to tests
            <<"storagePathType">> => [<<"canonical">>]
        },
        bad_values = [
            {<<"type">>, <<"bad_storage_type">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(?STORAGE_DATA_KEY(StorageName, <<"type">>), ?STORAGE_TYPES)},
            {<<"timeout">>, -?STORAGE_TIMEOUT, ?REST_ERROR(?ERROR_STORAGE_TEST_FAILED(write))},
            {<<"timeout">>, <<"timeout_as_string">>, ?ERROR_BAD_VALUE_INTEGER(?STORAGE_DATA_KEY(StorageName, <<"timeout">>))},
            {<<"qosParameters">>, <<"qos_not_a_map">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"qosParameters._">>))},
            {<<"qosParameters">>, #{<<"key">> => 1}, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"qosParameters.key">>))},
            {<<"qosParameters">>, #{<<"key">> => 0.1}, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"qosParameters.key">>))},
            {<<"storagePathType">>, 1, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"storagePathType">>))}
        ]
    };
build_add_s3_storage_data_spec(MemRef, s3, bad_args) ->
    ct:pal("~p", [node_cache:get(oct_mapping)]),
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),
    #data_spec{
        required = [
            {<<"type">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"type">>))},
            {<<"hostname">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"hostname">>))},
            {<<"bucketName">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"bucketName">>))}
        ],
        correct_values = #{
            name => [StorageName],
            <<"type">> => [<<"s3">>],
            <<"hostname">> => [?S3_HOSTNAME],
            <<"bucketName">> => [<<"unexistent_bucket">>]
        }
    }.


%% @private
-spec build_add_s3_storage_prepare_args_fun(api_test_memory:env_ref(), boolean()) ->
    api_test_runner:prepare_args_fun().
build_add_s3_storage_prepare_args_fun(MemRef, SkipStorageDetection) ->
    fun(#api_test_ctx{data = Data}) ->
        DataWithCredentials = maps:merge(Data, #{
            <<"accessKey">> => ?S3_KEY_ID,
            <<"secretKey">> => ?S3_ACCESS_KEY
        }),
        StorageName = api_test_memory:get(MemRef, storage_name),
        RequestBody = #{
            StorageName => maps:put(<<"skipStorageDetection">>, SkipStorageDetection, DataWithCredentials)
        },

        #rest_args{
            method = post,
            path = <<"provider/storages">>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(RequestBody)}
    end.


%% @private
-spec build_add_s3_storage_validate_result_fun(api_test_memory:env_ref(),
    api_oneprovider_storages_test_base:args_correctness(), boolean()) -> api_test_runner:validate_result_fun().
build_add_s3_storage_validate_result_fun(MemRef, correct_args, _) ->
    api_test_validate:http_200_ok(fun(Body) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        StorageId = kv_utils:get([StorageName, <<"id">>], Body),
        api_test_memory:set(MemRef, storage_id, StorageId)
    end);
build_add_s3_storage_validate_result_fun(MemRef, bad_args, true) ->
    api_test_validate:http_200_ok(fun(Body) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        StorageId = kv_utils:get([StorageName, <<"id">>], Body),
        api_test_memory:set(MemRef, storage_id, StorageId)
    end);
build_add_s3_storage_validate_result_fun(MemRef, bad_args, false) ->
    api_test_validate:http_400_bad_request(fun(Body) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        ExpRespBody = #{
            StorageName => ?REST_ERROR(?ERROR_STORAGE_TEST_FAILED(write))
        },
        ?assertEqual(ExpRespBody, Body)
    end).


%% @private
-spec build_add_s3_storage_verify_fun(api_test_memory:env_ref(), api_oneprovider_storages_test_base:args_correctness(),
    boolean()) -> api_test_runner:verify_fun().
build_add_s3_storage_verify_fun(MemRef, bad_args, false) ->
    fun(_, _) ->
        StoragesBeforeTest = api_test_memory:get(MemRef, existing_storages),
        StoragesAfterTest = opw_test_rpc:get_storages(krakow),
        ?assertEqual(StoragesBeforeTest, StoragesAfterTest),
        true
    end;
build_add_s3_storage_verify_fun(MemRef, ArgsCorrectness, _SkipStorageDetection) ->
    fun
        (expected_success, _) ->
            StoragesBeforeTest = api_test_memory:get(MemRef, existing_storages),
            StoragesAfterTest = opw_test_rpc:get_storages(krakow),
            [NewStorageId] = ?assertMatch([_], lists:subtract(StoragesAfterTest, StoragesBeforeTest)),
            case ArgsCorrectness of
                correct_args ->
                    ?assertEqual(ok, api_oneprovider_storages_test_base:perform_io_test_on_storage(NewStorageId), ?ATTEMPTS);
                bad_args ->
                    ?assertEqual(error, api_oneprovider_storages_test_base:perform_io_test_on_storage(NewStorageId), ?ATTEMPTS)
            end,
            true;
        (expected_failure, _) ->
            StoragesBeforeTest = api_test_memory:get(MemRef, existing_storages),
            StoragesAfterTest = opw_test_rpc:get_storages(krakow),
            ?assertEqual(StoragesBeforeTest, StoragesAfterTest),
            true
    end.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "storages_api_tests",
        envs = [{op_worker, op_worker, [{fuse_session_grace_period_seconds, 24 * 60 * 60}]}]
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().
