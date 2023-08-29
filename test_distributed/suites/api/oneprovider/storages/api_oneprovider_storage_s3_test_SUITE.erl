%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider S3 storage API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_oneprovider_storage_s3_test_SUITE).
-author("Piotr Duleba").

-include("api_test_storages.hrl").
-include("api_test_runner.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").
-include_lib("onenv_ct/include/chart_values.hrl").

%% API
-export([
    groups/0,
    all/0,

    init_per_suite/1,
    end_per_suite/1,

    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    add_correct_storage_and_perform_detection_test/1,
    add_correct_storage_and_skip_detection_test/1,
    add_bad_storage_and_perform_detection_test/1,
    add_bad_storage_and_skip_detection_test/1
]).

groups() -> [
    {all_tests, [parallel], [
        add_correct_storage_and_perform_detection_test,
        add_correct_storage_and_skip_detection_test,
        add_bad_storage_and_perform_detection_test,
        add_bad_storage_and_skip_detection_test
    ]}
].

all() -> [
    {group, all_tests}
].


%%%===================================================================
%%% API
%%%===================================================================


add_correct_storage_and_perform_detection_test(_Config) ->
    add_s3_storage_test_base(correct_args, true).


add_correct_storage_and_skip_detection_test(_Config) ->
    add_s3_storage_test_base(correct_args, false).


add_bad_storage_and_perform_detection_test(_Config) ->
    add_s3_storage_test_base(bad_args, true).


add_bad_storage_and_skip_detection_test(_Config) ->
    add_s3_storage_test_base(bad_args, false).


%% @private
-spec add_s3_storage_test_base(
    api_oneprovider_storages_test_base:args_correctness(),
    api_oneprovider_storages_test_base:skip_storage_detection()
) ->
    ok.
add_s3_storage_test_base(ArgsCorrectness, SkipStorageDetection) ->
    api_oneprovider_storages_test_base:add_storage_test_base(
        #add_storage_test_spec{
            storage_type = s3,
            args_correctness = ArgsCorrectness,
            skip_storage_detection = SkipStorageDetection,

            data_spec_fun = fun build_add_s3_storage_data_spec/3,
            prepare_args_fun = fun build_add_s3_storage_prepare_args_fun/2,
            data_spec_random_coverage = 5
        }).


%% @private
-spec build_add_s3_storage_data_spec(
    api_test_memory:env_ref(),
    api_oneprovider_storages_test_base:storage_type(),
    api_oneprovider_storages_test_base:args_correctness()
) -> api_test_runner:data_spec().
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
            <<"storagePathType">>,
            <<"signatureVersion">>,
            <<"maximumCanonicalObjectSize">>,
            <<"blockSize">>,
            <<"archiveStorage">>
        ],
        correct_values = #{
            <<"bucketName">> => [?S3_BUCKET_NAME],
            % Onepanel accepts both variants (with or without the scheme)
            <<"hostname">> => [?S3_HOSTNAME, <<"http://", (?S3_HOSTNAME)/binary>>],
            <<"accessKey">> => [?S3_KEY_ID],
            <<"secretKey">> => [?S3_ACCESS_KEY],
            <<"type">> => [<<"s3">>],
            <<"timeout">> => [?STORAGE_TIMEOUT],
            <<"qosParameters">> => [?STORAGE_QOS_PARAMETERS],
            <<"storagePathType">> => [<<"canonical">>, <<"flat">>],
            <<"signatureVersion">> => ?S3_ALLOWED_SIGNATURE_VERSIONS,
            <<"blockSize">> => [?STORAGE_DETECTION_FILE_SIZE],
            <<"maximumCanonicalObjectSize">> => [?STORAGE_DETECTION_FILE_SIZE],
            %% TODO VFS-8782 verify if archiveStorage option works properly on storage
            <<"archiveStorage">> => [true, false]
        },
        bad_values = [
            {<<"type">>, <<"bad_storage_type">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(?STORAGE_DATA_KEY(StorageName, <<"type">>), ?STORAGE_TYPES)},
            {<<"timeout">>, -?STORAGE_TIMEOUT, ?REST_ERROR(?ERROR_STORAGE_TEST_FAILED(write))},
            {<<"timeout">>, <<"timeout_as_string">>, ?ERROR_BAD_VALUE_INTEGER(?STORAGE_DATA_KEY(StorageName, <<"timeout">>))},
            %% TODO: VFS-7641 add records for badly formatted QoS
            {<<"qosParameters">>, <<"qos_not_a_map">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"qosParameters._">>))},
            {<<"qosParameters">>, #{<<"key">> => 1}, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"qosParameters.key">>))},
            {<<"qosParameters">>, #{<<"key">> => 0.1}, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"qosParameters.key">>))},
            {<<"storagePathType">>, 1, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"storagePathType">>))},
            {<<"signatureVersion">>, <<"signatureVersion_as_string">>, ?ERROR_BAD_VALUE_INTEGER(?STORAGE_DATA_KEY(StorageName, <<"signatureVersion">>))},
            {<<"signatureVersion">>, 2, ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(?STORAGE_DATA_KEY(StorageName, <<"signatureVersion">>), ?S3_ALLOWED_SIGNATURE_VERSIONS)},
            {<<"blockSize">>, <<"blockSize_as_string">>, ?ERROR_BAD_VALUE_INTEGER(?STORAGE_DATA_KEY(StorageName, <<"blockSize">>))},
            {<<"blockSize">>, -1, ?ERROR_BAD_VALUE_TOO_LOW(?STORAGE_DATA_KEY(StorageName, <<"blockSize">>), ?S3_MIN_BLOCK_SIZE)},
            {<<"maximumCanonicalObjectSize">>, <<"maximumCanonicalObjectSize_as_string">>, ?ERROR_BAD_VALUE_INTEGER(?STORAGE_DATA_KEY(StorageName, <<"maximumCanonicalObjectSize">>))},
            {<<"maximumCanonicalObjectSize">>, 0, ?ERROR_BAD_VALUE_TOO_LOW(?STORAGE_DATA_KEY(StorageName, <<"maximumCanonicalObjectSize">>), ?S3_MIN_MAX_CANONICAL_OBJECT_SIZE)},
            {<<"archiveStorage">>, <<"not_a_boolean">>, ?ERROR_BAD_VALUE_BOOLEAN(?STORAGE_DATA_KEY(StorageName, <<"archiveStorage">>))}
        ]
    };
build_add_s3_storage_data_spec(MemRef, s3, bad_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),
    #data_spec{
        required = [
            {<<"type">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"type">>))},
            {<<"hostname">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"hostname">>))},
            {<<"bucketName">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"bucketName">>))}
        ],
        correct_values = #{
            <<"type">> => [<<"s3">>],
            % Onepanel accepts both variants (with or without the scheme)
            <<"hostname">> => [?S3_HOSTNAME, <<"http://", (?S3_HOSTNAME)/binary>>],
            <<"bucketName">> => [<<"nonexistent_bucket">>]
        }
    }.


%% @private
-spec build_add_s3_storage_prepare_args_fun(
    api_test_memory:env_ref(),
    api_oneprovider_storages_test_base:skip_storage_detection()
) ->
    api_test_runner:prepare_args_fun().
build_add_s3_storage_prepare_args_fun(MemRef, SkipStorageDetection) ->
    fun(#api_test_ctx{data = Data}) ->

        %% S3 Storage that onenv creates, requires credentials even though swagger marks them as optional.
        %% Therefore, we need to inject them to each request body.
        DataWithCredentials = maps:merge(Data, #{
            <<"accessKey">> => ?S3_KEY_ID,
            <<"secretKey">> => ?S3_ACCESS_KEY
        }),

        %% TODO VFS-7706 currently storage detection always fails on s3 storage when archiveStorage is enabled
        IsArchiveStorage = maps:get(<<"archiveStorage">>, DataWithCredentials, false),
        SkipStorageDetection2 = case IsArchiveStorage of
            true -> true;
            _ -> SkipStorageDetection
        end,
        StorageName = api_test_memory:get(MemRef, storage_name),
        RequestBody = #{
            StorageName => maps:put(<<"skipStorageDetection">>, SkipStorageDetection2, DataWithCredentials)
        },

        #rest_args{
            method = post,
            path = <<"provider/storages">>,
            headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
            body = json_utils:encode(RequestBody)}
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


init_per_testcase(_, Config) ->
    ct:timetrap({minutes, 90}),
    Config.


end_per_testcase(_, Config) ->
    Config.