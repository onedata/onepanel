%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider ceph storage API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_oneprovider_storage_ceph_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_storages.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").
-include_lib("onenv_ct/include/chart_values.hrl").

%% API
-export([
    groups/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1
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
    add_ceph_storage_test_base(correct_args, false).


add_correct_storage_and_skip_detection_test(_Config) ->
    add_ceph_storage_test_base(correct_args, true).


add_bad_storage_and_perform_detection_test(_Config) ->
    add_ceph_storage_test_base(bad_args, false).


add_bad_storage_and_skip_detection_test(_Config) ->
    add_ceph_storage_test_base(bad_args, true).


%% @private
-spec add_ceph_storage_test_base(
    api_oneprovider_storages_test_base:args_correctness(),
    api_oneprovider_storages_test_base:skip_storage_detection()
) ->
    ok.
add_ceph_storage_test_base(ArgsCorrectness, SkipStorageDetection) ->
    api_oneprovider_storages_test_base:add_storage_test_base(
        #add_storage_test_spec{
            storage_type = ceph,
            args_correctness = ArgsCorrectness,
            skip_storage_detection = SkipStorageDetection,

            data_spec_fun = fun build_add_ceph_storage_data_spec/3,
            prepare_args_fun = fun build_add_ceph_storage_prepare_args_fun/2,

            data_spec_random_coverage = 60
        }).


%% @private
-spec build_add_ceph_storage_data_spec(
    api_test_memory:env_ref(),
    api_oneprovider_storages_test_base:storage_type(),
    api_oneprovider_storages_test_base:args_correctness()
) ->
    api_test_runner:data_spec().
build_add_ceph_storage_data_spec(MemRef, ceph, correct_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),
    #data_spec{
        required = [
            {<<"type">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"type">>))},
            {<<"monitorHostname">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"monitorHostname">>))},
            {<<"clusterName">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"clusterName">>))},
            {<<"poolName">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"poolName">>))},
            {<<"username">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"username">>))},
            {<<"key">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"key">>))}
        ],
        optional = [
            <<"timeout">>,
            <<"qosParameters">>,
            <<"storagePathType">>,
            <<"archiveStorage">>,
            <<"blockSize">>
        ],
        correct_values = #{
            <<"type">> => [<<"cephrados">>],
            <<"monitorHostname">> => [?CEPH_MONITOR_HOSTNAME],
            <<"clusterName">> => [?CEPH_CLUSTER_NAME],
            <<"poolName">> => [?CEPH_POOL_NAME],
            <<"username">> => [?CEPH_USERNAME],
            <<"key">> => [?CEPH_KEY],
            <<"blockSize">> => [1024],
            <<"timeout">> => [?STORAGE_TIMEOUT, ?STORAGE_TIMEOUT div 2],
            <<"qosParameters">> => [?STORAGE_QOS_PARAMETERS],
            <<"storagePathType">> => [<<"flat">>],
            %% TODO VFS-8782 verify if archiveStorage option works properly on storage
            <<"archiveStorage">> => [true, false]
        },
        bad_values = [
            {<<"skipStorageDetection">>, <<"not_a_boolean">>, ?ERROR_BAD_VALUE_BOOLEAN(?STORAGE_DATA_KEY(StorageName, <<"skipStorageDetection">>))},
            {<<"type">>, <<"bad_storage_type">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(?STORAGE_DATA_KEY(StorageName, <<"type">>), ?STORAGE_TYPES)},
            {<<"blockSize">>, <<"blockSize_as_string">>, ?ERROR_BAD_VALUE_INTEGER(?STORAGE_DATA_KEY(StorageName, <<"blockSize">>))},
            {<<"timeout">>, 0, ?ERROR_BAD_VALUE_TOO_LOW(?STORAGE_DATA_KEY(StorageName, <<"timeout">>), 1)},
            {<<"timeout">>, -?STORAGE_TIMEOUT, ?ERROR_BAD_VALUE_TOO_LOW(?STORAGE_DATA_KEY(StorageName, <<"timeout">>), 1)},
            {<<"timeout">>, <<"timeout_as_string">>, ?ERROR_BAD_VALUE_INTEGER(?STORAGE_DATA_KEY(StorageName, <<"timeout">>))},
            %% TODO: VFS-7641 add records for badly formatted QoS
            {<<"qosParameters">>, #{<<"key">> => 1}, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"qosParameters.key">>))},
            {<<"qosParameters">>, #{<<"key">> => 0.1}, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"qosParameters.key">>))},
            {<<"storagePathType">>, <<"canonical">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(?STORAGE_DATA_KEY(StorageName, <<"storagePathType">>), [<<"flat">>])},
            {<<"storagePathType">>, 1, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"storagePathType">>))},
            {<<"archiveStorage">>, <<"not_a_boolean">>, ?ERROR_BAD_VALUE_BOOLEAN(?STORAGE_DATA_KEY(StorageName, <<"archiveStorage">>))}
        ]
    };
build_add_ceph_storage_data_spec(MemRef, ceph, bad_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),
    #data_spec{
        required = [
            {<<"type">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"type">>))},
            {<<"monitorHostname">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"monitorHostname">>))},
            {<<"clusterName">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"clusterName">>))},
            {<<"poolName">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"poolName">>))},
            {<<"username">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"username">>))},
            {<<"key">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"key">>))}
        ],
        correct_values = #{
            <<"type">> => [<<"cephrados">>],
            <<"monitorHostname">> => [<<"incorrect-monitor-hostname">>],
            <<"clusterName">> => [<<"incorrect-cluster0name">>],
            <<"poolName">> => [<<"incorrect-pool-name">>],
            <<"username">> => [<<"incorrect-username">>],
            <<"key">> => [<<"incorrect-key">>]
        }
    }.


%% @private
-spec build_add_ceph_storage_prepare_args_fun(
    api_test_memory:env_ref(),
    api_oneprovider_storages_test_base:skip_storage_detection()
) ->
    api_test_runner:prepare_args_fun().
build_add_ceph_storage_prepare_args_fun(MemRef, SkipStorageDetection) ->
    fun(#api_test_ctx{data = Data}) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        RequestBody = case maps:is_key(<<"skipStorageDetection">>, Data) of
            true -> #{
                StorageName => Data
            };
            false -> #{
                StorageName => maps:put(<<"skipStorageDetection">>, SkipStorageDetection, Data)
            }
        end,
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
