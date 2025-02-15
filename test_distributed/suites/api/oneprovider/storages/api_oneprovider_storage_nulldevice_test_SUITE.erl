%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider nulldevice storage API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_oneprovider_storage_nulldevice_test_SUITE).
-author("Bartosz Walkowicz").

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
    add_storage_test/1,
    modify_storage_test/1
]).

groups() -> [
    {all_tests, [parallel], [
        add_storage_test,
        modify_storage_test
    ]}
].

all() -> [
    {group, all_tests}
].


%%%===================================================================
%%% API
%%%===================================================================


add_storage_test(_Config) ->
    api_oneprovider_storages_test_base:add_storage_test_base(
        #add_storage_test_spec{
            storage_type = nulldevice,
            args_correctness = correct_args,

            data_spec_fun = fun build_add_nulldevice_storage_data_spec/3,
            prepare_args_fun = fun build_add_nulldevice_storage_prepare_args_fun/1
        }).


%% @private
-spec build_add_nulldevice_storage_data_spec(
    api_test_memory:env_ref(),
    api_oneprovider_storages_test_base:storage_type(),
    api_oneprovider_storages_test_base:args_correctness()
) ->
    api_test_runner:data_spec().
build_add_nulldevice_storage_data_spec(MemRef, nulldevice, correct_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),

    K = fun(Field) -> ?STORAGE_DATA_KEY(StorageName, Field) end,

    #data_spec{
        required = [
            {<<"type">>, ?ERR_MISSING_REQUIRED_VALUE(K(<<"type">>))}
        ],
        optional = [
            <<"timeout">>,
            <<"qosParameters">>,
            <<"storagePathType">>,
            <<"archiveStorage">>,
            <<"latencyMin">>,
            <<"latencyMax">>,
            <<"timeoutProbability">>,
            <<"filter">>,
            <<"simulatedFilesystemParameters">>,
            <<"simulatedFilesystemGrowSpeed">>,
            <<"enableDataVerification">>
        ],
        correct_values = #{
            <<"type">> => [<<"nulldevice">>],
            <<"timeout">> => [?STORAGE_TIMEOUT, ?STORAGE_TIMEOUT div 2],
            <<"qosParameters">> => [?STORAGE_QOS_PARAMETERS],
            <<"storagePathType">> => [<<"canonical">>, <<"flat">>],
            %% TODO VFS-8782 verify if archiveStorage option works properly on storage
            <<"archiveStorage">> => [true, false],
            <<"latencyMin">> => [25],
            <<"latencyMax">> => [75],
            <<"timeoutProbability">> => [0.0],
            <<"filter">> => [<<"*">>],
            <<"simulatedFilesystemParameters">> => [<<>>],
            <<"simulatedFilesystemGrowSpeed">> => [0.0],
            <<"enableDataVerification">> => [true, false]
        },
        bad_values = [
            {<<"type">>, <<"bad_storage_type">>, ?ERR_BAD_VALUE_NOT_ALLOWED(K(<<"type">>), ?STORAGE_TYPES)},
            {<<"timeout">>, 0, ?ERR_BAD_VALUE_TOO_LOW(K(<<"timeout">>), 1)},
            {<<"timeout">>, -?STORAGE_TIMEOUT, ?ERR_BAD_VALUE_TOO_LOW(K(<<"timeout">>), 1)},
            {<<"timeout">>, <<"timeout_as_string">>, ?ERR_BAD_VALUE_INTEGER(K(<<"timeout">>))},
            %% TODO: VFS-7641 add records for badly formatted QoS
            {<<"qosParameters">>, #{<<"key">> => 1}, ?ERR_BAD_VALUE_STRING(K(<<"qosParameters.key">>))},
            {<<"qosParameters">>, #{<<"key">> => 0.1}, ?ERR_BAD_VALUE_STRING(K(<<"qosParameters.key">>))},
            {<<"storagePathType">>, 1, ?ERR_BAD_VALUE_STRING(K(<<"storagePathType">>))},
            {<<"archiveStorage">>, <<"not_a_boolean">>, ?ERR_BAD_VALUE_BOOLEAN(K(<<"archiveStorage">>))},
            {<<"latencyMin">>, <<"string">>, ?ERR_BAD_VALUE_INTEGER(K(<<"latencyMin">>))},
            {<<"latencyMax">>, <<"string">>, ?ERR_BAD_VALUE_INTEGER(K(<<"latencyMax">>))},
            {<<"timeoutProbability">>, <<"string">>, ?ERR_BAD_VALUE_FLOAT(K(<<"timeoutProbability">>))},
            {<<"filter">>, 5, ?ERR_BAD_VALUE_STRING(K(<<"filter">>))},
            {<<"simulatedFilesystemParameters">>, 5, ?ERR_BAD_VALUE_STRING(K(<<"simulatedFilesystemParameters">>))},
            {<<"simulatedFilesystemGrowSpeed">>, <<"str">>, ?ERR_BAD_VALUE_FLOAT(K(<<"simulatedFilesystemGrowSpeed">>))}
            %% TODO VFS-12391 debug why it returns bad_value_string
%%            {<<"enableDataVerification">>, 5, ?ERR_BAD_VALUE_BOOLEAN(K(<<"enableDataVerification">>))}
        ]
    }.

-define(COMMON_BAD_VALUES, [

]).

%% @private
-spec build_add_nulldevice_storage_prepare_args_fun(
    api_test_memory:env_ref()
) ->
    api_test_runner:prepare_args_fun().
build_add_nulldevice_storage_prepare_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        RequestBody = #{StorageName => Data},
        #rest_args{
            method = post,
            path = <<"provider/storages">>,
            headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
            body = json_utils:encode(RequestBody)}
    end.


modify_storage_test(_Config) ->
    api_oneprovider_storages_test_base:modify_storage_test_base(
        #modify_storage_test_spec{
            storage_type = nulldevice,
            args_correctness = correct_args,

            build_data_spec_fun = fun build_modify_nulldevice_storage_data_spec/3,
            build_setup_fun = fun build_modify_nulldevice_storage_setup_fun/1
        }).


%% @private
build_modify_nulldevice_storage_data_spec(MemRef, nulldevice, correct_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),

    K = fun(Field) -> ?STORAGE_DATA_KEY(StorageName, Field) end,

    #data_spec{
        required = [
            {<<"type">>, ?ERR_MISSING_REQUIRED_VALUE(K(<<"type">>))}
        ],
        optional = [
            <<"name">>,
            <<"timeout">>,
            <<"qosParameters">>,
            <<"archiveStorage">>,
            <<"latencyMin">>,
            <<"latencyMax">>,
%%            % TODO VFS-12391 float to binary conversion returns e.g. <<"1.00000000000000005551e-01">>
%%            <<"timeoutProbability">>,
            <<"filter">>,
            <<"simulatedFilesystemParameters">>,
%%            % TODO VFS-12391 float to binary conversion returns e.g. <<"1.00000000000000005551e-01">>
%%            <<"simulatedFilesystemGrowSpeed">>,
            <<"enableDataVerification">>
        ],
        correct_values = #{
            <<"type">> => [<<"nulldevice">>],
            <<"name">> => [?RAND_STR(10), StorageName],
            <<"timeout">> => [?STORAGE_TIMEOUT, ?STORAGE_TIMEOUT div 2],
            <<"qosParameters">> => [
                #{<<"key">> => <<"value1">>},
                #{<<"key">> => <<"value2">>}
            ],
            %% TODO VFS-8782 verify if archiveStorage option works properly on storage
            <<"archiveStorage">> => [true, false],
            <<"latencyMin">> => [5],
            <<"latencyMax">> => [55],
            <<"timeoutProbability">> => [0.5],
            <<"filter">> => [<<"*">>],
            <<"simulatedFilesystemParameters">> => [<<>>],
            <<"simulatedFilesystemGrowSpeed">> => [0.1],
            <<"enableDataVerification">> => [true, false]
        },
        bad_values = [
            {<<"name">>, 1, ?ERR_BAD_VALUE_STRING(K(<<"name">>))},
%%            % TODO VFS-12391 timeout is being changed to binary and not validated
%%%%            {<<"timeout">>, 0, ?ERR_BAD_VALUE_TOO_LOW(K(<<"timeout">>), 1)},
%%%%            {<<"timeout">>, -?STORAGE_TIMEOUT, ?ERR_BAD_VALUE_TOO_LOW(K(<<"timeout">>), 1)},
            {<<"type">>, <<"bad_storage_type">>, ?ERR_BAD_VALUE_NOT_ALLOWED(K(<<"type">>), ?MODIFY_STORAGE_TYPES)},
            {<<"timeout">>, <<"timeout_as_string">>, ?ERR_BAD_VALUE_INTEGER(K(<<"timeout">>))},
%%            %% TODO: VFS-7641 add records for badly formatted QoS
            {<<"qosParameters">>, #{<<"key">> => 1}, ?ERR_BAD_VALUE_STRING(K(<<"qosParameters.key">>))},
            {<<"qosParameters">>, #{<<"key">> => 0.1}, ?ERR_BAD_VALUE_STRING(K(<<"qosParameters.key">>))},
            {<<"archiveStorage">>, <<"not_a_boolean">>, ?ERR_BAD_VALUE_BOOLEAN(K(<<"archiveStorage">>))},
            {<<"latencyMin">>, <<"string">>, ?ERR_BAD_VALUE_INTEGER(K(<<"latencyMin">>))},
            {<<"latencyMax">>, <<"string">>, ?ERR_BAD_VALUE_INTEGER(K(<<"latencyMax">>))},
            {<<"timeoutProbability">>, <<"string">>, ?ERR_BAD_VALUE_FLOAT(K(<<"timeoutProbability">>))},
            {<<"filter">>, 5, ?ERR_BAD_VALUE_STRING(K(<<"filter">>))},
            {<<"simulatedFilesystemParameters">>, 5, ?ERR_BAD_VALUE_STRING(K(<<"simulatedFilesystemParameters">>))},
            {<<"simulatedFilesystemGrowSpeed">>, <<"str">>, ?ERR_BAD_VALUE_FLOAT(K(<<"simulatedFilesystemGrowSpeed">>))}
            %% TODO VFS-12391 debug why it returns bad_value_string
%%            {<<"enableDataVerification">>, 5, ?ERR_BAD_VALUE_BOOLEAN(K(<<"enableDataVerification">>))}
        ]
    }.


%% @private
build_modify_nulldevice_storage_setup_fun(MemRef) ->
    fun() ->
        StorageName = api_test_memory:get(MemRef, storage_name),

        StorageId = panel_test_rpc:add_storage(krakow,
            #{StorageName => #{
                <<"type">> => <<"nulldevice">>
            }}
        ),
        api_test_memory:set(MemRef, storage_id, StorageId),

        StorageDetails = opw_test_rpc:storage_describe(krakow, StorageId),
        api_test_memory:set(MemRef, storage_details, StorageDetails)
    end.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op",
        envs = [{op_worker, op_worker, [{fuse_session_grace_period_seconds, 24 * 60 * 60}]}]
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().
