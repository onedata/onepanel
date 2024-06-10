%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider glusterfs storage API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_oneprovider_storage_glusterfs_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_storages.hrl").
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
    add_correct_storage_test/1,
    add_bad_storage_test/1
]).

groups() -> [
    {all_tests, [parallel], [
        add_correct_storage_test,
        add_bad_storage_test
    ]}
].

all() -> [
    {group, all_tests}
].


%%%===================================================================
%%% API
%%%===================================================================


add_correct_storage_test(_Config) ->
    add_glusterfs_storage_test_base(correct_args).

add_bad_storage_test(_Config) ->
    add_glusterfs_storage_test_base(bad_args).


%% @private
-spec add_glusterfs_storage_test_base(
    api_oneprovider_storages_test_base:args_correctness()
) ->
    ok.
add_glusterfs_storage_test_base(ArgsCorrectness) ->
    api_oneprovider_storages_test_base:add_storage_test_base(
        #add_storage_test_spec{
            storage_type = glusterfs,
            args_correctness = ArgsCorrectness,

            data_spec_fun = fun build_add_glusterfs_storage_data_spec/3,
            prepare_args_fun = fun build_add_glusterfs_storage_prepare_args_fun/1,

            data_spec_random_coverage = 10
        }).


%% @private
-spec build_add_glusterfs_storage_data_spec(
    api_test_memory:env_ref(),
    api_oneprovider_storages_test_base:storage_type(),
    api_oneprovider_storages_test_base:args_correctness()
) ->
    api_test_runner:data_spec().
build_add_glusterfs_storage_data_spec(MemRef, glusterfs, correct_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),
    #data_spec{
        required = [
            {<<"type">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"type">>))},
            {<<"volume">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"volume">>))},
            {<<"hostname">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"hostname">>))}
        ],
        optional = [
            <<"port">>,
            <<"transport">>,
            <<"mountPoint">>,
            <<"xlatorOptions">>,
            <<"timeout">>,
            <<"qosParameters">>,
            <<"storagePathType">>,
            <<"archiveStorage">>
        ],
        correct_values = #{
            <<"type">> => [<<"glusterfs">>],
            <<"volume">> => [?GLUSTERFS_VOLUME],
            <<"hostname">> => [?GLUSTERFS_HOSTNAME],
            <<"port">> => [?GLUSTERFS_PORT],
            <<"transport">> => [?GLUSTERFS_TRANSPORT],
            <<"mountPoint">> => [<<"">>],
            <<"xlatorOptions">> => [<<"TRANSLATOR1.OPTION1=VALUE1">>],
            <<"timeout">> => [?STORAGE_TIMEOUT, ?STORAGE_TIMEOUT div 2],
            <<"qosParameters">> => [?STORAGE_QOS_PARAMETERS],
            <<"storagePathType">> => [<<"canonical">>],
            %% TODO VFS-8782 verify if archiveStorage option works properly on storage
            <<"archiveStorage">> => [true, false]
        },
        bad_values = [
            {<<"type">>, <<"bad_storage_type">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(?STORAGE_DATA_KEY(StorageName, <<"type">>), ?STORAGE_TYPES)},
            {<<"port">>, <<"port_as_string">>, ?ERROR_BAD_VALUE_INTEGER(?STORAGE_DATA_KEY(StorageName, <<"port">>))},
            {<<"transport">>, <<"bad_transport">>,
                ?ERROR_BAD_VALUE_NOT_ALLOWED(?STORAGE_DATA_KEY(StorageName, <<"transport">>), [<<"tcp">>, <<"rdma">>, <<"socket">>])},
            {<<"xlatorOptions">>, 132, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"xlatorOptions">>))},
            {<<"timeout">>, 0, ?ERROR_BAD_VALUE_TOO_LOW(?STORAGE_DATA_KEY(StorageName, <<"timeout">>), 1)},
            {<<"timeout">>, -?STORAGE_TIMEOUT, ?ERROR_BAD_VALUE_TOO_LOW(?STORAGE_DATA_KEY(StorageName, <<"timeout">>), 1)},
            {<<"timeout">>, <<"timeout_as_string">>, ?ERROR_BAD_VALUE_INTEGER(?STORAGE_DATA_KEY(StorageName, <<"timeout">>))},
            %% TODO: VFS-7641 add records for badly formatted QoS
            {<<"qosParameters">>, #{<<"key">> => 1}, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"qosParameters.key">>))},
            {<<"qosParameters">>, #{<<"key">> => 0.1}, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"qosParameters.key">>))},
            {<<"storagePathType">>, <<"flat">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(?STORAGE_DATA_KEY(StorageName, <<"storagePathType">>), [<<"canonical">>])},
            {<<"storagePathType">>, 1, ?ERROR_BAD_VALUE_ATOM(?STORAGE_DATA_KEY(StorageName, <<"storagePathType">>))},
            {<<"archiveStorage">>, <<"not_a_boolean">>, ?ERROR_BAD_VALUE_BOOLEAN(?STORAGE_DATA_KEY(StorageName, <<"archiveStorage">>))}
        ]
    };
build_add_glusterfs_storage_data_spec(MemRef, glusterfs, bad_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),
    #data_spec{
        required = [
            {<<"type">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"type">>))},
            {<<"volume">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"volume">>))},
            {<<"hostname">>, ?ERROR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"hostname">>))}
        ],
        optional = [
            <<"transport">>,
            <<"mountPoint">>
        ],
        correct_values = #{
            <<"type">> => [<<"glusterfs">>],
            <<"volume">> => [<<"bad-volume">>],
            <<"hostname">> => [<<"bad-hostname">>],
            <<"transport">> => [<<"rdma">>, <<"socket">>],
            <<"mountPoint">> => [<<"/bad/mountpoint">>]
        }
    }.


%% @private
-spec build_add_glusterfs_storage_prepare_args_fun(
    api_test_memory:env_ref()
) ->
    api_test_runner:prepare_args_fun().
build_add_glusterfs_storage_prepare_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        #rest_args{
            method = post,
            path = <<"provider/storages">>,
            headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
            body = json_utils:encode(#{StorageName => Data})}
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
