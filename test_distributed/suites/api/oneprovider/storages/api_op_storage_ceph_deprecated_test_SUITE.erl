%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider deprecated ceph storage
%%% API (REST).
%%% NOTE: it is no longer possible to create such storage via REST BUT some
%%% may still have it (from older versions of the system) In such case it is
%%% till possible to e.g update such storage.
%%% @end
%%%-------------------------------------------------------------------
-module(api_op_storage_ceph_deprecated_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_storages.hrl").
-include("onepanel_test_utils.hrl").
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
    adding_deprecated_ceph_should_fail_test/1,

    modify_correct_storage_test/1,
    modify_bad_storage_test/1
]).

groups() -> [
    {all_tests, [parallel], [
        adding_deprecated_ceph_should_fail_test,

        modify_correct_storage_test,
        modify_bad_storage_test
    ]}
].

all() -> [
    {group, all_tests}
].


%%%===================================================================
%%% API
%%%===================================================================


adding_deprecated_ceph_should_fail_test(_Config) ->
    StorageName = ?RAND_STR(),
    RequiredData = #{StorageName => #{
        <<"type">> => <<"ceph">>,
        <<"monitorHostname">> => ?CEPH_MONITOR_HOSTNAME,
        <<"clusterName">> => ?CEPH_CLUSTER_NAME,
        <<"poolName">> => ?CEPH_POOL_NAME,
        <<"username">> => ?CEPH_USERNAME,
        <<"key">> => ?CEPH_KEY
    }},

    ExpBody = #{<<"error">> => errors:to_json(?ERR_BAD_VALUE_NOT_ALLOWED(
        ?STORAGE_DATA_KEY(StorageName, <<"type">>), ?STORAGE_TYPES
    ))},
    ?assertMatch(
        {ok, ?HTTP_400_BAD_REQUEST, _, ExpBody},
        panel_test_rest:post(krakow, <<"/provider/storages">>, #{auth => root, json => RequiredData})
    ).


modify_correct_storage_test(_Config) ->
    modify_ceph_storage_test_base(correct_args).


modify_bad_storage_test(_Config) ->
    modify_ceph_storage_test_base(bad_args).


%% @private
modify_ceph_storage_test_base(ArgsCorrectness) ->
    api_op_storages_test_base:modify_storage_test_base(
        #modify_storage_test_spec{
            storage_type = ceph,
            args_correctness = ArgsCorrectness,

            build_data_spec_fun = fun build_modify_ceph_storage_data_spec/3,
            build_setup_fun = fun build_modify_ceph_storage_setup_fun/1
        }).


%% @private
build_modify_ceph_storage_data_spec(MemRef, ceph, correct_args) ->
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
            <<"archiveStorage">>

            %% TODO VFS-12391 it passes with dummy data but takes ~14 minutes - debug
%%            <<"clusterName">>
        ],
        correct_values = #{
            <<"type">> => [<<"ceph">>],
            <<"name">> => [?RAND_STR(10)],
            <<"timeout">> => [?STORAGE_TIMEOUT, ?STORAGE_TIMEOUT div 2],
            <<"qosParameters">> => [#{<<"key">> => <<"value">>}],
            %% TODO VFS-8782 verify if archiveStorage option works properly on storage
            <<"archiveStorage">> => [?RAND_BOOL()]

            %% TODO VFS-12391 it passes with dummy data but takes ~14 minutes - debug
%%            <<"clusterName">> => [<<"dummy">>]
        },

        bad_values = [
            {<<"type">>, <<"bad_storage_type">>, ?ERR_BAD_VALUE_NOT_ALLOWED(K(<<"type">>), ?MODIFY_STORAGE_TYPES)},
            {<<"name">>, 1, ?ERR_BAD_VALUE_STRING(K(<<"name">>))},
            % TODO VFS-12391 timeout is being changed to binary and not validated
%%            {<<"timeout">>, 0, ?ERR_BAD_VALUE_TOO_LOW(K(<<"timeout">>), 1)},
%%            {<<"timeout">>, -?STORAGE_TIMEOUT, ?ERR_BAD_VALUE_TOO_LOW(K(<<"timeout">>), 1)},
            {<<"timeout">>, <<"timeout_as_string">>, ?ERR_BAD_VALUE_INTEGER(K(<<"timeout">>))},
            %% TODO: VFS-7641 add records for badly formatted QoS
            {<<"qosParameters">>, <<"qos_not_a_map">>, ?ERR_MISSING_REQUIRED_VALUE(K(<<"qosParameters._">>))},
            {<<"qosParameters">>, #{<<"key">> => 1}, ?ERR_BAD_VALUE_STRING(K(<<"qosParameters.key">>))},
            {<<"qosParameters">>, #{<<"key">> => 0.1}, ?ERR_BAD_VALUE_STRING(K(<<"qosParameters.key">>))},
            {<<"archiveStorage">>, <<"not_a_boolean">>, ?ERR_BAD_VALUE_BOOLEAN(K(<<"archiveStorage">>))}
        ]
    };

build_modify_ceph_storage_data_spec(MemRef, ceph, bad_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),

    #data_spec{
        required = [
            {<<"type">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"type">>))}
        ],
        optional = [
            <<"name">>,
            %% TODO VFS-12391 changing hostname always timeouts
%%            <<"monitorHostname">>,
            <<"poolName">>,
            <<"username">>,
            <<"key">>
        ],
        correct_values = #{
            <<"type">> => [<<"ceph">>],
            <<"name">> => [<<"a">>],
            %% TODO VFS-12391 changing hostname always timeouts
%%            <<"monitorHostname">> => [<<"0.0.0.0">>],
            <<"poolName">> => [<<"dummy">>],
            <<"username">> => [<<"dummy">>],
            <<"key">> => [<<"dummy">>]
        },
        at_least_one_optional_value_in_data_sets = true
    }.


%% @private
build_modify_ceph_storage_setup_fun(MemRef) ->
    fun() ->
        StorageName = api_test_memory:get(MemRef, storage_name),

        % While it is not possible to create storage of type 'ceph' via REST due to validation
        % it can be done with rpc (bypassing REST checks)
        {_, {ok, StorageId}} = ?rpc(krakow, op_worker_storage:add(#{name => StorageName, params => #{
            type => <<"ceph">>,
            monitorHostname => ?CEPH_MONITOR_HOSTNAME,
            clusterName => ?CEPH_CLUSTER_NAME,
            poolName => ?CEPH_POOL_NAME,
            username => ?CEPH_USERNAME,
            key => ?CEPH_KEY,
            storagePathType => <<"flat">>,
            qosParameters => #{},
            lumaFeed => <<"auto">>
        }})),
        api_test_memory:set(MemRef, storage_id, StorageId),

        StorageDetails = opw_test_rpc:storage_describe(krakow, StorageId),
        api_test_memory:set(MemRef, storage_details, StorageDetails)
    end.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op_ceph",
        envs = [{op_worker, op_worker, [{fuse_session_grace_period_seconds, 24 * 60 * 60}]}]
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().
