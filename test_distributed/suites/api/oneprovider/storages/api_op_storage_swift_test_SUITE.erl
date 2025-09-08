%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider SWIFT storage API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_op_storage_swift_test_SUITE).
-author("Bartosz Walkowicz").

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
    end_per_suite/1
]).

-export([
    add_correct_storage_test/1,
    add_bad_storage_test/1,

    get_storage_test/1,

    modify_correct_storage_test/1,
    modify_bad_storage_test/1
]).

groups() -> [
    {all_tests, [parallel], [
        add_correct_storage_test,
        add_bad_storage_test,

        get_storage_test,

        modify_correct_storage_test,
        modify_bad_storage_test
    ]}
].

all() -> [
    {group, all_tests}
].

-define(MIN_SWIFT_STORAGE_SPEC, #{
    <<"type">> => <<"swift">>,
    <<"authUrl">> => ?SWIFT_AUTH_URL,
    <<"projectName">> => ?SWIFT_PROJECT_NAME,
    <<"username">> => ?SWIFT_USERNAME,
    <<"password">> => ?SWIFT_PASSWORD,
    <<"containerName">> => ?SWIFT_CONTAINER_NAME
}).


%%%===================================================================
%%% API
%%%===================================================================


add_correct_storage_test(_Config) ->
    add_swift_storage_test_base(correct_args).


add_bad_storage_test(_Config) ->
    add_swift_storage_test_base(bad_args).


%% @private
-spec add_swift_storage_test_base(api_op_storages_test_base:args_correctness()) -> ok.
add_swift_storage_test_base(ArgsCorrectness) ->
    api_op_storages_test_base:add_storage_test_base(
        #add_storage_test_spec{
            storage_type = swift,
            args_correctness = ArgsCorrectness,

            data_spec_fun = fun build_add_swift_storage_data_spec/3,
            prepare_args_fun = fun build_add_swift_storage_prepare_args_fun/1
        }).


%% @private
-spec build_add_swift_storage_data_spec(
    api_test_memory:env_ref(),
    api_op_storages_test_base:storage_type(),
    api_op_storages_test_base:args_correctness()
) ->
    api_test_runner:data_spec().
build_add_swift_storage_data_spec(MemRef, swift, correct_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),

    #data_spec{
        required = [
            {<<"type">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"type">>))},
            {<<"authUrl">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"authUrl">>))},
            {<<"projectName">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"projectName">>))},
            {<<"username">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"username">>))},
            {<<"password">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"password">>))},
            {<<"containerName">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"containerName">>))}
        ],
        optional = [
            <<"userDomainName">>,
            <<"projectDomainName">>,
            <<"timeout">>,
            <<"qosParameters">>,
            <<"storagePathType">>,
            <<"archiveStorage">>,
            <<"blockSize">>
        ],
        correct_values = #{
            <<"type">> => [<<"swift">>],
            <<"authUrl">> => [?SWIFT_AUTH_URL],
            <<"projectName">> => [?SWIFT_PROJECT_NAME],
            <<"username">> => [?SWIFT_USERNAME],
            <<"password">> => [?SWIFT_PASSWORD],
            <<"containerName">> => [?SWIFT_CONTAINER_NAME],
            <<"userDomainName">> => [?SWIFT_USER_DOMAIN_NAME],
            <<"projectDomainName">> => [?SWIFT_PROJECT_DOMAIN_NAME],
            <<"blockSize">> => [1024],
            <<"timeout">> => [?STORAGE_TIMEOUT, ?STORAGE_TIMEOUT div 2],
            <<"qosParameters">> => [?STORAGE_QOS_PARAMETERS],
            %% TODO VFS-12772 Specify and test which storages can have flat or canonical as storage_path_type
            <<"storagePathType">> => [<<"flat">>],
            %% TODO VFS-8782 verify if archiveStorage option works properly on storage
            <<"archiveStorage">> => [true, false]
        },
        bad_values = [
            {<<"type">>, <<"bad_storage_type">>, ?ERR_BAD_VALUE_NOT_ALLOWED(?STORAGE_DATA_KEY(StorageName, <<"type">>), ?STORAGE_TYPES)},
            {<<"blockSize">>, <<"blockSize_as_string">>, ?ERR_BAD_VALUE_INTEGER(?STORAGE_DATA_KEY(StorageName, <<"blockSize">>))},
            {<<"storagePathType">>, 1, ?ERR_BAD_VALUE_STRING(?STORAGE_DATA_KEY(StorageName, <<"storagePathType">>))},
            {<<"timeout">>, 0, ?ERR_BAD_VALUE_TOO_LOW(?STORAGE_DATA_KEY(StorageName, <<"timeout">>), 1)},
            {<<"timeout">>, -?STORAGE_TIMEOUT, ?ERR_BAD_VALUE_TOO_LOW(?STORAGE_DATA_KEY(StorageName, <<"timeout">>), 1)},
            {<<"timeout">>, <<"timeout_as_string">>, ?ERR_BAD_VALUE_INTEGER(?STORAGE_DATA_KEY(StorageName, <<"timeout">>))},
            %% TODO: VFS-7641 add records for badly formatted QoS
            {<<"qosParameters">>, #{<<"key">> => 1}, ?ERR_BAD_VALUE_STRING(?STORAGE_DATA_KEY(StorageName, <<"qosParameters.key">>))},
            {<<"qosParameters">>, #{<<"key">> => 0.1}, ?ERR_BAD_VALUE_STRING(?STORAGE_DATA_KEY(StorageName, <<"qosParameters.key">>))},
            {<<"archiveStorage">>, <<"not_a_boolean">>, ?ERR_BAD_VALUE_BOOLEAN(?STORAGE_DATA_KEY(StorageName, <<"archiveStorage">>))}
        ]
    };

build_add_swift_storage_data_spec(MemRef, swift, bad_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),

    #data_spec{
        required = [
            {<<"type">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"type">>))},
            {<<"authUrl">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"authUrl">>))},
            {<<"projectName">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"projectName">>))},
            {<<"username">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"username">>))},
            {<<"password">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"password">>))},
            {<<"containerName">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"containerName">>))}
        ],
        correct_values = #{
            <<"type">> => [<<"swift">>],
            <<"authUrl">> => [<<"nonexistent.host">>],
            <<"projectName">> => [<<"nonexistent_project_name">>],
            <<"username">> => [<<"invalid_user">>],
            <<"password">> => [<<"invalid_password">>],
            <<"containerName">> => [<<"nonexistent_container">>]
        }
    }.


%% @private
-spec build_add_swift_storage_prepare_args_fun(api_test_memory:env_ref()) ->
    api_test_runner:prepare_args_fun().
build_add_swift_storage_prepare_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        RequestBody = #{StorageName => Data},

        #rest_args{
            method = post,
            path = <<"provider/storages">>,
            headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
            body = json_utils:encode(RequestBody)}
    end.


get_storage_test(_Config) ->
    StorageName = ?RAND_STR(),
    StorageSpec = ?MIN_SWIFT_STORAGE_SPEC,
    StorageId = panel_test_rpc:add_storage(krakow, #{StorageName => StorageSpec}),

    %% TODO VFS-12773 debug why storage get omits timeout parameter
    api_op_storages_test_base:get_storage_test_base(StorageId, StorageSpec#{
        <<"id">> => StorageId,
        <<"name">> => StorageName,

        % password MUST BE shadowed
        <<"password">> => <<"*****">>,

        % default values for not supplied parameters
        <<"projectDomainName">> => ?SWIFT_PROJECT_DOMAIN_NAME,
        <<"userDomainName">> => ?SWIFT_USER_DOMAIN_NAME,
        % TODO VFS-12391 shouldn't this be int?
        <<"blockSize">> => str_utils:to_binary(?SWIFT_DEFAULT_BLOCK_SIZE),

        <<"storagePathType">> => <<"flat">>,
        <<"lumaFeed">> => <<"auto">>,
        % additional qosParameters (not supplied when creating) SHOULD BE present
        <<"qosParameters">> => #{
            <<"providerId">> => oct_background:get_provider_id(krakow),
            <<"storageId">> => StorageId
        },
        <<"archiveStorage">> => <<"false">>,
        <<"importedStorage">> => <<"false">>,
        <<"readonly">> => <<"false">>,
        <<"rootGid">> => <<"0">>,
        <<"rootUid">> => <<"0">>
    }).


modify_correct_storage_test(_Config) ->
    modify_swift_storage_test_base(correct_args).


modify_bad_storage_test(_Config) ->
    modify_swift_storage_test_base(bad_args).


%% @private
-spec modify_swift_storage_test_base(api_op_storages_test_base:args_correctness()) ->
    ok.
modify_swift_storage_test_base(ArgsCorrectness) ->
    api_op_storages_test_base:modify_storage_test_base(
        #modify_storage_test_spec{
            storage_type = swift,
            args_correctness = ArgsCorrectness,

            build_data_spec_fun = fun build_modify_swift_storage_data_spec/3,
            build_setup_fun = fun build_modify_swift_storage_setup_fun/1
        }).


%% @private
build_modify_swift_storage_data_spec(MemRef, swift, correct_args) ->
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
        ],
        correct_values = #{
            <<"type">> => [<<"swift">>],
            <<"name">> => [?RAND_STR(10)],
            <<"timeout">> => [?STORAGE_TIMEOUT, ?STORAGE_TIMEOUT div 2],
            <<"qosParameters">> => [#{<<"key">> => <<"value">>}],
            %% TODO VFS-8782 verify if archiveStorage option works properly on storage
            <<"archiveStorage">> => [?RAND_BOOL()]
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

build_modify_swift_storage_data_spec(MemRef, swift, bad_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),

    #data_spec{
        required = [
            {<<"type">>, ?ERR_MISSING_REQUIRED_VALUE(?STORAGE_DATA_KEY(StorageName, <<"type">>))}
        ],
        optional = [
            <<"name">>,
            <<"authUrl">>,
            <<"projectName">>,
            <<"username">>,
            <<"password">>,
            <<"containerName">>,
            <<"userDomainName">>,
            <<"projectDomainName">>
        ],
        correct_values = #{
            <<"type">> => [<<"swift">>],
            <<"name">> => [<<"a">>],
            <<"authUrl">> => [<<"nonexistent.host">>],
            <<"projectName">> => [<<"nonexistent_project_name">>],
            <<"username">> => [<<"invalid_user">>],
            <<"password">> => [<<"invalid_password">>],
            <<"containerName">> => [<<"nonexistent_container">>],
            <<"userDomainName">> => [<<"invalid_user_domain_name">>],
            <<"projectDomainName">> => [<<"nonexistent_project_domain_name">>]
        },
        at_least_one_optional_value_in_data_sets = true
    }.


%% @private
-spec build_modify_swift_storage_setup_fun(api_test_memory:env_ref()) ->
    api_test_runner:setup_fun().
build_modify_swift_storage_setup_fun(MemRef) ->
    fun() ->
        StorageName = api_test_memory:get(MemRef, storage_name),

        StorageId = panel_test_rpc:add_storage(krakow, #{StorageName => ?MIN_SWIFT_STORAGE_SPEC}),
        api_test_memory:set(MemRef, storage_id, StorageId),

        StorageDetails = opw_test_rpc:storage_describe(krakow, StorageId),
        api_test_memory:set(MemRef, storage_details, StorageDetails)
    end.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op_swift",
        envs = [{op_worker, op_worker, [{fuse_session_grace_period_seconds, 24 * 60 * 60}]}]
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().
