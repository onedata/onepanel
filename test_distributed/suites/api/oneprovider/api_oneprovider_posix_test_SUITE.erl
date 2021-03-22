%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider posix storage API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_oneprovider_posix_test_SUITE).
-author("Piotr Duleba").

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
    add_correct_storage_perform_detection_test/1,
    add_correct_storage_skip_detection/1,
    add_bad_storage_perform_detection/1,
    add_bad_storage_skip_detection/1
]).

all() -> [
    add_correct_storage_perform_detection_test,
    add_correct_storage_skip_detection,
    add_bad_storage_perform_detection,
    add_bad_storage_skip_detection
].

-define(SUPPORT_SIZE, 10000000).
-define(ATTEMPTS, 60).
-define(SAMPLE_FILE_CONTENT, <<"abcdef">>).
-define(STORAGE_TYPES, [
    <<"cephrados">>,
    <<"glusterfs">>,
    <<"http">>,
    <<"localceph">>,
    <<"nulldevice">>,
    <<"posix">>,
    <<"s3">>,
    <<"swift">>,
    <<"webdav">>,
    <<"xrootd">>
]).
-define(POSIX_TIMEOUT, 5000).
-define(POSIX_QOS_PARAMETERS, #{
    <<"key">> => <<"value">>
}).


%%%===================================================================
%%% API
%%%===================================================================


add_correct_storage_perform_detection_test(Config) ->
    add_storage_test_base(Config, posix, correct_args, false).

add_correct_storage_skip_detection(Config) ->
    add_storage_test_base(Config, posix, correct_args, true).

add_bad_storage_perform_detection(Config) ->
    add_storage_test_base(Config, posix, bad_args, false).

add_bad_storage_skip_detection(Config) ->
    add_storage_test_base(Config, posix, bad_args, true).


%% @private
-spec add_storage_test_base(test_config:config(), atom, correct_args | bad_args, true | false) -> boolean().
add_storage_test_base(Config, StorageType, ArgsCorrectness, SkipStorageDetection) ->
    MemRef = api_test_memory:init(),
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Add storage using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = #client_spec{
                correct = [

                    {member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            setup_fun = build_add_posix_storage_setup_fun(MemRef),
            data_spec = build_add_storage_data_spec(MemRef, StorageType, ArgsCorrectness),
            prepare_args_fun = build_add_storage_prepare_args_fun(MemRef, SkipStorageDetection),
            validate_result_fun = build_add_posix_storage_validate_result_fun(ArgsCorrectness, SkipStorageDetection),
            verify_fun = build_add_posix_storage_verify_fun(MemRef, ArgsCorrectness, SkipStorageDetection)

        }
    ])).


%% @private
-spec build_add_posix_storage_setup_fun(api_test_memory:env_ref()) ->
    api_test_runner:setup_fun().
build_add_posix_storage_setup_fun(MemRef) ->
    fun() ->
        ExistingStorages = opw_test_rpc:get_storages(krakow),
        api_test_memory:set(MemRef, existing_storages, ExistingStorages)
    end.


%% @private
-spec build_add_storage_data_spec(api_test_memory:env_ref(), atom, correct_args | bad_args) ->
    api_test_runner:data_spec().
build_add_storage_data_spec(MemRef, posix, correct_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),
    #data_spec{
        required_with_custom_error = [
            {<<"type">>, ?ERROR_MISSING_REQUIRED_VALUE(iolist_to_binary([StorageName, <<".type">>]))},
            {<<"mountPoint">>, ?ERROR_MISSING_REQUIRED_VALUE(iolist_to_binary([StorageName, <<".mountPoint">>]))}
        ],
        optional = [
            <<"timeout">>,
            <<"qosParameters">>,
            <<"storagePathType">>
        ],
        correct_values = #{
            name => [StorageName],
            <<"type">> => [<<"posix">>],
            <<"mountPoint">> => [?POSIX_MOUNTPOINT],
            <<"timeout">> => [?POSIX_TIMEOUT],
            <<"qosParameters">> => [?POSIX_QOS_PARAMETERS],
            <<"storagePathType">> => [<<"canonical">>]
        },
        bad_values = [
            {<<"type">>, <<"bad_storage_type">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(iolist_to_binary([StorageName, <<".type">>]), ?STORAGE_TYPES)},
            {<<"timeout">>, <<"timeout_as_string">>, ?ERROR_BAD_VALUE_INTEGER(iolist_to_binary([StorageName, <<".timeout">>]))},
            {<<"qosParameters">>, <<"qos_not_a_map">>, ?ERROR_MISSING_REQUIRED_VALUE(iolist_to_binary([StorageName, <<".qosParameters._">>]))},
            {<<"storagePathType">>, 1, ?ERROR_BAD_VALUE_ATOM(iolist_to_binary([StorageName, <<".storagePathType">>]))}
        ]
    };
build_add_storage_data_spec(MemRef, posix, bad_args) ->
    StorageName = str_utils:rand_hex(10),
    api_test_memory:set(MemRef, storage_name, StorageName),
    #data_spec{
        required_with_custom_error = [
            {<<"type">>, ?ERROR_MISSING_REQUIRED_VALUE(iolist_to_binary([StorageName, <<".type">>]))},
            {<<"mountPoint">>, ?ERROR_MISSING_REQUIRED_VALUE(iolist_to_binary([StorageName, <<".mountPoint">>]))}
        ],
        correct_values = #{
            name => [StorageName],
            <<"type">> => [<<"posix">>],
            <<"mountPoint">> => [<<"/volumes/wrong/path">>]
        }
    }.


%% @private
-spec build_add_storage_prepare_args_fun(api_test_memory:env_ref(), true| false) ->
    api_test_runner:prepare_args_fun().
build_add_storage_prepare_args_fun(MemRef, SkipStorageDetection) ->
    fun(#api_test_ctx{data = Data}) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        RequestBody = #{
            StorageName => maps:put(<<"skipStorageDetection">>, SkipStorageDetection, Data)
        },
        #rest_args{
            method = post,
            path = <<"provider/storages">>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(RequestBody)}
    end.


%% @private
-spec build_add_posix_storage_validate_result_fun(bad_args | correct_args, true| false) ->
    api_test_runner:validate_result_fun().
build_add_posix_storage_validate_result_fun(correct_args, _) ->
    api_test_validate:http_204_no_content();
build_add_posix_storage_validate_result_fun(bad_args, true) ->
    api_test_validate:http_204_no_content();
build_add_posix_storage_validate_result_fun(bad_args, false) ->
    HostNames = api_test_utils:to_hostnames(oct_background:get_provider_nodes(krakow)),
    api_test_validate:http_400_bad_request(?ERROR_ON_NODES(?ERROR_STORAGE_TEST_FAILED(write), HostNames)).


%% @private
-spec build_add_posix_storage_verify_fun(api_test_memory:env_ref(), bad_args | correct_args, true| false) ->
    api_test_runner:verify_fun().
build_add_posix_storage_verify_fun(MemRef, bad_args, false) ->
    fun
        (_, _) ->
            StoragesBeforeTest = api_test_memory:get(MemRef, existing_storages),
            StoragesAfterTest = opw_test_rpc:get_storages(krakow),
            ?assertEqual(StoragesBeforeTest, StoragesAfterTest),
            true
    end;
build_add_posix_storage_verify_fun(MemRef, ArgsCorrectness, _SkipStorageDetection) ->
    fun
        (expected_success, _) ->
            StoragesBeforeTest = api_test_memory:get(MemRef, existing_storages),
            StoragesAfterTest = opw_test_rpc:get_storages(krakow),
            [NewStorageId] = ?assertMatch([_], lists:subtract(StoragesAfterTest, StoragesBeforeTest)),
            case ArgsCorrectness of
                correct_args -> ?assertEqual(ok, perform_io_test_on_storage(NewStorageId));
                bad_args -> ?assertEqual(error, perform_io_test_on_storage(NewStorageId))
            end,
            true;
        (expected_failure, _) ->
            StoragesBeforeTest = api_test_memory:get(MemRef, existing_storages),
            StoragesAfterTest = opw_test_rpc:get_storages(krakow),
            ?assertEqual(StoragesBeforeTest, StoragesAfterTest),
            true
    end.


%%%===================================================================
%%% Helpers
%%%===================================================================


%% @private
-spec perform_io_test_on_storage(binary()) -> ok | error.
perform_io_test_on_storage(StorageId) ->
    {UserSession, SpaceName} = create_and_support_space_with_storage(StorageId),
    FilePath = filename:join(["/", SpaceName, <<"test_file">>]),
    case opw_test_rpc:lfm_create(krakow, UserSession, FilePath) of
        {error, _} -> error;
        {ok, Guid} ->
            OpenHandle = opw_test_rpc:lfm_open(krakow, UserSession, {guid, Guid}, rdwr),

            {WriteHandle, Size} = opw_test_rpc:lfm_write(krakow, OpenHandle, 0, ?SAMPLE_FILE_CONTENT),
            ?assertEqual(length(binary_to_list(?SAMPLE_FILE_CONTENT)), Size),

            {_ReadHandle, Data} = opw_test_rpc:lfm_read(krakow, WriteHandle, 0, Size),
            ?assertEqual(?SAMPLE_FILE_CONTENT, Data)
    end.


%% @private
-spec create_and_support_space_with_storage(binary()) -> {binary(), binary()}.
create_and_support_space_with_storage(StorageId) ->
    SpaceName = str_utils:rand_hex(10),
    UserId = oct_background:get_user_id(joe),
    SpaceId = ozw_test_rpc:create_space(UserId, SpaceName),
    Token = ozw_test_rpc:create_space_support_token(UserId, SpaceId),
    {ok, SerializedToken} = tokens:serialize(Token),
    opw_test_rpc:support_space(krakow, StorageId, SerializedToken, ?SUPPORT_SIZE),
    UserSessId = oct_background:get_user_session_id(UserId, krakow),

    ?assertMatch(SpaceId, opw_test_rpc:get_user_space_by_name(krakow, UserSessId, UserId, SpaceName)),
    ?assertEqual(true, lists:member(SpaceId, opw_test_rpc:get_spaces(krakow)), ?ATTEMPTS),

    {UserSessId, SpaceName}.


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
