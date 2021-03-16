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

add_storage_test_base(Config, StorageType, ArgsCorrectness, SkipStorageDetection) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Add storage using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            data_spec = build_add_storage_data_spec(StorageType, ArgsCorrectness),
            prepare_args_fun = build_add_storage_prepare_args_fun(SkipStorageDetection),
            validate_result_fun = build_add_posix_storage_validate_result_fun(ArgsCorrectness, SkipStorageDetection)
            }
    ])).

build_add_storage_data_spec(posix, correct_args) ->
    StorageName = str_utils:rand_hex(10),
    node_cache:put(storage_name, StorageName),
    #data_spec{
        required_with_custom_error = [
            {<<"type">>, ?ERROR_MISSING_REQUIRED_VALUE(iolist_to_binary([StorageName, <<".type">>]))},
            {<<"mountPoint">>, ?ERROR_MISSING_REQUIRED_VALUE(iolist_to_binary([StorageName, <<".mountPoint">>]))}

        ],
        correct_values = #{
            name => [StorageName],
            <<"type">> => [<<"posix">>],
            <<"mountPoint">> => [<<"/volumes/persistence/storage">>]
        }
    };
build_add_storage_data_spec(posix, bad_args) ->
    StorageName = str_utils:rand_hex(10),
    node_cache:put(storage_name, StorageName),
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


build_add_storage_prepare_args_fun(SkipStorageDetection) ->
    fun(#api_test_ctx{data = Data}) ->
        StorageName = node_cache:get(storage_name),
        RequestBody = #{
            StorageName => maps:put(<<"skipStorageDetection">>, SkipStorageDetection, Data)
        },
        #rest_args{
            method = post,
            path = <<"provider/storages">>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(RequestBody)}
    end.


build_add_posix_storage_validate_result_fun(correct_args, true) ->
    api_test_validate:http_204_no_content();
build_add_posix_storage_validate_result_fun(correct_args, false) ->
    api_test_validate:http_204_no_content();
build_add_posix_storage_validate_result_fun(bad_args, true) ->
    api_test_validate:http_204_no_content();
build_add_posix_storage_validate_result_fun(bad_args, false) ->
    HostNames = api_test_utils:to_hostnames(oct_background:get_provider_nodes(krakow)),
    api_test_validate:http_400_bad_request(?ERROR_ON_NODES(?ERROR_STORAGE_TEST_FAILED(write), HostNames)).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "storages_api_tests"
    }).

end_per_suite(_Config) ->
    oct_background:end_per_suite().
