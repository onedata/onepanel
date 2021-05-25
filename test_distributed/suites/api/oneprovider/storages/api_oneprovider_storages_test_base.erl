%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains common test functions
%%% for tests concerning onepanel Storages API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_oneprovider_storages_test_base).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_storages.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

-export([
    add_storage_test_base/1
]).

-type storage_type() :: cephrados | glusterfs | http | localceph | nulldvice | posix | s3 | swift | webdav | xrootd.
-type args_correctness() :: bad_args | correct_args.
-type skip_storage_detection() :: boolean().
-type add_storage_test_spec() :: #add_storage_test_spec{}.

-type data_spec_builder() :: fun((_, _, _)-> api_test_runner:data_spec()).
-type prepare_args_fun_builder() :: fun((_, _)-> api_test_runner:prepare_args_fun()).

-type storage_id() :: binary().

-export_type([
    skip_storage_detection/0,
    storage_type/0,
    args_correctness/0,
    add_storage_test_spec/0,
    data_spec_builder/0,
    prepare_args_fun_builder/0,
    storage_id/0
]).

-define(DEFAULT_TEMP_CAVEAT_TTL, 360000).

%%%===================================================================
%%% API
%%%===================================================================


-spec add_storage_test_base(add_storage_test_spec()) -> ok.
add_storage_test_base(#add_storage_test_spec{
    storage_type = StorageType,
    args_correctness = ArgsCorrectness,
    skip_storage_detection = SkipStorageDetection,

    data_spec_fun = DataSpecFun,
    prepare_args_fun = PrepareArgsFun
}) ->

    MemRef = api_test_memory:init(),
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    ?assert(api_test_runner:run_tests([
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
            data_spec = DataSpecFun(MemRef, StorageType, ArgsCorrectness),
            prepare_args_fun = PrepareArgsFun(MemRef, SkipStorageDetection),
            validate_result_fun = build_add_storage_validate_result_fun(MemRef, ArgsCorrectness, SkipStorageDetection),
            verify_fun = build_add_storage_verify_fun(MemRef, ArgsCorrectness, SkipStorageDetection)
        }
    ])).


%% @private
-spec build_add_storage_verify_fun(
    api_test_memory:env_ref(),
    api_oneprovider_storages_test_base:args_correctness(),
    api_oneprovider_storages_test_base:skip_storage_detection()
) -> api_test_runner:verify_fun().
build_add_storage_verify_fun(_MemRef, bad_args, false) ->
    fun(_, _) ->
        true
    end;
build_add_storage_verify_fun(MemRef, ArgsCorrectness, _SkipStorageDetection) ->
    fun
        (expected_success, _) ->
            NewStorageId = api_test_memory:get(MemRef, storage_id),
            ?assertEqual(true, lists:member(NewStorageId, opw_test_rpc:get_storages(krakow)), ?ATTEMPTS),
            case ArgsCorrectness of
                correct_args ->
                    ?assertEqual(ok, perform_io_test_on_storage(NewStorageId), ?ATTEMPTS);
                bad_args ->
                    ?assertEqual(error, perform_io_test_on_storage(NewStorageId), ?ATTEMPTS)
            end,
            true;
        (expected_failure, _) ->
            true
    end.


%% @private
-spec build_add_storage_validate_result_fun(
    api_test_memory:env_ref(),
    args_correctness(),
    skip_storage_detection()
) ->
    api_test_runner:validate_result_fun().
build_add_storage_validate_result_fun(MemRef, correct_args, _) ->
    api_test_validate:http_200_ok(fun(Body) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        StorageId = kv_utils:get([StorageName, <<"id">>], Body),
        api_test_memory:set(MemRef, storage_id, StorageId)
    end);
build_add_storage_validate_result_fun(MemRef, bad_args, true) ->
    api_test_validate:http_200_ok(fun(Body) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        StorageId = kv_utils:get([StorageName, <<"id">>], Body),
        api_test_memory:set(MemRef, storage_id, StorageId)
    end);
build_add_storage_validate_result_fun(MemRef, bad_args, false) ->
    api_test_validate:http_400_bad_request(fun(Body) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        ExpRespBody = #{
            StorageName => ?REST_ERROR(?ERROR_STORAGE_TEST_FAILED(write))
        },
        ?assertEqual(ExpRespBody, Body)
    end).


%%%===================================================================
%%% Helpers
%%%===================================================================


%% @private
-spec perform_io_test_on_storage(storage_id()) -> ok | error.
perform_io_test_on_storage(StorageId) ->
    SpaceName = str_utils:rand_hex(10),
    UserId = oct_background:get_user_id(joe),
    SpaceId = ozw_test_rpc:create_space(UserId, SpaceName),
    Token = ozw_test_rpc:create_space_support_token(UserId, SpaceId),
    {ok, SerializedToken} = tokens:serialize(Token),
    opw_test_rpc:support_space(krakow, StorageId, SerializedToken, ?SUPPORT_SIZE),
    AccessToken = create_oz_temp_access_token(UserId),

    ?assertEqual(SpaceId, opw_test_rpc:get_user_space_by_name(krakow, UserId, SpaceName, AccessToken), ?ATTEMPTS),
    ?assertEqual(true, lists:member(SpaceId, opw_test_rpc:get_spaces(krakow)), ?ATTEMPTS),
    Path = filename:join(["/", SpaceName]),
    opw_test_rpc:perform_io_test(krakow, Path, UserId, AccessToken).


%% @private
-spec create_oz_temp_access_token(UserId :: binary()) -> tokens:serialized().
create_oz_temp_access_token(UserId) ->
    OzNode = hd(oct_background:get_zone_nodes()),
    Auth = ?USER(UserId),
    Now = ozw_test_rpc:timestamp_seconds(OzNode),
    Token = ozw_test_rpc:create_user_temporary_token(OzNode, Auth, UserId, #{
        <<"type">> => ?ACCESS_TOKEN,
        <<"caveats">> => [#cv_time{valid_until = Now + ?DEFAULT_TEMP_CAVEAT_TTL}]
    }),

    {ok, SerializedToken} = tokens:serialize(Token),
    SerializedToken.
