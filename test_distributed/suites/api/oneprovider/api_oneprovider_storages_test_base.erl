%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains common test functions for tests concerning onepanel Storages API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_oneprovider_storages_test_base).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_storages.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

-export([
    add_storage_test_base/9,
    perform_io_test_on_storage/1
]).

-type storage_type() :: cephrados | glusterfs | http | localceph | nulldvice | posix | s3 | swift | webdav | xrootd.
-type args_correctness() :: bad_args | correct_args.

-export_type([
    storage_type/0,
    args_correctness/0
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec add_storage_test_base(test_config:config(), storage_type(), args_correctness(), boolean(),
    api_test_runner:setup_fun(), api_test_runner:data_spec(), api_test_runner:prepare_args_fun(),
    api_test_runner:validate_call_result_fun(), api_test_runner:verify_fun()) -> ok.
add_storage_test_base(Config, StorageType, ArgsCorrectness, SkipStorageDetection,
    SetupFun, DataSpecFun, PrepareArgsFun, ValidateResultFun, VerifyFun) ->
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
            setup_fun = SetupFun(MemRef),
            data_spec = DataSpecFun(MemRef, StorageType, ArgsCorrectness),
            prepare_args_fun = PrepareArgsFun(MemRef, SkipStorageDetection),
            validate_result_fun = ValidateResultFun(MemRef, ArgsCorrectness, SkipStorageDetection),
            verify_fun = VerifyFun(MemRef, ArgsCorrectness, SkipStorageDetection)

        }
    ])).


-spec perform_io_test_on_storage(binary()) -> ok | error.
perform_io_test_on_storage(StorageId) ->
    SpaceName = str_utils:rand_hex(10),
    UserId = oct_background:get_user_id(joe),
    SpaceId = ozw_test_rpc:create_space(UserId, SpaceName),
    Token = ozw_test_rpc:create_space_support_token(UserId, SpaceId),
    {ok, SerializedToken} = tokens:serialize(Token),
    opw_test_rpc:support_space(krakow, StorageId, SerializedToken, ?SUPPORT_SIZE),
    UserSessId = oct_background:get_user_session_id(UserId, krakow),

    ?assertMatch(SpaceId, opw_test_rpc:get_user_space_by_name(krakow, UserSessId, UserId, SpaceName)),
    ?assertEqual(true, lists:member(SpaceId, opw_test_rpc:get_spaces(krakow)), ?ATTEMPTS),
    Path = filename:join(["/", SpaceName]),
    opw_test_rpc:perform_io_test(krakow, UserSessId, Path).
