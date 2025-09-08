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
-module(api_op_storages_test_base).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_storages.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

-export([
    add_storage_test_base/1,
    get_storage_test_base/2,
    modify_storage_test_base/1
]).

-type storage_type() :: ceph | cephrados | glusterfs | http | nfs | nulldevice | posix | s3 | swift | webdav | xrootd.
-type args_correctness() :: bad_args | correct_args.

-type add_storage_test_spec() :: #add_storage_test_spec{}.
-type modify_storage_test_spec() :: #modify_storage_test_spec{}.

-type data_spec_builder() :: fun((_, _, _)-> api_test_runner:data_spec()).
-type setup_fun_builder() :: fun((api_test_memory:env_ref()) -> api_test_runner:setup_fun_builder()).
-type prepare_args_fun_builder() :: fun((_)-> api_test_runner:prepare_args_fun()).

-type data_spec_random_coverage() :: 1..100.

-type storage_id() :: binary().

-export_type([
    storage_type/0,
    args_correctness/0,
    add_storage_test_spec/0,
    data_spec_builder/0,
    setup_fun_builder/0,
    prepare_args_fun_builder/0,
    data_spec_random_coverage/0,
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

    data_spec_fun = DataSpecFun,
    prepare_args_fun = PrepareArgsFun
}) ->

    MemRef = api_test_memory:init(),
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    ?assert(api_test_runner:run_tests([
        #suite_spec{
            target_nodes = ProviderPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },

            scenario_templates = [#scenario_template{
                name = <<"Add storage using /provider/storages rest endpoint">>,
                type = rest,
                prepare_args_fun = PrepareArgsFun(MemRef),
                validate_result_fun = build_add_storage_validate_result_fun(MemRef, ArgsCorrectness)
            }],
            test_case_generation_policy = randomly_select_scenarios_and_clients,

            data_spec = DataSpecFun(MemRef, StorageType, ArgsCorrectness),
            verify_fun = build_add_storage_verify_fun(MemRef, ArgsCorrectness)
        }
    ])).


get_storage_test_base(StorageId, ExpStorageDetails) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get storage details using /provider/storages/{storage_id} rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, []}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = <<"provider/storages/", StorageId/binary>>
                }
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                RespBodyBinary = onepanel_utils:convert_recursive(RespBody, {map, binary}),
                ?assertEqual(ExpStorageDetails, RespBodyBinary)
            end)
        }
    ])).


-spec modify_storage_test_base(modify_storage_test_spec()) -> ok.
modify_storage_test_base(TestSpec = #modify_storage_test_spec{
    storage_type = StorageType,
    args_correctness = ArgsCorrectness,

    build_data_spec_fun = DataSpecFun,
    build_setup_fun = SetupFun
}) ->

    MemRef = api_test_memory:init(),
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    ?assert(api_test_runner:run_tests([
        #suite_spec{
            target_nodes = ProviderPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },

            setup_fun = SetupFun(MemRef),
            verify_fun = build_modify_storage_verify_fun(MemRef),

            scenario_templates = [#scenario_template{
                name = <<"Modify storage using /provider/storages/{id} rest endpoint">>,
                type = rest,
                prepare_args_fun = build_modify_storage_prepare_args_fun(MemRef),
                validate_result_fun = build_modify_storage_validate_result_fun(MemRef, TestSpec)
            }],
            test_case_generation_policy = randomly_select_scenarios_and_clients,

            data_spec = DataSpecFun(MemRef, StorageType, ArgsCorrectness)
        }
    ])).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
build_add_storage_validate_result_fun(MemRef, correct_args) ->
    api_test_validate:http_200_ok(fun(Body) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        StorageId = kv_utils:get([StorageName, <<"id">>], Body),
        api_test_memory:set(MemRef, storage_id, StorageId)
    end);
build_add_storage_validate_result_fun(MemRef, bad_args) ->
    api_test_validate:http_400_bad_request(fun(Body) ->
        StorageName = api_test_memory:get(MemRef, storage_name),
        ExpRespBody = #{
            StorageName => ?REST_ERROR(?ERR_STORAGE_TEST_FAILED(access))
        },
        ?assertEqual(ExpRespBody, Body)
    end).


%% @private
build_add_storage_verify_fun(_MemRef, bad_args) ->
    fun(_, _) ->
        true
    end;
build_add_storage_verify_fun(MemRef, _ArgsCorrectness) ->
    fun
        (expected_success, _) ->
            NewStorageId = api_test_memory:get(MemRef, storage_id),
            ?assertEqual(true, lists:member(NewStorageId, opw_test_rpc:get_storages(krakow)), ?ATTEMPTS),
            StorageDetails = opw_test_rpc:storage_describe(krakow, NewStorageId),
            check_io_on_storage_if_not_nulldevice(NewStorageId, StorageDetails),
            true;
        (expected_failure, _) ->
            true
    end.


%% @private
build_modify_storage_prepare_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        StorageId = api_test_memory:get(MemRef, storage_id),
        StorageName = api_test_memory:get(MemRef, storage_name),

        api_test_memory:set(MemRef, storage_diff, Data),
        RequestBody = #{StorageName => Data},

        #rest_args{
            method = patch,
            path = <<"provider/storages/", StorageId/binary>>,
            headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
            body = json_utils:encode(RequestBody)}
    end.


%% @private
build_modify_storage_validate_result_fun(MemRef, #modify_storage_test_spec{
    args_correctness = correct_args,
    map_storage_description_to_exp_rest_response_fun = MappingFun
}) ->
    api_test_validate:http_200_ok(fun(Response) ->
        NewStorageName = maps:get(<<"name">>, Response),
        api_test_memory:set(MemRef, storage_name, NewStorageName),

        PrevStorageDetails = api_test_memory:get(MemRef, storage_details),
        StorageDiff = api_test_memory:get(MemRef, storage_diff),

        ExpNewStorageDetails = convert_fields_to_binary(json_utils:merge([
            PrevStorageDetails, StorageDiff
        ])),
        api_test_memory:set(MemRef, storage_details, ExpNewStorageDetails#{
            % TODO VFS-12391 Despite lumaFeed being changed to binary in response,
            % when you later asks for details with rpc it is atom :)
            <<"lumaFeed">> => binary_to_atom(maps:get(<<"lumaFeed">>, ExpNewStorageDetails))
        }),

        ExpResponse = MappingFun(ExpNewStorageDetails#{<<"verificationPassed">> => true}),
        ?assertEqual(ExpResponse, Response)
    end);
build_modify_storage_validate_result_fun(MemRef, #modify_storage_test_spec{
    args_correctness = bad_args,
    map_storage_description_to_exp_rest_response_fun = MappingFun
}) ->
    api_test_validate:http_200_ok(fun(Response) ->
        PrevStorageDetails = api_test_memory:get(MemRef, storage_details),
        ExpStorageDetails = convert_fields_to_binary(PrevStorageDetails),
        ExpResponse = MappingFun(ExpStorageDetails#{<<"verificationPassed">> => false}),

        ?assertEqual(ExpResponse, Response)
    end).


%% @private
build_modify_storage_verify_fun(MemRef) ->
    fun(_, _) ->
        StorageId = api_test_memory:get(MemRef, storage_id),
        ExpStorageDetails = api_test_memory:get(MemRef, storage_details),
        StorageDetails = opw_test_rpc:storage_describe(krakow, StorageId),
        ?assertEqual(ExpStorageDetails, StorageDetails),
        check_io_on_storage_if_not_nulldevice(StorageId, StorageDetails),
        true
    end.


check_io_on_storage_if_not_nulldevice(_StorageId, #{<<"type">> := <<"nulldevice">>}) ->
    ok;
check_io_on_storage_if_not_nulldevice(StorageId, _StorageDetails) ->
    ?assertMatch({ok, _}, api_test_utils:perform_io_test_on_storage(StorageId), ?ATTEMPTS).


% TODO VFS-12391 storage update changes types of several fields to binary - debug
%% @private
convert_fields_to_binary(StorageDetails) ->
    lists:foldl(fun(Key, DetailsAcc) ->
        case maps:is_key(Key, DetailsAcc) of
            true -> maps:update_with(Key, fun str_utils:to_binary/1, DetailsAcc);
            false -> DetailsAcc
        end
    end, StorageDetails, [
        <<"archiveStorage">>,  % TODO VFS-12391 boolean
        <<"lumaFeed">>,
        <<"timeout">>,  % TODO VFS-12391 integer
        <<"maximumCanonicalObjectSize">>,  % TODO VFS-12391 integer
        <<"verifyServerCertificate">>,  % TODO VFS-12391 boolean
        <<"connectionPoolSize">>,  % TODO VFS-12391 int
        <<"maximumUploadSize">>,  % TODO VFS-12391 int
        <<"latencyMin">>,
        <<"latencyMax">>,
        <<"timeoutProbability">>,
        <<"filter">>,
        <<"simulatedFilesystemParameters">>,
        <<"simulatedFilesystemGrowSpeed">>,
        <<"enableDataVerification">>
    ]).
