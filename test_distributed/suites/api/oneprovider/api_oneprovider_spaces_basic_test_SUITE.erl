%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider space API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_oneprovider_spaces_basic_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/space_support/support_parameters.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

-define(ATTEMPTS, 60).

-define(SUPPORT_SIZE, 300000000).
-define(STORAGE_NAME, <<"posix">>).
-define(MIN_SUPPORT_SIZE, 10000000).

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1,

    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    get_space_ids_test/1,
    get_space_details_test/1,

    support_space_test/1,
    modify_space_support_test/1,

    revoke_space_support_test/1
]).

all() -> [
    get_space_ids_test,
    get_space_details_test,

    support_space_test,
    modify_space_support_test,

    revoke_space_support_test
].


-define(ERROR_DIR_STATS_DISABLED_WHEN_ACCOUNTING_ENABLED, ?ERROR_BAD_DATA(
    <<"dirStatsServiceEnabled">>,
    <<"Dir stats service must be enabled if accounting is enabled">>
)).


%%%===================================================================
%%% API
%%%===================================================================


get_space_ids_test(_Config) ->
    get_space_ids_test_base([]),

    FirstSpaceId = create_and_support_space(),
    get_space_ids_test_base([FirstSpaceId]),

    ManySpacesId = [create_and_support_space() || _ <- lists:seq(1, 5)],
    get_space_ids_test_base([FirstSpaceId | ManySpacesId]).


%% @private
get_space_ids_test_base(ExpSpaceIds) ->
    ProviderId = oct_background:get_provider_id(krakow),
    OpPanelNodes = oct_background:get_provider_panels(krakow),
    SortedExpSpaceIds = lists:sort(ExpSpaceIds),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get space ids using /provider/spaces rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, []}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) ->
                #rest_args{method = get, path = <<"provider/spaces">>}
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertEqual(SortedExpSpaceIds, lists:sort(maps:get(<<"ids">>, RespBody)))
            end)
        }
    ])).


get_space_details_test(_Config) ->
    get_space_details_test_base(<<"posix-space">>, ?STORAGE_NAME, 10000000),
    get_space_details_test_base(<<"null-space">>, <<"IdealNullStorage">>, 20000000).


%% @private
get_space_details_test_base(SpaceName, StorageName, SupportSize) ->
    ProviderId = oct_background:get_provider_id(krakow),
    OpWorkerNodes = oct_background:get_provider_nodes(krakow),
    OpPanelNodes = oct_background:get_provider_panels(krakow),

    StorageId = api_test_utils:get_storage_id_by_name(krakow, StorageName),
    SpaceId = create_and_support_space(SpaceName, StorageName, SupportSize),
    ExpResult0 = get_expected_space_details(
        SpaceId, SpaceName, StorageId, SupportSize, false, false
    ),
    ExpResult1 = ExpResult0#{<<"dirStatsServiceStatus">> => <<"disabled">>},

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get space details using /provider/spaces/{space_id} rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, []}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = build_get_space_details_prepare_rest_args_fun(SpaceId),
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertEqual(ExpResult1, RespBody)
            end),

            data_spec = build_get_space_details_data_spec(OpWorkerNodes)
        }
    ])).


%% @private
build_get_space_details_data_spec(OpWorkerNodes) ->
    HostNames = api_test_utils:to_hostnames(OpWorkerNodes),
    #data_spec{
        bad_values = [{bad_id, <<"NonExistentSpace">>, ?ERROR_ON_NODES(?ERROR_NOT_FOUND, HostNames)}]
    }.


%% @private
build_get_space_details_prepare_rest_args_fun(SpaceId) ->
    fun(#api_test_ctx{data = Data}) ->
        {Id, _} = api_test_utils:maybe_substitute_bad_id(SpaceId, Data),
        #rest_args{
            method = get,
            path = <<"provider/spaces/", Id/binary>>
        }
    end.


support_space_test(_Config) ->
    MemRef = api_test_memory:init(),
    ProviderId = oct_background:get_provider_id(krakow),
    OpWorkerNodes = oct_background:get_provider_nodes(krakow),
    OpPanelNodes = oct_background:get_provider_panels(krakow),

    SpaceName = str_utils:rand_hex(12),
    StorageId = api_test_utils:get_storage_id_by_name(krakow, ?STORAGE_NAME),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Support space using /provider/spaces rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
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

            setup_fun = build_support_space_setup_fun(MemRef, SpaceName),
            verify_fun = build_support_space_verify_fun(MemRef),

            prepare_args_fun = build_support_space_prepare_args_fun(MemRef),
            validate_result_fun = api_test_validate:http_201_created("provider/spaces/", <<"id">>,
                fun(SupportedSpaceId) ->
                    ?assertEqual(api_test_memory:get(MemRef, space_id), SupportedSpaceId)
                end),

            data_spec = build_support_space_data_spec(StorageId, OpWorkerNodes)
        }
    ])).


%% @private
build_support_space_setup_fun(MemRef, SpaceName) ->
    fun() ->
        {SpaceId, Token} = create_space_and_support_token(SpaceName),
        api_test_memory:set(MemRef, support_token, Token),
        api_test_memory:set(MemRef, space_id, SpaceId),
        api_test_memory:set(MemRef, space_name, SpaceName),
        ok
    end.


%% @private
build_support_space_data_spec(StorageId, OpWorkerNodes) ->
    HostNames = api_test_utils:to_hostnames(OpWorkerNodes),
    #data_spec{
        required = [<<"size">>, <<"storageId">>, <<"token">>],
        optional = [<<"accountingEnabled">>, <<"dirStatsServiceEnabled">>],
        correct_values = #{
            <<"size">> => [?SUPPORT_SIZE, 5 * ?SUPPORT_SIZE],
            <<"storageId">> => [StorageId],
            <<"token">> => [token_placeholder],
            <<"accountingEnabled">> => [false, true_with_dir_stats_service_enabled],
            <<"dirStatsServiceEnabled">> => [false, true]
        },
        bad_values = [
            {<<"size">>, -?SUPPORT_SIZE, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, ?MIN_SUPPORT_SIZE), HostNames)},
            {<<"size">>, ?MIN_SUPPORT_SIZE - 1, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, ?MIN_SUPPORT_SIZE), HostNames)},
            {<<"size">>, <<"Nan">>, ?ERROR_BAD_VALUE_INTEGER(<<"size">>)},
            {<<"storageId">>, <<"inexistientStorageId">>, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"storageId">>), HostNames)},
            {<<"accountingEnabled">>, <<"NaN">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"accountingEnabled">>)},
            {<<"accountingEnabled">>, true_with_dir_stats_service_disabled, ?ERROR_ON_NODES(
                ?ERROR_DIR_STATS_DISABLED_WHEN_ACCOUNTING_ENABLED, HostNames
            )},
            {<<"dirStatsServiceEnabled">>, <<"NaN">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"dirStatsServiceEnabled">>)}
        ]
    }.


%% @private
build_support_space_prepare_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        Token = api_test_memory:get(MemRef, support_token),

        RequestMap1 = case maps:get(<<"token">>, Data, undefined) of
            token_placeholder -> maps:put(<<"token">>, Token, Data);
            _ -> Data
        end,
        RequestMap2 = replace_accounting_related_placeholders(RequestMap1),

        #rest_args{
            method = post,
            path = <<"provider/spaces">>,
            headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
            body = json_utils:encode(RequestMap2)}
    end.


%% @private
build_support_space_verify_fun(MemRef) ->
    fun
        (expected_success, #api_test_ctx{data = Data0}) ->
            Data1 = replace_accounting_related_placeholders(Data0),

            SpaceId = api_test_memory:get(MemRef, space_id),
            ExpSpaceName = api_test_memory:get(MemRef, space_name),
            ExpStorageId = maps:get(<<"storageId">>, Data1),
            ExpSupportSize = maps:get(<<"size">>, Data1),
            ExpAccountingEnabled = maps:get(<<"accountingEnabled">>, Data1, false),
            ExpDirStatsEnabled = maps:get(<<"dirStatsServiceEnabled">>, Data1, true),

            ExpectedSpaceDetails = get_expected_space_details(
                SpaceId, ExpSpaceName, ExpStorageId, ExpSupportSize,
                ExpAccountingEnabled, ExpDirStatsEnabled
            ),
            SpaceDetails = get_space_details_with_rpc(SpaceId),

            ?assertEqual(ExpectedSpaceDetails, SpaceDetails),
            true;
        (expected_failure, _) ->
            SpaceId = api_test_memory:get(MemRef, space_id),
            SpaceIds = opw_test_rpc:get_spaces(krakow),

            ?assertNot(lists:member(SpaceId, SpaceIds)),
            true
    end.


modify_space_support_test(_Config) ->
    MemRef = api_test_memory:init(),
    ProviderId = oct_background:get_provider_id(krakow),
    OpWorkerNodes = oct_background:get_provider_nodes(krakow),
    OpPanelNodes = oct_background:get_provider_panels(krakow),

    StorageId = api_test_utils:get_storage_id_by_name(krakow, ?STORAGE_NAME),
    SpaceName = str_utils:rand_hex(12),
    SupportSize = ?SUPPORT_SIZE,

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Modify space support using /provider/spaces rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
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

            setup_fun = build_modify_space_support_setup_fun(MemRef, SpaceName, SupportSize, StorageId),
            prepare_args_fun = build_modify_space_support_prepare_rest_args_fun(MemRef),

            verify_fun = build_modify_space_support_verify_fun(MemRef),
            validate_result_fun = api_test_validate:http_204_no_content(),

            data_spec = build_modify_space_support_data_spec(SupportSize, OpWorkerNodes)
        }
    ])).


%% @private
build_modify_space_support_setup_fun(MemRef, SpaceName, SupportSize, StorageId) ->
    fun() ->
        {SpaceId, Token} = create_space_and_support_token(SpaceName),
        opw_test_rpc:support_space(krakow, StorageId, Token, SupportSize),
        SpaceDetails = get_space_details_with_rpc(SpaceId),
        api_test_memory:set(MemRef, space_id, SpaceId),
        api_test_memory:set(MemRef, space_name, SpaceName),
        api_test_memory:set(MemRef, storage_id, StorageId),
        api_test_memory:set(MemRef, support_size, SupportSize),
        api_test_memory:set(MemRef, space_details, SpaceDetails)
    end.


%% @private
build_modify_space_support_data_spec(SupportSize, OpWorkerNodes) ->
    HostNames = api_test_utils:to_hostnames(OpWorkerNodes),
    #data_spec{
        optional = [<<"size">>, <<"accountingEnabled">>, <<"dirStatsServiceEnabled">>],
        correct_values = #{
            <<"size">> => [SupportSize, ?SUPPORT_SIZE div 2, 200 * SupportSize],
            <<"accountingEnabled">> => [false, true_with_dir_stats_service_enabled],
            <<"dirStatsServiceEnabled">> => [false, true]
        },
        bad_values = [
            % support size is checked on two levels:
            % 1) by Oneprovider, which disallows values lower than the space occupancy (0 in case of an empty space)
            % 2) by Onezone, which has a configurable lower limit
            % because of this, the resulting error may be different depending on the requested size
            {<<"size">>, -?SUPPORT_SIZE, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, 0), HostNames)},
            {<<"size">>, ?MIN_SUPPORT_SIZE - 1, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, ?MIN_SUPPORT_SIZE), HostNames)},
            {<<"size">>, <<"Nan">>, ?ERROR_BAD_VALUE_INTEGER(<<"size">>)},
            {<<"accountingEnabled">>, <<"NaN">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"accountingEnabled">>)},
            {<<"accountingEnabled">>, true_with_dir_stats_service_disabled, ?ERROR_ON_NODES(
                ?ERROR_DIR_STATS_DISABLED_WHEN_ACCOUNTING_ENABLED, HostNames
            )},
            {<<"dirStatsServiceEnabled">>, <<"NaN">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"dirStatsServiceEnabled">>)},
            {bad_id, <<"inexistentSpaceId">>, ?ERROR_ON_NODES(?ERROR_NOT_FOUND, HostNames)}
        ]
    }.


%% @private
build_modify_space_support_prepare_rest_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        SpaceId = api_test_memory:get(MemRef, space_id),
        {Id, LeftoverData1} = api_test_utils:maybe_substitute_bad_id(SpaceId, Data),
        LeftoverData2 = replace_accounting_related_placeholders(LeftoverData1),

        #rest_args{
            method = patch,
            path = <<"provider/spaces/", Id/binary>>,
            headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
            body = json_utils:encode(LeftoverData2)
        }
    end.


%% @private
build_modify_space_support_verify_fun(MemRef) ->
    fun
        (expected_success, #api_test_ctx{data = Data0}) ->
            Data1 = replace_accounting_related_placeholders(Data0),

            SpaceId = api_test_memory:get(MemRef, space_id),
            ExpSpaceName = api_test_memory:get(MemRef, space_name),
            ExpStorageId = api_test_memory:get(MemRef, storage_id),
            ExpAccountingEnabled = maps:get(<<"accountingEnabled">>, Data1, false),
            ExpDirStatsEnabled = maps:get(<<"dirStatsServiceEnabled">>, Data1, false),
            SupportSize = api_test_memory:get(MemRef, support_size),
            ExpSpaceSupportSize = maps:get(<<"size">>, Data1, SupportSize),

            ExpectedSpaceDetails = get_expected_space_details(
                SpaceId, ExpSpaceName, ExpStorageId, ExpSpaceSupportSize,
                ExpAccountingEnabled, ExpDirStatsEnabled
            ),

            % TODO VFS-6780 - currently, supporting providers are calculated asynchronously
            % (effective relation) and the information with updated support size might come with a delay.
            ?assertEqual(ExpectedSpaceDetails, catch get_space_details_with_rpc(SpaceId), ?ATTEMPTS),
            true;
        (expected_failure, _) ->
            SpaceId = api_test_memory:get(MemRef, space_id),
            SpaceDetailsBeforeTest = api_test_memory:get(MemRef, space_details),

            % TODO VFS-6780 - currently, supporting providers are calculated asynchronously
            % (effective relation) and the information with updated support size might come with a delay.
            ?assertEqual(SpaceDetailsBeforeTest, catch get_space_details_with_rpc(SpaceId), ?ATTEMPTS),
            true
    end.


revoke_space_support_test(_Config) ->
    MemRef = api_test_memory:init(),
    ProviderId = oct_background:get_provider_id(krakow),
    OpWorkerNodes = oct_background:get_provider_nodes(krakow),
    OpPanelNodes = oct_background:get_provider_panels(krakow),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Revoke space support using /provider/spaces/{space_id} rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
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

            setup_fun = build_revoke_space_support_setup_fun(MemRef),
            prepare_args_fun = build_revoke_space_support_prepare_rest_args_fun(MemRef),
            verify_fun = build_revoke_space_support_verify_fun(MemRef),
            data_spec = build_revoke_space_support_data_spec(OpWorkerNodes),

            validate_result_fun = api_test_validate:http_204_no_content()
        }
    ])).


%% @private
build_revoke_space_support_data_spec(OpWorkerNodes) ->
    HostNames = api_test_utils:to_hostnames(OpWorkerNodes),
    #data_spec{
        bad_values = [{bad_id, <<"NonExistentSpace">>, ?ERROR_ON_NODES(?ERROR_NOT_FOUND, HostNames)}]
    }.


%% @private
build_revoke_space_support_setup_fun(MemRef) ->
    fun() ->
        SpaceId = create_and_support_space(),
        api_test_memory:set(MemRef, space_id, SpaceId)
    end.


%% @private
build_revoke_space_support_prepare_rest_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        SpaceId = api_test_memory:get(MemRef, space_id),
        {Id, _} = api_test_utils:maybe_substitute_bad_id(SpaceId, Data),

        #rest_args{
            method = delete,
            path = <<"provider/spaces/", Id/binary>>
        }
    end.


%% @private
build_revoke_space_support_verify_fun(MemRef) ->
    fun(ExpectedResult, _) ->
        SpaceId = api_test_memory:get(MemRef, space_id),

        case ExpectedResult of
            expected_success -> ?assertNot(lists:member(SpaceId, opw_test_rpc:get_spaces(krakow)), ?ATTEMPTS);
            expected_failure -> ?assert(lists:member(SpaceId, opw_test_rpc:get_spaces(krakow)), ?ATTEMPTS)
        end,
        true
    end.


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
-spec get_expected_space_details(binary(), binary(), binary(), binary(), boolean(), boolean()) ->
    json_utils:json_map().
get_expected_space_details(
    SpaceId,
    SpaceName,
    StorageId,
    SupportSize,
    AccountingEnabled,
    DirStatsEnabled
) ->
    [ProviderId] = opw_test_rpc:get_space_providers(krakow, SpaceId),
    SupportingProviders = #{
        ProviderId => SupportSize
    },

    #{
        <<"id">> => SpaceId,
        <<"name">> => SpaceName,
        <<"storageId">> => StorageId,
        <<"localStorages">> => [StorageId],
        <<"supportingProviders">> => SupportingProviders,
        <<"importedStorage">> => false,
        <<"spaceOccupancy">> => 0,
        <<"accountingEnabled">> => AccountingEnabled,
        <<"dirStatsServiceEnabled">> => DirStatsEnabled
    }.


%% @private
-spec get_space_details_with_rpc(binary()) -> map().
get_space_details_with_rpc(SpaceId) ->
    SpaceDoc = opw_test_rpc:get_space_details(krakow, SpaceId),

    % TODO VFS-5497 Until multisupport is implemented, this list always has 1 storage
    [StorageId] = opw_test_rpc:get_space_local_storages(krakow, SpaceId),
    IsImportedStorage = opw_test_rpc:is_storage_imported(krakow, StorageId),
    AutocleaningStatus = opw_test_rpc:get_autocleaning_status(krakow, SpaceId),
    SpaceSupportParameters = opw_test_rpc:get_space_support_parameters(krakow, SpaceId),

    #{
        <<"id">> => SpaceId,
        <<"name">> => maps:get(name, SpaceDoc),
        <<"storageId">> => StorageId,
        <<"localStorages">> => [StorageId],
        <<"supportingProviders">> => maps:get(providers, SpaceDoc),
        <<"importedStorage">> => IsImportedStorage,
        <<"spaceOccupancy">> => maps:get(spaceOccupancy, AutocleaningStatus),
        <<"accountingEnabled">> => SpaceSupportParameters#support_parameters.accounting_enabled,
        <<"dirStatsServiceEnabled">> => SpaceSupportParameters#support_parameters.dir_stats_service_enabled
    }.


%% @private
-spec create_space_and_support_token(binary()) -> {SpaceId :: binary(), SerializedToken :: binary()}.
create_space_and_support_token(SpaceName) ->
    UserId = ozw_test_rpc:create_user(),
    SpaceId = ozw_test_rpc:create_space(UserId, SpaceName),
    SerializedToken = ozw_test_rpc:create_space_support_token(UserId, SpaceId),
    {SpaceId, SerializedToken}.


%% @private
-spec create_and_support_space() -> SpaceId :: binary().
create_and_support_space() ->
    SpaceName = str_utils:rand_hex(12),
    create_and_support_space(SpaceName, ?STORAGE_NAME, ?SUPPORT_SIZE).


%% @private
-spec create_and_support_space(binary(), binary(), binary()) -> SpaceId :: binary().
create_and_support_space(SpaceName, StorageName, SupportSize) ->
    {_, SerializedToken} = create_space_and_support_token( SpaceName),
    StorageId = api_test_utils:get_storage_id_by_name(krakow, StorageName),
    SpaceId = opw_test_rpc:support_space(krakow, StorageId, SerializedToken, SupportSize),
    ?assertEqual(true, lists:member(SpaceId, opw_test_rpc:get_spaces(krakow)), ?ATTEMPTS),
    SpaceId.


%% @private
-spec unsupport_all_spaces() -> ok.
unsupport_all_spaces() ->
    SpacesId = opw_test_rpc:get_spaces(krakow),
    [opw_test_rpc:revoke_space_support(krakow, X) || X <- SpacesId].


%% @private
-spec delete_all_spaces() -> ok.
delete_all_spaces() ->
    SpacesId = ozw_test_rpc:list_spaces(),
    [ozw_test_rpc:delete_space(X) || X <- SpacesId].


%% @private
-spec replace_accounting_related_placeholders(json_utils:json_map()) ->
    json_utils:json_map().
replace_accounting_related_placeholders(Data = #{<<"accountingEnabled">> := true_with_dir_stats_service_enabled}) ->
    Data#{<<"accountingEnabled">> => true, <<"dirStatsServiceEnabled">> => true};

replace_accounting_related_placeholders(Data = #{<<"accountingEnabled">> := true_with_dir_stats_service_disabled}) ->
    Data#{<<"accountingEnabled">> => true, <<"dirStatsServiceEnabled">> => false};

replace_accounting_related_placeholders(Data) ->
    Data.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        envs = [
            {oz_worker, oz_worker, [{minimum_space_support_size, ?MIN_SUPPORT_SIZE}]}
        ],
        onenv_scenario = "1op"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


init_per_testcase(get_space_ids_test, Config) ->
    unsupport_all_spaces(),
    delete_all_spaces(),
    ?assertEqual([], opw_test_rpc:get_spaces(krakow), ?ATTEMPTS),
    Config;

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(get_space_ids_test, Config) ->
    Config;

end_per_testcase(_, Config) ->
    Config.
