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
-module(spaces_basic_api_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

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


%%%===================================================================
%%% API
%%%===================================================================


get_space_ids_test(Config) ->
    get_space_ids_test_base(Config, []),

    FirstSpaceId = create_and_support_space(Config),
    get_space_ids_test_base(Config, [FirstSpaceId]),

    ManySpacesId = [create_and_support_space(Config) || _ <- lists:seq(1, 5)],
    get_space_ids_test_base(Config, [FirstSpaceId | ManySpacesId]).


%% @private
get_space_ids_test_base(Config, ExpSpaceIds) ->
    [P1] = test_config:get_providers(Config),
    OpPanelNodes = test_config:get_all_op_panel_nodes(Config),
    SortedExpSpaceIds = lists:sort(ExpSpaceIds),

    ?assert(api_test_runner:run_tests(Config, [
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
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, P1))}
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


get_space_details_test(Config) ->
    get_space_details_test_base(Config, <<"posix-space">>, ?STORAGE_NAME, 10000000),
    get_space_details_test_base(Config, <<"null-space">>, <<"IdealNullStorage">>, 20000000).


%% @private
get_space_details_test_base(Config, SpaceName, StorageName, SupportSize) ->
    [P1] = test_config:get_providers(Config),
    OpPanelNodes = test_config:get_all_op_panel_nodes(Config),
    OpWorkerNodes = test_config:get_all_op_worker_nodes(Config),

    StorageId = api_test_utils:get_storage_id_by_name(Config, StorageName),
    SpaceId = create_and_support_space(Config, SpaceName, StorageName, SupportSize),
    ExpResult = get_expected_space_details(Config, SpaceId, SpaceName, StorageId, SupportSize),

    ?assert(api_test_runner:run_tests(Config, [
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
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, P1))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = build_get_space_details_prepare_rest_args_fun(SpaceId),
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertEqual(ExpResult, RespBody)
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


support_space_test(Config) ->
    MemRef = api_test_memory:init(),
    [P1] = test_config:get_providers(Config),
    OpPanelNodes = test_config:get_all_op_panel_nodes(Config),
    OpWorkerNodes = test_config:get_all_op_worker_nodes(Config),

    SpaceName = str_utils:rand_hex(12),
    StorageId = api_test_utils:get_storage_id_by_name(Config, ?STORAGE_NAME),

    ?assert(api_test_runner:run_tests(Config, [
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
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, P1))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },

            setup_fun = build_support_space_setup_fun(MemRef, Config, SpaceName),
            verify_fun = build_support_space_verify_fun(MemRef, Config),

            validate_result_fun = api_test_validate:http_201_created("provider/spaces/", <<"id">>,
                fun(SupportedSpaceId) ->
                    ?assertEqual(api_test_memory:get(MemRef, space_id), SupportedSpaceId)
                end),

            data_spec = build_support_space_data_spec(StorageId, OpWorkerNodes)
        }
    ])).


%% @private
build_support_space_setup_fun(MemRef, Config, SpaceName) ->
    fun() ->
        {SpaceId, Token} = create_space_and_support_token(Config, SpaceName),
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
        correct_values = #{
            <<"size">> => [?SUPPORT_SIZE, 5 * ?SUPPORT_SIZE],
            <<"storageId">> => [StorageId],
            <<"token">> => [token_placeholder]
        },
        bad_values = [
            {<<"size">>, -?SUPPORT_SIZE, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, ?MIN_SUPPORT_SIZE), HostNames)},
            {<<"size">>, ?MIN_SUPPORT_SIZE - 1, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, ?MIN_SUPPORT_SIZE), HostNames)},
            {<<"size">>, <<"Nan">>, ?ERROR_BAD_VALUE_INTEGER(<<"size">>)},
            {<<"storageId">>, <<"inexistientStorageId">>, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"storageId">>), HostNames)},
            {<<"token">>, <<"badToken">>, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN), HostNames)}
        ]
    }.


%% @private
build_support_space_prepare_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        Token = api_test_memory:get(MemRef, support_token),

        RequestMap = case maps:get(<<"token">>, Data, undefined) of
            token_placeholder -> maps:put(<<"token">>, Token, Data);
            _ -> Data
        end,

        #rest_args{
            method = post,
            path = <<"provider/spaces">>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(RequestMap)}
    end.


%% @private
build_support_space_verify_fun(MemRef, Config) ->
    fun
        (expected_success, #api_test_ctx{data = Data}) ->
            SpaceId = api_test_memory:get(MemRef, space_id),
            SpaceName = api_test_memory:get(MemRef, space_name),
            StorageId = maps:get(<<"storageId">>, Data),
            SupportSize = maps:get(<<"size">>, Data),

            ExpectedSpaceDetails = get_expected_space_details(Config, SpaceId, SpaceName, StorageId, SupportSize),
            SpaceDetails = get_space_details_with_rpc(Config, SpaceId),

            ?assertEqual(ExpectedSpaceDetails, SpaceDetails),
            true;
        (expected_failure, _) ->
            SpaceId = api_test_memory:get(MemRef, space_id),
            SpaceIds = op_worker_test_rpc:get_space_ids(Config),

            ?assertNot(lists:member(SpaceId, SpaceIds)),
            true
    end.


modify_space_support_test(Config) ->
    MemRef = api_test_memory:init(),
    [P1] = test_config:get_providers(Config),
    OpPanelNodes = test_config:get_all_op_panel_nodes(Config),
    OpWorkerNodes = test_config:get_all_op_worker_nodes(Config),

    StorageId = api_test_utils:get_storage_id_by_name(Config, ?STORAGE_NAME),
    SpaceName = str_utils:rand_hex(12),
    SupportSize = ?SUPPORT_SIZE,

    ?assert(api_test_runner:run_tests(Config, [
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
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, P1))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },

            setup_fun = build_modify_space_support_setup_fun(MemRef, Config, SpaceName, SupportSize, StorageId),
            prepare_args_fun = build_modify_space_support_prepare_rest_args_fun(MemRef),

            verify_fun = build_modify_space_support_verify_fun(MemRef, Config),
            validate_result_fun = api_test_validate:http_204_no_content(),

            data_spec = build_modify_space_support_data_spec(SupportSize, OpWorkerNodes)
        }
    ])).


%% @private
build_modify_space_support_setup_fun(MemRef, Config, SpaceName, SupportSize, StorageId) ->
    fun() ->
        {SpaceId, Token} = create_space_and_support_token(Config, SpaceName),
        op_worker_test_rpc:support_space(Config, StorageId, Token, SupportSize),
        SpaceDetails = get_space_details_with_rpc(Config, SpaceId),
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
        optional = [<<"size">>],
        correct_values = #{
            <<"size">> => [SupportSize, ?SUPPORT_SIZE div 2, 200 * SupportSize]
        },
        bad_values = [
            % support size is checked on two levels:
            % 1) by Oneprovider, which disallows values lower than the space occupancy (0 in case of an empty space)
            % 2) by Onezone, which has a configurable lower limit
            % because of this, the resulting error may be different depending on the requested size
            {<<"size">>, -?SUPPORT_SIZE, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, 0), HostNames)},
            {<<"size">>, ?MIN_SUPPORT_SIZE - 1, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_TOO_LOW(<<"size">>, ?MIN_SUPPORT_SIZE), HostNames)},
            {<<"size">>, <<"Nan">>, ?ERROR_BAD_VALUE_INTEGER(<<"size">>)},
            {bad_id, <<"inexistentSpaceId">>, ?ERROR_ON_NODES(?ERROR_NOT_FOUND, HostNames)}
        ]
    }.


%% @private
build_modify_space_support_prepare_rest_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        SpaceId = api_test_memory:get(MemRef, space_id),
        {Id, LeftoverData} = api_test_utils:maybe_substitute_bad_id(SpaceId, Data),

        #rest_args{
            method = patch,
            path = <<"provider/spaces/", Id/binary>>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(LeftoverData)
        }
    end.


%% @private
build_modify_space_support_verify_fun(MemRef, Config) ->
    fun
        (expected_success, #api_test_ctx{data = Data}) ->
            SpaceId = api_test_memory:get(MemRef, space_id),
            SpaceName = api_test_memory:get(MemRef, space_name),
            StorageId = api_test_memory:get(MemRef, storage_id),
            SupportSize = api_test_memory:get(MemRef, support_size),

            ExpectedSpaceSupportSize = maps:get(<<"size">>, Data, SupportSize),
            ExpectedSpaceDetails = get_expected_space_details(Config, SpaceId, SpaceName, StorageId, ExpectedSpaceSupportSize),

            % TODO VFS-6780 - currently, supporting providers are calculated asynchronously
            % (effective relation) and the information with updated support size might come with a delay.
            ?assertEqual(ExpectedSpaceDetails, catch get_space_details_with_rpc(Config, SpaceId), ?ATTEMPTS),
            true;
        (expected_failure, _) ->
            SpaceId = api_test_memory:get(MemRef, space_id),
            SpaceDetailsBeforeTest = api_test_memory:get(MemRef, space_details),

            % TODO VFS-6780 - currently, supporting providers are calculated asynchronously
            % (effective relation) and the information with updated support size might come with a delay.
            ?assertEqual(SpaceDetailsBeforeTest, catch get_space_details_with_rpc(Config, SpaceId), ?ATTEMPTS),
            true
    end.


revoke_space_support_test(Config) ->
    MemRef = api_test_memory:init(),
    [P1] = test_config:get_providers(Config),
    OpPanelNodes = test_config:get_all_op_panel_nodes(Config),
    OpWorkerNodes = test_config:get_all_op_worker_nodes(Config),

    ?assert(api_test_runner:run_tests(Config, [
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
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, P1))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },

            setup_fun = build_revoke_space_support_setup_fun(MemRef, Config),
            prepare_args_fun = build_revoke_space_support_prepare_rest_args_fun(MemRef),
            verify_fun = build_revoke_space_support_verify_fun(MemRef, Config),
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
build_revoke_space_support_setup_fun(MemRef, Config) ->
    fun() ->
        SpaceId = create_and_support_space(Config),
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
build_revoke_space_support_verify_fun(MemRef, Config) ->
    fun(ExpectedResult, _) ->
        SpaceId = api_test_memory:get(MemRef, space_id),
        SupportedSpaces = op_worker_test_rpc:get_space_ids(Config),

        case ExpectedResult of
            expected_success -> ?assertNot(lists:member(SpaceId, SupportedSpaces));
            expected_failure -> ?assert(lists:member(SpaceId, SupportedSpaces))
        end,
        true
    end.


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
-spec get_expected_space_details(test_config:config(), binary(), binary(), binary(), binary()) -> map().
get_expected_space_details(Config, SpaceId, SpaceName, StorageId, SupportSize) ->
    [ProviderId] = op_worker_test_rpc:get_space_providers(Config, SpaceId),
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
        <<"spaceOccupancy">> => 0
    }.


%% @private
-spec get_space_details_with_rpc(test_config:config(), binary()) -> map().
get_space_details_with_rpc(Config, SpaceId) ->
    SpaceDoc = op_worker_test_rpc:get_space_document(Config, SpaceId),
    StorageId = op_worker_test_rpc:get_local_storage_id(Config, SpaceId),
    LocalStorages = op_worker_test_rpc:get_local_storage_ids(Config, SpaceId),
    IsImportedStorage = op_worker_test_rpc:is_storage_imported(Config, StorageId),
    AutocleaningStatus = op_worker_test_rpc:get_autocleaning_status(Config, SpaceId),

    #{
        <<"id">> => SpaceId,
        <<"name">> => maps:get(name, SpaceDoc),
        <<"storageId">> => StorageId,
        <<"localStorages">> => LocalStorages,
        <<"supportingProviders">> => maps:get(providers, SpaceDoc),
        <<"importedStorage">> => IsImportedStorage,
        <<"spaceOccupancy">> => maps:get(space_occupancy, AutocleaningStatus)
    }.


%% @private
-spec create_space_and_support_token(test_config:config(), binary()) -> {SpaceId :: binary(), SerializedToken :: binary()}.
create_space_and_support_token(Config, SpaceName) ->
    UserId = oz_worker_test_rpc:create_user(Config),
    SpaceId = oz_worker_test_rpc:create_space(Config, UserId, SpaceName),
    Token = oz_worker_test_rpc:create_space_support_token(Config, UserId, SpaceId),
    {ok, SerializedToken} = tokens:serialize(Token),
    {SpaceId, SerializedToken}.


%% @private
-spec create_and_support_space(test_config:config()) -> SpaceId :: binary().
create_and_support_space(Config) ->
    SpaceName = str_utils:rand_hex(12),
    create_and_support_space(Config, SpaceName, ?STORAGE_NAME, ?SUPPORT_SIZE).


%% @private
-spec create_and_support_space(test_config:config(), binary(), binary(), binary()) -> SpaceId :: binary().
create_and_support_space(Config, SpaceName, StorageName, SupportSize) ->
    {_, SerializedToken} = create_space_and_support_token(Config, SpaceName),
    StorageId = api_test_utils:get_storage_id_by_name(Config, StorageName),
    SpaceId = op_worker_test_rpc:support_space(Config, StorageId, SerializedToken, SupportSize),
    ?assertEqual(true, lists:member(SpaceId, op_worker_test_rpc:get_space_ids(Config)), ?ATTEMPTS),
    SpaceId.


%% @private
-spec unsupport_all_spaces(test_config:config()) -> ok.
unsupport_all_spaces(Config) ->
    SpacesId = op_worker_test_rpc:get_space_ids(Config),
    [op_worker_test_rpc:revoke_space_support(Config, X) || X <- SpacesId].


%% @private
-spec delete_all_spaces(test_config:config()) -> ok.
delete_all_spaces(Config) ->
    SpacesId = oz_worker_test_rpc:get_spaces_ids(Config),
    [oz_worker_test_rpc:delete_space(Config, X) || X <- SpacesId].


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    application:start(ssl),
    hackney:start(),
    test_config:set_many(Config, [
        {add_envs, [oz_worker, oz_worker, [{minimum_space_support_size, ?MIN_SUPPORT_SIZE}]]},
        {set_onenv_scenario, ["1op"]}, % name of yaml file in test_distributed/onenv_scenarios
        {set_posthook, fun onenv_test_utils:prepare_base_test_config/1}
    ]).


end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(ssl).


init_per_testcase(get_space_ids_test, Config) ->
    unsupport_all_spaces(Config),
    delete_all_spaces(Config),
    ?assertEqual([], op_worker_test_rpc:get_space_ids(Config), ?ATTEMPTS),
    Config;

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(get_space_ids_test, Config) ->
    Config;

end_per_testcase(_, Config) ->
    Config.
