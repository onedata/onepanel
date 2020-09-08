%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider storages API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(storages_api_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    get_storages_ids/1, add_s3_storage/1, get_s3_storage/1, delete_s3_storage/1
]).

-define(S3_STORAGE_NAME, <<"s3Storage-1">>).
-define(S3_STORAGE_SPEC,#{
    ?S3_STORAGE_NAME => #{
        <<"type">> => <<"s3">>,
        <<"bucketName">> => <<"bucket2.iam.example.com">>,
        <<"hostname">> => <<"s3.amazonaws.com:80/">>,
        <<"skipStorageDetection">> => <<"true">>
    }
} ).

all() -> [
    get_storages_ids, add_s3_storage, get_s3_storage, delete_s3_storage
].


%%%===================================================================
%%% API
%%%===================================================================


get_storages_ids(Config) ->
    [P1] = test_config:get_providers(Config),
    OpWorkerNodes = test_config:get_custom(Config, [provider_nodes, P1]),
    OpPanelNodes = test_config:get_custom(Config, [provider_panels, P1]),

    StoragesIds = rpc_api:get_storages_ids(OpWorkerNodes),

    ExpectedData = #{
        <<"ids">> => StoragesIds
    },

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get storage ids using /provider/storages rest endpoint">>,
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
                #rest_args{method = get, path = <<"provider/storages">>}
            end,
            validate_result_fun = fun(_, {ok, RespCode, _, RespBody}) ->
                ?assertEqual(?HTTP_200_OK, RespCode),
                ?assertEqual(ExpectedData, RespBody)
            end
        }
    ])).


add_s3_storage(Config) ->
    % todo: VFS-6717 delete s3 storage after test
    [P1] = test_config:get_providers(Config),
    OpWorkerNodes = test_config:get_custom(Config, [provider_nodes, P1]),
    OpPanelNodes = test_config:get_custom(Config, [provider_panels, P1]),

    StorageIdsBeforeAdd = rpc_api:get_storages_ids(OpWorkerNodes),

    RequestBody = ?S3_STORAGE_SPEC,

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Add s3 storage using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root
                    % todo: VFS-6717 uncomment when there will be no dependencies in test
                    %{member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, P1))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) -> #rest_args{
                method = post,
                path = <<"provider/storages">>,
                headers = #{<<"content-type">> => <<"application/json">>},
                body = json_utils:encode(RequestBody)}
            end,
            validate_result_fun = fun(_, {ok, RespCode, _, _}) ->
                StorageIdsAfterAdd = rpc_api:get_storages_ids(OpWorkerNodes),

                % todo: VFS-6716 get NewStorageId from HTTP response
                [NewStorageID | _] = lists:subtract(StorageIdsAfterAdd, StorageIdsBeforeAdd),
                StorageDetails = rpc_api:get_storage_details(OpWorkerNodes, NewStorageID),

                ?assertEqual(204, RespCode),
                ?assert(add_request_match_response(RequestBody, StorageDetails))
            end
        }
    ])).


%% @private
add_request_match_response(RequestBody, StorageDetails) ->
    RequestMap = maps:get(?S3_STORAGE_NAME, RequestBody),
    maps_utils:is_submap(RequestMap, StorageDetails) and (maps:get(<<"name">>, StorageDetails) =:= ?S3_STORAGE_NAME).


get_s3_storage(Config) ->
    % todo: VFS-6717 add s3 storage before, and delete after test
    [P1] = test_config:get_providers(Config),
    OpPanelNodes = test_config:get_custom(Config, [provider_panels, P1]),
    OpWorkerNodes = test_config:get_custom(Config, [provider_nodes, P1]),

    [StorageName|_] = maps:keys(?S3_STORAGE_SPEC),
    StorageId = get_storage_id_by_name(Config, StorageName),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get s3 storage details using /provider/storages/{storage_id} rest endpoint">>,
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
                #rest_args{
                    method = get,
                    path = <<"provider/storages/", StorageId/binary>>}
            end,
            validate_result_fun = fun(_, {ok, RespCode, _, RespBody}) ->
                StorageDetails = rpc_api:get_storage_details(OpWorkerNodes, StorageId),
                StorageDetailsBinary = onepanel_utils:convert_recursive(StorageDetails, {map, binary}),
                RespBodyBinary = onepanel_utils:convert_recursive(RespBody, {map, binary}),

                ?assertEqual(?HTTP_200_OK, RespCode),
                ?assertEqual(StorageDetailsBinary, RespBodyBinary)
            end
        }
    ])).


%% @private
get_storage_id_by_name(Config, StorageName)->
    [P1] = test_config:get_providers(Config),
    OpWorkerNodes = test_config:get_custom(Config, [provider_nodes, P1]),

    StorageIds = rpc_api:get_storages_ids(OpWorkerNodes),
    Storages = [rpc_api:get_storage_details(OpWorkerNodes, X) || X <- StorageIds],

    [StorageId | _] = [maps:get(<<"id">>, X) || X <- Storages, (maps:get(<<"name">>, X) == StorageName)],
    StorageId.


delete_s3_storage(Config)->
    % todo: VFS-6717 add s3 storage before test
    [P1] = test_config:get_providers(Config),
    OpPanelNodes = test_config:get_custom(Config, [provider_panels, P1]),
    OpWorkerNodes = test_config:get_custom(Config, [provider_nodes, P1]),

    StorageId = get_storage_id_by_name(Config, ?S3_STORAGE_NAME),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Delete s3 storage using /provider/storages/{storage_id} rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root
                    % todo: VFS-6717 uncomment when there will be no dependencies in test
                    %{member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, P1))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = delete,
                    path = <<"provider/storages/", StorageId/binary>>}
            end,
            validate_result_fun = fun(_, {ok, RespCode, _, _}) ->
                StorageIdsAfterDelete = rpc_api:get_storages_ids(OpWorkerNodes),
                ?assertEqual(?HTTP_204_NO_CONTENT, RespCode),
                ?assertNot(lists:member(StorageId, StorageIdsAfterDelete))
            end
        }
    ])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    Posthook = fun(NewConfig) ->
        application:start(ssl),
        hackney:start(),
        onenv_test_utils:prepare_base_test_config(NewConfig)
    end,
    test_config:set_many(Config, [
        {set_onenv_scenario, ["1op"]}, % name of yaml file in test_distributed/onenv_scenarios
        {set_posthook, Posthook}
    ]).


end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(ssl),
    ok.
