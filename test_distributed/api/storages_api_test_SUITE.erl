%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% test SUITE for testing storage utils
%%% @end
%%%-------------------------------------------------------------------
-module(storages_api_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    get_storages_ids/1, add_s3_storage/1, get_s3_storage/1, delete_s3_storage/1
]).

-define(S3_Storage_Spec,#{
    <<"s3Storage-1">> => #{
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

    {ok, StorageIds} = rpc:call(hd(OpWorkerNodes), provider_logic, get_storage_ids, []),

    ExpectedData = #{
        <<"ids">> => StorageIds
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
            prepare_args_fun = fun(_) -> #rest_args{method = get, path = <<"provider/storages">>}
            end,
            validate_result_fun = fun(_, {ok, RespCode, _, RespBody}) ->
                ?assertEqual({?HTTP_200_OK, ExpectedData}, {RespCode, RespBody})
            end
        }
    ])).


add_s3_storage(Config) ->
    [P1] = test_config:get_providers(Config),
    OpWorkerNodes = test_config:get_custom(Config, [provider_nodes, P1]),
    OpPanelNodes = test_config:get_custom(Config, [provider_panels, P1]),

    {ok, StorageIdsBeforeAdd} = rpc:call(hd(OpWorkerNodes), provider_logic, get_storage_ids, []),

    RequestBody = ?S3_Storage_Spec,

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Add s3 storage using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root
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
            validate_result_fun = fun(_, {ok, RespCode,_,_}) ->
                {ok, StorageIdsAfterAdd} = rpc:call(hd(OpWorkerNodes), provider_logic, get_storage_ids, []),
                [NewStorageID|_] = lists:subtract(StorageIdsAfterAdd, StorageIdsBeforeAdd),

                {ok, StorageDetails} = rpc:call(hd(OpWorkerNodes), storage, describe, [NewStorageID]),
                ?assertEqual({204, true}, {RespCode,assert_add_request_match_resp(RequestBody,StorageDetails)})
            end
        }
    ])).


%% @private
assert_add_request_match_resp(RequestBody, StorageDetails) ->
    [Name|_] = maps:keys(RequestBody),
    RequestMap = maps:get(Name, RequestBody),
    F = fun(K,_,Acc) -> (maps:get(K,StorageDetails)==maps:get(K,RequestMap)) and Acc
    end,
    maps:fold(F,true, RequestMap) and (maps:get(<<"name">>,StorageDetails)==Name).


get_s3_storage(Config) ->
    [P1] = test_config:get_providers(Config),
    OpPanelNodes = test_config:get_custom(Config, [provider_panels, P1]),
    OpWorkerNodes = test_config:get_custom(Config, [provider_nodes, P1]),

    [StorageName|_] = maps:keys(?S3_Storage_Spec),
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
                {ok, StorageDetails} = rpc:call(hd(OpWorkerNodes), storage, describe, [StorageId]),
                StorageDetailsBinary = make_all_values_in_map_binary(StorageDetails),
                RespBodyBinary = make_all_values_in_map_binary(RespBody),
                ?assertEqual({?HTTP_200_OK, StorageDetailsBinary}, {RespCode, RespBodyBinary})
            end
        }
    ])).


%% @private
get_storage_id_by_name(Config, StorageName)->
    [P1] = test_config:get_providers(Config),
    OpWorkerNodes = test_config:get_custom(Config, [provider_nodes, P1]),

    {ok, StorageIds} = rpc:call(hd(OpWorkerNodes), provider_logic, get_storage_ids, []),
    Storages = [element(2,rpc:call(hd(OpWorkerNodes), storage, describe, [X]))||X <- StorageIds],

    [StorageId|_] = [maps:get(<<"id">>,X)||X <- Storages, (maps:get(<<"name">>,X)==StorageName)],
    StorageId.


% There are some diffs between rpc and http response.
% Not all values in http or rpc response are binary - type, for example:
% rpc: <<"signatureVersion">> => <<"4">>
% http: <<"signatureVersion">> => 4
%% @private
make_all_values_in_map_binary(Map) ->
    Fun = fun(K, V) ->
        case {is_atom(V), is_binary(V), is_integer(V), is_map(V)} of
            {true, false, false, false} -> atom_to_binary(V, latin1);
            {false, true, false, false} -> V;
            {false, false, true, false} -> integer_to_binary(V);
            {false, false, false, true} -> make_all_values_in_map_binary(V)
        end
    end,
    maps:map(Fun, Map).


delete_s3_storage(Config)->
    [P1] = test_config:get_providers(Config),
    OpPanelNodes = test_config:get_custom(Config, [provider_panels, P1]),
    OpWorkerNodes = test_config:get_custom(Config, [provider_nodes, P1]),

    [StorageName|_] = maps:keys(?S3_Storage_Spec),
    StorageId = get_storage_id_by_name(Config, StorageName),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Delete s3 storage using /provider/storages/{storage_id} rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root
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
                {ok, StorageIdsAfterDelete} = rpc:call(hd(OpWorkerNodes), provider_logic, get_storage_ids, []),
                ?assertEqual({?HTTP_204_NO_CONTENT, false}, {RespCode, lists:member(StorageId, StorageIdsAfterDelete)})
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
