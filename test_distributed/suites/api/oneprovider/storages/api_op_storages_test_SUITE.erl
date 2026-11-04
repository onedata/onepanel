%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider storages API (REST): the
%%% operations' answers that don't depend on a storage's type, and those of a
%%% type that need no backend of that type to run against. The details that
%%% do are checked by each type's suite.
%%% @end
%%%-------------------------------------------------------------------
-module(api_op_storages_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").
-include_lib("onenv_ct/include/chart_values.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    get_storage_ids_test/1,
    get_storage_test/1,
    modify_storage_test/1,
    add_storage_with_external_luma_feed_without_url_test/1,
    add_http_storage_without_credentials_test/1,
    modify_http_storage_to_credentials_type_without_credentials_test/1,
    add_storage_with_external_luma_feed_with_empty_url_test/1,
    modify_storage_to_external_luma_feed_with_empty_url_test/1,
    remove_storage_supporting_space_test/1
]).

all() -> [
    get_storage_ids_test,
    get_storage_test,
    modify_storage_test,
    add_storage_with_external_luma_feed_without_url_test,
    add_http_storage_without_credentials_test,
    modify_http_storage_to_credentials_type_without_credentials_test,
    add_storage_with_external_luma_feed_with_empty_url_test,
    % modifies the storage the other tests find first, should the refusal fail
    modify_storage_to_external_luma_feed_with_empty_url_test,
    % removes a storage that supports a space, should the refusal fail
    remove_storage_supporting_space_test
].

-define(INEXISTENT_STORAGE_ID, <<"inexistentStorageId">>).


%%%===================================================================
%%% API
%%%===================================================================


get_storage_ids_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    StoragesIds = opw_test_rpc:get_storages(krakow),

    ExpectedData = #{
        <<"ids">> => StoragesIds
    },

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get storage ids using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                ProviderId
            ),
            prepare_args_fun = fun(_) ->
                #rest_args{method = get, path = <<"provider/storages">>}
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ?assertEqual(ExpectedData, Body)
            end)
        }
    ])).


get_storage_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    [StorageId | _] = opw_test_rpc:get_storages(krakow),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get storage details using /provider/storages/{storage_id} rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(ProviderId),
            prepare_args_fun = fun(#api_test_ctx{data = Data}) ->
                {Id, _} = api_test_utils:maybe_substitute_bad_id(StorageId, Data),
                #rest_args{
                    method = get,
                    path = <<"provider/storages/", Id/binary>>
                }
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertMatch(#{<<"id">> := StorageId}, RespBody)
            end),
            data_spec = #data_spec{
                bad_values = [{bad_id, ?INEXISTENT_STORAGE_ID, ?ERROR_NOT_FOUND}]
            }
        }
    ])).


%% The correct request changes nothing, so that the storage stays as the other
%% tests of the suite find it.
modify_storage_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    [StorageId | _] = opw_test_rpc:get_storages(krakow),
    #{<<"name">> := StorageName, <<"type">> := StorageType} = api_test_utils:describe_storage(krakow, StorageId),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Modify storage using /provider/storages/{id} rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                ProviderId, [?CLUSTER_UPDATE]
            ),
            prepare_args_fun = fun(#api_test_ctx{data = Data}) ->
                {Id, LeftoverData} = api_test_utils:maybe_substitute_bad_id(StorageId, Data),
                Body = case LeftoverData of
                    #{body := BadBody} -> BadBody;
                    _ -> #{StorageName => #{<<"type">> => StorageType}}
                end,
                #rest_args{
                    method = patch,
                    path = <<"provider/storages/", Id/binary>>,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(Body)
                }
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertMatch(#{<<"id">> := StorageId, <<"verificationPassed">> := true}, RespBody)
            end),
            data_spec = #data_spec{
                bad_values = [
                    {bad_id, ?INEXISTENT_STORAGE_ID, ?ERROR_NOT_FOUND},
                    % the body names exactly the storage in the path
                    {body, #{}, ?ERR_MISSING_REQUIRED_VALUE(StorageName)},
                    {body, #{
                        StorageName => #{<<"type">> => StorageType},
                        <<"otherStorage">> => #{<<"type">> => StorageType}
                    }, ?ERR_BAD_VALUE_NOT_ALLOWED(<<"otherStorage">>, [StorageName])},
                    % refused by Onezone, as a refusal and not as a failed
                    % verification of the storage
                    {body, #{
                        StorageName => #{<<"type">> => StorageType, <<"name">> => <<"a">>}
                    }, ?ERR_BAD_VALUE_NAME(undefined)}
                ]
            }
        }
    ])).


%% The REST model has the feed's URL optional; a storage with an external feed
%% and no URL could resolve none of its users' mappings.
add_storage_with_external_luma_feed_without_url_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    StorageName = <<"externalLumaFeedWithoutUrl">>,

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Add storage with an external LUMA feed and no URL using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                ProviderId, [?CLUSTER_UPDATE]
            ),
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = post,
                    path = <<"provider/storages">>,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(#{StorageName => #{
                        <<"type">> => <<"posix">>,
                        <<"mountPoint">> => ?POSIX_MOUNTPOINT,
                        <<"lumaFeed">> => <<"external">>
                    }})
                }
            end,
            validate_result_fun = api_test_validate:http_400_bad_request(fun(RespBody) ->
                ?assertEqual(#{StorageName => ?REST_ERROR(?ERR_MISSING_REQUIRED_VALUE(<<"url">>))}, RespBody),
                ?assertEqual([], storage_ids_named(StorageName))
            end)
        }
    ])).


%% The REST model has the credentials optional, while every use of an HTTP
%% storage requires them unless its credentials type is none. The diagnostics
%% skip HTTP, so no HTTP server is needed for the storage to be added.
add_http_storage_without_credentials_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    StorageName = <<"httpWithoutCredentials">>,

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Add HTTP storage with basic credentials type and no credentials using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                ProviderId, [?CLUSTER_UPDATE]
            ),
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = post,
                    path = <<"provider/storages">>,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(#{StorageName => #{
                        <<"type">> => <<"http">>,
                        <<"endpoint">> => <<"https://example.com">>,
                        <<"credentialsType">> => <<"basic">>,
                        % an HTTP storage can't be written to, and a readonly
                        % storage must be imported
                        <<"readonly">> => true,
                        <<"importedStorage">> => true
                    }})
                }
            end,
            validate_result_fun = api_test_validate:http_400_bad_request(fun(RespBody) ->
                ?assertEqual(
                    #{StorageName => ?REST_ERROR(?ERR_MISSING_REQUIRED_VALUE(<<"credentials">>))},
                    RespBody
                ),
                ?assertEqual([], storage_ids_named(StorageName))
            end)
        }
    ])).


%% As above: a credentials type other than none needs the credentials, which
%% the update must refuse to leave out, and report as a refusal.
modify_http_storage_to_credentials_type_without_credentials_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    StorageName = <<"httpWithNoCredentialsType">>,
    StorageId = panel_test_rpc:add_storage(krakow, #{StorageName => #{
        <<"type">> => <<"http">>,
        <<"endpoint">> => <<"https://example.com">>,
        <<"credentialsType">> => <<"none">>,
        <<"readonly">> => true,
        <<"importedStorage">> => true
    }}),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Modify HTTP storage to basic credentials type with no credentials using /provider/storages/{id} rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                ProviderId, [?CLUSTER_UPDATE]
            ),
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = patch,
                    path = <<"provider/storages/", StorageId/binary>>,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(#{StorageName => #{
                        <<"type">> => <<"http">>,
                        <<"credentialsType">> => <<"basic">>
                    }})
                }
            end,
            validate_result_fun = api_test_validate:http_400_bad_request(fun(RespBody) ->
                ?assertEqual(?REST_ERROR(?ERR_MISSING_REQUIRED_VALUE(<<"credentials">>)), RespBody),
                ?assertMatch(
                    #{<<"credentialsType">> := <<"none">>},
                    api_test_utils:describe_storage(krakow, StorageId)
                )
            end)
        }
    ])),

    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:delete(krakow, <<"/provider/storages/", StorageId/binary>>, #{auth => root})
    ).


%% The REST model takes an empty string as well, which no request to the feed
%% could go to.
add_storage_with_external_luma_feed_with_empty_url_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    StorageName = <<"externalLumaFeedWithEmptyUrl">>,

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Add storage with an external LUMA feed and an empty URL using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                ProviderId, [?CLUSTER_UPDATE]
            ),
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = post,
                    path = <<"provider/storages">>,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(#{StorageName => #{
                        <<"type">> => <<"posix">>,
                        <<"mountPoint">> => ?POSIX_MOUNTPOINT,
                        <<"lumaFeed">> => <<"external">>,
                        <<"lumaFeedUrl">> => <<"">>
                    }})
                }
            end,
            validate_result_fun = api_test_validate:http_400_bad_request(fun(RespBody) ->
                ?assertEqual(#{StorageName => ?REST_ERROR(?ERR_BAD_VALUE_EMPTY(<<"url">>))}, RespBody),
                ?assertEqual([], storage_ids_named(StorageName))
            end)
        }
    ])).


%% As above, for a storage switched to an external feed.
modify_storage_to_external_luma_feed_with_empty_url_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    [StorageId | _] = opw_test_rpc:get_storages(krakow),
    StorageDetails = api_test_utils:describe_storage(krakow, StorageId),
    #{<<"name">> := StorageName, <<"type">> := StorageType} = StorageDetails,

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Modify storage to an external LUMA feed with an empty URL using /provider/storages/{id} rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                ProviderId, [?CLUSTER_UPDATE]
            ),
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = patch,
                    path = <<"provider/storages/", StorageId/binary>>,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(#{StorageName => #{
                        <<"type">> => StorageType,
                        <<"lumaFeed">> => <<"external">>,
                        <<"lumaFeedUrl">> => <<"">>
                    }})
                }
            end,
            validate_result_fun = api_test_validate:http_400_bad_request(fun(RespBody) ->
                ?assertEqual(?REST_ERROR(?ERR_BAD_VALUE_EMPTY(<<"url">>)), RespBody),
                ?assertEqual(StorageDetails, api_test_utils:describe_storage(krakow, StorageId))
            end)
        }
    ])).


%% A storage that supports a space can't be removed, so the correct request is
%% refused too, and the storage stays for the other tests.
remove_storage_supporting_space_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    [SpaceId | _] = opw_test_rpc:get_spaces(krakow),
    [StorageId | _] = opw_test_rpc:get_space_local_storages(krakow, SpaceId),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Remove storage that supports a space using /provider/storages/{id} rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                ProviderId, [?CLUSTER_UPDATE]
            ),
            prepare_args_fun = fun(#api_test_ctx{data = Data}) ->
                {Id, _} = api_test_utils:maybe_substitute_bad_id(StorageId, Data),
                #rest_args{
                    method = delete,
                    path = <<"provider/storages/", Id/binary>>
                }
            end,
            validate_result_fun = api_test_validate:http_400_bad_request(fun(RespBody) ->
                ?assertEqual(?REST_ERROR(?ERR_STORAGE_IN_USE), RespBody),
                ?assert(lists:member(StorageId, opw_test_rpc:get_storages(krakow)))
            end),
            data_spec = #data_spec{
                bad_values = [{bad_id, ?INEXISTENT_STORAGE_ID, ?ERROR_NOT_FOUND}]
            }
        }
    ])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op_s3"
    }).

end_per_suite(_Config) ->
    oct_background:end_per_suite().


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
storage_ids_named(StorageName) ->
    [StorageId || StorageId <- opw_test_rpc:get_storages(krakow),
        maps:get(<<"name">>, api_test_utils:describe_storage(krakow, StorageId)) == StorageName].
