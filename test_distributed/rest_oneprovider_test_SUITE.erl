%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of 'rest_oneprovider' module.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_oneprovider_test_SUITE).
-author("Krzysztof Trzepla").

-include("authentication.hrl").
-include("modules/models.hrl").
-include("onepanel_test_utils.hrl").
-include("onepanel_test_rest.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/http/codes.hrl").


%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2]).

%% tests
-export([
    method_should_return_unauthorized_error/1,
    method_should_return_forbidden_error/1,
    delete_should_return_forbidden_error/1,
    method_should_return_conflict_error/1,
    method_should_return_service_unavailable_error/1,
    get_should_return_provider_details/1,
    get_should_return_cluster_ips/1,
    post_should_register_provider/1,
    patch_should_modify_provider_details/1,
    patch_should_modify_provider_ips/1,
    delete_should_unregister_provider/1,
    get_should_return_supported_spaces/1,
    post_should_create_or_support_space/1,
    patch_should_modify_space_support/1,
    get_should_return_space_details/1,
    delete_should_revoke_space_support/1,
    get_should_return_storages/1,
    get_should_return_storage/1,
    post_should_add_storage/1,
    patch_should_modify_storage_update/1,
    patch_should_update_storage/1,
    get_should_return_autocleaning_reports/1,
    get_should_return_autocleaning_report/1,
    get_should_return_autocleaning_status/1,
    get_should_return_autocleaning_configuration/1,
    get_should_return_file_popularity_configuration/1,
    patch_should_update_file_popularity/1,
    patch_should_update_auto_cleaning/1,
    patch_with_incomplete_config_should_update_auto_cleaning/1,
    patch_with_incorrect_config_should_fail/1,
    patch_should_invalidate_luma_cache/1,
    patch_should_update_transfers_mock/1,
    get_should_return_transfers_mock/1]).

all() ->
    ?ALL([
        method_should_return_unauthorized_error,
        method_should_return_forbidden_error,
        delete_should_return_forbidden_error,
        method_should_return_conflict_error,
        method_should_return_service_unavailable_error,
        get_should_return_provider_details,
        get_should_return_cluster_ips,
        post_should_register_provider,
        patch_should_modify_provider_details,
        patch_should_modify_provider_ips,
        delete_should_unregister_provider,
        get_should_return_supported_spaces,
        post_should_create_or_support_space,
        patch_should_modify_space_support,
        get_should_return_space_details,
        delete_should_revoke_space_support,
        get_should_return_storages,
        get_should_return_storage,
        post_should_add_storage,
        patch_should_modify_storage_update,
        patch_should_update_storage,
        get_should_return_autocleaning_reports,
        get_should_return_autocleaning_report,
        get_should_return_autocleaning_status,
        get_should_return_autocleaning_configuration,
        get_should_return_file_popularity_configuration,
        patch_should_update_file_popularity,
        patch_should_update_auto_cleaning,
        patch_with_incomplete_config_should_update_auto_cleaning,
        patch_with_incorrect_config_should_fail,
        patch_should_invalidate_luma_cache,
        patch_should_update_transfers_mock,
        get_should_return_transfers_mock
    ]).

-define(TIMEOUT, timer:seconds(5)).

-define(COMMON_ENDPOINTS_WITH_METHODS, [
    {<<"/provider">>, get},
    {<<"/provider">>, post},
    {<<"/provider">>, patch},
    {<<"/provider">>, delete},
    {<<"/provider/spaces">>, get},
    {<<"/provider/spaces">>, post},
    {<<"/provider/spaces/someSpaceId">>, get},
    {<<"/provider/spaces/someSpaceId">>, patch},
    {<<"/provider/spaces/someSpaceId">>, delete},
    {<<"/provider/spaces/someSpaceId/sync">>, get},

    {<<"/provider/spaces/someSpaceId/auto-cleaning/configuration">>, get},
    {<<"/provider/spaces/someSpaceId/auto-cleaning/configuration">>, patch},
    {<<"/provider/spaces/someSpaceId/auto-cleaning/reports">>, get},
    {<<"/provider/spaces/someSpaceId/auto-cleaning/reports/someReportId">>, get},
    {<<"/provider/spaces/someSpaceId/auto-cleaning/start">>, post},
    {<<"/provider/spaces/someSpaceId/auto-cleaning/status">>, get},

    {<<"/provider/storages">>, get},
    {<<"/provider/storages">>, post},
    {<<"/provider/storages/someStorageId">>, get},
    {<<"/provider/storages/someStorageId">>, patch},
    {<<"/provider/storages/someStorageId/invalidate_luma">>, patch},

    {<<"/provider/cluster_ips">>, get},
    {<<"/provider/cluster_ips">>, patch},

    {<<"/provider/debug/transfers_mock">>, get},
    {<<"/provider/debug/transfers_mock">>, patch}
]).

-define(REGISTER_REQUEST_JSON, #{
    <<"name">> => <<"someName">>,
    <<"subdomainDelegation">> => false,
    <<"domain">> => <<"somedomain">>,
    <<"adminEmail">> => <<"admin@onedata.org">>,
    <<"geoLongitude">> => 10.0,
    <<"geoLatitude">> => 20.0,
    <<"token">> => <<"someToken">>
}).

-define(PROVIDER_DETAILS_JSON, #{
    <<"domain">> => <<"someDomain">>,
    <<"geoLatitude">> => 10.0,
    <<"geoLongitude">> => 20.0,
    <<"id">> => <<"someId">>,
    <<"name">> => <<"someName">>,
    <<"subdomainDelegation">> => false
}).

-define(SPACE_JSON, #{<<"id">> => <<"someId1">>}).

-define(SPACES_JSON, #{
    <<"ids">> => [<<"someId1">>, <<"someId2">>, <<"someId3">>]
}).

-define(CLUSTER_IPS_JSON(_Hosts), #{
    <<"hosts">> =>
    lists:foldl(fun(_Host, _Acc) ->
        maps:put(list_to_binary(_Host), <<"1.2.3.4">>, _Acc)
    end, #{}, _Hosts),
    <<"isConfigured">> => false
}).


-define(STORAGE_JSON, #{
    <<"id">> => <<"somePosixId">>,
    <<"mountPoint">> => <<"someMountPoint">>,
    <<"name">> => <<"somePosix">>,
    <<"type">> => <<"posix">>
}).

-define(STORAGE_UPDATE_JSON, #{
    <<"somePosix">> =>
    #{
        <<"type">> => <<"posix">>,
        <<"timeout">> => 10000,
        <<"mountPoint">> => <<"someNewMountPoint">>
    }
}).

-define(STORAGES_JSON, #{
    <<"someCeph">> => #{
        <<"type">> => <<"ceph">>,
        <<"username">> => <<"someName">>,
        <<"key">> => <<"someKey">>,
        <<"monitorHostname">> => <<"someHostname">>,
        <<"poolName">> => <<"someName">>,
        <<"clusterName">> => <<"someName">>,
        <<"timeout">> => 5000
    },
    <<"someS3">> => #{
        <<"type">> => <<"s3">>,
        <<"hostname">> => <<"someHostname">>,
        <<"bucketName">> => <<"someName">>,
        <<"accessKey">> => <<"someKey">>,
        <<"secretKey">> => <<"someKey">>,
        <<"blockSize">> => 1024
    }
}).

-define(STORAGE_IMPORT_DETAILS_JSON, #{
    <<"someIntegerDetail">> => 1,
    <<"strategy">> => <<"someStrategy">>
}).

-define(STORAGE_UPDATE_DETAILS_JSON, #{
    <<"someBooleanDetail">> => false,
    <<"someIntegerDetail">> => 2,
    <<"strategy">> => <<"someStrategy">>
}).

-define(SPACE_DETAILS_JSON, #{
    <<"id">> => <<"someId">>,
    <<"name">> => <<"someName">>,
    <<"storageId">> => <<"someId">>,
    <<"storageImport">> => ?STORAGE_IMPORT_DETAILS_JSON,
    <<"storageUpdate">> => ?STORAGE_UPDATE_DETAILS_JSON,
    <<"supportingProviders">> => #{
        <<"someId1">> => 1024,
        <<"someId2">> => 2048,
        <<"someId3">> => 4096
    }
}).

-define(AUTO_CLEANING_REPORT1, #{
    <<"id">> => <<"id1">>,
    <<"index">> => <<"index1">>,
    <<"bytesToRelease">> => 125,
    <<"filesNumber">> => 10,
    <<"releasedBytes">> => 100,
    <<"startedAt">> => <<"2004-02-12T15:19:21.423Z">>,
    <<"stoppedAt">> => <<"2004-02-12T15:29:11.598Z">>
}).

-define(AUTO_CLEANING_REPORT2, #{
    <<"id">> => <<"id2">>,
    <<"index">> => <<"index2">>,
    <<"bytesToRelease">> => 1313125,
    <<"filesNumber">> => 1056,
    <<"releasedBytes">> => 1001234,
    <<"startedAt">> => <<"2014-07-16T15:19:21.423Z">>,
    <<"stoppedAt">> => null
}).

-define(AUTO_CLEANING_REPORTS, [?AUTO_CLEANING_REPORT1, ?AUTO_CLEANING_REPORT2]).

-define(AUTO_CLEANING_STATUS, #{
    <<"inProgress">> => false,
    <<"usedSpace">> => 1234123
}).

-define(FILE_POPULARITY_CONFIG, #{
    <<"enabled">> => true,
    <<"lastOpenHourWeight">> => 1.0,
    <<"avgOpenCountPerDayWeight">> => 1.0,
    <<"maxAvgOpenCountPerDay">> => 100
}).

-define(AUTO_CLEANING_CONFIG, #{
    <<"enabled">> => true,
    <<"target">> => 32,
    <<"threshold">> => 64,
    <<"rules">> => #{
        <<"enabled">> => true,
        <<"maxOpenCount">> => #{<<"enabled">> => true, <<"value">> => 1},
        <<"minHoursSinceLastOpen">> => #{<<"enabled">> => true, <<"value">> => 2},
        <<"minFileSize">> => #{<<"enabled">> => true, <<"value">> => 3},
        <<"maxFileSize">> => #{<<"enabled">> => true, <<"value">> => 4},
        <<"maxHourlyMovingAverage">> => #{<<"enabled">> => true, <<"value">> => 5},
        <<"maxDailyMovingAverage">> => #{<<"enabled">> => true, <<"value">> => 6},
        <<"maxMonthlyMovingAverage">> => #{<<"enabled">> => true, <<"value">> => 7}
    }
}).

-define(INCOMPLETE_AUTO_CLEANING_CONFIG, #{
    <<"enabled">> => true,
    <<"target">> => 32
}).

-define(INCORRECT_AUTO_CLEANING_CONFIG, #{
    <<"enabled">> => true,
    <<"target">> => <<"WRONG TYPE">>,
    <<"threshold">> => wrong_type
}).

-define(TRANSFERS_MOCK_CONFIG, #{
    <<"transfersMock">> => true
}).

-define(run(Config, Function), Function(hd(?config(oneprovider_hosts, Config)))).

%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_unauthorized_error(Config) ->
    ?run(Config, fun(Host) ->
        lists:foreach(fun({Endpoint, Method}) ->
            lists:foreach(fun(Auth) ->
                ?assertMatch({ok, ?HTTP_401_UNAUTHORIZED, _, _}, onepanel_test_rest:auth_request(
                    Host, Endpoint, Method, Auth
                ))
            end, ?INCORRECT_AUTHS() ++ ?NONE_AUTHS())
        end, ?COMMON_ENDPOINTS_WITH_METHODS)
    end).


method_should_return_forbidden_error(Config) ->
    ?run(Config, fun(Host) ->
        lists:foreach(fun({Endpoint, Method}) ->
            % highest rights which still should not grant access to these endpoints
            Auths = case {Endpoint, Method} of
                {<<"/provider">>, delete} ->
                    ?OZ_AUTHS(Host, privileges:cluster_admin() -- [?CLUSTER_DELETE]);
                _ ->
                    ?OZ_AUTHS(Host, privileges:cluster_admin() -- [?CLUSTER_UPDATE])
            end,

            ?assertMatch({ok, ?HTTP_403_FORBIDDEN, _, _}, onepanel_test_rest:auth_request(
                Host, Endpoint, Method, Auths
            ))
        end, [{E, M} || {E, M} <- ?COMMON_ENDPOINTS_WITH_METHODS, M /= get])
    end).


delete_should_return_forbidden_error(Config) ->
    ?run(Config, fun(Host) ->
        % verify that user with cluster_update but without cluster_delete
        % cannot deregister the Oneprovider. Less privileged users
        % are check in method_should_return_forbidden_error/1
        Privileges = privileges:cluster_admin() -- [?CLUSTER_DELETE],
        ?assertMatch({ok, ?HTTP_403_FORBIDDEN, _, _}, onepanel_test_rest:auth_request(
            Host, "/provider", delete, ?OZ_AUTHS(Host, Privileges)
        ))
    end).


method_should_return_conflict_error(Config) ->
    ?run(Config, fun(Host) ->
        % provider is mocked as already registered
        ?assertMatch({ok, ?HTTP_409_CONFLICT, _, _}, onepanel_test_rest:auth_request(
            Host, "/provider", post, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
            ?REGISTER_REQUEST_JSON
        ))
    end).


method_should_return_service_unavailable_error(Config) ->
    ?run(Config, fun(Host) ->
        lists:foreach(fun({Endpoint, Method}) ->
            ?assertMatch({ok, ?HTTP_503_SERVICE_UNAVAILABLE, _, _}, onepanel_test_rest:auth_request(
                Host, Endpoint, Method, ?ALL_AUTHS(Host)
            ))
        end, lists:subtract(
            ?COMMON_ENDPOINTS_WITH_METHODS, [
                {<<"/provider/cluster_ips">>, get},
                {<<"/provider">>, get},
                {<<"/provider/debug/transfers_mock">>, get},
                {<<"/provider/debug/transfers_mock">>, patch}
            ])
        )
    end).


get_should_return_provider_details(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?PROVIDER_DETAILS_JSON)
    end).


get_should_return_cluster_ips(Config) ->
    Nodes = ?config(oneprovider_nodes, Config),
    Hosts = lists:map(fun hosts:from_node/1, Nodes),
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/cluster_ips">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?CLUSTER_IPS_JSON(Hosts))
    end).


post_should_register_provider(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider">>, post,
            ?ROOT_AUTHS(Host), ?REGISTER_REQUEST_JSON
        )),
        ?assertReceivedMatch({service, oneprovider, register, #{
            oneprovider_token := <<"someToken">>,
            oneprovider_name := <<"someName">>,
            oneprovider_domain := <<"somedomain">>,
            oneprovider_geo_latitude := 20.0,
            oneprovider_geo_longitude := 10.0
        }}, ?TIMEOUT)
    end).


patch_should_modify_provider_details(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider">>, patch,
            ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), #{
                <<"name">> => <<"someName">>,
                <<"domain">> => <<"someDomain">>,
                <<"geoLongitude">> => 10.0,
                <<"geoLatitude">> => 20.0
            }
        )),
        ?assertReceivedMatch({service, oneprovider, modify_details, #{
            oneprovider_name := <<"someName">>,
            oneprovider_domain := <<"someDomain">>,
            oneprovider_geo_latitude := 20.0,
            oneprovider_geo_longitude := 10.0
        }}, ?TIMEOUT)
    end).


patch_should_modify_provider_ips(Config) ->
    % There is one node in test environment
    NewIP = <<"1.2.3.4">>,
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/cluster_ips">>, patch,
            ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), #{
                hosts => #{
                    list_to_binary(Host) => NewIP
                }
            }
        )),
        ?assertReceivedMatch({service, oneprovider, set_cluster_ips, #{
            cluster_ips := #{
                Host := NewIP
            }
        }}, ?TIMEOUT)
    end).


delete_should_unregister_provider(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider">>, delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_DELETE])
        )),
        ?assertReceivedMatch({service, oneprovider, unregister, #{}}, ?TIMEOUT)
    end).


get_should_return_supported_spaces(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces">>, get, ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACES_JSON)
    end).


post_should_create_or_support_space(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces">>, post,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), #{
                    <<"token">> => <<"someToken">>,
                    <<"size">> => 1024,
                    <<"storageId">> => <<"someId">>,
                    <<"storageImport">> => ?STORAGE_IMPORT_DETAILS_JSON,
                    <<"storageUpdate">> => ?STORAGE_UPDATE_DETAILS_JSON
                }
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACE_JSON)
    end).


patch_should_modify_space_support(Config) ->
    NewSize = 99000000,
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId1">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                #{<<"size">> => NewSize}
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACE_JSON)
    end).


get_should_return_space_details(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACE_DETAILS_JSON)
    end).


delete_should_revoke_space_support(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/spaces/someId">>, delete,
            ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, oneprovider, revoke_space_support,
            #{id := <<"someId">>}
        }, ?TIMEOUT)
    end).


get_should_return_storages(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body_fields(JsonBody, [<<"ids">>])
    end).


get_should_return_storage(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages/somePosixId">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?STORAGE_JSON)
    end).


post_should_add_storage(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages">>,
            post, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
            ?STORAGES_JSON
        )),
        ?assertReceivedMatch({service, op_worker, add_storages, #{
            storages := #{
                <<"someCeph">> := #{
                    type := <<"ceph">>,
                    clusterName := <<"someName">>,
                    key := <<"someKey">>,
                    monitorHostname := <<"someHostname">>,
                    poolName := <<"someName">>,
                    username := <<"someName">>,
                    timeout := 5000
                },
                <<"someS3">> := #{
                    type := <<"s3">>,
                    accessKey := <<"someKey">>,
                    bucketName := <<"someName">>,
                    hostname := <<"someHostname">>,
                    secretKey := <<"someKey">>,
                    blockSize := 1024
                }
            }
        }}, ?TIMEOUT)
    end).


patch_should_modify_storage_update(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId1">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), #{
                    <<"storageImport">> => ?STORAGE_IMPORT_DETAILS_JSON,
                    <<"storageUpdate">> => ?STORAGE_UPDATE_DETAILS_JSON
                }
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACE_JSON)
    end).


patch_should_update_storage(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages/somePosixId">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), ?STORAGE_UPDATE_JSON
            )
        ),
        ?assertReceivedMatch({service, op_worker, update_storage, #{
            id := <<"somePosixId">>,
            storage := #{timeout := 10000, mountPoint := <<"someNewMountPoint">>}
        }}, ?TIMEOUT)
    end).


get_should_return_autocleaning_reports(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/reports">>,
                get, ?OZ_OR_ROOT_AUTHS(Host, []), [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?AUTO_CLEANING_REPORTS)
    end).

get_should_return_autocleaning_report(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/reports/someReportId">>,
                get, ?OZ_OR_ROOT_AUTHS(Host, []), [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?AUTO_CLEANING_REPORT1)
    end).

get_should_return_autocleaning_status(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/status">>,
                get, ?OZ_OR_ROOT_AUTHS(Host, []), [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?AUTO_CLEANING_STATUS)
    end).

get_should_return_autocleaning_configuration(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/configuration">>,
                get, ?OZ_OR_ROOT_AUTHS(Host, []), [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?AUTO_CLEANING_CONFIG)
    end).

get_should_return_file_popularity_configuration(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/file-popularity/configuration">>,
                get, ?OZ_OR_ROOT_AUTHS(Host, []), [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?FILE_POPULARITY_CONFIG)
    end).

patch_should_update_file_popularity(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId/file-popularity/configuration">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                ?FILE_POPULARITY_CONFIG
            )
        ),
        ?assertReceivedMatch({service, oneprovider, configure_file_popularity, #{
            space_id := <<"someId">>,
            enabled := true,
            last_open_hour_weight := 1.0,
            avg_open_count_per_day_weight := 1.0,
            max_avg_open_count_per_day := 100.0
        }}, ?TIMEOUT)
    end).

patch_should_update_auto_cleaning(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId/auto-cleaning/configuration">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), ?AUTO_CLEANING_CONFIG
            )
        ),
        ?assertReceivedMatch({service, oneprovider, configure_auto_cleaning, #{
            space_id := <<"someId">>,
            enabled := true,
            target := 32,
            threshold := 64,
            rules := #{
                enabled := true,
                max_open_count := #{enabled := true, value := 1},
                min_hours_since_last_open := #{enabled := true, value := 2},
                min_file_size := #{enabled := true, value := 3},
                max_file_size := #{enabled := true, value := 4},
                max_hourly_moving_average := #{enabled := true, value := 5},
                max_daily_moving_average := #{enabled := true, value := 6},
                max_monthly_moving_average := #{enabled := true, value := 7}
            }
        }}, ?TIMEOUT)
    end).

patch_with_incomplete_config_should_update_auto_cleaning(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId/auto-cleaning/configuration">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                ?INCOMPLETE_AUTO_CLEANING_CONFIG
            )
        ),
        ?assertReceivedMatch({service, oneprovider, configure_auto_cleaning, #{
            space_id := <<"someId">>,
            enabled := true,
            target := 32
        }}, ?TIMEOUT)
    end).

patch_with_incorrect_config_should_fail(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_400_BAD_REQUEST, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId/auto-cleaning/configuration">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), ?INCORRECT_AUTO_CLEANING_CONFIG
            )
        )
    end).

patch_should_invalidate_luma_cache(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages/someId/invalidate_luma">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), #{}
            )
        ),
        ?assertReceivedMatch({service, op_worker, invalidate_luma_cache, #{
            id := <<"someId">>
        }}, ?TIMEOUT)
    end).

patch_should_update_transfers_mock(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/debug/transfers_mock">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), ?TRANSFERS_MOCK_CONFIG
            )
        ),
        ?assertReceivedMatch({service, op_worker, set_transfers_mock, #{
            transfers_mock := true
        }}, ?TIMEOUT)
    end).

get_should_return_transfers_mock(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/debug/transfers_mock">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?TRANSFERS_MOCK_CONFIG)
    end).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) ->
        NewConfig2 = onepanel_test_utils:init(NewConfig),
        onepanel_test_rest:set_default_passphrase(NewConfig2),
        NewConfig2
    end,
    [{?LOAD_MODULES, [onepanel_test_rest]}, {?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(method_should_return_service_unavailable_error, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_expect(Nodes, service, all_healthy, fun() -> false end),
    NewConfig;

init_per_testcase(get_should_return_provider_details, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, get_details, {
            [{'node@host1', ?PROVIDER_DETAILS_JSON}], []
        }},
        {task_finished, {service, action, ok}}
    ] end),
    NewConfig;

init_per_testcase(get_should_return_cluster_ips, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    Hosts = lists:map(fun hosts:from_node/1, Nodes),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, format_cluster_ips, {
            [{'node@host1', ?CLUSTER_IPS_JSON(Hosts)}], []
        }},
        {task_finished, {service, action, ok}}
    ] end),
    NewConfig;


init_per_testcase(post_should_register_provider, Config) ->
    NewConfig = init_per_testcase(default, Config),

    Nodes = ?config(oneprovider_nodes, Config),
    Hosts = ?config(oneprovider_hosts, Config),
    test_utils:mock_expect(Nodes, service, get, fun
        (oneprovider) -> {ok, #service{ctx = #{registered => false}}};
        (op_worker) -> {ok, #service{hosts = Hosts}}
    end),
    NewConfig;


init_per_testcase(get_should_return_storage, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, NewConfig),
    test_utils:mock_new(Nodes, rest_oneprovider),
    test_utils:mock_expect(Nodes, rest_oneprovider, exists_resource, fun(Req, _) ->
        {true, Req}
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_op_worker, get_storages, {[{'node@host1', keys_to_atoms(?STORAGE_JSON)}], []}},
        {task_finished, {service, action, ok}}
    ] end),
    NewConfig;


init_per_testcase(get_should_return_storages, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, NewConfig),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_op_worker, get_storages, {[{'node@host1', [
            {<<"ids">>, [<<"id1">>, <<"id2">>, <<"id3">>]}
        ]}], []}},
        {task_finished, {service, action, ok}}
    ] end),
    NewConfig;

init_per_testcase(get_should_return_supported_spaces, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, get_spaces, {
            [{'node@host1', ?SPACES_JSON}], []
        }},
        {task_finished, {service, action, ok}}
    ] end),
    test_utils:mock_expect(Nodes, service_oneprovider, is_space_supported,
        fun(#{space_id := _Id}) -> true end),
    NewConfig;

init_per_testcase(get_should_return_space_details, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, get_space_details, {
            [{'node@host1', ?SPACE_DETAILS_JSON}], []
        }},
        {task_finished, {service, action, ok}}
    ] end),
    test_utils:mock_expect(Nodes, service_oneprovider, is_space_supported,
        fun(#{space_id := _Id}) -> true end),
    NewConfig;

init_per_testcase(delete_should_revoke_space_support, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service_oneprovider, is_space_supported,
        fun(#{space_id := _Id}) -> true end),
    NewConfig;

init_per_testcase(post_should_create_or_support_space, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, support_space, {
            [{'node@host1', ?SPACE_JSON}], []
        }},
        {task_finished, {service, action, ok}}
    ]
    end),
    NewConfig;

init_per_testcase(patch_should_modify_space_support, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, modify_space, {
            [{'node@host1', ?SPACE_JSON}], []
        }},
        {task_finished, {service, action, ok}}
    ]
    end),
    test_utils:mock_expect(Nodes, service_oneprovider, is_space_supported,
        fun(#{space_id := _Id}) -> true end),
    NewConfig;

init_per_testcase(patch_should_modify_storage_update, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, modify_space, {
            [{'node@host1', ?SPACE_JSON}], []
        }},
        {task_finished, {service, action, ok}}
    ]
    end),
    test_utils:mock_expect(Nodes, service_oneprovider, is_space_supported,
        fun(#{space_id := _Id}) -> true end),
    NewConfig;

init_per_testcase(patch_should_update_storage, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_new(Nodes, rest_oneprovider),
    test_utils:mock_expect(Nodes, rest_oneprovider, exists_resource, fun(Req, _) ->
        {true, Req}
    end),
    Self = self(),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Data  = ?STORAGE_JSON#{timeout => 10000, verificationPassed => true},
        Result = onepanel_utils:convert(Data, {keys, atom}),
        Self ! {service, Service, Action, Ctx},
        [
            {service_op_worker, update_storage, {[{hd(Nodes), Result}], []}},
            {task_finished, {service, action, ok}}
        ]
    end),
    test_utils:mock_expect(Nodes, service, apply_async, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        <<"someTaskId">>
    end),
    test_utils:mock_expect(Nodes, op_worker_storage, get, fun(<<"somePosixId">>) ->
        onepanel_utils:convert(?STORAGE_JSON, {keys, atom})
    end),
    NewConfig;

init_per_testcase(get_should_return_autocleaning_reports, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, get_auto_cleaning_reports, {
            [{'node@host1', ?AUTO_CLEANING_REPORTS}], []
        }},
        {task_finished, {service, action, ok}}
    ]
    end),
    NewConfig;

init_per_testcase(get_should_return_autocleaning_report, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, get_auto_cleaning_report, {
            [{'node@host1', ?AUTO_CLEANING_REPORT1}], []
        }},
        {task_finished, {service, action, ok}}
    ]
    end),
    NewConfig;

init_per_testcase(get_should_return_autocleaning_status, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, get_auto_cleaning_status, {
            [{'node@host1', ?AUTO_CLEANING_STATUS}], []
        }},
        {task_finished, {service, action, ok}}
    ]
    end),
    NewConfig;

init_per_testcase(get_should_return_autocleaning_configuration, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, get_auto_cleaning_configuration, {
            [{'node@host1', ?AUTO_CLEANING_CONFIG}], []
        }},
        {task_finished, {service, action, ok}}
    ]
    end),
    NewConfig;

init_per_testcase(get_should_return_file_popularity_configuration, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, get_file_popularity_configuration, {
            [{'node@host1', ?FILE_POPULARITY_CONFIG}], []
        }},
        {task_finished, {service, action, ok}}
    ]
    end),
    NewConfig;

init_per_testcase(get_should_return_transfers_mock, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_op_worker, get_transfers_mock, {
            [{'node@host1', ?TRANSFERS_MOCK_CONFIG}], []
        }},
        {task_finished, {service, action, ok}}
    ]
    end),
    NewConfig;

init_per_testcase(patch_should_update_file_popularity, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_new(Nodes, rest_oneprovider),
    Self = self(),
    test_utils:mock_expect(Nodes, service, apply_async, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        <<"someTaskId">>
    end),
    NewConfig;

init_per_testcase(Case, Config) when
    Case =:= patch_should_update_auto_cleaning;
    Case =:= patch_with_incomplete_config_should_update_auto_cleaning;
    Case =:= patch_with_incorrect_config_should_fail
    ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_new(Nodes, rest_oneprovider),
    Self = self(),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {service, action, ok}}]
    end),
    NewConfig;

init_per_testcase(_Case, Config) ->
    Nodes = ?config(oneprovider_nodes, Config),
    Hosts = ?config(oneprovider_hosts, Config),
    Self = self(),
    test_utils:mock_new(Nodes, [service, service_oneprovider]),
    test_utils:mock_expect(Nodes, service, exists, fun
        (oneprovider) -> true; (op_worker) -> true
    end),
    test_utils:mock_expect(Nodes, service, get, fun
        (oneprovider) -> {ok, #service{ctx = #{registered => true}}};
        (op_worker) -> {ok, #service{hosts = Hosts}}
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {service, action, ok}}]
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {service, action, ok}}]
    end),
    ok = onepanel_test_rest:mock_token_authentication(Nodes),

    Config.

end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes).


end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec keys_to_atoms(#{term() => term()}) -> #{atom() => term()}.
keys_to_atoms(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Acc#{onepanel_utils:convert(K, atom) => V}
    end, #{}, Map).
