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

-include("modules/models.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").


%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2]).

%% tests
-export([
    method_should_return_unauthorized_error/1,
    method_should_return_forbidden_error/1,
    get_should_return_provider_details/1,
    get_should_return_cluster_ips/1,
    put_should_register_provider/1,
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
    patch_should_invalidate_luma_cache/1]).

-define(ADMIN_USER_NAME, <<"admin1">>).
-define(ADMIN_USER_PASSWORD, <<"Admin1Password">>).
-define(REG_USER_NAME, <<"user1">>).
-define(REG_USER_PASSWORD, <<"User1Password">>).
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
    {<<"/provider/spaces/someSpaceId">>, delete}
]).

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
        lists:foldl(fun(Host, Acc) ->
            maps:put(list_to_binary(Host), <<"1.2.3.4">>, Acc)
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
    <<"timeout">> => 10000
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

-define(run(Config, Function), Function(hd(?config(oneprovider_hosts, Config)))).

all() ->
    ?ALL([
        method_should_return_unauthorized_error,
        method_should_return_forbidden_error,
        get_should_return_provider_details,
        get_should_return_cluster_ips,
        put_should_register_provider,
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
        patch_should_invalidate_luma_cache
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_unauthorized_error(Config) ->
    ?run(Config, fun(Host) ->
        lists:foreach(fun({Endpoint, Method}) ->
            ?assertMatch({ok, 401, _, _}, onepanel_test_rest:noauth_request(
                Host, Endpoint, Method
            )),
            ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
                Host, Endpoint, Method, {<<"someUser">>, <<"somePassword">>}
            ))
        end, ?COMMON_ENDPOINTS_WITH_METHODS)
    end).


method_should_return_forbidden_error(Config) ->
    ?run(Config, fun(Host) ->
        lists:foreach(fun({Endpoint, Method}) ->
            ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
                Host, Endpoint, Method, {?REG_USER_NAME, ?REG_USER_PASSWORD}
            ))
        end, ?COMMON_ENDPOINTS_WITH_METHODS)
    end).


get_should_return_provider_details(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?PROVIDER_DETAILS_JSON)
    end).


get_should_return_cluster_ips(Config) ->
    Nodes = ?config(oneprovider_nodes, Config),
    Hosts = lists:map(fun onepanel_cluster:node_to_host/1, Nodes),
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/cluster_ips">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?CLUSTER_IPS_JSON(Hosts))
    end).


put_should_register_provider(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider">>, post,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, #{
                <<"name">> => <<"someName">>,
                <<"subdomainDelegation">> => false,
                <<"domain">> => <<"somedomain">>,
                <<"adminEmail">> => <<"admin@onedata.org">>,
                <<"geoLongitude">> => 10.0,
                <<"geoLatitude">> => 20.0,
                <<"onezoneDomainName">> => <<"someDomain">>
            }
        )),
        ?assertReceivedMatch({service, oneprovider, register, #{
            onezone_domain := <<"someDomain">>,
            oneprovider_name := <<"someName">>,
            oneprovider_domain := <<"somedomain">>,
            oneprovider_geo_latitude := 20.0,
            oneprovider_geo_longitude := 10.0
        }}, ?TIMEOUT)
    end).


patch_should_modify_provider_details(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider">>, patch,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, #{
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
    [Node] = ?config(oneprovider_nodes, Config),
    Host = onepanel_cluster:node_to_host(Node),
    NewIP = <<"1.2.3.4">>,
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/cluster_ips">>, patch,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, #{
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
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider">>, delete,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
        )),
        ?assertReceivedMatch({service, oneprovider, unregister, #{}}, ?TIMEOUT)
    end).


get_should_return_supported_spaces(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACES_JSON)
    end).


post_should_create_or_support_space(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces">>, post,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, #{
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
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId1">>, patch,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, #{
                    <<"size">> => NewSize
                }
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACE_JSON)
    end).


get_should_return_space_details(Config) ->
    [N | _] = ?config(oneprovider_nodes, Config),
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACE_DETAILS_JSON)
    end).


delete_should_revoke_space_support(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/spaces/someId">>, delete,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
        )),
        ?assertReceivedMatch({service, oneprovider, revoke_space_support,
            #{id := <<"someId">>}
        }, ?TIMEOUT)
    end).


get_should_return_storages(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        onepanel_test_rest:assert_body_fields(JsonBody, [<<"ids">>])
    end).


get_should_return_storage(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages/somePosixId">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?STORAGE_JSON)
    end).


post_should_add_storage(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages">>,
            post, {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD},
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
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId1">>, patch,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, #{
                    <<"storageImport">> => ?STORAGE_IMPORT_DETAILS_JSON,
                    <<"storageUpdate">> => ?STORAGE_UPDATE_DETAILS_JSON
                }
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACE_JSON)
    end).


patch_should_update_storage(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages/somePosixId">>, patch,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, ?STORAGE_UPDATE_JSON
            )
        ),
        ?assertReceivedMatch({service, op_worker, update_storage, #{
            id := <<"somePosixId">>,
            args := #{timeout := 10000}
        }}, ?TIMEOUT)
    end).


get_should_return_autocleaning_reports(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/reports">>,
                get, {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?AUTO_CLEANING_REPORTS)
    end).

get_should_return_autocleaning_report(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/reports/someReportId">>,
                get, {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?AUTO_CLEANING_REPORT1)
    end).

get_should_return_autocleaning_status(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/status">>,
                get, {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?AUTO_CLEANING_STATUS)
    end).

get_should_return_autocleaning_configuration(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/configuration">>,
                get, {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?AUTO_CLEANING_CONFIG)
    end).

get_should_return_file_popularity_configuration(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/file-popularity/configuration">>,
                get, {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?FILE_POPULARITY_CONFIG)
    end).

patch_should_update_file_popularity(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId/file-popularity/configuration">>, patch,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, ?FILE_POPULARITY_CONFIG
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
        ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId/auto-cleaning/configuration">>, patch,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, ?AUTO_CLEANING_CONFIG
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
        ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId/auto-cleaning/configuration">>, patch,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, ?INCOMPLETE_AUTO_CLEANING_CONFIG
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
        ?assertMatch({ok, 400, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId/auto-cleaning/configuration">>, patch,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, ?INCORRECT_AUTO_CLEANING_CONFIG
            )
        )
    end).

patch_should_invalidate_luma_cache(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages/someId/invalidate_luma">>, patch,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, #{}
            )
        ),
        ?assertReceivedMatch({service, op_worker, invalidate_luma_cache, #{
            id := <<"someId">>
        }}, ?TIMEOUT)
    end).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) ->
        NewConfig2 = onepanel_test_utils:init(NewConfig),
        ?assertAllMatch({ok, _}, ?callAll(NewConfig2, onepanel_user, create,
            [?REG_USER_NAME, ?REG_USER_PASSWORD, regular]
        )),
        ?assertAllMatch({ok, _}, ?callAll(NewConfig2, onepanel_user, create,
            [?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD, admin]
        )),
        NewConfig2
    end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].


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
    Hosts = lists:map(fun onepanel_cluster:node_to_host/1, Nodes),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, format_cluster_ips, {
            [{'node@host1', ?CLUSTER_IPS_JSON(Hosts)}], []
        }},
        {task_finished, {service, action, ok}}
    ] end),
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
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {service, action, ok}}]
    end),
    test_utils:mock_expect(Nodes, service, apply_async, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        <<"someTaskId">>
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

init_per_testcase(patch_should_update_file_popularity, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_new(Nodes, rest_oneprovider),
    Self = self(),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {service, action, ok}}]
    end),
    NewConfig;

init_per_testcase(Case, Config) when
    Case =:= patch_should_update_auto_cleaning;
    Case =:=  patch_with_incomplete_config_should_update_auto_cleaning;
    Case =:=  patch_with_incorrect_config_should_fail
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
    test_utils:mock_expect(Nodes, service, get, fun
        (oneprovider) -> {ok, #service{ctx = #{registered => true}}};
        (op_worker) -> {ok, #service{hosts = Hosts}}
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {service, action, ok}}]
    end),
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
