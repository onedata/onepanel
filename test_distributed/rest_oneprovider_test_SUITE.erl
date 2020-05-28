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
-include("names.hrl").
-include("onepanel_test_rest.hrl").
-include("onepanel_test_utils.hrl").
-include("service.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/logging.hrl").


%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2]).

%% tests
-export([
    method_should_return_unauthorized_error/1,
    method_should_return_forbidden_error/1,
    method_should_return_conflict_error/1,
    method_should_return_service_unavailable_error/1,
    get_should_return_provider_details/1,
    get_should_return_cluster_ips/1,
    post_should_register_provider/1,
    patch_should_modify_provider_details/1,
    patch_should_modify_provider_ips/1,
    delete_should_unregister_provider/1,
    get_should_return_supported_spaces/1,
    post_should_support_space/1,
    patch_should_modify_space_support/1,
    get_should_return_space_details/1,
    delete_should_revoke_space_support/1,
    get_should_return_storages/1,
    get_should_return_storage/1,
    post_should_add_storage/1,
    patch_should_configure_storage_update/1,
    patch_should_update_storage/1,
    get_should_return_autocleaning_reports/1,
    get_should_return_autocleaning_report/1,
    get_should_return_autocleaning_status/1,
    get_should_return_autocleaning_configuration/1,
    get_should_return_file_popularity_configuration/1,
    patch_should_update_file_popularity/1,
    post_should_start_auto_cleaning/1,
    patch_should_update_auto_cleaning/1,
    patch_with_incomplete_config_should_update_auto_cleaning/1,
    patch_with_incorrect_config_should_fail/1,

    % tests of LUMA endpoints
    get_should_return_luma_configuration/1,
    delete_should_clear_luma_db/1,

    % local feed LUMA
    post_should_add_user_mapping_to_local_feed_luma/1,
    get_should_return_user_mapping_from_local_feed_luma/1,
    patch_should_modify_user_mapping_in_local_feed_luma/1,
    delete_should_remove_user_mapping_from_local_feed_luma/1,
    get_should_return_default_credentials_from_local_feed_luma/1,
    put_should_set_default_credentials_in_local_feed_luma/1,
    delete_should_remove_default_credentials_from_local_feed_luma/1,
    get_should_return_display_credentials_from_local_feed_luma/1,
    put_should_set_display_credentials_in_local_feed_luma/1,
    delete_should_remove_display_credentials_from_local_feed_luma/1,
    get_should_return_uid_to_onedata_user_mapping_from_local_feed_luma/1,
    put_should_set_uid_to_onedata_user_mapping_in_local_feed_luma/1,
    delete_should_remove_uid_to_onedata_user_mapping_from_local_feed_luma/1,
    get_should_return_acl_user_to_onedata_user_mapping_from_local_feed_luma/1,
    put_should_set_acl_user_to_onedata_user_mapping_in_local_feed_luma/1,
    delete_should_remove_acl_user_to_onedata_user_mapping_from_local_feed_luma/1,
    get_should_return_acl_group_to_onedata_group_mapping_from_local_feed_luma/1,
    put_should_set_acl_group_to_onedata_group_mapping_in_local_feed_luma/1,
    delete_should_remove_acl_group_to_onedata_group_mapping_from_local_feed_luma/1,

    % LUMA with all feed types
    get_should_return_user_mapping_from_luma/1,
    delete_should_remove_user_mapping_from_luma/1,
    get_should_return_default_credentials_from_luma/1,
    delete_should_remove_default_credentials_from_luma/1,
    get_should_return_display_credentials_from_luma/1,
    delete_should_remove_display_credentials_from_luma/1,
    get_should_return_uid_to_onedata_user_mapping_from_luma/1,
    delete_should_remove_uid_to_onedata_user_mapping_from_luma/1,
    get_should_return_acl_user_to_onedata_user_mapping_from_luma/1,
    delete_should_remove_acl_user_to_onedata_user_mapping_from_luma/1,
    get_should_return_acl_group_to_onedata_group_mapping_from_luma/1,
    delete_should_remove_acl_group_to_onedata_group_mapping_from_luma/1,
    
    patch_should_update_transfers_mock/1,
    get_should_return_transfers_mock/1]).

all() ->
    ?ALL([
        method_should_return_unauthorized_error,
        method_should_return_forbidden_error,
        method_should_return_conflict_error,
        method_should_return_service_unavailable_error,
        get_should_return_provider_details,
        get_should_return_cluster_ips,
        post_should_register_provider,
        patch_should_modify_provider_details,
        patch_should_modify_provider_ips,
        delete_should_unregister_provider,
        get_should_return_supported_spaces,
        post_should_support_space,
        patch_should_modify_space_support,
        get_should_return_space_details,
        delete_should_revoke_space_support,
        get_should_return_storages,
        get_should_return_storage,
        post_should_add_storage,
        patch_should_configure_storage_update,
        patch_should_update_storage,
        get_should_return_autocleaning_reports,
        get_should_return_autocleaning_report,
        get_should_return_autocleaning_status,
        get_should_return_autocleaning_configuration,
        get_should_return_file_popularity_configuration,
        patch_should_update_file_popularity,
        post_should_start_auto_cleaning,
        patch_should_update_auto_cleaning,
        patch_with_incomplete_config_should_update_auto_cleaning,
        patch_with_incorrect_config_should_fail,

        % tests of LUMA endpoints
        get_should_return_luma_configuration,
        delete_should_clear_luma_db,

        % local feed LUMA
        post_should_add_user_mapping_to_local_feed_luma,
        get_should_return_user_mapping_from_local_feed_luma,
        patch_should_modify_user_mapping_in_local_feed_luma,
        delete_should_remove_user_mapping_from_local_feed_luma,
        get_should_return_default_credentials_from_local_feed_luma,
        put_should_set_default_credentials_in_local_feed_luma,
        delete_should_remove_default_credentials_from_local_feed_luma,
        get_should_return_display_credentials_from_local_feed_luma,
        put_should_set_display_credentials_in_local_feed_luma,
        delete_should_remove_display_credentials_from_local_feed_luma,
        get_should_return_uid_to_onedata_user_mapping_from_local_feed_luma,
        put_should_set_uid_to_onedata_user_mapping_in_local_feed_luma,
        delete_should_remove_uid_to_onedata_user_mapping_from_local_feed_luma,
        get_should_return_acl_user_to_onedata_user_mapping_from_local_feed_luma,
        put_should_set_acl_user_to_onedata_user_mapping_in_local_feed_luma,
        delete_should_remove_acl_user_to_onedata_user_mapping_from_local_feed_luma,
        get_should_return_acl_group_to_onedata_group_mapping_from_local_feed_luma,
        put_should_set_acl_group_to_onedata_group_mapping_in_local_feed_luma,
        delete_should_remove_acl_group_to_onedata_group_mapping_from_local_feed_luma,

        % LUMA with all feed types
        get_should_return_user_mapping_from_luma,
        delete_should_remove_user_mapping_from_luma,
        get_should_return_default_credentials_from_luma,
        delete_should_remove_default_credentials_from_luma,
        get_should_return_display_credentials_from_luma,
        delete_should_remove_display_credentials_from_luma,
        get_should_return_uid_to_onedata_user_mapping_from_luma,
        delete_should_remove_uid_to_onedata_user_mapping_from_luma,
        get_should_return_acl_user_to_onedata_user_mapping_from_luma,
        delete_should_remove_acl_user_to_onedata_user_mapping_from_luma,
        get_should_return_acl_group_to_onedata_group_mapping_from_luma,
        delete_should_remove_acl_group_to_onedata_group_mapping_from_luma,
        
        patch_should_update_transfers_mock,
        get_should_return_transfers_mock
    ]).

-define(TIMEOUT, timer:seconds(5)).
-define(TASK_ID, "someTaskId").

-define(COMMON_ENDPOINTS_WITH_METHODS, [
    {<<"/provider">>, get},
    {<<"/provider">>, post},
    {<<"/provider">>, patch},
    {<<"/provider">>, delete},
    {<<"/provider/nagios">>, get},
    {<<"/provider/spaces">>, get},
    {<<"/provider/spaces">>, post},
    {<<"/provider/spaces/someSpaceId">>, get},
    {<<"/provider/spaces/someSpaceId">>, patch},
    {<<"/provider/spaces/someSpaceId">>, delete},
    {<<"/provider/spaces/someSpaceId/sync">>, get},

    {<<"/provider/spaces/someSpaceId/file-popularity/configuration">>, get},
    {<<"/provider/spaces/someSpaceId/file-popularity/configuration">>, patch},

    {<<"/provider/spaces/someSpaceId/auto-cleaning/configuration">>, get},
    {<<"/provider/spaces/someSpaceId/auto-cleaning/configuration">>, patch},
    {<<"/provider/spaces/someSpaceId/auto-cleaning/reports">>, get},
    {<<"/provider/spaces/someSpaceId/auto-cleaning/reports/someReportId">>, get},
    {<<"/provider/spaces/someSpaceId/auto-cleaning/start">>, post},
    {<<"/provider/spaces/someSpaceId/auto-cleaning/cancel">>, post},
    {<<"/provider/spaces/someSpaceId/auto-cleaning/status">>, get},

    {<<"/provider/storages">>, get},
    {<<"/provider/storages">>, post},
    {<<"/provider/storages/someStorageId">>, get},
    {<<"/provider/storages/someStorageId">>, patch},
    {<<"/provider/storages/someStorageId">>, delete},

    {<<"/provider/storages/someStorageId/luma/config">>, get},
    {<<"/provider/storages/someStorageId/luma/db">>, delete},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_access/all/onedata_user_to_credentials">>, post},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_access/all/onedata_user_to_credentials/onedataUserId">>, get},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_access/all/onedata_user_to_credentials/onedataUserId">>, patch},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_access/all/onedata_user_to_credentials/onedataUserId">>, delete},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_access/posix_compatible/default_credentials/someSpaceId">>, get},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_access/posix_compatible/default_credentials/someSpaceId">>, delete},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_access/posix_compatible/default_credentials/someSpaceId">>, get},
    {<<"/provider/storages/someStorageId/luma/local_feed/oneclient_display_credentials/all/default/someSpaceId">>, get},
    {<<"/provider/storages/someStorageId/luma/local_feed/oneclient_display_credentials/all/default/someSpaceId">>, put},
    {<<"/provider/storages/someStorageId/luma/local_feed/oneclient_display_credentials/all/default/someSpaceId">>, delete},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/uid_to_onedata_user/1234">>, get},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/uid_to_onedata_user/1234">>, put},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/uid_to_onedata_user/1234">>, delete},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_user_to_onedata_user/someUsername">>, get},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_user_to_onedata_user/someUsername">>, put},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_user_to_onedata_user/someUsername">>, delete},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_group_to_onedata_group/someGroupname">>, get},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_group_to_onedata_group/someGroupname">>, put},
    {<<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_group_to_onedata_group/someGroupname">>, delete},
    {<<"/provider/storages/someStorageId/luma/db/storage_access/all/onedata_user_to_credentials/onedataUserId">>, get},
    {<<"/provider/storages/someStorageId/luma/db/storage_access/all/onedata_user_to_credentials/onedataUserId">>, delete},
    {<<"/provider/storages/someStorageId/luma/db/storage_access/posix_compatible/default_credentials/someSpaceId">>, get},
    {<<"/provider/storages/someStorageId/luma/db/storage_access/posix_compatible/default_credentials/someSpaceId">>, delete},
    {<<"/provider/storages/someStorageId/luma/db/oneclient_display_credentials/all/default/someSpaceId">>, get},
    {<<"/provider/storages/someStorageId/luma/db/oneclient_display_credentials/all/default/someSpaceId">>, delete},
    {<<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/uid_to_onedata_user/1234">>, get},
    {<<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/uid_to_onedata_user/1234">>, delete},
    {<<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/acl_user_to_onedata_user/someUsername">>, get},
    {<<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/acl_group_to_onedata_group/someGroupname">>, delete},
    {<<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/acl_user_to_onedata_user/someUsername">>, get},
    {<<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/acl_group_to_onedata_group/someGroupname">>, delete},

    {<<"/provider/cluster_ips">>, get},
    {<<"/provider/cluster_ips">>, patch},

    {<<"/provider/debug/transfers_mock">>, get},
    {<<"/provider/debug/transfers_mock">>, patch}
]).

-define(REGISTER_REQUEST_JSON(Token), #{
    <<"name">> => <<"someName">>,
    <<"subdomainDelegation">> => false,
    <<"domain">> => <<"somedomain">>,
    <<"adminEmail">> => <<"admin@onedata.org">>,
    <<"geoLongitude">> => 10.0,
    <<"geoLatitude">> => 20.0,
    <<"token">> => Token
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

-define(SPACE_IDS, [<<"someId1">>, <<"someId2">>, <<"someId3">>]).

-define(CLUSTER_IPS_JSON(_Hosts), #{
    <<"hosts">> =>
    lists:foldl(fun(_Host, _Acc) ->
        maps:put(list_to_binary(_Host), <<"1.2.3.4">>, _Acc)
    end, #{}, _Hosts),
    <<"isConfigured">> => false
}).


-define(STORAGE_JSON, (maps:merge(#{
    <<"id">> => <<"somePosixId">>,
    <<"mountPoint">> => <<"someMountPoint">>,
    <<"name">> => <<"somePosix">>,
    <<"type">> => <<"posix">>
}, ?LUMA_CONFIG_JSON))).

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
        <<"type">> => <<"cephrados">>,
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
    <<"localStorages">> => [<<"someId">>],
    <<"storageImport">> => ?STORAGE_IMPORT_DETAILS_JSON,
    <<"storageUpdate">> => ?STORAGE_UPDATE_DETAILS_JSON,
    <<"spaceOccupancy">> => 1000,
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

-define(AUTO_CLEANING_REPORT_IDS, [<<"id1">>, <<"id2">>]).

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

-define(LUMA_CONFIG_JSON, #{
    <<"lumaFeed">> => <<"someFeed">>,
    <<"lumaUrl">> => <<"someUrl">>,
    <<"lumaApiKey">> => <<"someApiKey">>
}).


-define(LUMA_USER_MAPPING_JSON, #{
    <<"onedata">> => ?LUMA_ONEDATA_USER_JSON,
    <<"storage">> => #{
        <<"storageCredentials">> => #{
            <<"type">> => <<"posix">>,
            <<"uid">> => 1000
        },
        <<"displayUid">> => 2000
    }
}).

-define(LUMA_USER_STORAGE_CREDENTIALS_JSON, #{
    <<"storageCredentials">> => #{
        <<"type">> => <<"posix">>,
        <<"uid">> => 2000
    }
}).

-define(LUMA_POSIX_CREDENTIALS_JSON, #{
    <<"uid">> => 1111,
    <<"gid">> => 9999
}).

-define(LUMA_ONEDATA_USER_JSON, #{
    <<"mappingScheme">> => <<"onedataUser">>,
    <<"onedataUserId">> => <<"someUserId">>
}).

-define(LUMA_ONEDATA_GROUP_JSON, #{
    <<"mappingScheme">> => <<"onedataGroup">>,
    <<"onedataGroupId">> => <<"someGroupId">>
}).

%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_unauthorized_error(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        lists:foreach(fun(Auth) ->
            ?assertMatch({ok, ?HTTP_401_UNAUTHORIZED, _, _}, onepanel_test_rest:auth_request(
                Host, Endpoint, Method, Auth
            ))
        end, ?INCORRECT_AUTHS() ++ ?NONE_AUTHS())
    end, [{<<"/provider/onezone_info">>, get}] ++ ?COMMON_ENDPOINTS_WITH_METHODS).


method_should_return_forbidden_error(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        % highest rights which still should not grant access to these endpoints
        Auths = ?PEER_AUTHS(Host) ++ case {Endpoint, Method} of
            {_, get} ->
                [];
            {<<"/provider">>, delete} ->
                ?OZ_AUTHS(Host, privileges:cluster_admin() -- [?CLUSTER_DELETE]);
            _ ->
                ?OZ_AUTHS(Host, privileges:cluster_admin() -- [?CLUSTER_UPDATE])
        end,

        ?assertMatch({ok, ?HTTP_403_FORBIDDEN, _, _}, onepanel_test_rest:auth_request(
            Host, Endpoint, Method, Auths ++ ?PEER_AUTHS(Host)
        ))
    end, [{<<"/provider/onezone_info">>, get}] ++ ?COMMON_ENDPOINTS_WITH_METHODS).


method_should_return_conflict_error(Config) ->
    ?eachHost(Config, fun(Host) ->
        % provider is mocked as already registered
        ?assertMatch({ok, ?HTTP_409_CONFLICT, _, _}, onepanel_test_rest:auth_request(
            Host, "/provider", post, ?ROOT_AUTHS(Config),
            ?REGISTER_REQUEST_JSON(<<"someToken">>)
        ))
    end).


method_should_return_service_unavailable_error(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        ?assertMatch({ok, ?HTTP_503_SERVICE_UNAVAILABLE, _, _},
            onepanel_test_rest:auth_request(
                Host, Endpoint, Method,
                ?OZ_OR_ROOT_AUTHS(Host, privileges:cluster_admin())
            ))
    end, ?COMMON_ENDPOINTS_WITH_METHODS -- [
        {<<"/provider/cluster_ips">>, get},
        {<<"/provider">>, get},
        {<<"/provider/debug/transfers_mock">>, get},
        {<<"/provider/debug/transfers_mock">>, patch}
    ]).


get_should_return_provider_details(Config) ->
    ?eachHost(Config, fun(Host) ->
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
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/cluster_ips">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?CLUSTER_IPS_JSON(Hosts))
    end).


post_should_register_provider(Config) ->
    ?eachHost(Config, fun(Host) ->
        % a proper token must be generated to allow extracting onezone domain
        Token = onepanel_test_utils:create_registration_token(<<"some.domain">>),
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider">>, post,
            ?ROOT_AUTHS(Host), ?REGISTER_REQUEST_JSON(Token)
        )),
        ?assertReceivedMatch({service, oneprovider, register, #{
            oneprovider_token := Token,
            onezone_domain := <<"some.domain">>,
            oneprovider_name := <<"someName">>,
            oneprovider_domain := <<"somedomain">>,
            oneprovider_geo_latitude := 20.0,
            oneprovider_geo_longitude := 10.0
        }}, ?TIMEOUT)
    end).


patch_should_modify_provider_details(Config) ->
    ?eachHost(Config, fun(Host) ->
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
    ?eachHost(Config, fun(Host) ->
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
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider">>, delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_DELETE])
        )),
        ?assertReceivedMatch({service, oneprovider, unregister, #{}}, ?TIMEOUT)
    end).


get_should_return_supported_spaces(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces">>, get, ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, #{<<"ids">> => ?SPACE_IDS})
    end).


post_should_support_space(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, Headers, JsonBody} = ?assertMatch({ok, ?HTTP_201_CREATED, _, _},
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
        ?assertMatch( #{<<"location">> :=
            <<"/api/v3/onepanel/provider/spaces/", _/binary>>}, Headers),
        onepanel_test_rest:assert_body(JsonBody, ?SPACE_JSON)
    end).


patch_should_modify_space_support(Config) ->
    NewSize = 99000000,
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, <<>>},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId1">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                #{<<"size">> => NewSize}
            )
        ),
        ?assertReceivedMatch({service, oneprovider, modify_space,
            #{space_id := <<"someId1">>, size := NewSize}
        }, ?TIMEOUT)
    end).


get_should_return_space_details(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACE_DETAILS_JSON)
    end).


delete_should_revoke_space_support(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/spaces/someId">>, delete,
            ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, oneprovider, revoke_space_support,
            #{id := <<"someId">>}
        }, ?TIMEOUT)
    end).


get_should_return_storages(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body_fields(JsonBody, [<<"ids">>])
    end).


get_should_return_storage(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages/somePosixId">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?STORAGE_JSON)
    end).


post_should_add_storage(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages">>,
            post, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
            ?STORAGES_JSON
        )),
        ?assertReceivedMatch({service, op_worker, add_storages, #{
            storages := #{
                <<"someCeph">> := #{
                    type := <<"cephrados">>,
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


patch_should_configure_storage_update(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, <<>>},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId1">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), #{
                    <<"storageImport">> => ?STORAGE_IMPORT_DETAILS_JSON,
                    <<"storageUpdate">> => ?STORAGE_UPDATE_DETAILS_JSON
                }
            )
        )
    end).


patch_should_update_storage(Config) ->
    ?eachHost(Config, fun(Host) ->
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
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/reports">>,
                get, ?OZ_OR_ROOT_AUTHS(Host, []), [])
        ),
        onepanel_test_rest:assert_body(JsonBody, #{<<"ids">> => ?AUTO_CLEANING_REPORT_IDS})
    end).

get_should_return_autocleaning_report(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/reports/someReportId">>,
                get, ?OZ_OR_ROOT_AUTHS(Host, []), [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?AUTO_CLEANING_REPORT1)
    end).

get_should_return_autocleaning_status(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/status">>,
                get, ?OZ_OR_ROOT_AUTHS(Host, []), [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?AUTO_CLEANING_STATUS)
    end).

get_should_return_autocleaning_configuration(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/auto-cleaning/configuration">>,
                get, ?OZ_OR_ROOT_AUTHS(Host, []), [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?AUTO_CLEANING_CONFIG)
    end).

get_should_return_file_popularity_configuration(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(Host,
                <<"/provider/spaces/someId/file-popularity/configuration">>,
                get, ?OZ_OR_ROOT_AUTHS(Host, []), [])
        ),
        onepanel_test_rest:assert_body(JsonBody, ?FILE_POPULARITY_CONFIG)
    end).

patch_should_update_file_popularity(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertAsyncTask(?TASK_ID, onepanel_test_rest:auth_request(
            Host, <<"/provider/spaces/someId/file-popularity/configuration">>,
            patch, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
            ?FILE_POPULARITY_CONFIG
        )),
        ?assertReceivedMatch({service, oneprovider, configure_file_popularity, #{
            space_id := <<"someId">>,
            enabled := true,
            last_open_hour_weight := 1.0,
            avg_open_count_per_day_weight := 1.0,
            max_avg_open_count_per_day := 100.0
        }}, ?TIMEOUT)
    end).


post_should_start_auto_cleaning(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, Headers, JsonBody} = ?assertMatch(
            {ok, ?HTTP_202_ACCEPTED, #{?HDR_LOCATION := _}, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId/auto-cleaning/start">>, post,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
            )
        ),
        ?assertReceivedMatch({service, oneprovider, start_auto_cleaning, #{
            space_id := <<"someId">>
        }}, ?TIMEOUT),
        onepanel_test_rest:assert_body_fields(JsonBody, [<<"reportId">>]),
        #{<<"reportId">> := ReportId} = json_utils:decode(JsonBody),
        ?assertMatch(<<
            "/api/v3/onepanel/provider/spaces/someId/"
            "auto-cleaning/reports/", ReportId/binary
        >>, maps:get(?HDR_LOCATION, Headers))
    end).


patch_should_update_auto_cleaning(Config) ->
    ?eachHost(Config, fun(Host) ->
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
    ?eachHost(Config, fun(Host) ->
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
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_400_BAD_REQUEST, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId/auto-cleaning/configuration">>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), ?INCORRECT_AUTO_CLEANING_CONFIG
            )
        )
    end).

get_should_return_luma_configuration(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages/someId/luma/config">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), #{}
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_CONFIG_JSON)
    end).

delete_should_clear_luma_db(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/storages/someId/luma/db">>, delete,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), #{}
            )
        ),
        ?assertReceivedMatch({service, op_worker, clear_luma_db, #{
            id := <<"someId">>
        }}, ?TIMEOUT)
    end).

post_should_add_user_mapping_to_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_access/all/onedata_user_to_credentials">>,
            post, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
            ?LUMA_USER_MAPPING_JSON
        )),
        ?assertReceivedMatch({service, op_worker, add_user_mapping, #{
            id := <<"someStorageId">>,
            mapping := #{
                onedata := #{
                    mappingScheme := <<"onedataUser">>,
                    onedataUserId := <<"someUserId">>
                },
                storage := #{
                    storageCredentials := #{
                        type := <<"posix">>,
                        uid := 1000
                   },
                    displayUid := 2000
                }
            }
        }}, ?TIMEOUT)
    end).

get_should_return_user_mapping_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_access/all/onedata_user_to_credentials/someUserId">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_USER_MAPPING_JSON)
    end).

patch_should_modify_user_mapping_in_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_access/all/onedata_user_to_credentials/someUserId">>,
            patch, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), ?LUMA_USER_STORAGE_CREDENTIALS_JSON
        )),
        ?assertReceivedMatch({service, op_worker, update_user_mapping, #{
            id := <<"someStorageId">>,
            onedataUserId := <<"someUserId">>,
            storageUser := #{
                storageCredentials := #{
                   type := <<"posix">>,
                    uid := 2000
        }}}}, ?TIMEOUT)
    end).

delete_should_remove_user_mapping_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_access/all/onedata_user_to_credentials/someUserId">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_user_mapping, #{
            id := <<"someStorageId">>,
            onedataUserId := <<"someUserId">>
        }}, ?TIMEOUT)
    end).

get_should_return_default_credentials_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_access/posix_compatible/default_credentials/someSpaceId">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_POSIX_CREDENTIALS_JSON)
    end).

put_should_set_default_credentials_in_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_access/posix_compatible/default_credentials/someSpaceId">>,
            put, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), ?LUMA_POSIX_CREDENTIALS_JSON
        )),
        ?assertReceivedMatch({service, op_worker, add_default_posix_credentials, #{
            id := <<"someStorageId">>,
            spaceId := <<"someSpaceId">>,
            credentials := #{
                    uid := 1111,
                    gid := 9999
            }}}, ?TIMEOUT)
    end).

delete_should_remove_default_credentials_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_access/posix_compatible/default_credentials/someSpaceId">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_default_posix_credentials, #{
            id := <<"someStorageId">>,
            spaceId := <<"someSpaceId">>
        }}, ?TIMEOUT)
    end).

get_should_return_display_credentials_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/oneclient_display_credentials/all/default/someSpaceId">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_POSIX_CREDENTIALS_JSON)
    end).

put_should_set_display_credentials_in_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/oneclient_display_credentials/all/default/someSpaceId">>,
            put, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), ?LUMA_POSIX_CREDENTIALS_JSON
        )),
        ?assertReceivedMatch({service, op_worker, add_display_credentials, #{
            id := <<"someStorageId">>,
            spaceId := <<"someSpaceId">>,
            credentials := #{
                uid := 1111,
                gid := 9999
            }}}, ?TIMEOUT)
    end).

delete_should_remove_display_credentials_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/oneclient_display_credentials/all/default/someSpaceId">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_display_credentials, #{
            id := <<"someStorageId">>,
            spaceId := <<"someSpaceId">>
        }}, ?TIMEOUT)
    end).

get_should_return_uid_to_onedata_user_mapping_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/uid_to_onedata_user/1234">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_ONEDATA_USER_JSON)
    end).

put_should_set_uid_to_onedata_user_mapping_in_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/uid_to_onedata_user/1234">>,
            put, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), ?LUMA_ONEDATA_USER_JSON
        )),
        ?assertReceivedMatch({service, op_worker, add_uid_to_onedata_user_mapping, #{
            id := <<"someStorageId">>,
            uid := 1234,
            mapping := #{
                mappingScheme := <<"onedataUser">>,
                onedataUserId := <<"someUserId">>
            }}}, ?TIMEOUT)
    end).

delete_should_remove_uid_to_onedata_user_mapping_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/uid_to_onedata_user/1234">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_uid_to_onedata_user_mapping, #{
            id := <<"someStorageId">>,
            uid := 1234
        }}, ?TIMEOUT)
    end).

get_should_return_acl_user_to_onedata_user_mapping_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_user_to_onedata_user/someUsername">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_ONEDATA_USER_JSON)
    end).

put_should_set_acl_user_to_onedata_user_mapping_in_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_user_to_onedata_user/someUsername">>,
            put, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), ?LUMA_ONEDATA_USER_JSON
        )),
        ?assertReceivedMatch({service, op_worker, add_acl_user_to_onedata_user_mapping, #{
            id := <<"someStorageId">>,
            aclUser := <<"someUsername">>,
            mapping := #{
                mappingScheme := <<"onedataUser">>,
                onedataUserId := <<"someUserId">>
            }}}, ?TIMEOUT)
    end).

delete_should_remove_acl_user_to_onedata_user_mapping_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_user_to_onedata_user/someUsername">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_acl_user_to_onedata_user_mapping, #{
            id := <<"someStorageId">>,
            aclUser := <<"someUsername">>
        }}, ?TIMEOUT)
    end).

get_should_return_acl_group_to_onedata_group_mapping_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_group_to_onedata_group/someGroupname">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_ONEDATA_GROUP_JSON)
    end).

put_should_set_acl_group_to_onedata_group_mapping_in_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_group_to_onedata_group/someGroupname">>,
            put, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]), ?LUMA_ONEDATA_GROUP_JSON
        )),
        ?assertReceivedMatch({service, op_worker, add_acl_group_to_onedata_group_mapping, #{
            id := <<"someStorageId">>,
            aclGroup := <<"someGroupname">>,
            mapping := #{
                mappingScheme := <<"onedataGroup">>,
                onedataGroupId := <<"someGroupId">>
            }}}, ?TIMEOUT)
    end).

delete_should_remove_acl_group_to_onedata_group_mapping_from_local_feed_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/local_feed/storage_sync/posix_compatible/acl_group_to_onedata_group/someGroupname">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_acl_group_to_onedata_group_mapping, #{
            id := <<"someStorageId">>,
            aclGroup := <<"someGroupname">>
        }}, ?TIMEOUT)
    end).

get_should_return_user_mapping_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/storage_access/all/onedata_user_to_credentials/someUserId">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_USER_MAPPING_JSON)
    end).

delete_should_remove_user_mapping_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/storage_access/all/onedata_user_to_credentials/someUserId">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_user_mapping, #{
            id := <<"someStorageId">>,
            onedataUserId := <<"someUserId">>
        }}, ?TIMEOUT)
    end).

get_should_return_default_credentials_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/storage_access/posix_compatible/default_credentials/someSpaceId">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_POSIX_CREDENTIALS_JSON)
    end).

delete_should_remove_default_credentials_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/storage_access/posix_compatible/default_credentials/someSpaceId">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_default_posix_credentials, #{
            id := <<"someStorageId">>,
            spaceId := <<"someSpaceId">>
        }}, ?TIMEOUT)
    end).

get_should_return_display_credentials_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/oneclient_display_credentials/all/default/someSpaceId">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_POSIX_CREDENTIALS_JSON)
    end).

delete_should_remove_display_credentials_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/oneclient_display_credentials/all/default/someSpaceId">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_display_credentials, #{
            id := <<"someStorageId">>,
            spaceId := <<"someSpaceId">>
        }}, ?TIMEOUT)
    end).

get_should_return_uid_to_onedata_user_mapping_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/uid_to_onedata_user/1234">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_ONEDATA_USER_JSON)
    end).

delete_should_remove_uid_to_onedata_user_mapping_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/uid_to_onedata_user/1234">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_uid_to_onedata_user_mapping, #{
            id := <<"someStorageId">>,
            uid := 1234
        }}, ?TIMEOUT)
    end).


get_should_return_acl_user_to_onedata_user_mapping_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/acl_user_to_onedata_user/someUsername">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_ONEDATA_USER_JSON)
    end).


delete_should_remove_acl_user_to_onedata_user_mapping_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/acl_user_to_onedata_user/someUsername">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_acl_user_to_onedata_user_mapping, #{
            id := <<"someStorageId">>,
            aclUser := <<"someUsername">>
        }}, ?TIMEOUT)
    end).

get_should_return_acl_group_to_onedata_group_mapping_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/acl_group_to_onedata_group/someGroupname">>,
            get, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        onepanel_test_rest:assert_body(JsonBody, ?LUMA_ONEDATA_GROUP_JSON)
    end).

delete_should_remove_acl_group_to_onedata_group_mapping_from_luma(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/storages/someStorageId/luma/db/storage_sync/posix_compatible/acl_group_to_onedata_group/someGroupname">>,
            delete, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
        )),
        ?assertReceivedMatch({service, op_worker, remove_acl_group_to_onedata_group_mapping, #{
            id := <<"someStorageId">>,
            aclGroup := <<"someGroupname">>
        }}, ?TIMEOUT)
    end).

patch_should_update_transfers_mock(Config) ->
    ?eachHost(Config, fun(Host) ->
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
    ?eachHost(Config, fun(Host) ->
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
    test_utils:mock_new(Nodes, [onepanel_parser]),
    test_utils:mock_expect(Nodes, service, is_healthy, fun(_) -> false end),
    test_utils:mock_expect(Nodes, service, all_healthy, fun() -> false end),
    % do not require valid payload in requests
    test_utils:mock_expect(Nodes, onepanel_parser, parse, fun(_, _) -> #{} end),
    NewConfig;

init_per_testcase(get_should_return_provider_details, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        #step_end{module = service_oneprovider, function = get_details,
            good_bad_results = {[{'node@host1', ?PROVIDER_DETAILS_JSON}], []}},
        #action_end{service = service, action = action, result = ok}
    ] end),
    NewConfig;

init_per_testcase(get_should_return_cluster_ips, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    Hosts = lists:map(fun hosts:from_node/1, Nodes),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        #step_end{module = service_oneprovider, function = format_cluster_ips,
            good_bad_results = {
                [{'node@host1', ?CLUSTER_IPS_JSON(Hosts)}], []
            }},
        #action_end{service = service, action = action, result = ok}
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
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        #step_end{module = service_op_worker, function = get_storages,
            good_bad_results = {
                [{'node@host1', keys_to_atoms(?STORAGE_JSON)}],
                []
            }},
        #action_end{service = service, action = action, result = ok}
    ] end),
    NewConfig;


init_per_testcase(get_should_return_storages, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, NewConfig),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        #step_end{module = service_op_worker, function = get_storages,
            good_bad_results = {
                [{'node@host1', [<<"id1">>, <<"id2">>, <<"id3">>]}],
                []
            }},
        #action_end{service = service, action = action, result = ok}
    ] end),
    NewConfig;

init_per_testcase(get_should_return_supported_spaces, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        #step_end{module = service_oneprovider, function = get_spaces,
            good_bad_results = {[{'node@host1', ?SPACE_IDS}], []}},
        #action_end{service = service, action = action, result = ok}
    ] end),
    test_utils:mock_expect(Nodes, service_oneprovider, is_space_supported,
        fun(#{space_id := _Id}) -> true end),
    NewConfig;

init_per_testcase(get_should_return_space_details, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),

    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        #step_end{module = service_oneprovider, function = get_space_details,
            good_bad_results = {
                [{'node@host1',
                    onepanel_utils:convert(?SPACE_DETAILS_JSON, {keys, atom})}],
                []
            }},
        #action_end{service = service, action = action, result = ok}
    ] end),
    test_utils:mock_expect(Nodes, service_oneprovider, is_space_supported,
        fun(#{space_id := _Id}) -> true end),
    NewConfig;

init_per_testcase(post_should_support_space, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        #step_end{module = service_oneprovider, function = support_space,
            good_bad_results = {
                [{'node@host1', <<"someId1">>}], []
            }},
        #action_end{service = service, action = action, result = ok}
    ]
    end),
    NewConfig;

init_per_testcase(Case, Config) when
    Case == get_should_return_autocleaning_reports;
    Case == get_should_return_autocleaning_status;
    Case == patch_should_modify_space_support;
    Case == patch_should_configure_storage_update;
    Case == post_should_start_auto_cleaning;
    Case == patch_should_update_auto_cleaning;
    Case == patch_should_update_file_popularity;
    Case == patch_with_incomplete_config_should_update_auto_cleaning;
    Case == patch_with_incorrect_config_should_fail;
    Case == delete_should_revoke_space_support;
    Case == post_should_add_user_mapping_to_local_feed_luma;
    Case == patch_should_modify_user_mapping_in_local_feed_luma;
    Case == delete_should_remove_user_mapping_from_local_feed_luma;
    Case == put_should_set_default_credentials_in_local_feed_luma;
    Case == delete_should_remove_default_credentials_from_local_feed_luma;
    Case == put_should_set_display_credentials_in_local_feed_luma;
    Case == delete_should_remove_display_credentials_from_local_feed_luma;
    Case == put_should_set_uid_to_onedata_user_mapping_in_local_feed_luma;
    Case == delete_should_remove_uid_to_onedata_user_mapping_from_local_feed_luma;
    Case == put_should_set_acl_user_to_onedata_user_mapping_in_local_feed_luma;
    Case == delete_should_remove_acl_user_to_onedata_user_mapping_from_local_feed_luma;
    Case == put_should_set_acl_group_to_onedata_group_mapping_in_local_feed_luma;
    Case == delete_should_remove_acl_group_to_onedata_group_mapping_from_local_feed_luma;
    Case == delete_should_remove_user_mapping_from_luma;
    Case == delete_should_remove_default_credentials_from_luma;
    Case == delete_should_remove_display_credentials_from_luma;
    Case == delete_should_remove_uid_to_onedata_user_mapping_from_luma;
    Case == delete_should_remove_acl_user_to_onedata_user_mapping_from_luma;
    Case == delete_should_remove_acl_group_to_onedata_group_mapping_from_luma
->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    Self = self(),
    test_utils:mock_expect(Nodes, service, apply_sync, fun
        (?SERVICE_OP, get_space_details, _) -> [
            % satisfy middleware:fetch_entity
            #step_end{module = service_oneprovider, function = get_space_details,
                good_bad_results = {
                    [{'node@host1',
                        onepanel_utils:convert(?SPACE_DETAILS_JSON, {keys, atom})}],
                    []
                }}];
        (?SERVICE_OP = Service, modify_space = Action, Ctx) ->
            Self ! {service, Service, Action, Ctx},
            [
                #step_end{module = service_oneprovider, function = modify_space,
                    good_bad_results = {
                        [{'node@host1', ?SPACE_JSON}], []
                    }},
                #action_end{service = service, action = action, result = ok}
            ];
        (?SERVICE_OP = Service, start_auto_cleaning = Action, Ctx) ->
            Self ! {service, Service, Action, Ctx},
            [
                #step_end{module = service_oneprovider, function = start_auto_cleaning,
                    good_bad_results = {
                        [{'node@host1', {ok, <<"someReportId">>}}], []
                    }},
                #action_end{service = service, action = action, result = ok}
            ];
        (?SERVICE_OP, get_auto_cleaning_reports, _Ctx) -> [
            #step_end{module = service_oneprovider, function = get_auto_cleaning_reports,
                good_bad_results = {
                    [{'node@host1', ?AUTO_CLEANING_REPORT_IDS}], []
                }},
            #action_end{service = service, action = action, result = ok}
        ];
        (?SERVICE_OP, get_auto_cleaning_status, _Ctx) -> [
            #step_end{module = service_oneprovider, function = get_auto_cleaning_status,
                good_bad_results = {
                    [{'node@host1', ?AUTO_CLEANING_STATUS}], []
                }},
            #action_end{service = service, action = action, result = ok}
        ];
        (?SERVICE_OPW, get_storages, _Ctx) ->
            [
                % satisfy middleware:fetch_entity
                #step_end{module = service_op_worker, function = get_storages,
                    good_bad_results = {
                        [{'node@host1', keys_to_atoms(?STORAGE_JSON)}],
                        []
                    }},
                #action_end{service = service, action = action, result = ok}
            ];
        (Service, Action, Ctx) ->
            Self ! {service, Service, Action, Ctx},
            [#action_end{service = service, action = action, result = ok}]
    end),
    test_utils:mock_expect(Nodes, service_oneprovider, is_space_supported,
        fun(#{space_id := _Id}) -> true end),
    NewConfig;

init_per_testcase(patch_should_update_storage, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    Self = self(),
    test_utils:mock_expect(Nodes, service, apply_sync, fun
        (?SERVICE_OPW = Service, update_storage = Action, Ctx) ->
            Data = ?STORAGE_JSON#{timeout => 10000, verificationPassed => true},
            Result = onepanel_utils:convert(Data, {keys, atom}),
            Self ! {service, Service, Action, Ctx},
            [
                #step_end{module = service_op_worker, function = update_storage,
                    good_bad_results = {[{hd(Nodes), Result}], []}},
                #action_end{service = service, action = action, result = ok}
            ];
        (?SERVICE_OPW, get_storages, _) -> [
            #step_end{module = service_op_worker, function = get_storages,
                good_bad_results = {
                    [{'node@host1', keys_to_atoms(?STORAGE_JSON)}],
                    []
                }}
        ]
    end),
    NewConfig;

init_per_testcase(get_should_return_autocleaning_report, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        % satisfy middleware:fetch_entity
        #step_end{module = service_oneprovider, function = get_space_details,
            good_bad_results = {
                [{'node@host1',
                    onepanel_utils:convert(?SPACE_DETAILS_JSON, {keys, atom})}],
                []
            }},
        #step_end{module = service_oneprovider, function = get_auto_cleaning_report,
            good_bad_results = {
                [{'node@host1', ?AUTO_CLEANING_REPORT1}], []
            }},
        #action_end{service = service, action = action, result = ok}
    ]
    end),
    NewConfig;

init_per_testcase(get_should_return_autocleaning_configuration, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun
        (?SERVICE_OP, get_space_details, _) -> [
            % satisfy middleware:fetch_entity
            #step_end{module = service_oneprovider, function = get_space_details,
                good_bad_results = {
                    [{'node@host1',
                        onepanel_utils:convert(?SPACE_DETAILS_JSON, {keys, atom})}],
                    []
                }}];
        (?SERVICE_OP, get_auto_cleaning_configuration, _Ctx) -> [
            #step_end{module = service_oneprovider, function = get_auto_cleaning_configuration,
                good_bad_results = {
                    [{'node@host1', ?AUTO_CLEANING_CONFIG}], []
                }},
            #action_end{service = service, action = action, result = ok}
        ]
    end),
    NewConfig;

init_per_testcase(get_should_return_file_popularity_configuration, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        % satisfy middleware:fetch_entity
        #step_end{module = service_oneprovider, function = get_space_details,
            good_bad_results = {
                [{'node@host1',
                    onepanel_utils:convert(?SPACE_DETAILS_JSON, {keys, atom})}],
                []
            }},
        #step_end{module = service_oneprovider, function = get_file_popularity_configuration,
            good_bad_results = {
                [{'node@host1', ?FILE_POPULARITY_CONFIG}], []
            }},
        #action_end{service = service, action = action, result = ok}
    ]
    end),
    NewConfig;

init_per_testcase(get_should_return_transfers_mock, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service_op_worker, is_transfers_mock_enabled,
        fun() -> true end),
    NewConfig;

init_per_testcase(Case, Config) when
    Case == method_should_return_forbidden_error;
    Case == method_should_return_unauthorized_error
    ->
    Config2 = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config2),
    test_utils:mock_new(Nodes, [space_middleware, onepanel_parser]),
    test_utils:mock_expect(Nodes, space_middleware, fetch_entity, fun
        (_) -> {ok, {undefined, 1}}
    end),
    % do not require valid payload in requests
    test_utils:mock_expect(Nodes, onepanel_parser, parse, fun(_, _) -> #{} end),
    Config2;

init_per_testcase(get_should_return_luma_configuration, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        % satisfy middleware:fetch_entity
        #step_end{module = service_op_worker, function = get_storages,
            good_bad_results = {
                [{'node@host1', keys_to_atoms(?STORAGE_JSON)}],
                []
            }},
        #step_end{module = service_op_worker, function = get_luma_configuration,
            good_bad_results = {
                [{'node@host1', ?LUMA_CONFIG_JSON}], []
            }},
        #action_end{service = service, action = action, result = ok}
    ]
    end),
    NewConfig;

init_per_testcase(Case, Config) when
    Case =:= get_should_return_user_mapping_from_local_feed_luma;
    Case =:= get_should_return_user_mapping_from_luma
->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        % satisfy middleware:fetch_entity
        #step_end{module = service_op_worker, function = get_storages,
            good_bad_results = {
                [{'node@host1', keys_to_atoms(?STORAGE_JSON)}],
                []
            }},
        #step_end{module = service_op_worker, function = get_user_mapping,
            good_bad_results = {
                [{'node@host1', ?LUMA_USER_MAPPING_JSON}], []
            }},
        #action_end{service = service, action = action, result = ok}
    ]
    end),
    NewConfig;

init_per_testcase(Case, Config) when
    Case =:= get_should_return_default_credentials_from_local_feed_luma;
    Case =:= get_should_return_default_credentials_from_luma
->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        % satisfy middleware:fetch_entity
        #step_end{module = service_op_worker, function = get_storages,
            good_bad_results = {
                [{'node@host1', keys_to_atoms(?STORAGE_JSON)}],
                []
            }},
        #step_end{module = service_op_worker, function = get_default_posix_credentials,
            good_bad_results = {
                [{'node@host1', ?LUMA_POSIX_CREDENTIALS_JSON}], []
            }},
        #action_end{service = service, action = action, result = ok}
    ]
    end),
    NewConfig;

init_per_testcase(Case, Config) when
    Case =:= get_should_return_display_credentials_from_local_feed_luma;
    Case =:= get_should_return_display_credentials_from_luma
->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        % satisfy middleware:fetch_entity
        #step_end{module = service_op_worker, function = get_storages,
            good_bad_results = {
                [{'node@host1', keys_to_atoms(?STORAGE_JSON)}],
                []
            }},
        #step_end{module = service_op_worker, function = get_display_credentials,
            good_bad_results = {
                [{'node@host1', ?LUMA_POSIX_CREDENTIALS_JSON}], []
            }},
        #action_end{service = service, action = action, result = ok}
    ]
    end),
    NewConfig;

init_per_testcase(Case, Config) when
    Case =:= get_should_return_uid_to_onedata_user_mapping_from_local_feed_luma;
    Case =:= get_should_return_uid_to_onedata_user_mapping_from_luma
->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        % satisfy middleware:fetch_entity
        #step_end{module = service_op_worker, function = get_storages,
            good_bad_results = {
                [{'node@host1', keys_to_atoms(?STORAGE_JSON)}],
                []
            }},
        #step_end{module = service_op_worker, function = get_uid_to_onedata_user_mapping,
            good_bad_results = {
                [{'node@host1', ?LUMA_ONEDATA_USER_JSON}], []
            }},
        #action_end{service = service, action = action, result = ok}
    ]
    end),
    NewConfig;

init_per_testcase(Case, Config) when
    Case =:= get_should_return_acl_user_to_onedata_user_mapping_from_local_feed_luma;
    Case =:= get_should_return_acl_user_to_onedata_user_mapping_from_luma
->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        % satisfy middleware:fetch_entity
        #step_end{module = service_op_worker, function = get_storages,
            good_bad_results = {
                [{'node@host1', keys_to_atoms(?STORAGE_JSON)}],
                []
            }},
        #step_end{module = service_op_worker, function = get_acl_user_to_onedata_user_mapping,
            good_bad_results = {
                [{'node@host1', ?LUMA_ONEDATA_USER_JSON}], []
            }},
        #action_end{service = service, action = action, result = ok}
    ]
    end),
    NewConfig;

init_per_testcase(Case, Config) when
    Case =:= get_should_return_acl_group_to_onedata_group_mapping_from_local_feed_luma;
    Case =:= get_should_return_acl_group_to_onedata_group_mapping_from_luma
->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        % satisfy middleware:fetch_entity
        #step_end{module = service_op_worker, function = get_storages,
            good_bad_results = {
                [{'node@host1', keys_to_atoms(?STORAGE_JSON)}],
                []
            }},
        #step_end{module = service_op_worker, function = get_acl_group_to_onedata_group_mapping,
            good_bad_results = {
                [{'node@host1', ?LUMA_ONEDATA_GROUP_JSON}], []
            }},
        #action_end{service = service, action = action, result = ok}
    ]
    end),
    NewConfig;

init_per_testcase(_Case, Config) ->
    Nodes = ?config(oneprovider_nodes, Config),
    Hosts = ?config(oneprovider_hosts, Config),
    Self = self(),
    test_utils:mock_new(Nodes, [service, service_oneprovider, op_worker_storage]),
    test_utils:mock_expect(Nodes, service, exists, fun
        (oneprovider) -> true; (op_worker) -> true; (ceph) -> false
    end),
    test_utils:mock_expect(Nodes, service, get, fun
        (oneprovider) -> {ok, #service{
            ctx = #{registered => true, onezone_domain => "oz.example.local"}
        }};
        (op_worker) -> {ok, #service{hosts = Hosts, ctx = #{
            status => maps:from_list([{Host, healthy} || Host <- Hosts])
        }}}
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        % various step results for entity fetches
        [
            #step_end{module = service_oneprovider, function = get_space_details,
                good_bad_results = {
                    [{'node@host1',
                        onepanel_utils:convert(?SPACE_DETAILS_JSON, {keys, atom})}],
                    []
                }},
            #step_end{module = service_op_worker, function = get_storages,
                good_bad_results = {
                    [{'node@host1', keys_to_atoms(?STORAGE_JSON)}], []
                }},
            #action_end{service = service, action = action, result = ok}
        ]
    end),
    test_utils:mock_expect(Nodes, service, apply_async, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        <<?TASK_ID>>
    end),
    test_utils:mock_expect(Nodes, op_worker_storage, exists,
        fun(_) -> true end),
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
