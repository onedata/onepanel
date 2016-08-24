%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of 'rest_service' module.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_service_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
    end_per_testcase/2]).

%% tests
-export([
    method_should_return_unauthorized_error/1,
    method_should_return_forbidden_error/1,
    method_should_return_not_found_error/1,
    get_should_return_service_status/1,
    get_should_return_service_host_status/1,
    get_should_return_storages/1,
    get_should_return_storage/1,
    get_should_return_service_task_results/1,
    patch_should_start_stop_service/1,
    put_should_add_storage/1,
    put_should_configure_database_service/1,
    put_should_configure_cluster_manager_service/1,
    put_should_configure_cluster_worker_service/1,
    put_should_configure_oneprovider_service/1,
    put_should_configure_onezone_service/1
]).

-define(ADMIN_USER_NAME, <<"admin1">>).
-define(ADMIN_USER_PASSWORD, <<"Admin1Password">>).
-define(REG_USER_NAME, <<"user1">>).
-define(REG_USER_PASSWORD, <<"User1Password">>).
-define(TIMEOUT, timer:seconds(5)).

-define(COMMON_HOST_ENDPOINTS_WITH_METHODS, [
    {<<"/databases/someHost">>, get},
    {<<"/managers/someHost">>, get},
    {<<"/workers/someHost">>, get},
    {<<"/databases/someHost">>, patch},
    {<<"/managers/someHost">>, patch},
    {<<"/workers/someHost">>, patch}
]).

-define(COMMON_ENDPOINTS_WITH_METHODS, [
    {<<"/databases">>, get},
    {<<"/managers">>, get},
    {<<"/workers">>, get},
    {<<"/databases">>, patch},
    {<<"/managers">>, patch},
    {<<"/workers">>, patch},
    {<<"/databases">>, post},
    {<<"/managers">>, post},
    {<<"/workers">>, post} |
    ?COMMON_HOST_ENDPOINTS_WITH_METHODS
]).

-define(STORAGE_JSON, [
    {<<"somePosix">>, [
        {<<"type">>, <<"posix">>},
        {<<"mountPoint">>, <<"someMountPoint">>}
    ]}
]).

-define(STORAGES_JSON, [
    {<<"someCeph">>, [
        {<<"type">>, <<"ceph">>},
        {<<"username">>, <<"someName">>},
        {<<"key">>, <<"someKey">>},
        {<<"monitorHostname">>, <<"someHostname">>},
        {<<"poolName">>, <<"someName">>},
        {<<"clusterName">>, <<"someName">>}
    ]},
    {<<"someS3">>, [
        {<<"type">>, <<"s3">>},
        {<<"s3Hostname">>, <<"someHostname">>},
        {<<"iamHostname">>, <<"someHostname">>},
        {<<"bucketName">>, <<"someName">>},
        {<<"accessKey">>, <<"someKey">>},
        {<<"secretKey">>, <<"someKey">>}
    ]} | ?STORAGE_JSON
]).

-define(CLUSTER_JSON, [{<<"domainName">>, <<"someDomain">>},
    {<<"nodes">>, [
        {<<"n1">>, [{<<"hostname">>, <<"host1">>}]},
        {<<"n2">>, [{<<"hostname">>, <<"host2">>}]},
        {<<"n3">>, [{<<"hostname">>, <<"host3">>}]}
    ]},
    {<<"databases">>, [
        {<<"nodes">>, [<<"n1">>, <<"n2">>]}
    ]},
    {<<"managers">>, [
        {<<"mainNode">>, <<"n3">>},
        {<<"nodes">>, [<<"n2">>, <<"n3">>]}
    ]},
    {<<"workers">>, [
        {<<"nodes">>, [<<"n1">>, <<"n2">>, <<"n3">>]}
    ]}
]).

-define(run(Config, Function), ?run(Config, Function, [
    {oneprovider_hosts, <<"/provider">>},
    {onezone_hosts, <<"/zone">>}
])).

-define(run(Config, Function, HostsTypeAndArgs), begin
    lists:foreach(fun({_Type_, _Args_}) ->
        Function({hd(?config(_Type_, Config)), _Args_})
    end, HostsTypeAndArgs)
end).

all() ->
    ?ALL([
        method_should_return_unauthorized_error,
        method_should_return_forbidden_error,
        method_should_return_not_found_error,
        get_should_return_service_status,
        get_should_return_service_host_status,
        get_should_return_storages,
        get_should_return_storage,
        get_should_return_service_task_results,
        patch_should_start_stop_service,
        put_should_add_storage,
        put_should_configure_database_service,
        put_should_configure_cluster_manager_service,
        put_should_configure_cluster_worker_service,
        put_should_configure_oneprovider_service,
        put_should_configure_onezone_service
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_unauthorized_error(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        lists:foreach(fun({Endpoint, Method}) ->
            ?assertMatch({ok, 401, _, _}, onepanel_test_rest:noauth_request(
                Host, <<Prefix/binary, Endpoint/binary>>, Method
            )),
            ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, Endpoint/binary>>, Method,
                <<"someUser">>, <<"somePassword">>
            ))
        end, ?COMMON_ENDPOINTS_WITH_METHODS)
    end).


method_should_return_forbidden_error(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        lists:foreach(fun({Endpoint, Method}) ->
            ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, Endpoint/binary>>, Method,
                ?REG_USER_NAME, ?REG_USER_PASSWORD
            ))
        end, ?COMMON_ENDPOINTS_WITH_METHODS)
    end).


method_should_return_not_found_error(Config) ->
    ?run(Config, fun({Host, _}) ->
        ?assertMatch({ok, 404, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/tasks/someTaskId">>, get,
            ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD
        ))
    end, [{oneprovider_hosts, <<>>}, {onezone_hosts, <<>>}]),

    ?run(Config, fun({Host, Prefix}) ->
        lists:foreach(fun({Endpoint, Method}) ->
            ?assertMatch({ok, 404, _, _}, onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, Endpoint/binary>>, Method,
                ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD
            ))
        end, ?COMMON_HOST_ENDPOINTS_WITH_METHODS)
    end).


get_should_return_service_status(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        lists:foreach(fun(Endpoint) ->
            {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
                onepanel_test_rest:auth_request(
                    Host, <<Prefix/binary, Endpoint/binary>>, get,
                    ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD
                )
            ),
            onepanel_test_rest:assert_body_values(JsonBody, [
                {<<"host1">>, <<"running">>},
                {<<"host2">>, <<"stopped">>},
                {<<"host3">>, <<"missing">>}
            ])
        end, [<<"/databases">>, <<"/managers">>, <<"/workers">>])
    end).


get_should_return_service_host_status(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        lists:foreach(fun(Endpoint) ->
            {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
                onepanel_test_rest:auth_request(
                    Host, <<Prefix/binary, Endpoint/binary, "/someHost">>, get,
                    ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD
                )
            ),
            onepanel_test_rest:assert_body(JsonBody, <<"running">>)
        end, [<<"/databases">>, <<"/managers">>, <<"/workers">>])
    end).


get_should_return_storages(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/storages">>, get,
                ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?STORAGES_JSON)
    end, [{oneprovider_hosts, <<"/provider">>}]).


get_should_return_storage(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/storages/somePosix">>, get,
                ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?STORAGE_JSON)
    end, [{oneprovider_hosts, <<"/provider">>}]).


get_should_return_service_task_results(Config) ->
    ?run(Config, fun({Host, _}) ->
        lists:foreach(fun({TaskId, Fields, Values}) ->
            {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
                onepanel_test_rest:auth_request(
                    Host, <<"/tasks/", TaskId/binary>>, get,
                    ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD
                )
            ),
            onepanel_test_rest:assert_body_fields(JsonBody, Fields),
            onepanel_test_rest:assert_body_values(JsonBody, Values)
        end, [
            {<<"someTaskId1">>, [], [{<<"status">>, <<"ok">>}, {<<"steps">>, [
                <<"module1:function1">>, <<"module2:function2">>, <<"module3:function3">>
            ]}]},
            {<<"someTaskId2">>, [], [{<<"status">>, <<"running">>}, {<<"steps">>, [
                <<"module1:function1">>, <<"module2:function2">>
            ]}]},
            {<<"someTaskId3">>, [<<"error">>, <<"description">>, <<"steps">>,
                <<"module">>, <<"function">>, <<"hosts">>],
                [{<<"status">>, <<"error">>}]
            },
            {<<"someTaskId4">>, [<<"error">>, <<"description">>],
                [{<<"status">>, <<"error">>}]
            }
        ])
    end, [{oneprovider_hosts, <<>>}, {onezone_hosts, <<>>}]).


patch_should_start_stop_service(Config) ->
    ?run(Config, fun({Host, {Prefix, WorkerService}}) ->
        lists:foreach(fun({Service, Endpoint}) ->
            lists:foreach(fun({Action, StartedParam}) ->
                lists:foreach(fun({Ctx, HostParam}) ->
                    ?assertMatch({ok, 204, _, _},
                        onepanel_test_rest:auth_request(
                            Host, <<Prefix/binary, Endpoint/binary,
                                HostParam/binary, StartedParam/binary>>,
                            patch, ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD
                        )
                    ),
                    ?assertReceivedMatch({service, Service, Action, Ctx}, ?TIMEOUT)
                end, [
                    {#{}, <<>>},
                    {#{hosts => ["someHost"]}, <<"/someHost">>}
                ])
            end, [
                {start, <<"?started=true">>},
                {stop, <<"?started=false">>}
            ])
        end, [
            {couchbase, <<"/databases">>},
            {cluster_manager, <<"/managers">>},
            {WorkerService, <<"/workers">>}
        ])
    end, [
        {oneprovider_hosts, {<<"/provider">>, op_worker}},
        {onezone_hosts, {<<"/zone">>, oz_worker}}
    ]).


put_should_add_storage(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Host, <<Prefix/binary, "/storages">>,
            post, ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD,
            ?STORAGES_JSON
        )),
        ?assertReceivedMatch({service, op_worker, add_storages, #{
            storages := #{
                <<"somePosix">> := #{
                    type := <<"posix">>,
                    mountPoint := <<"someMountPoint">>
                },
                <<"someCeph">> := #{
                    type := <<"ceph">>,
                    clusterName := <<"someName">>,
                    key := <<"someKey">>,
                    monitorHostname := <<"someHostname">>,
                    poolName := <<"someName">>,
                    username := <<"someName">>
                },
                <<"someS3">> := #{
                    type := <<"s3">>,
                    accessKey := <<"someKey">>,
                    bucketName := <<"someName">>,
                    iamHostname := <<"someHostname">>,
                    s3Hostname := <<"someHostname">>,
                    secretKey := <<"someKey">>
                }
            }
        }}, ?TIMEOUT)
    end, [{oneprovider_hosts, <<"/provider">>}]).


put_should_configure_database_service(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        {_, _, Headers, _} = ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/databases">>, post,
                ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD,
                [{hosts, [<<"host1">>, <<"host2">>, <<"host3">>]}]
            )
        ),
        onepanel_test_utils:assert_values(Headers, [
            {<<"location">>, <<"/api/v3/onepanel/tasks/someTaskId">>}
        ]),
        ?assertReceivedMatch({service, couchbase, deploy, #{
            hosts := ["host1", "host2", "host3"]
        }}, ?TIMEOUT)
    end).


put_should_configure_cluster_manager_service(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        {_, _, Headers, _} = ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/managers">>, post,
                ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD,
                [
                    {mainHost, <<"host1">>},
                    {hosts, [<<"host1">>, <<"host2">>, <<"host3">>]}
                ]
            )
        ),
        onepanel_test_utils:assert_values(Headers, [
            {<<"location">>, <<"/api/v3/onepanel/tasks/someTaskId">>}
        ]),
        ?assertReceivedMatch({service, cluster_manager, deploy, #{
            main_host := "host1",
            hosts := ["host1", "host2", "host3"]
        }}, ?TIMEOUT)
    end).


put_should_configure_cluster_worker_service(Config) ->
    ?run(Config, fun({Host, {Prefix, Service}}) ->
        {_, _, Headers, _} = ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/workers">>, post,
                ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD,
                [{hosts, [<<"host1">>, <<"host2">>, <<"host3">>]}]
            )
        ),
        onepanel_test_utils:assert_values(Headers, [
            {<<"location">>, <<"/api/v3/onepanel/tasks/someTaskId">>}
        ]),
        ?assertReceivedMatch({service, Service, deploy, #{
            hosts := ["host1", "host2", "host3"], db_hosts := ["host1", "host2"],
            cm_hosts := ["host2", "host3"], main_cm_host := "host3"
        }}, ?TIMEOUT)
    end, [
        {oneprovider_hosts, {<<"/provider">>, op_worker}},
        {onezone_hosts, {<<"/zone">>, oz_worker}}
    ]).


put_should_configure_onezone_service(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        {_, _, Headers, _} = ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/configuration">>, post,
                ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD,
                [
                    {<<"cluster">>, ?CLUSTER_JSON},
                    {<<"onezone">>, [
                        {<<"name">>, <<"someName">>},
                        {<<"domainName">>, <<"someDomain">>}
                    ]}
                ]
            )
        ),
        onepanel_test_utils:assert_values(Headers, [
            {<<"location">>, <<"/api/v3/onepanel/tasks/someTaskId">>}
        ]),
        ?assertReceivedMatch({service, onezone, deploy, #{
            cluster := #{
                couchbase := #{
                    hosts := ["host1.someDomain", "host2.someDomain"]
                },
                cluster_manager := #{
                    hosts := ["host2.someDomain", "host3.someDomain"],
                    main_host := "host3.someDomain"
                },
                oz_worker := #{
                    cm_hosts := ["host2.someDomain", "host3.someDomain"],
                    db_hosts := ["host1.someDomain", "host2.someDomain"],
                    hosts := ["host1.someDomain", "host2.someDomain", "host3.someDomain"],
                    main_cm_host := "host3.someDomain",
                    onezone_name := <<"someName">>,
                    onezone_domain := <<"someDomain">>
                }
            }
        }}, ?TIMEOUT)
    end, [{onezone_hosts, <<"/zone">>}]).


put_should_configure_oneprovider_service(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        {_, _, Headers, _} = ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/configuration">>, post,
                ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD,
                [
                    {<<"cluster">>, [
                        {<<"storages">>, ?STORAGES_JSON} |
                        ?CLUSTER_JSON
                    ]},
                    {<<"oneprovider">>, [
                        {<<"register">>, true},
                        {<<"name">>, <<"someName">>},
                        {<<"redirectionPoint">>, <<"someUrl">>},
                        {<<"geoLongitude">>, 10.0},
                        {<<"geoLatitude">>, 20.0}
                    ]},
                    {<<"onezone">>, [
                        {<<"domainName">>, <<"someDomain">>}
                    ]}
                ]
            )
        ),
        onepanel_test_utils:assert_values(Headers, [
            {<<"location">>, <<"/api/v3/onepanel/tasks/someTaskId">>}
        ]),
        ?assertReceivedMatch({service, oneprovider, deploy, #{
            cluster := #{
                couchbase := #{
                    hosts := ["host1.someDomain", "host2.someDomain"]
                },
                cluster_manager := #{
                    hosts := ["host2.someDomain", "host3.someDomain"],
                    main_host := "host3.someDomain"
                },
                op_worker := #{
                    cm_hosts := ["host2.someDomain", "host3.someDomain"],
                    db_hosts := ["host1.someDomain", "host2.someDomain"],
                    hosts := ["host1.someDomain", "host2.someDomain", "host3.someDomain"],
                    main_cm_host := "host3.someDomain"
                },
                storages := #{
                    hosts := ["host1.someDomain", "host2.someDomain", "host3.someDomain"],
                    storages := #{
                        <<"somePosix">> := #{
                            type := <<"posix">>,
                            mountPoint := <<"someMountPoint">>
                        },
                        <<"someCeph">> := #{
                            type := <<"ceph">>,
                            clusterName := <<"someName">>,
                            key := <<"someKey">>,
                            monitorHostname := <<"someHostname">>,
                            poolName := <<"someName">>,
                            username := <<"someName">>
                        },
                        <<"someS3">> := #{
                            type := <<"s3">>,
                            accessKey := <<"someKey">>,
                            bucketName := <<"someName">>,
                            iamHostname := <<"someHostname">>,
                            s3Hostname := <<"someHostname">>,
                            secretKey := <<"someKey">>
                        }
                    }
                }
            },
            oneprovider := #{
                hosts := ["host1.someDomain", "host2.someDomain", "host3.someDomain"],
                oneprovider_geo_latitude := 20.0,
                oneprovider_geo_longitude := 10.0,
                oneprovider_name := <<"someName">>,
                oneprovider_redirection_point := <<"someUrl">>,
                oneprovider_register := true,
                onezone_domain := <<"someDomain">>
            }
        }}, ?TIMEOUT)
    end, [{oneprovider_hosts, <<"/provider">>}]).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(etls),
    hackney:start(),
    onepanel_test_utils:init(
        ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json"))
    ).


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


init_per_testcase(method_should_return_not_found_error, Config) ->
    ?assertAllEqual(ok, ?callAll(Config, onepanel_user, new,
        [?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD, admin]
    )),
    Config;

init_per_testcase(get_should_return_service_status, Config) ->
    mock_service_status(init_per_testcase(default, Config), [
        {'node@host1', running},
        {'node@host2', stopped},
        {'node@host3', missing}
    ]);

init_per_testcase(get_should_return_service_host_status, Config) ->
    mock_service_status(init_per_testcase(default, Config), [
        {'node@host1', running}
    ]);

init_per_testcase(get_should_return_storages, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, NewConfig),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_op_worker, get_storages, {[{'node@host1', ?STORAGES_JSON}], []}},
        {task_finished, {service, action, ok}}
    ] end),
    NewConfig;

init_per_testcase(get_should_return_storage, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, NewConfig),
    test_utils:mock_new(Nodes, rest_service),
    test_utils:mock_expect(Nodes, rest_service, exists_resource, fun(Req, _) ->
        {true, Req}
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_op_worker, get_storages, {[{'node@host1', ?STORAGE_JSON}], []}},
        {task_finished, {service, action, ok}}
    ] end),
    NewConfig;

init_per_testcase(get_should_return_service_task_results, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, NewConfig),
    test_utils:mock_expect(Nodes, service, exists_task, fun(_) -> true end),
    test_utils:mock_expect(Nodes, service, get_results, fun
        (<<"someTaskId1">>) -> [
            {module1, function1},
            {module2, function2},
            {module3, function3},
            {task_finished, {service, action, ok}}
        ];
        (<<"someTaskId2">>) -> [
            {module1, function1},
            {module2, function2}
        ];
        (<<"someTaskId3">>) -> [
            {module1, function1},
            {module2, function2},
            {module3, function3, {[], [
                {'node@host1', #error{}}, {'node@host2', #error{}}
            ]}},
            {task_finished, {service, action, #error{}}}
        ];
        (<<"someTaskId4">>) -> [
            {task_finished, {service, action, #error{}}}
        ]
    end),
    NewConfig;

init_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    Self = self(),

    test_utils:mock_new(Nodes, service),
    test_utils:mock_expect(Nodes, service, is_member, fun(_, _) -> true end),
    test_utils:mock_expect(Nodes, service, exists, fun(_) -> true end),
    test_utils:mock_expect(Nodes, service, get, fun
        (couchbase) -> {ok, #service{hosts = ["host1", "host2"]}};
        (cluster_manager) -> {ok, #service{
            hosts = ["host2", "host3"], ctx = #{main_host => "host3"}
        }};
        (op_worker) -> {ok, #service{hosts = ["host1", "host2", "host3"]}}
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {service, action, ok}}]
    end),
    test_utils:mock_expect(Nodes, service, apply_async, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        <<"someTaskId">>
    end),

    ?assertAllEqual(ok, ?callAll(Config, onepanel_user, new,
        [?REG_USER_NAME, ?REG_USER_PASSWORD, regular]
    )),
    ?assertAllEqual(ok, ?callAll(Config, onepanel_user, new,
        [?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD, admin]
    )),
    Config.


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes),
    ?callAll(Config, model, clear, [onepanel_user]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

mock_service_status(Config, HostsStatuses) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, status, _) ->
        [
            {service:get_module(Service), status, {HostsStatuses, []}},
            {task_finished, {service, action, ok}}
        ]
    end),
    Config.
