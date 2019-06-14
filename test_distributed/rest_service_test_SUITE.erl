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
-include("modules/onepanel_dns.hrl").
-include("onepanel_test_utils.hrl").
-include("onepanel_test_rest.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2,
    end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    method_should_return_unauthorized_error/1,
    method_should_return_forbidden_error/1,
    method_should_return_service_unavailable_error/1,
    method_should_return_not_found_error/1,
    get_should_return_service_status/1,
    get_should_return_service_host_status/1,
    get_should_return_service_task_results/1,
    get_should_return_nagios_response/1,
    get_should_return_dns_check/1,
    patch_should_start_stop_service/1,
    post_should_configure_database_service/1,
    post_should_configure_cluster_manager_service/1,
    post_should_configure_cluster_worker_service/1,
    post_should_configure_oneprovider_service/1,
    post_should_configure_onezone_service/1,
    post_should_return_conflict_on_configured_onezone/1,
    post_should_return_conflict_on_configured_oneprovider/1
]).

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
    {<<"/configuration">>, get},
    {<<"/configuration">>, post},
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

-define(CLUSTER_JSON, #{
    <<"domainName">> => <<"someDomain">>,
    <<"nodes">> => #{
        <<"n1">> => #{<<"hostname">> => <<"host1">>},
        <<"n2">> => #{<<"hostname">> => <<"host2">>},
        <<"n3">> => #{<<"hostname">> => <<"host3">>}
    },
    <<"databases">> => #{
        <<"nodes">> => [<<"n1">>, <<"n2">>]
    },
    <<"managers">> => #{
        <<"mainNode">> => <<"n3">>,
        <<"nodes">> => [<<"n2">>, <<"n3">>]
    },
    <<"workers">> => #{
        <<"nodes">> => [<<"n1">>, <<"n2">>, <<"n3">>]
    }
}).


-define(SOME_IP_STR1, "127.0.0.1").
-define(SOME_IP_STR2, "10.0.0.2").

-define(DNS_CHECK_TIMESTAMP, 1500000000).
-define(DNS_CHECK_JSON_OP, #{
    <<"timestamp">> => time_utils:epoch_to_iso8601(?DNS_CHECK_TIMESTAMP),
    <<"domain">> => #{
        <<"summary">> => <<"ok">>,
        <<"expected">> => [<<?SOME_IP_STR1>>],
        <<"got">> => [<<?SOME_IP_STR1>>],
        <<"recommended">> => []
    }
}).

-define(DNS_CHECK_JSON_OZ, #{
    <<"timestamp">> => time_utils:epoch_to_iso8601(?DNS_CHECK_TIMESTAMP),
    <<"domain">> => #{
        <<"summary">> => <<"ok">>,
        <<"expected">> => [<<?SOME_IP_STR1>>],
        <<"got">> => [<<?SOME_IP_STR1>>],
        <<"recommended">> => []
    },
    <<"dnsZone">> => #{
        <<"summary">> => <<"bad_records">>,
        <<"expected">> => [<<?SOME_IP_STR1>>],
        <<"got">> => [<<?SOME_IP_STR2>>],
        <<"recommended">> => []
    }
}).

-define(NAGIOS_REPORT_XML, <<"<?xml version=\"1.0\"?>"
"<healthdata status=\"ok\">"
"</healthdata>">>
).

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
        method_should_return_service_unavailable_error,
        method_should_return_not_found_error,
        get_should_return_service_status,
        get_should_return_service_host_status,
        get_should_return_service_task_results,
        get_should_return_nagios_response,
        get_should_return_dns_check,
        patch_should_start_stop_service,
        post_should_configure_database_service,
        post_should_configure_cluster_manager_service,
        post_should_configure_cluster_worker_service,
        post_should_configure_oneprovider_service,
        post_should_configure_onezone_service,
        post_should_return_conflict_on_configured_onezone,
        post_should_return_conflict_on_configured_oneprovider
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
                {<<"someUser">>, <<"somePassword">>}
            ))
        end, ?COMMON_ENDPOINTS_WITH_METHODS)
    end).


method_should_return_forbidden_error(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        lists:foreach(fun({Endpoint, Method}) ->
            Auths = ?OZ_AUTHS(Host, privileges:cluster_admin() -- [?CLUSTER_UPDATE]),
            ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, Endpoint/binary>>, Method, ?OZ_AUTHS(Host, Auths)
            ))
        end, [{E, M} || {E, M} <- ?COMMON_ENDPOINTS_WITH_METHODS, M /= get])
    end).


method_should_return_service_unavailable_error(Config) ->
    ?run(Config, fun
        ({Host, <<"/provider">> = Prefix}) ->
            lists:foreach(fun({Endpoint, Method}) ->
                ?assertMatch({ok, 503, _, _}, onepanel_test_rest:auth_request(
                    Host, <<Prefix/binary, Endpoint/binary>>, Method,
                    ?ALL_AUTHS(Host)
                ))
            end, [
                {<<"/storages">>, get},
                {<<"/storages">>, post},
                {<<"/storages/somePosixId">>, get},
                {<<"/storages/somePosixId">>, patch},
                {<<"/storages/somePosixId/invalidate_luma">>, patch}
            ]);
        ({_, <<"/zone">>}) -> ok
    end).


method_should_return_not_found_error(Config) ->
    ?run(Config, fun({Host, _}) ->
        ?assertMatch({ok, 404, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/tasks/someTaskId">>, get,
            ?OZ_OR_ROOT_AUTHS(Host, [])
        ))
    end, [{oneprovider_hosts, <<>>}, {onezone_hosts, <<>>}]),

    ?run(Config, fun({Host, Prefix}) ->
        lists:foreach(fun({Endpoint, Method}) ->
            ?assertMatch({ok, 404, _, _}, onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, Endpoint/binary>>, Method,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
            ))
        end, ?COMMON_HOST_ENDPOINTS_WITH_METHODS)
    end).


get_should_return_service_status(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        lists:foreach(fun(Endpoint) ->
            {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
                onepanel_test_rest:auth_request(
                    Host, <<Prefix/binary, Endpoint/binary>>, get,
                    ?OZ_OR_ROOT_AUTHS(Host, [])
                )
            ),
            onepanel_test_rest:assert_body_values(JsonBody, [
                {<<"host1">>, <<"healthy">>},
                {<<"host2">>, <<"stopped">>},
                {<<"host3">>, <<"missing">>}
            ])
        end, [<<"/databases">>, <<"/managers">>, <<"/workers">>])
    end).


get_should_return_service_host_status(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        lists:foreach(fun({Endpoint, QueryHost}) ->
            {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
                onepanel_test_rest:auth_request(
                    Host, <<Prefix/binary, Endpoint/binary, QueryHost/binary>>, get,
                    ?OZ_OR_ROOT_AUTHS(Host, [])
                )
            ),
            onepanel_test_rest:assert_body(JsonBody, <<"healthy">>)
        end, [
            {<<"/databases">>, <<"/host1">>},
            {<<"/managers">>, <<"/host2">>},
            {<<"/workers">>, <<"/host2">>}
        ])
    end).


get_should_return_service_task_results(Config) ->
    ?run(Config, fun({Host, _}) ->
        lists:foreach(fun({TaskId, Fields, Values}) ->
            {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
                onepanel_test_rest:auth_request(
                    Host, <<"/tasks/", TaskId/binary>>, get,
                    ?OZ_OR_ROOT_AUTHS(Host, [])
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


get_should_return_nagios_response(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        ?assertMatch({ok, 200, _, ?NAGIOS_REPORT_XML},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/nagios">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        )
    end, [
        {oneprovider_hosts, <<"/provider">>},
        {onezone_hosts, <<"/zone">>}
    ]).


get_should_return_dns_check(Config) ->
    [OpHost | _] = ?config(oneprovider_hosts, Config),
    [OzHost | _] = ?config(onezone_hosts, Config),

    {_, _, _, OpJsonBody} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(
            OpHost, <<"/dns_check">>, get,
            ?OZ_OR_ROOT_AUTHS(OpHost, [])
        )
    ),
    onepanel_test_rest:assert_body(OpJsonBody, ?DNS_CHECK_JSON_OP),

    {_, _, _, OzJsonBody} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(
            OzHost, <<"/dns_check">>, get,
            ?OZ_OR_ROOT_AUTHS(OzHost, [])
        )
    ),
    onepanel_test_rest:assert_body(OzJsonBody, ?DNS_CHECK_JSON_OZ).


patch_should_start_stop_service(Config) ->
    ?run(Config, fun({Host, {Prefix, WorkerService}}) ->
        lists:foreach(fun({Service, Endpoint}) ->
            lists:foreach(fun({Action, StartedParam}) ->
                lists:foreach(fun({Ctx, HostParam}) ->
                    ?assertMatch({ok, 204, _, _},
                        onepanel_test_rest:auth_request(
                            Host, <<Prefix/binary, Endpoint/binary,
                                HostParam/binary, StartedParam/binary>>,
                            patch, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
                        )
                    ),
                    ?assertReceivedMatch({service, Service, Action, Ctx}, ?TIMEOUT)
                end, [
                    {#{}, <<>>},
                    {#{hosts => ["host2"]}, <<"/host2">>}
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


post_should_configure_database_service(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        {_, _, Headers, _} = ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/databases">>, post,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                #{hosts => [<<"host1">>, <<"host2">>, <<"host3">>]}
            )
        ),
        onepanel_test_utils:assert_values(Headers, [
            {<<"location">>, <<"/api/v3/onepanel/tasks/someTaskId">>}
        ]),
        ?assertReceivedMatch({service, couchbase, deploy, #{
            hosts := ["host1", "host2", "host3"]
        }}, ?TIMEOUT)
    end).


post_should_configure_cluster_manager_service(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        {_, _, Headers, _} = ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/managers">>, post,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                #{
                    mainHost => <<"host1">>,
                    hosts => [<<"host1">>, <<"host2">>, <<"host3">>]
                }
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


post_should_configure_cluster_worker_service(Config) ->
    ?run(Config, fun({Host, {Prefix, Service}}) ->
        {_, _, Headers, _} = ?assertMatch({ok, 204, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/workers">>, post,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                #{hosts => [<<"host1">>, <<"host2">>, <<"host3">>]}
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


post_should_configure_onezone_service(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        {_, _, Headers, _} = ?assertMatch({ok, 201, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/configuration">>, post,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                #{
                    <<"cluster">> => ?CLUSTER_JSON,
                    <<"onezone">> => #{
                        <<"name">> => <<"someName">>,
                        <<"domainName">> => <<"someDomain">>,
                        <<"policies">> => #{
                            <<"subdomainDelegation">> => false,
                            <<"guiPackageVerification">> => false,
                            <<"harvesterGuiPackageVerification">> => false
                        }
                    },
                    <<"onepanel">> => #{
                        <<"guiDebugMode">> => true
                    }
                }
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
                    onezone_domain := <<"someDomain">>,
                    gui_debug_mode := true,
                    policies := #{
                        subdomain_delegation := false,
                        gui_package_verification := false,
                        harvester_gui_package_verification := false
                    }
                },
                onepanel := #{
                    gui_debug_mode := true
                }
            }
        }}, ?TIMEOUT)
    end, [{onezone_hosts, <<"/zone">>}]).


post_should_configure_oneprovider_service(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        {_, _, Headers, _} = ?assertMatch({ok, 201, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/configuration">>, post,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                #{
                    <<"cluster">> => maps:merge(#{
                        <<"storages">> => ?STORAGES_JSON},
                        ?CLUSTER_JSON),
                    <<"oneprovider">> => #{
                        <<"register">> => true,
                        <<"token">> => <<"someToken">>,
                        <<"name">> => <<"someName">>,
                        <<"subdomainDelegation">> => false,
                        <<"domain">> => <<"someDomain">>,
                        <<"adminEmail">> => <<"admin@onedata.org">>,
                        <<"geoLongitude">> => <<"10">>,
                        <<"geoLatitude">> => <<"20.0">>
                    },
                    <<"onepanel">> => #{
                        <<"guiDebugMode">> => true
                    }
                }
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
                            hostname := <<"someHostname">>,
                            secretKey := <<"someKey">>
                        }
                    }
                },
                onepanel := #{
                    gui_debug_mode := true
                }
            },
            oneprovider := #{
                hosts := ["host1.someDomain", "host2.someDomain", "host3.someDomain"],
                oneprovider_geo_latitude := 20.0,
                oneprovider_geo_longitude := 10.0,
                oneprovider_name := <<"someName">>,
                oneprovider_domain := <<"someDomain">>,
                oneprovider_register := true,
                oneprovider_token := <<"someToken">>
            }
        }}, ?TIMEOUT)
    end, [{oneprovider_hosts, <<"/provider">>}]).


post_should_return_conflict_on_configured_onezone(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        ?assertMatch({ok, 409, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/configuration">>, post,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                #{
                    <<"cluster">> => ?CLUSTER_JSON,
                    <<"onezone">> => #{
                        <<"name">> => <<"someName">>,
                        <<"domainName">> => <<"someDomain">>
                    }
                }
            )
        )
    end, [{onezone_hosts, <<"/zone">>}]).


post_should_return_conflict_on_configured_oneprovider(Config) ->
    ?run(Config, fun({Host, Prefix}) ->
        ?assertMatch({ok, 409, _, _},
            onepanel_test_rest:auth_request(
                Host, <<Prefix/binary, "/configuration">>, post,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                #{
                    <<"cluster">> => maps:merge(#{
                        <<"storages">> => ?STORAGES_JSON},
                        ?CLUSTER_JSON),
                    <<"oneprovider">> => #{
                        <<"register">> => true,
                        <<"name">> => <<"someName">>,
                        <<"subdomainDelegation">> => false,
                        <<"domain">> => <<"someDomain">>,
                        <<"adminEmail">> => <<"admin@onedata.org">>,
                        <<"geoLongitude">> => <<"10">>,
                        <<"geoLatitude">> => <<"20.0">>
                    },
                    <<"onezone">> => #{
                        <<"domainName">> => <<"someDomain">>
                    }
                }
            )
        )
    end, [{oneprovider_hosts, <<"/provider">>}]).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) -> onepanel_test_utils:init(NewConfig) end,
    [{?LOAD_MODULES, [onepanel_test_rest]}, {?ENV_UP_POSTHOOK, Posthook} | Config].

init_per_testcase(method_should_return_service_unavailable_error, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_expect(Nodes, service, all_healthy, fun() -> false end),
    NewConfig;

init_per_testcase(method_should_return_not_found_error, Config) ->
    onepanel_test_rest:set_default_passphrase(Config),
    onepanel_test_rest:mock_token_authentication(Config),
    Config;

init_per_testcase(get_should_return_service_status, Config) ->
    mock_service_status(init_per_testcase(default, Config), [
        {'node@host1', healthy},
        {'node@host2', stopped},
        {'node@host3', missing}
    ]);

init_per_testcase(get_should_return_service_host_status, Config) ->
    mock_service_status(init_per_testcase(default, Config), [
        {'node@host1', healthy}
    ]);

init_per_testcase(get_should_return_nagios_response, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, NewConfig),
    test_utils:mock_new(Nodes, rest_service),
    test_utils:mock_expect(Nodes, rest_service, exists_resource, fun(Req, _) ->
        {true, Req}
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_op_worker, get_nagios_response, {
            [{'node@host1', {ok, 200, #{}, ?NAGIOS_REPORT_XML}}], []
        }},
        {service_oz_worker, get_nagios_response, {
            [{'node@host1', {ok, 200, #{}, ?NAGIOS_REPORT_XML}}], []
        }},
        {task_finished, {service, action, ok}}
    ] end),
    NewConfig;

init_per_testcase(get_should_return_dns_check, Config) ->
    Nodes = ?config(all_nodes, Config),
    OpHosts = ?config(oneprovider_hosts, Config),
    OzHosts = ?config(onezone_hosts, Config),
    OpNodes = ?config(oneprovider_nodes, Config),
    OzNodes = ?config(onezone_nodes, Config),

    test_utils:mock_new(Nodes, [model, onepanel_deployment, service,
        service_oneprovider, dns_check]),
    test_utils:mock_expect(Nodes, model, exists,
        fun(_) -> true end),
    test_utils:mock_expect(Nodes, onepanel_deployment, is_set,
        fun(_) -> true end),
    test_utils:mock_expect(Nodes, service, get_hosts, fun
        (op_worker) -> OpHosts;
        (oz_worker) -> OzHosts
    end),
    test_utils:mock_expect(OpNodes, service_oneprovider, is_registered, fun() -> true end),
    test_utils:mock_expect(OpNodes, dns_check, get, fun
        (op_worker, _) -> #{
            timestamp => ?DNS_CHECK_TIMESTAMP,
            domain => #dns_check{
                summary = ok, expected = [?SOME_IP_STR1], got = [?SOME_IP_STR1],
                bind_records = []
            }
        } end),

    test_utils:mock_expect(OzNodes, dns_check, get, fun
        (oz_worker, _) -> #{
            timestamp => ?DNS_CHECK_TIMESTAMP,
            domain => #dns_check{
                summary = ok, expected = [?SOME_IP_STR1], got = [?SOME_IP_STR1],
                bind_records = []
            },
            dnsZone => #dns_check{
                summary = bad_records, expected = [?SOME_IP_STR1], got = [?SOME_IP_STR2],
                bind_records = []
            }
        } end),

    onepanel_test_rest:set_default_passphrase(Config),
    onepanel_test_rest:mock_token_authentication(Nodes),
    Config;

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

init_per_testcase(Case, Config) when
    Case == post_should_return_conflict_on_configured_onezone;
    Case == post_should_return_conflict_on_configured_oneprovider ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_new(Nodes, onepanel_deployment),
    test_utils:mock_expect(Nodes, onepanel_deployment, is_set, fun(_) -> true end),
    init_per_testcase(default, Config);

init_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    OzNodes = ?config(onezone_nodes, Config),
    Self = self(),
    OzDomain = onepanel_test_utils:get_domain(hosts:from_node(hd(OzNodes))),

    test_utils:mock_new(Nodes, [service, service_oz_worker, service_oneprovider]),
    test_utils:mock_expect(Nodes, service_oneprovider, is_registered, fun() -> true end),

    GetDetails = fun() -> #{name => <<"zoneName">>, domain => OzDomain} end,
    test_utils:mock_expect(Nodes, service_oz_worker, get_details, GetDetails),
    test_utils:mock_expect(Nodes, service_oz_worker, get_details,
        fun(#{}) -> GetDetails() end),

    test_utils:mock_expect(Nodes, service, exists, fun(_) -> true end),
    test_utils:mock_expect(Nodes, service, get, fun
        (couchbase) -> {ok, #service{hosts = ["host1", "host2"]}};
        (cluster_manager) -> {ok, #service{
            hosts = ["host2", "host3"], ctx = #{main_host => "host3"}
        }};
        (op_worker) -> {ok, #service{hosts = ["host1", "host2", "host3"]}};
        (oz_worker) -> {ok, #service{hosts = ["host1", "host2", "host3"]}}
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {service, action, ok}}]
    end),
    test_utils:mock_expect(Nodes, service, apply_async, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        <<"someTaskId">>
    end),

    onepanel_test_rest:set_default_passphrase(Config),
    onepanel_test_rest:mock_token_authentication(Nodes),
    Config.


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes),
    ?callAll(Config, model, clear, [onepanel_user]).

end_per_suite(_Config) ->
    ok.

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
