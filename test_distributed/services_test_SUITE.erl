%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of onedata services.
%%% @end
%%%--------------------------------------------------------------------
-module(services_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/models.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2]).

%% tests
-export([
    service_oneprovider_unregister_register_test/1,
    service_oneprovider_modify_details_test/1,
    service_oneprovider_get_details_test/1,
    service_oneprovider_get_supported_spaces_test/1,
    service_op_worker_add_storage_test/1,
    service_op_worker_get_storages_test/1,
    services_status_test/1,
    services_stop_start_test/1
]).

-define(SERVICE_OPA, service_onepanel:name()).
-define(SERVICE_OP, service_oneprovider:name()).
-define(SERVICE_OZ, service_onezone:name()).
-define(SERVICE_CB, service_couchbase:name()).
-define(SERVICE_CM, service_cluster_manager:name()).
-define(SERVICE_OPW, service_op_worker:name()).
-define(SERVICE_OZW, service_oz_worker:name()).
-define(TIMEOUT, timer:seconds(5)).

-define(run(Config, Function, HostsType), begin
    lists:foreach(fun(_Type_) ->
        Function(hd(?config(_Type_, Config)))
    end, HostsType)
end).

all() ->
    ?ALL([
%%        service_oneprovider_unregister_register_test, todo VFS-3339
        service_oneprovider_modify_details_test,
        service_oneprovider_get_details_test,
        service_oneprovider_get_supported_spaces_test,
        service_op_worker_get_storages_test,
        service_op_worker_add_storage_test,
        services_status_test
%%        services_stop_start_test todo VFS-3347
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

service_oneprovider_unregister_register_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    service_action(Node, oneprovider, unregister, #{}),
    service_action(Node, oneprovider, register, #{
        oneprovider_geo_latitude => 20.0,
        oneprovider_geo_longitude => 20.0,
        oneprovider_name => <<"provider2">>,
        oneprovider_redirection_point => onepanel_utils:join(
            ["https://", onepanel_cluster:node_to_host(Node)]
        )
    }).


service_oneprovider_modify_details_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    RedirectionPoint = onepanel_utils:join(
        ["https://", onepanel_cluster:node_to_host(Node)]
    ),
    service_action(Node, oneprovider, modify_details, #{
        oneprovider_geo_latitude => 30.0,
        oneprovider_geo_longitude => 40.0,
        oneprovider_name => <<"provider3">>,
        oneprovider_redirection_point => RedirectionPoint
    }),
    service_action(Node, oneprovider, get_details, #{
        hosts => [onepanel_cluster:node_to_host(Node)]
    }),
    Results = assert_service_step(service:get_module(oneprovider), get_details),
    [{_, Details}] = ?assertMatch([{Node, _}], Results),
    onepanel_test_utils:assert_fields(Details,
        [id, name, redirectionPoint, urls, geoLatitude, geoLongitude]
    ),
    onepanel_test_utils:assert_values(Details, [
        {name, <<"provider3">>},
        {redirectionPoint, RedirectionPoint},
        {geoLatitude, 30.0},
        {geoLongitude, 40.0}
    ]).


service_oneprovider_get_details_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    service_action(Node, oneprovider, get_details, #{
        hosts => [onepanel_cluster:node_to_host(Node)]
    }),
    Results = assert_service_step(service:get_module(oneprovider), get_details),
    [{_, Details}] = ?assertMatch([{Node, _}], Results),
    onepanel_test_utils:assert_fields(Details,
        [id, name, redirectionPoint, urls, geoLatitude, geoLongitude]
    ).


service_oneprovider_get_supported_spaces_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    service_action(Node, oneprovider, get_spaces, #{
        hosts => [onepanel_cluster:node_to_host(Node)]
    }),
    ?assertEqual([{Node, [{ids, []}]}], assert_service_step(
        service:get_module(oneprovider), get_spaces
    )).


service_op_worker_get_storages_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    Ctx = #{hosts => [onepanel_cluster:node_to_host(Node)]},
    service_action(Node, op_worker, get_storages, Ctx),
    Results = assert_service_step(service:get_module(op_worker), get_storages),
    [{Node, [{ids, [Id]}]}] = ?assertMatch([{Node, [{ids, [_]}]}], Results),

    service_action(Node, op_worker, get_storages, Ctx#{id => Id}),
    Results2 = assert_service_step(service:get_module(op_worker), get_storages),
    [{Node, Storage}] = ?assertMatch([{Node, _}], Results2),
    onepanel_test_utils:assert_values(Storage, [
        {id, Id},
        {name, <<"somePosix1">>},
        {type, <<"posix">>},
        {<<"mountPoint">>, onepanel_utils:typed_get(
            [storages, posix, '/mnt/st1', docker_path], Config, binary
        )}
    ]).


service_op_worker_add_storage_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    {ok, Posix} = onepanel_lists:get([storages, posix, '/mnt/st2'], Config),
    {ok, Ceph} = onepanel_lists:get([storages, ceph, someCeph], Config),
    {ok, S3} = onepanel_lists:get([storages, s3, someS3], Config),
    {ok, Swift} = onepanel_lists:get([storages, swift, someSwift], Config),
    {ok, Glusterfs} = onepanel_lists:get([storages, glusterfs, someGlusterfs], Config),
    service_action(Node, op_worker, add_storages, #{
        hosts => [hd(?config(oneprovider_hosts, Config))],
        storages => #{
            <<"somePosix2">> => #{
                type => <<"posix">>,
                mountPoint => onepanel_utils:typed_get(docker_path, Posix, binary)
            },
            <<"someCeph">> => #{
                type => <<"ceph">>,
                clusterName => <<"ceph">>,
                key => onepanel_utils:typed_get(key, Ceph, binary),
                monitorHostname => onepanel_utils:typed_get(host_name, Ceph, binary),
                poolName => <<"onedata">>,
                username => onepanel_utils:typed_get(username, Ceph, binary)
            },
            <<"someS3">> => #{
                type => <<"s3">>,
                accessKey => onepanel_utils:typed_get(access_key, S3, binary),
                secretKey => onepanel_utils:typed_get(secret_key, S3, binary),
                bucketName => <<"onedata">>,
                hostname => <<"http://", (onepanel_utils:typed_get(host_name,
                    S3, binary))/binary>>
            },
            <<"someSwift">> => #{
                type => <<"swift">>,
                username => onepanel_utils:typed_get(user_name, Swift, binary),
                password => onepanel_utils:typed_get(password, Swift, binary),
                authUrl => onepanel_utils:join(["http://",
                    onepanel_utils:typed_get(host_name, Swift, binary), ":",
                    onepanel_utils:typed_get(keystone_port, Swift, binary), "/v2.0/tokens"]),
                tenantName => onepanel_utils:typed_get(tenant_name, Swift, binary),
                containerName => <<"swift">>
            },
            <<"someGluster">> => #{
                type => <<"glusterfs">>,
                volume => <<"data">>,
                hostname => onepanel_utils:typed_get(host_name, Glusterfs, binary),
                port => onepanel_utils:typed_get(port, Glusterfs, binary),
                transport => onepanel_utils:typed_get(transport, Glusterfs, binary),
                mountPoint => onepanel_utils:typed_get(mountpoint, Glusterfs, binary),
                xlatorOptions => <<"cluster.write-freq-threshold=100;">>
            }
        }
    }),
    assert_service_step(service:get_module(op_worker), add_storages, [Node], ok).


services_status_test(Config) ->
    lists:foreach(fun({Nodes, MainService, Services}) ->
        lists:foreach(fun(Service) ->
            SModule = service:get_module(Service),
            lists:foreach(fun(Node) ->
                service_host_action(Node, Service, status),
                assert_service_step(SModule, status, [Node], running)
            end, Nodes),

            service_action(hd(Nodes), Service, status),
            assert_service_step(SModule, status, Nodes, running)
        end, Services),

        service_action(hd(Nodes), MainService, status),
        lists:foreach(fun(Service) ->
            SModule = service:get_module(Service),
            assert_service_step(SModule, status, Nodes, running)
        end, Services)
    end, [
        {?config(onezone_nodes, Config), ?SERVICE_OZ,
            [?SERVICE_CB, ?SERVICE_CM, ?SERVICE_OZW]},
        {?config(oneprovider_nodes, Config), ?SERVICE_OP,
            [?SERVICE_CB, ?SERVICE_CM, ?SERVICE_OPW]}
    ]).


services_stop_start_test(Config) ->
    ActionsWithResults = [
        {stop, ok}, {status, stopped}, {start, ok}, {status, running}
    ],

    lists:foreach(fun({Nodes, MainService, Services}) ->
        lists:foreach(fun(Service) ->
            SModule = service:get_module(Service),

            lists:foreach(fun(Node) ->
                lists:foreach(fun({Action, Result}) ->
                    service_host_action(Node, Service, Action),
                    assert_service_step(SModule, Action, [Node], Result)
                end, ActionsWithResults)
            end, Nodes),

            lists:foreach(fun({Action, Result}) ->
                service_action(hd(Nodes), Service, Action),
                assert_service_step(SModule, Action, Nodes, Result)
            end, ActionsWithResults)
        end, Services),

        lists:foreach(fun({Action, Result}) ->
            service_action(hd(Nodes), MainService, Action),
            lists:foreach(fun(Service) ->
                SModule = service:get_module(Service),
                assert_service_step(SModule, Action, Nodes, Result)
            end, Services)
        end, ActionsWithResults)
    end, [
        {?config(onezone_nodes, Config), ?SERVICE_OZ,
            [?SERVICE_CB, ?SERVICE_CM, ?SERVICE_OZW]},
        {?config(oneprovider_nodes, Config), ?SERVICE_OP,
            [?SERVICE_CB, ?SERVICE_CM, ?SERVICE_OPW]}
    ]).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    Posthook = fun(NewConfig) ->
        NewConfig2 = onepanel_test_utils:init(NewConfig),
        [OzNode | _] = OzNodes = ?config(onezone_nodes, NewConfig2),
        OzHosts = onepanel_cluster:nodes_to_hosts(OzNodes),
        service_action(OzNode, ?SERVICE_OZ, deploy, #{
            cluster => #{
                ?SERVICE_OPA => #{
                    hosts => OzHosts
                },
                ?SERVICE_CB => #{
                    hosts => OzHosts
                },
                ?SERVICE_CM => #{
                    hosts => OzHosts, main_cm_host => hd(OzHosts), worker_num => 2
                },
                ?SERVICE_OZW => #{
                    hosts => OzHosts, main_cm_host => hd(OzHosts),
                    cm_hosts => OzHosts, db_hosts => OzHosts
                }
            }
        }),
        [OpNode | _] = OpNodes = ?config(oneprovider_nodes, NewConfig2),
        OpHosts = onepanel_cluster:nodes_to_hosts(OpNodes),
        {ok, Posix} = onepanel_lists:get([storages, posix, '/mnt/st1'], NewConfig2),
        service_action(OpNode, ?SERVICE_OP, deploy, #{
            cluster => #{
                ?SERVICE_OPA => #{
                    hosts => OpHosts
                },
                ?SERVICE_CB => #{
                    hosts => OpHosts
                },
                ?SERVICE_CM => #{
                    hosts => OpHosts, main_cm_host => hd(OpHosts), worker_num => 2
                },
                ?SERVICE_OPW => #{
                    hosts => OpHosts, main_cm_host => hd(OpHosts),
                    cm_hosts => OpHosts, db_hosts => OpHosts
                },
                storages => #{
                    hosts => OpHosts,
                    storages => #{
                        <<"somePosix1">> => #{
                            type => <<"posix">>,
                            mountPoint => onepanel_utils:typed_get(
                                docker_path, Posix, binary
                            )
                        }
                    }
                }
            },
            ?SERVICE_OP => #{
                hosts => OpHosts,
                oneprovider_geo_latitude => 10.0,
                oneprovider_geo_longitude => 10.0,
                oneprovider_name => <<"provider1">>,
                oneprovider_redirection_point => onepanel_utils:join(
                    ["https://", hd(OpHosts)]
                ),
                oneprovider_register => true,
                onezone_domain => hd(OzHosts)
            }
        }),
        NewConfig2
    end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(_Case, Config) ->
    onepanel_test_utils:clear_msg_inbox(),
    Config.

%%%===================================================================
%%% Internal functions
%%%===================================================================

service_host_action(Node, Service, Action) ->
    service_host_action(Node, Service, Action, #{}).


service_host_action(Node, Service, Action, Ctx) ->
    Host = onepanel_cluster:node_to_host(Node),
    service_action(Node, Service, Action, Ctx#{hosts => [Host]}).


service_action(Node, Service, Action) ->
    service_action(Node, Service, Action, #{}).


service_action(Node, Service, Action, Ctx) ->
    Self = self(),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [Service, Action, Ctx, Self]
    )).

assert_service_step(Module, Function) ->
    {_, {_, _, {Results, _}}} = ?assertReceivedMatch(
        {step_end, {Module, Function, {_, []}}}, ?TIMEOUT
    ),
    Results.

assert_service_step(Module, Function, Nodes, Result) ->
    Results = assert_service_step(Module, Function),
    onepanel_test_utils:assert_values(Results, lists:zip(
        Nodes, lists:duplicate(erlang:length(Nodes), Result)
    )).
