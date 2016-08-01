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
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2]).

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
        service_oneprovider_unregister_register_test,
        service_oneprovider_modify_details_test,
        service_oneprovider_get_details_test,
        service_oneprovider_get_supported_spaces_test,
        service_op_worker_add_storage_test,
        service_op_worker_get_storages_test,
        services_status_test,
        services_stop_start_test
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
    ?assertEqual([{Node, []}], assert_service_step(
        service:get_module(oneprovider), get_spaces
    )).


service_op_worker_add_storage_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    {ok, Posix} = onepanel_lists:get([storages, posix, '/mnt/st2'], Config),
    {ok, Ceph} = onepanel_lists:get([storages, ceph, 'someCeph'], Config),
    {ok, S3} = onepanel_lists:get([storages, s3, 'someS3'], Config),
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
                s3Hostname => <<"http://", (onepanel_utils:typed_get(host_name,
                    S3, binary))/binary>>,
                iamHostname => <<"http://", (onepanel_utils:typed_get(iam_host,
                    S3, binary))/binary>>
            }
        }
    }),
    assert_service_step(service:get_module(op_worker), add_storages, [Node], ok).


service_op_worker_get_storages_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    Name = <<"somePosix1">>,
    lists:foreach(fun(Ctx) ->
        service_action(Node, op_worker, get_storages, Ctx#{
            hosts => [onepanel_cluster:node_to_host(Node)]
        }),
        Results = assert_service_step(service:get_module(op_worker), get_storages),
        [{_, Storages}] = ?assertMatch([{Node, _}], Results),
        onepanel_test_utils:assert_fields(Storages, [Name]),
        onepanel_test_utils:assert_fields(?config(Name, Storages), [id]),
        onepanel_test_utils:assert_values(?config(Name, Storages), [
            {type, <<"posix">>},
            {mountPoint, onepanel_utils:typed_get(
                [storages, posix, '/mnt/st1', docker_path], Config, binary
            )}
        ])
    end, [#{}, #{name => Name}]).


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
    NewConfig = onepanel_test_utils:init(
        ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json"))
    ),
    [OzNode | _] = OzNodes = ?config(onezone_nodes, NewConfig),
    OzHosts = onepanel_cluster:nodes_to_hosts(OzNodes),
    service_action(OzNode, ?SERVICE_OZ, deploy, #{
        cluster => #{
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
    [OpNode | _] = OpNodes = ?config(oneprovider_nodes, NewConfig),
    OpHosts = onepanel_cluster:nodes_to_hosts(OpNodes),
    {ok, Posix} = onepanel_lists:get([storages, posix, '/mnt/st1'], NewConfig),
    service_action(OpNode, ?SERVICE_OP, deploy, #{
        cluster => #{
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
    NewConfig.


end_per_suite(Config) ->
    Config.%test_node_starter:clean_environment(Config).


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