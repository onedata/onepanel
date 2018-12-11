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
-export([all/0, init_per_suite/1, init_per_testcase/2,
    end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    service_oneprovider_unregister_register_test/1,
    service_oneprovider_modify_details_test/1,
    service_oneprovider_get_details_test/1,
    service_oneprovider_get_supported_spaces_test/1,
    service_op_worker_add_storage_test/1,
    service_op_worker_get_storages_test/1,
    service_op_worker_update_storage_test/1,
    services_status_test/1,
    services_stop_start_test/1
]).

-define(SERVICE_OPA, service_onepanel:name()).
-define(SERVICE_OP, service_oneprovider:name()).
-define(SERVICE_LE, service_letsencrypt:name()).
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

-define(AWAIT_OZ_CONNECTIVITY_ATTEMPTS, 30).

all() ->
    ?ALL([
        service_oneprovider_unregister_register_test,
        service_oneprovider_modify_details_test,
        service_oneprovider_get_details_test,
        service_oneprovider_get_supported_spaces_test,
        service_op_worker_get_storages_test,
        service_op_worker_add_storage_test,
        service_op_worker_update_storage_test,
        services_status_test
        %% TODO VFS-4056
        %% services_stop_start_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

service_oneprovider_unregister_register_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    onepanel_test_utils:service_action(Node, oneprovider, unregister, #{}),
    onepanel_test_utils:service_action(Node, oneprovider, register, #{
        oneprovider_geo_latitude => 20.0,
        oneprovider_geo_longitude => 20.0,
        oneprovider_name => <<"provider2">>,
        oneprovider_domain => onepanel_cluster:node_to_host(Node),
        oneprovider_admin_email => <<"admin@onedata.org">>
    }).


service_oneprovider_modify_details_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    Domain = list_to_binary(onepanel_cluster:node_to_host(Node)),
    onepanel_test_utils:service_action(Node, oneprovider, modify_details, #{
        oneprovider_geo_latitude => 30.0,
        oneprovider_geo_longitude => 40.0,
        oneprovider_name => <<"provider3">>,
        oneprovider_subdomain_delegation => false,
        oneprovider_domain => Domain,
        oneprovider_admin_email => <<"admin@onedata.org">>
    }),

    % wait for graph sync to converge
    % (won't be necessary when modification is done via graph sync - VFS-4644)
    timer:sleep(timer:seconds(20)),

    onepanel_test_utils:service_action(Node, oneprovider, get_details, #{
        hosts => [onepanel_cluster:node_to_host(Node)]
    }),
    Results = assert_service_step(service:get_module(oneprovider), get_details),
    [{_, Details}] = ?assertMatch([{Node, _}], Results),
    onepanel_test_utils:assert_fields(Details,
        [id, name, subdomainDelegation, domain, adminEmail, geoLatitude, geoLongitude]
    ),
    onepanel_test_utils:assert_values(Details, [
        {name, <<"provider3">>},
        {subdomainDelegation, false},
        {domain, Domain},
        {adminEmail, <<"admin@onedata.org">>},
        {geoLatitude, 30.0},
        {geoLongitude, 40.0}
    ]).


service_oneprovider_get_details_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    onepanel_test_utils:service_action(Node, oneprovider, get_details, #{
        hosts => [onepanel_cluster:node_to_host(Node)]
    }),
    Results = assert_service_step(service:get_module(oneprovider), get_details),
    [{_, Details}] = ?assertMatch([{Node, _}], Results),
    onepanel_test_utils:assert_fields(Details,
        [id, name, domain, adminEmail, geoLatitude, geoLongitude]
    ).


service_oneprovider_get_supported_spaces_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    onepanel_test_utils:service_action(Node, oneprovider, get_spaces, #{
        hosts => [onepanel_cluster:node_to_host(Node)]
    }),
    ?assertEqual([{Node, [{ids, []}]}], assert_service_step(
        service:get_module(oneprovider), get_spaces
    )).


service_op_worker_get_storages_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    Ctx = #{hosts => [onepanel_cluster:node_to_host(Node)]},
    onepanel_test_utils:service_action(Node, op_worker, get_storages, Ctx),
    Results = assert_service_step(service:get_module(op_worker), get_storages),
    [{Node, #{ids := [Id]}}] = ?assertMatch([{Node, #{ids := [_]}}], Results),

    onepanel_test_utils:service_action(Node, op_worker, get_storages, Ctx#{id => Id}),
    Results2 = assert_service_step(service:get_module(op_worker), get_storages),
    [{Node, Storage}] = ?assertMatch([{Node, _}], Results2),
    onepanel_test_utils:assert_values(Storage, [
        {id, Id},
        {name, <<"somePosix1">>},
        {type, <<"posix">>},
        {mountPoint, onepanel_utils:typed_get(
            [storages, posix, '/mnt/st1', docker_path], Config, binary
        )}
    ]).


service_op_worker_add_storage_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    {ok, Posix} = onepanel_lists:get([storages, posix, '/mnt/st2'], Config),
    {ok, Ceph} = onepanel_lists:get([storages, ceph, someCeph], Config),
    {ok, CephRados} = onepanel_lists:get([storages, cephrados, someCephRados], Config),
    {ok, S3} = onepanel_lists:get([storages, s3, someS3], Config),
    {ok, Swift} = onepanel_lists:get([storages, swift, someSwift], Config),
    {ok, Glusterfs} = onepanel_lists:get([storages, glusterfs, someGlusterfs], Config),
    {ok, WebDAV} = onepanel_lists:get([storages, webdav, someWebDAV], Config),
    onepanel_test_utils:service_action(Node, op_worker, add_storages, #{
        hosts => [hd(?config(oneprovider_hosts, Config))],
        storages => #{
            <<"somePosix2">> => #{
                type => <<"posix">>,
                mountPoint => onepanel_utils:typed_get(docker_path, Posix, binary),
                storagePathType => <<"canonical">>
            },
            <<"someCeph">> => #{
                type => <<"ceph">>,
                clusterName => <<"ceph">>,
                key => onepanel_utils:typed_get(key, Ceph, binary),
                monitorHostname => onepanel_utils:typed_get(host_name, Ceph, binary),
                poolName => <<"onedata">>,
                username => onepanel_utils:typed_get(username, Ceph, binary),
                storagePathType => <<"flat">>
            },
            <<"someCephRados">> => #{
                type => <<"cephrados">>,
                clusterName => <<"ceph">>,
                key => onepanel_utils:typed_get(key, CephRados, binary),
                monitorHostname => onepanel_utils:typed_get(host_name, CephRados, binary),
                poolName => <<"onedata">>,
                username => onepanel_utils:typed_get(username, CephRados, binary),
                storagePathType => <<"flat">>
            },
            <<"someS3">> => #{
                type => <<"s3">>,
                accessKey => onepanel_utils:typed_get(access_key, S3, binary),
                secretKey => onepanel_utils:typed_get(secret_key, S3, binary),
                bucketName => <<"onedata">>,
                hostname => <<"http://", (onepanel_utils:typed_get(host_name,
                    S3, binary))/binary>>,
                storagePathType => <<"flat">>
            },
            <<"someSwift">> => #{
                type => <<"swift">>,
                username => onepanel_utils:typed_get(user_name, Swift, binary),
                password => onepanel_utils:typed_get(password, Swift, binary),
                authUrl => onepanel_utils:join(["http://",
                    onepanel_utils:typed_get(host_name, Swift, binary), ":",
                    onepanel_utils:typed_get(keystone_port, Swift, binary), "/v2.0/tokens"]),
                tenantName => onepanel_utils:typed_get(tenant_name, Swift, binary),
                containerName => <<"swift">>,
                storagePathType => <<"flat">>
            },
            <<"someGluster">> => #{
                type => <<"glusterfs">>,
                volume => <<"data">>,
                hostname => onepanel_utils:typed_get(host_name, Glusterfs, binary),
                port => onepanel_utils:typed_get(port, Glusterfs, binary),
                transport => onepanel_utils:typed_get(transport, Glusterfs, binary),
                mountPoint => onepanel_utils:typed_get(mountpoint, Glusterfs, binary),
                xlatorOptions => <<"cluster.write-freq-threshold=100;">>,
                storagePathType => <<"canonical">>
            },
            <<"someWebDAV">> => #{
                type => <<"webdav">>,
                endpoint => onepanel_utils:typed_get(endpoint, WebDAV, binary),
                credentials => onepanel_utils:typed_get(credentials, WebDAV, binary),
                credentialsType => onepanel_utils:typed_get(credentials_type, WebDAV, binary),
                verifyServerCertificate => onepanel_utils:typed_get(verify_server_certificate, WebDAV, binary),
                rangeWriteSupport => onepanel_utils:typed_get(range_write_support, WebDAV, binary),
                authorizationHeader => onepanel_utils:typed_get(authorization_header, WebDAV, binary),
                connectionPoolSize => onepanel_utils:typed_get(connection_pool_size, WebDAV, binary),
                storagePathType => <<"canonical">>
            }
        }
    }),
    assert_service_step(service:get_module(op_worker), add_storages, [Node], ok).


service_op_worker_update_storage_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    [Host | _] = ?config(oneprovider_hosts, Config),


    % @fixme update docker and add other storage changes
    ChangesByName = #{
        <<"somePosix2">> => #{
            type => <<"posix">>, mountPoint => <<"newMountPoint">>, timeout => 500
        }
    },

    ExistingStorages = get_storages(Config),
    lists:foreach(fun
        (#{id := Id, type := _Type, name := Name} = Storage) ->
            case maps:find(Name, ChangesByName) of
                {ok, Changes} ->
                    Expected = maps:merge(Storage, Changes#{verificationPassed => false}),

                    onepanel_test_utils:service_action(Node, op_worker, update_storage, #{
                        hosts => [Host], storage => Changes, id => Id
                    }),
                    assert_service_step(service:get_module(op_worker),
                        update_storage, [Node], Expected);
                error -> skip
            end
    end, ExistingStorages).


services_status_test(Config) ->
    lists:foreach(fun({Nodes, MainService, Services}) ->
        lists:foreach(fun(Service) ->
            SModule = service:get_module(Service),
            lists:foreach(fun(Node) ->
                onepanel_test_utils:service_host_action(Node, Service, status),
                assert_service_step(SModule, status, [Node], running)
            end, Nodes),

            onepanel_test_utils:service_action(hd(Nodes), Service, status),
            assert_service_step(SModule, status, Nodes, running)
        end, Services),

        onepanel_test_utils:service_action(hd(Nodes), MainService, status),
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
                    onepanel_test_utils:service_host_action(Node, Service, Action),
                    assert_service_step(SModule, Action, [Node], Result)
                end, ActionsWithResults)
            end, Nodes),

            lists:foreach(fun({Action, Result}) ->
                onepanel_test_utils:service_action(hd(Nodes), Service, Action),
                assert_service_step(SModule, Action, Nodes, Result)
            end, ActionsWithResults)
        end, Services),

        lists:foreach(fun({Action, Result}) ->
            onepanel_test_utils:service_action(hd(Nodes), MainService, Action),
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
        OzIp = test_utils:get_docker_ip(OzNode),
        OzDomain = onepanel_test_utils:get_domain(hd(OzHosts)),
        onepanel_test_utils:set_test_envs(OzNodes, [{test_web_cert_domain, OzDomain}]),

        % generate certificate with correct onezone domain
        regenerate_web_certificate(OzNodes, OzDomain),

        onepanel_test_utils:service_action(OzNode, ?SERVICE_OZ, deploy, #{
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
                },
                ?SERVICE_LE => #{
                    hosts => OzHosts,
                    letsencrypt_enabled => false
                }
            }
        }),
        [OpNode | _] = OpNodes = ?config(oneprovider_nodes, NewConfig2),
        OpHosts = onepanel_cluster:nodes_to_hosts(OpNodes),
        % We do not have a DNS server that would resolve OZ domain for provider,
        % so we need to simulate it using /etc/hosts.
        lists:foreach(fun(Node) ->
            rpc:call(Node, file, write_file, [
                "/etc/hosts",
                <<"\n", OzIp/binary, "\t", OzDomain/binary>>,
                [append]
            ])
        end, OpNodes),

        {ok, Posix} = onepanel_lists:get([storages, posix, '/mnt/st1'], NewConfig2),
        onepanel_test_utils:service_action(OpNode, ?SERVICE_OP, deploy, #{
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
                            ),
                            storagePathType => <<"canonical">>
                        }
                    }
                },
                ?SERVICE_LE => #{
                    hosts => OpHosts,
                    letsencrypt_enabled => false
                }
            },
            ?SERVICE_OP => #{
                hosts => OpHosts,
                oneprovider_geo_latitude => 10.0,
                oneprovider_geo_longitude => 10.0,
                oneprovider_name => <<"provider1">>,
                oneprovider_domain => hd(OpHosts),
                oneprovider_register => true,
                oneprovider_admin_email => <<"admin@onedata.org">>,
                onezone_domain => OzDomain
            }
        }),
        NewConfig2
    end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(services_stop_start_test, Config) ->
    ct:timetrap({minutes, 60}),
    init_per_testcase(default, Config);

init_per_testcase(Case, Config) when
    Case == service_oneprovider_get_details_test;
    Case == service_oneprovider_modify_details_test ->

    [Node | _] = ?config(oneprovider_nodes, Config),
    await_oz_connectivity(Node),

    init_per_testcase(default, Config);

init_per_testcase(_Case, Config) ->
    onepanel_test_utils:clear_msg_inbox(),
    Config.


end_per_testcase(_Case, _Config) ->
    ok.

end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for subscriptions channel to be active in the provider.
%% @end
%%--------------------------------------------------------------------
-spec await_oz_connectivity(Node :: node()) -> ok | no_return().
await_oz_connectivity(Node) ->
    OpNode = onepanel_cluster:service_to_node(service_op_worker:name(), Node),
    ?assertMatch({ok, _},
        % direct rpc from testmaster apparently does not work
        rpc:call(Node, rpc, call, [OpNode, provider_logic, get, []]),
        ?AWAIT_OZ_CONNECTIVITY_ATTEMPTS),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates certificate using test CA for given domain.
%% @end
%%--------------------------------------------------------------------
-spec regenerate_web_certificate([node()], Domain :: string()) -> ok | no_return().
regenerate_web_certificate(Nodes, Domain) ->
    [Node | _] = Nodes,
    WebKeyPath = rpc_get_env(Node, web_key_file),
    WebCertPath = rpc_get_env(Node, web_cert_file),
    WebChainPath = rpc_get_env(Node, web_cert_chain_file),

    % Both key and cert are expected in the same file
    CAPath = rpc_get_env(Node, test_web_cert_ca_path),

    {_, []} = rpc:multicall(Nodes, cert_utils, create_signed_webcert, [
        WebKeyPath, WebCertPath, Domain, CAPath, CAPath]),
    {_, []} = rpc:multicall(Nodes, file, copy, [CAPath, WebChainPath]),

    rpc:multicall(Nodes, service_op_worker, reload_webcert, [#{}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads app config from given Node
%% @end
%%--------------------------------------------------------------------
-spec rpc_get_env(node(), atom()) -> term().
rpc_get_env(Node, Key) ->
    rpc:call(Node, onepanel_env, get, [Key]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns list of op worker storages
%% @end
%%--------------------------------------------------------------------
-spec get_storages(Config :: proplists:proplist()) ->
    [op_worker_storage:storage_details()].
get_storages(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    onepanel_test_utils:service_action(Node, op_worker, get_storages, #{
        hosts => [onepanel_cluster:node_to_host(Node)]
    }),
    Results = assert_service_step(service:get_module(op_worker), get_storages),
    [{_, #{ids := Ids}}] = ?assertMatch([{Node, _}], Results),

    lists:map(fun(Id) ->
        onepanel_test_utils:service_action(Node, op_worker, get_storages, #{
            hosts => [onepanel_cluster:node_to_host(Node)], id => Id
        }),
        Results2 = assert_service_step(service:get_module(op_worker), get_storages),
        [{_, Details}] = ?assertMatch([{Node, _}], Results2),
        Details
    end, Ids).
