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

-include("names.hrl").
-include("modules/models.hrl").
-include("onepanel_test_utils.hrl").
-include("service.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2,
    end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    domain_is_lowercased_test/1,
    default_admin_is_created_test/1,
    batch_config_creates_users/1,
    service_oneprovider_modify_details_test/1,
    service_oneprovider_get_details_test/1,
    service_oneprovider_get_supported_spaces_test/1,
    service_op_worker_add_storage_test/1,
    service_op_worker_get_storages_test/1,
    service_oneprovider_unregister_register_test/1,
    service_op_worker_update_storage_test/1,
    service_op_worker_add_node_test/1,
    service_oz_worker_add_node_test/1,
    service_oneprovider_fetch_compatibility_registry_test/1,
    services_status_test/1,
    cluster_clocks_sync_test/1,
    services_stop_start_test/1
]).

-define(TIMEOUT, timer:seconds(5)).

-define(run(Config, Function, HostsType), begin
    lists:foreach(fun(_Type_) ->
        Function(hd(?config(_Type_, Config)))
    end, HostsType)
end).

-define(AWAIT_OZ_CONNECTIVITY_ATTEMPTS, 30).
-define(AWAIT_CLOCK_SYNC_ATTEMPTS, 30).

-define(OZ_USERNAME, <<"joe">>).
-define(OZ_PASSWORD, <<"password">>).
-define(PASSPHRASE, <<"passphrase">>).

all() ->
    ?ALL([
        domain_is_lowercased_test,
        default_admin_is_created_test,
        batch_config_creates_users,
        service_oneprovider_modify_details_test,
        service_oneprovider_get_details_test,
        service_oneprovider_get_supported_spaces_test,
        service_op_worker_get_storages_test,
        service_oneprovider_unregister_register_test,
        service_op_worker_add_storage_test,
        service_op_worker_update_storage_test,
        service_op_worker_add_node_test,
        service_oz_worker_add_node_test,
        service_oneprovider_fetch_compatibility_registry_test,
        services_status_test,
        cluster_clocks_sync_test
        %% TODO VFS-4056
        %% services_stop_start_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================


domain_is_lowercased_test(Config) ->
    % Deployment in init_per_suite provides domain in uppercase.
    % Verify the domain to be lowercased by the deployment functions.
    [OzNode | _] = ?config(onezone_nodes, Config),
    [OpNode | _] = ?config(oneprovider_nodes, Config),
    ExpectedOpDomain = ?config(oneprovider_domain, Config),
    ExpectedOzDomain = ?config(onezone_domain, Config),

    ?assertEqual(ExpectedOpDomain, rpc:call(OpNode, service_op_worker, get_domain, [])),
    ?assertEqual(ExpectedOzDomain, rpc:call(OzNode, service_oz_worker, get_domain, [])).


default_admin_is_created_test(Config) ->
    % After deployment, there should exist Onezone user <<"admin">>
    % with password set to the emergency passphrase.
    [OzNode | _] = ?config(onezone_nodes, Config),
    OzwNode = nodes:service_to_node(?SERVICE_OZW, OzNode),
    ?assertMatch({true, #auth{}}, image_test_utils:proxy_rpc(OzNode, OzwNode,
        basic_auth, authenticate, [<<"admin">>, ?PASSPHRASE])).


batch_config_creates_users(Config) ->
    [OzNode | _] = ?config(onezone_nodes, Config),
    OzwNode = nodes:service_to_node(?SERVICE_OZW, OzNode),
    {true, #auth{subject = #subject{id = UserId}}} =
        ?assertMatch({true, #auth{}}, image_test_utils:proxy_rpc(OzNode, OzwNode,
            basic_auth, authenticate, [?OZ_USERNAME, ?OZ_PASSWORD])),

    ?assert(image_test_utils:proxy_rpc(OzNode, OzwNode, group_logic, has_direct_user,
        [<<"admins">>, UserId])).


service_oneprovider_modify_details_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    Domain = list_to_binary(hosts:from_node(Node)),
    onepanel_test_utils:service_action(Node, oneprovider, modify_details, #{
        oneprovider_geo_latitude => 30.0,
        oneprovider_geo_longitude => 40.0,
        oneprovider_name => <<"provider3">>,
        oneprovider_subdomain_delegation => false,
        oneprovider_domain => Domain,
        oneprovider_admin_email => <<"admin@onedata.org">>
    }),

    Results = onepanel_test_utils:service_action(Node, oneprovider, get_details, #{
        hosts => [hosts:from_node(Node)]
    }),
    NodeToResult = assert_step_present(service:get_module(oneprovider), get_details, Results),
    [{_, Details}] = ?assertMatch([{Node, _}], NodeToResult),
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
    Results = onepanel_test_utils:service_action(Node, oneprovider, get_details, #{
        hosts => [hosts:from_node(Node)]
    }),
    ResultToNode = assert_step_present(service:get_module(oneprovider), get_details, Results),
    [{_, Details}] = ?assertMatch([{Node, _}], ResultToNode),
    onepanel_test_utils:assert_fields(Details,
        [id, name, domain, adminEmail, geoLatitude, geoLongitude]
    ).


service_oneprovider_get_supported_spaces_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    Results = onepanel_test_utils:service_action(Node, oneprovider, get_spaces, #{
        hosts => [hosts:from_node(Node)]
    }),
    assert_expected_result(
        service:get_module(oneprovider), get_spaces, [Node], [], Results
    ).


service_op_worker_get_storages_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    Ctx = #{hosts => [hosts:from_node(Node)]},
    Results = onepanel_test_utils:service_action(Node, op_worker, get_storages, Ctx),
    ResultToNode = assert_step_present(service:get_module(op_worker), get_storages, Results),
    [{Node, [Id]}] = ?assertMatch([{Node, [_]}], ResultToNode),

    Results2 = onepanel_test_utils:service_action(Node, op_worker, get_storages, Ctx#{id => Id}),
    ResultToNode2 = assert_step_present(service:get_module(op_worker), get_storages, Results2),
    [{Node, Storage}] = ?assertMatch([{Node, _}], ResultToNode2),
    onepanel_test_utils:assert_values(Storage, [
        {id, Id},
        {name, <<"somePosix1">>},
        {type, <<"posix">>},
        {mountPoint, onepanel_utils:get_converted(
            [storages, posix, '/mnt/st1', docker_path], Config, binary
        )}
    ]).


service_oneprovider_unregister_register_test(Config) ->
    [OzNode | _] = ?config(onezone_nodes, Config),
    [OpNode | _] = ?config(oneprovider_nodes, Config),
    OpDomain = ?config(oneprovider_domain, Config),
    onepanel_test_utils:service_action(OpNode, oneprovider, unregister, #{}),

    % test the alternative way of providing the registration token
    % (the default method is used during environment setup for this suite).
    RegistrationTokenFile = <<"/tmp/provider-registration-token.txt">>,
    RegistrationToken = image_test_utils:get_registration_token(OzNode),
    spawn(fun() ->
        % Onepanel should wait for the file to appear
        timer:sleep(timer:minutes(1)),
        ?assertEqual(ok, image_test_utils:proxy_rpc(OpNode, file, write_file, [
            RegistrationTokenFile, RegistrationToken
        ]))
    end),
    onepanel_test_utils:service_action(OpNode, oneprovider, register, #{
        oneprovider_geo_latitude => 20.0,
        oneprovider_geo_longitude => 20.0,
        oneprovider_name => <<"provider2">>,
        oneprovider_domain => OpDomain,
        oneprovider_admin_email => <<"admin@onedata.org">>,
        oneprovider_token_provision_method => <<"fromFile">>,
        oneprovider_token_file => RegistrationTokenFile
    }).


service_op_worker_add_storage_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    Posix = kv_utils:get([storages, posix, '/mnt/st2'], Config),
    Ceph = kv_utils:get([storages, ceph, someCeph], Config),
    CephRados = kv_utils:get([storages, cephrados, someCephRados], Config),
    S3 = kv_utils:get([storages, s3, someS3], Config),
    % @TODO VFS-8296 swift helper is currently not tested
    % Swift = kv_utils:get([storages, swift, someSwift], Config),
    Glusterfs = kv_utils:get([storages, glusterfs, someGlusterfs], Config),
    WebDAV = kv_utils:get([storages, webdav, someWebDAV], Config),
    XRootD = kv_utils:get([storages, xrootd, someXRootD], Config),
    Results = onepanel_test_utils:service_action(Node, op_worker, add_storages, #{
        hosts => [hd(?config(oneprovider_hosts, Config))],
        storages => #{
            <<"somePosix2">> => #{
                type => <<"posix">>,
                mountPoint => onepanel_utils:get_converted(docker_path, Posix, binary),
                storagePathType => <<"canonical">>,
                qosParameters => #{},
                lumaFeed => <<"auto">>
            },
            <<"someCeph">> => #{
                type => <<"ceph">>,
                clusterName => <<"ceph">>,
                key => onepanel_utils:get_converted(key, Ceph, binary),
                monitorHostname => onepanel_utils:get_converted(host_name, Ceph, binary),
                poolName => <<"onedata">>,
                username => onepanel_utils:get_converted(username, Ceph, binary),
                storagePathType => <<"flat">>,
                qosParameters => #{},
                lumaFeed => <<"auto">>
            },
            <<"someCephRados">> => #{
                type => <<"cephrados">>,
                clusterName => <<"ceph">>,
                key => onepanel_utils:get_converted(key, CephRados, binary),
                monitorHostname => onepanel_utils:get_converted(host_name, CephRados, binary),
                poolName => <<"onedata">>,
                username => onepanel_utils:get_converted(username, CephRados, binary),
                storagePathType => <<"flat">>,
                qosParameters => #{},
                lumaFeed => <<"auto">>
            },
            <<"someS3">> => #{
                type => <<"s3">>,
                accessKey => onepanel_utils:get_converted(access_key, S3, binary),
                secretKey => onepanel_utils:get_converted(secret_key, S3, binary),
                bucketName => <<"onedata">>,
                hostname => <<"http://", (onepanel_utils:get_converted(host_name,
                    S3, binary))/binary>>,
                storagePathType => <<"flat">>,
                qosParameters => #{},
                lumaFeed => <<"auto">>
            },
            % @TODO VFS-8296 swift helper is currently not tested
%%            <<"someSwift">> => #{
%%                type => <<"swift">>,
%%                username => onepanel_utils:get_converted(user_name, Swift, binary),
%%                password => onepanel_utils:get_converted(password, Swift, binary),
%%                authUrl => onepanel_utils:join(["http://",
%%                    onepanel_utils:get_converted(host_name, Swift, binary), ":",
%%                    onepanel_utils:get_converted(keystone_port, Swift, binary), "/v2.0/tokens"]),
%%                tenantName => onepanel_utils:get_converted(tenant_name, Swift, binary),
%%                containerName => <<"swift">>,
%%                storagePathType => <<"flat">>,
%%                qosParameters => #{},
%%                lumaFeed => <<"auto">>
%%            },
            <<"someGluster">> => #{
                type => <<"glusterfs">>,
                volume => <<"data">>,
                hostname => onepanel_utils:get_converted(host_name, Glusterfs, binary),
                port => onepanel_utils:get_converted(port, Glusterfs, binary),
                transport => onepanel_utils:get_converted(transport, Glusterfs, binary),
                mountPoint => onepanel_utils:get_converted(mountpoint, Glusterfs, binary),
                xlatorOptions => <<"cluster.write-freq-threshold=100;">>,
                storagePathType => <<"canonical">>,
                qosParameters => #{},
                lumaFeed => <<"auto">>
            },
            <<"someWebDAV">> => #{
                type => <<"webdav">>,
                endpoint => onepanel_utils:get_converted(endpoint, WebDAV, binary),
                credentials => onepanel_utils:get_converted(credentials, WebDAV, binary),
                credentialsType => onepanel_utils:get_converted(credentials_type, WebDAV, binary),
                verifyServerCertificate => onepanel_utils:get_converted(verify_server_certificate, WebDAV, binary),
                rangeWriteSupport => onepanel_utils:get_converted(range_write_support, WebDAV, binary),
                authorizationHeader => onepanel_utils:get_converted(authorization_header, WebDAV, binary),
                connectionPoolSize => onepanel_utils:get_converted(connection_pool_size, WebDAV, binary),
                storagePathType => <<"canonical">>,
                qosParameters => #{},
                lumaFeed => <<"auto">>
            },
            <<"someXRootD">> => #{
                type => <<"xrootd">>,
                url => onepanel_utils:get_converted(url, XRootD, binary),
                credentials => onepanel_utils:get_converted(credentials, XRootD, binary),
                credentialsType => onepanel_utils:get_converted(credentials_type, XRootD, binary),
                storagePathType => <<"canonical">>,
                qosParameters => #{},
                lumaFeed => <<"auto">>
            },
            <<"someNullDevice">> => #{
                type => <<"nulldevice">>,
                name => <<"someNullDevice">>,
                latencyMin => 25,
                latencyMax => 75,
                timeoutProbability => 0.0,
                filter => <<"*">>,
                simulatedFilesystemParameters => <<>>,
                simulatedFilesystemGrowSpeed => 0.0,
                storagePathType => <<"canonical">>,
                importedStorage => true,
                readonly => true,
                qosParameters => #{},
                lumaFeed => <<"auto">>
            }
        }
    }),

    {_ResponseMap, ErrorOccurred} = parse_add_storages_results(Results),
    ?assertEqual(ErrorOccurred, false).


service_op_worker_update_storage_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    [Host | _] = ?config(oneprovider_hosts, Config),

    %% These storage parameter changes should provide invalid storage connection
    %% or access details (e.g. fake url or mountpoint), as the test will verify
    %% the parameter modification based on the lack of connectivity to the storage
    %% after the change.
    ChangesByName = #{
        <<"somePosix2">> => #{
            type => <<"posix">>, mountPoint => <<"newMountPoint">>, timeout => 500
        },
        <<"someCeph">> => #{
            type => <<"ceph">>,
            monitorHostname => <<"newHostName">>,
            username => <<"changedCephAdmin">>
        },
        <<"someCephRados">> => #{
            type => <<"cephrados">>,
            monitorHostname => <<"newHostName">>,
            username => <<"changedCephAdmin">>
        },
        <<"someS3">> => #{
            type => <<"s3">>,
            bucketName => <<"onedataNew">>
        },
        % @TODO VFS-8296 swift helper is currently not tested
%%        <<"someSwift">> => #{
%%            type => <<"swift">>,
%%            tenantName => <<"changedTenant">>,
%%            containerName => <<"swift2">>
%%        },
        <<"someGluster">> => #{
            type => <<"glusterfs">>,
            transport => <<"http">>,
            mountPoint => <<"otherMountPoint">>
        },
        <<"someWebDAV">> => #{
            type => <<"webdav">>,
            rangeWriteSupport => <<"moddav">>,
            fileMode => <<"0333">>,
            dirMode => <<"0333">>
        },
        <<"someXRootD">> => #{
            type => <<"xrootd">>,
            url => <<"root://domain.invalid:1094/data/">>
        },
        <<"someNullDevice">> => #{
            type => <<"nulldevice">>,
            latencyMin => 100,
            latencyMax => 150
        }
    },

    ExistingStorages = get_storages(Config),
    lists:foreach(fun
        (#{id := Id, type := _Type, name := Name} = Storage) ->
            case maps:find(Name, ChangesByName) of
                {ok, Changes} ->
                    ChangesBinary = onepanel_utils:convert(Changes, {values, binary}),
                    Expected = case Name of
                        <<"someNullDevice">> ->
                            maps:merge(Storage, ChangesBinary);
                        _ ->
                            maps:merge(Storage, ChangesBinary#{verificationPassed => false})
                    end,

                    Results = onepanel_test_utils:service_action(Node, op_worker, update_storage, #{
                        hosts => [Host], storage => Changes, id => Id
                    }),
                    assert_expected_result(service:get_module(op_worker),
                        update_storage, [Node], Expected, Results);
                error -> skip
            end
    end, ExistingStorages).


service_op_worker_add_node_test(Config) ->
    AllHosts = ?config(oneprovider_hosts, Config),
    OldHosts = ?config(op_worker_hosts, Config),
    NewHost = hd(AllHosts -- OldHosts),
    OldNode = nodes:service_to_node(?SERVICE_PANEL, hd(OldHosts)),
    NewNode = nodes:service_to_node(?SERVICE_PANEL, NewHost),
    OldOpNode = nodes:service_to_node(?SERVICE_OPW, OldNode),

    TokenFilePath = onepanel_env:get_remote(OldNode,
        op_worker_root_token_path, ?APP_NAME),
    {ok, CurrentFileContents} = rpc:call(OldNode, file, read_file, [TokenFilePath]),

    onepanel_test_utils:service_action(OldNode, ?SERVICE_OPW, add_nodes,
        #{new_hosts => [NewHost]}),

    ?assertEqual(true, rpc:call(NewNode, service, is_healthy, [?SERVICE_OPW])),
    ?assertEqual({ok, CurrentFileContents},
        rpc:call(NewNode, file, read_file, [TokenFilePath])),
    OpwNodesList = image_test_utils:proxy_rpc(OldNode,
        OldOpNode, consistent_hashing, get_all_nodes, []),
    ?assert(is_list(OpwNodesList)),
    ?assertEqual(length(OldHosts) + 1, length(OpwNodesList)).


service_oz_worker_add_node_test(Config) ->
    AllHosts = ?config(onezone_hosts, Config),
    OldHosts = ?config(oz_worker_hosts, Config),
    NewHost = hd(AllHosts -- OldHosts),
    OldNode = nodes:service_to_node(?SERVICE_PANEL, hd(OldHosts)),
    NewNode = nodes:service_to_node(?SERVICE_PANEL, NewHost),
    OldOzNode = nodes:service_to_node(?SERVICE_OZW, OldNode),

    onepanel_test_utils:service_action(OldNode, ?SERVICE_OZW, add_nodes,
        #{new_hosts => [NewHost]}),

    ?assertEqual(true, rpc:call(NewNode, service, is_healthy, [?SERVICE_OZW])),
    OzwNodesList = image_test_utils:proxy_rpc(OldNode,
        OldOzNode, consistent_hashing, get_all_nodes, []),
    ?assert(is_list(OzwNodesList)),
    ?assertEqual(length(OldHosts) + 1, length(OzwNodesList)),
    ?assertEqual(rpc:call(OldNode, service_oz_worker, get_policies, []),
        rpc:call(NewNode, service_oz_worker, get_policies, [])).


service_oneprovider_fetch_compatibility_registry_test(Config) ->
    % place some initial, outdated compatibility registry on all nodes
    OnepanelNodes = ?config(oneprovider_nodes, Config),
    OldRevision = 2000010100,
    lists:foreach(fun(Node) ->
        CurrentRegistryPath = rpc:call(Node, ctool, get_env, [current_compatibility_registry_file]),
        DefaultRegistryPath = rpc:call(Node, ctool, get_env, [default_compatibility_registry_file]),
        OldRegistry = #{<<"revision">> => OldRevision},
        ok = rpc:call(Node, ctool, set_env, [compatibility_registry_mirrors, []]),
        ok = rpc:call(Node, file, write_file, [CurrentRegistryPath, json_utils:encode(OldRegistry)]),
        ok = rpc:call(Node, file, write_file, [DefaultRegistryPath, json_utils:encode(OldRegistry)]),
        ok = rpc:call(Node, compatibility, clear_registry_cache, [])
    end, OnepanelNodes),

    % force a registry query that should cause Onepanel to fetch a newer one from Onezone
    ChosenNode = lists_utils:random_element(OnepanelNodes),
    ?assertMatch({ok, 200, _, _}, onepanel_test_rest:auth_request(
        hosts:from_node(ChosenNode), "/provider/onezone_info", get, ?PASSPHRASE
    )),
    NewerRevision = peek_current_registry_revision_on_node(ChosenNode),
    ?assertNotEqual(NewerRevision, OldRevision),

    % in the process, the new registry should be propagated to all nodes
    lists:foreach(fun(Node) ->
        ?assertEqual(NewerRevision, peek_current_registry_revision_on_node(Node))
    end, OnepanelNodes -- [ChosenNode]).


services_status_test(Config) ->
    lists:foreach(fun({NodesType, MainService, Services}) ->
        Nodes = ?config(NodesType, Config),
        lists:foreach(fun(Service) ->
            SModule = service:get_module(Service),
            lists:foreach(fun(Node) ->
                Results = onepanel_test_utils:service_host_action(Node, Service, status),
                assert_expected_result(SModule, status, [Node], healthy, Results)
            end, Nodes),

            Results = onepanel_test_utils:service_action(hd(Nodes), Service, status),
            assert_expected_result(SModule, status, Nodes, healthy, Results)
        end, Services),

        Results = onepanel_test_utils:service_action(hd(Nodes), MainService, status),
        lists:foreach(fun(Service) ->
            SModule = service:get_module(Service),
            assert_expected_result(SModule, status, Nodes, healthy, Results)
        end, Services)
    end, [
        {onezone_nodes, ?SERVICE_OZ,
            [?SERVICE_CB, ?SERVICE_CM, ?SERVICE_OZW]},
        {oneprovider_nodes, ?SERVICE_OP,
            [?SERVICE_CB, ?SERVICE_CM, ?SERVICE_OPW]}
    ]).


cluster_clocks_sync_test(Config) ->
    % the clock_synchronization_interval_seconds env is set in the env.json to
    % 5 seconds for the sake of this test

    OzpNodes = ?config(onezone_nodes, Config),
    % master node is selected as the first from sorted list
    OzpMasterNode = hd(lists:sort(OzpNodes)),
    OzCmNode = rpc:call(OzpMasterNode, service_cluster_manager, get_current_primary_node, []),
    OzwNodes = rpc:call(OzpMasterNode, service_oz_worker, get_nodes, []),

    OppNodes = ?config(oneprovider_nodes, Config),
    OpCmNode = rpc:call(hd(OppNodes), service_cluster_manager, get_current_primary_node, []),
    OpwNodes = rpc:call(hd(OppNodes), service_op_worker, get_nodes, []),

    IsSyncedWithMaster = fun(Node) ->
        MasterTimestamp = rpc:call(OzpMasterNode, global_clock, timestamp_millis, []),
        NodeTimestamp = image_test_utils:proxy_rpc(Node, global_clock, timestamp_millis, []),
        are_timestamps_in_sync(MasterTimestamp, NodeTimestamp)
    end,

    % after the environment is deployed and periodic sync has run at least once,
    % all nodes in Onezone and Oneprovider clusters should be synced with the master Onezone node
    AllNonMasterNodes = lists:flatten([
        OzpNodes, OzCmNode, OzwNodes,
        OppNodes, OpCmNode, OpwNodes
    ]) -- [OzpMasterNode],

    ?assertEqual(true, lists:all(IsSyncedWithMaster, AllNonMasterNodes), ?AWAIT_CLOCK_SYNC_ATTEMPTS),

    % simulate a situation when the time changes on the master node by 50 hours
    % and see if (after some time) the clocks are unified again
    ok = rpc:call(OzpMasterNode, global_clock, store_bias, [local_clock, timer:hours(50)]),
    ?assertEqual(false, lists:all(IsSyncedWithMaster, AllNonMasterNodes)),
    ?assertEqual(true, lists:all(IsSyncedWithMaster, AllNonMasterNodes), ?AWAIT_CLOCK_SYNC_ATTEMPTS),

    % simulate a situation when the time changes on another, non-master node by
    % 50 hours and see if (after some time) it catches up with the master again
    RandomNonMasterNode = lists_utils:random_element(AllNonMasterNodes),
    image_test_utils:proxy_rpc(RandomNonMasterNode, global_clock, store_bias, [local_clock, timer:hours(-50)]),
    ?assertEqual(false, IsSyncedWithMaster(RandomNonMasterNode)),
    ?assertEqual(true, IsSyncedWithMaster(RandomNonMasterNode), ?AWAIT_CLOCK_SYNC_ATTEMPTS),
    ?assertEqual(true, lists:all(IsSyncedWithMaster, AllNonMasterNodes), ?AWAIT_CLOCK_SYNC_ATTEMPTS),

    % simulate a situation when one of the nodes fails to synchronize its clock
    % and check if the procedure that restarts clock sync correctly awaits and
    % retries until the problem is resolved
    OppMasterNode = hd(lists:sort(OppNodes)), % master node is selected as the first from sorted list
    % below envs make it impossible for the node to successfully synchronize
    image_test_utils:proxy_rpc(OppMasterNode, ctool, set_env, [clock_sync_satisfying_delay, -1]),
    image_test_utils:proxy_rpc(OppMasterNode, ctool, set_env, [clock_sync_max_allowed_delay, -1]),

    % try to restart the clock sync in an async process (it should block until the sync is successful)
    image_test_utils:proxy_rpc(OppMasterNode, global_clock, reset_to_system_time, []),
    Master = self(),
    AsyncProcess = spawn(fun() ->
        Result = image_test_utils:proxy_rpc(OppMasterNode, oneprovider_cluster_clocks, restart_periodic_sync, []),
        Master ! {restart_result, Result}
    end),

    ?assertEqual(false, IsSyncedWithMaster(OppMasterNode)),
    timer:sleep(timer:seconds(10)),
    ?assertEqual(false, IsSyncedWithMaster(OppMasterNode)),
    % check that the async process is still waiting
    ?assert(erlang:is_process_alive(AsyncProcess)),

    % bring back the sane config and wait until the sync is successful
    image_test_utils:proxy_rpc(OppMasterNode, ctool, set_env, [clock_sync_satisfying_delay, 2000]),
    image_test_utils:proxy_rpc(OppMasterNode, ctool, set_env, [clock_sync_max_allowed_delay, 10000]),
    ?assertReceivedMatch({restart_result, ok}, timer:seconds(10)),
    ?assertEqual(true, lists:all(IsSyncedWithMaster, AllNonMasterNodes), ?AWAIT_CLOCK_SYNC_ATTEMPTS).


services_stop_start_test(Config) ->
    ActionsWithResults = [
        {stop, ok}, {status, stopped}, {start, ok}, {status, unhealthy}
    ],

    lists:foreach(fun({Nodes, MainService, Services}) ->
        lists:foreach(fun(Service) ->
            SModule = service:get_module(Service),

            lists:foreach(fun(Node) ->
                lists:foreach(fun({Action, Result}) ->
                    Results = onepanel_test_utils:service_host_action(Node, Service, Action),
                    assert_expected_result(SModule, Action, [Node], Result, Results)
                end, ActionsWithResults)
            end, Nodes),

            lists:foreach(fun({Action, Result}) ->
                Results = onepanel_test_utils:service_action(hd(Nodes), Service, Action),
                assert_expected_result(SModule, Action, Nodes, Result, Results)
            end, ActionsWithResults)
        end, Services),

        lists:foreach(fun({Action, Result}) ->
            Results = onepanel_test_utils:service_action(hd(Nodes), MainService, Action),
            lists:foreach(fun(Service) ->
                SModule = service:get_module(Service),
                assert_expected_result(SModule, Action, Nodes, Result, Results)
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
    ssl:start(),
    application:ensure_all_started(hackney),
    Posthook = fun(NewConfig) ->
        NewConfig2 = onepanel_test_utils:init(NewConfig),

        NewConfig3 = image_test_utils:deploy_onezone(?PASSPHRASE,
            ?OZ_USERNAME, ?OZ_PASSWORD, 1, NewConfig2),

        Posix = kv_utils:get([storages, posix, '/mnt/st1'], NewConfig2),
        Storages = #{
            <<"somePosix1">> => #{
                type => <<"posix">>,
                mountPoint => onepanel_utils:get_converted(
                    docker_path, Posix, binary
                ),
                storagePathType => <<"canonical">>,
                qosParameters => #{}
            }
        },
        image_test_utils:deploy_oneprovider(?PASSPHRASE, Storages, NewConfig3)
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
    ssl:stop(),
    application:stop(hackney).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec assert_step_present(module(), Function :: atom(),
    service_executor:results()) -> onepanel_rpc:results().
assert_step_present(Module, Function, Results) ->
    case lists:filtermap(fun
        (#step_end{module = M, function = F, good_bad_results = {GoodResults, []}})
            when M == Module, F == Function
            ->
            {true, GoodResults};
        (_) ->
            false
    end, Results) of
        [NodesToResult | _] -> NodesToResult;
        [] -> ct:fail("Step ~ts:~ts not found among results:~n~p", [Module, Function, Results])
    end.


%% @private
-spec assert_expected_result(Module :: module(), Function :: atom(),
    Nodes :: [node()], Expected :: term(),
    Results :: service_executor:results()) -> ok.
assert_expected_result(Module, Function, Nodes, ExpectedValue, Results) ->
    NodesToResult = assert_step_present(Module, Function, Results),
    Expected = same_result_for_nodes(Nodes, ExpectedValue),
    onepanel_test_utils:assert_values(NodesToResult, Expected),
    NodesToResult.


%% @private
-spec same_result_for_nodes(Nodes :: [node()], Result) -> [{node(), Result}]
    when Result :: term().
same_result_for_nodes(Nodes, Result) ->
    [{Node, Result} || Node <- Nodes].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for subscriptions channel to be active in the provider.
%% @end
%%--------------------------------------------------------------------
-spec await_oz_connectivity(Node :: node()) -> ok | no_return().
await_oz_connectivity(Node) ->
    OpNode = nodes:service_to_node(?SERVICE_OPW, Node),
    ?assertMatch({ok, _},
        image_test_utils:proxy_rpc(Node, OpNode, provider_logic, get, []),
        ?AWAIT_OZ_CONNECTIVITY_ATTEMPTS),
    ok.


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
    Results = onepanel_test_utils:service_action(Node, op_worker, get_storages, #{
        hosts => [hosts:from_node(Node)]
    }),
    NodeToResult = assert_step_present(service:get_module(op_worker), get_storages, Results),
    [{_, Ids}] = ?assertMatch([{Node, List}] when is_list(List), NodeToResult),

    lists:map(fun(Id) ->
        Results2 = onepanel_test_utils:service_action(Node, op_worker, get_storages, #{
            hosts => [hosts:from_node(Node)], id => Id
        }),
        NodeToResult2 = assert_step_present(service:get_module(op_worker),
            get_storages, Results2),
        [{_, Details}] = ?assertMatch([{Node, _}], NodeToResult2),
        Details
    end, Ids).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parses result from add_storages steps to storage map and searches for any error occurred during adding storages.
%% @end
%%--------------------------------------------------------------------
-spec parse_add_storages_results(list()) -> map().
parse_add_storages_results(ActionResults) ->
    lists:foldl(fun(StepResult, {AccMap, AccErrorOccurred}) ->
        case StepResult of
            {step_end, _, add_storage, {[{_, {StorageName, {error, Reason}}}], []}}->
                {AccMap#{StorageName => #{<<"error">> => errors:to_json({error, Reason})}}, true};
            {step_end, _, add_storage, {[{_, {StorageName, {ok, StorageId}}}], []}} ->
                {AccMap#{StorageName => #{<<"id">> => StorageId}}, AccErrorOccurred};
            _ ->
                {AccMap, AccErrorOccurred}
        end
    end, {#{}, false}, ActionResults).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compares two timestamps and returns true if they are at most 5 seconds apart
%% (bigger clock differences should be tested to make this a reliable check).
%% @end
%%--------------------------------------------------------------------
-spec are_timestamps_in_sync(time:millis(), time:millis()) -> boolean().
are_timestamps_in_sync(TimestampA, TimestampB) ->
    TimestampA - TimestampB > -5000 andalso TimestampA - TimestampB < 5000.


%% @private
-spec peek_current_registry_revision_on_node(node()) -> integer().
peek_current_registry_revision_on_node(Node) ->
    Resolver = compatibility:build_resolver([Node], []),
    {ok, Rev} = rpc:call(Node, compatibility, peek_current_registry_revision, [Resolver]),
    Rev.