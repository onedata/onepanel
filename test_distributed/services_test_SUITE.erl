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
    services_status_test/1,
    services_stop_start_test/1
]).

-define(TIMEOUT, timer:seconds(5)).

-define(run(Config, Function, HostsType), begin
    lists:foreach(fun(_Type_) ->
        Function(hd(?config(_Type_, Config)))
    end, HostsType)
end).

-define(AWAIT_OZ_CONNECTIVITY_ATTEMPTS, 30).

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
        services_status_test
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
    onepanel_test_utils:service_action(OpNode, oneprovider, register, #{
        oneprovider_geo_latitude => 20.0,
        oneprovider_geo_longitude => 20.0,
        oneprovider_name => <<"provider2">>,
        oneprovider_domain => OpDomain,
        oneprovider_admin_email => <<"admin@onedata.org">>,
        oneprovider_token => image_test_utils:get_registration_token(OzNode)
    }).


service_op_worker_add_storage_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    Posix = kv_utils:get([storages, posix, '/mnt/st2'], Config),
    Ceph = kv_utils:get([storages, ceph, someCeph], Config),
    CephRados = kv_utils:get([storages, cephrados, someCephRados], Config),
    S3 = kv_utils:get([storages, s3, someS3], Config),
    Swift = kv_utils:get([storages, swift, someSwift], Config),
    Glusterfs = kv_utils:get([storages, glusterfs, someGlusterfs], Config),
    WebDAV = kv_utils:get([storages, webdav, someWebDAV], Config),
    Results = onepanel_test_utils:service_action(Node, op_worker, add_storages, #{
        hosts => [hd(?config(oneprovider_hosts, Config))],
        storages => #{
            <<"somePosix2">> => #{
                type => <<"posix">>,
                mountPoint => onepanel_utils:get_converted(docker_path, Posix, binary),
                storagePathType => <<"canonical">>,
                qosParameters => #{}
            },
            <<"someCeph">> => #{
                type => <<"ceph">>,
                clusterName => <<"ceph">>,
                key => onepanel_utils:get_converted(key, Ceph, binary),
                monitorHostname => onepanel_utils:get_converted(host_name, Ceph, binary),
                poolName => <<"onedata">>,
                username => onepanel_utils:get_converted(username, Ceph, binary),
                storagePathType => <<"flat">>,
                qosParameters => #{}
            },
            <<"someCephRados">> => #{
                type => <<"cephrados">>,
                clusterName => <<"ceph">>,
                key => onepanel_utils:get_converted(key, CephRados, binary),
                monitorHostname => onepanel_utils:get_converted(host_name, CephRados, binary),
                poolName => <<"onedata">>,
                username => onepanel_utils:get_converted(username, CephRados, binary),
                storagePathType => <<"flat">>,
                qosParameters => #{}
            },
            <<"someS3">> => #{
                type => <<"s3">>,
                accessKey => onepanel_utils:get_converted(access_key, S3, binary),
                secretKey => onepanel_utils:get_converted(secret_key, S3, binary),
                bucketName => <<"onedata">>,
                hostname => <<"http://", (onepanel_utils:get_converted(host_name,
                    S3, binary))/binary>>,
                storagePathType => <<"flat">>,
                qosParameters => #{}
            },
            <<"someSwift">> => #{
                type => <<"swift">>,
                username => onepanel_utils:get_converted(user_name, Swift, binary),
                password => onepanel_utils:get_converted(password, Swift, binary),
                authUrl => onepanel_utils:join(["http://",
                    onepanel_utils:get_converted(host_name, Swift, binary), ":",
                    onepanel_utils:get_converted(keystone_port, Swift, binary), "/v2.0/tokens"]),
                tenantName => onepanel_utils:get_converted(tenant_name, Swift, binary),
                containerName => <<"swift">>,
                storagePathType => <<"flat">>,
                qosParameters => #{}
            },
            <<"someGluster">> => #{
                type => <<"glusterfs">>,
                volume => <<"data">>,
                hostname => onepanel_utils:get_converted(host_name, Glusterfs, binary),
                port => onepanel_utils:get_converted(port, Glusterfs, binary),
                transport => onepanel_utils:get_converted(transport, Glusterfs, binary),
                mountPoint => onepanel_utils:get_converted(mountpoint, Glusterfs, binary),
                xlatorOptions => <<"cluster.write-freq-threshold=100;">>,
                storagePathType => <<"canonical">>,
                qosParameters => #{}
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
                qosParameters => #{}
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
                readonly => true,
                qosParameters => #{}
            }
        }
    }),
    assert_expected_result(service:get_module(op_worker), add_storage, [Node], ok, Results).


service_op_worker_update_storage_test(Config) ->
    [Node | _] = ?config(oneprovider_nodes, Config),
    [Host | _] = ?config(oneprovider_hosts, Config),

    ChangesByName = #{
        <<"somePosix2">> => #{
            type => <<"posix">>, mountPoint => <<"newMountPoint">>, timeout => 500
        },
        <<"someCeph">> => #{
            monitorHostname => <<"newHostName">>, username => <<"changedCephAdmin">>
        },
        <<"someCephRados">> => #{
            monitorHostname => <<"newHostName">>, username => <<"changedCephAdmin">>
        },
        <<"someS3">> => #{
            type => <<"s3">>,
            bucketName => <<"onedataNew">>
        },
        <<"someSwift">> => #{
            type => <<"swift">>,
            tenantName => <<"changedTenant">>,
            containerName => <<"swift2">>
        },
        <<"someGluster">> => #{
            type => <<"glusterfs">>,
            transport => <<"http">>,
            mountPoint => <<"otherMountPoint">>
        },
        <<"someWebDAV">> => #{
            type => <<"webdav">>,
            rangeWriteSupport => <<"moddav">>
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


services_status_test(Config) ->
    lists:foreach(fun({Nodes, MainService, Services}) ->
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
        {?config(onezone_nodes, Config), ?SERVICE_OZ,
            [?SERVICE_CB, ?SERVICE_CM, ?SERVICE_OZW]},
        {?config(oneprovider_nodes, Config), ?SERVICE_OP,
            [?SERVICE_CB, ?SERVICE_CM, ?SERVICE_OPW]}
    ]).


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
    Posthook = fun(NewConfig) ->
        NewConfig2 = onepanel_test_utils:init(NewConfig),

        NewConfig3 = image_test_utils:deploy_onezone(?PASSPHRASE,
            ?OZ_USERNAME, ?OZ_PASSWORD, NewConfig2),

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
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec assert_step_present(module(), Function :: atom(),
    service_executor:results()) -> onepanel_rpc:results().
assert_step_present(Module, Function, Results) ->
    case lists:filtermap(fun
        ({M, F, {GoodResults, []}}) when M == Module, F == Function ->
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
