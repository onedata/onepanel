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
    service_oneprovider_unregister_register_test/1,
    service_op_worker_add_storage_test/1,
    service_op_worker_update_storage_test/1,
    service_op_worker_add_node_test/1,
    service_oz_worker_add_node_test/1,
    services_stop_start_test/1
]).

-define(NON_ADMIN_USERNAME, <<"joe">>).
-define(NON_ADMIN_PASSWORD, <<"password">>).
-define(PASSPHRASE, <<"passphrase">>).

all() ->
    ?ALL([
        service_oneprovider_unregister_register_test,
        service_op_worker_add_storage_test,
        service_op_worker_update_storage_test,
        service_op_worker_add_node_test,
        service_oz_worker_add_node_test
        %% TODO VFS-4056
        %% services_stop_start_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================


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
    Ceph = kv_utils:get([storages, ceph, someCeph], Config),
    Glusterfs = kv_utils:get([storages, glusterfs, someGlusterfs], Config),
    XRootD = kv_utils:get([storages, xrootd, someXRootD], Config),
    Results = onepanel_test_utils:service_action(Node, op_worker, add_storages, #{
        hosts => [hd(?config(oneprovider_hosts, Config))],
        storages => #{
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
            <<"someXRootD">> => #{
                type => <<"xrootd">>,
                url => onepanel_utils:get_converted(url, XRootD, binary),
                credentials => onepanel_utils:get_converted(credentials, XRootD, binary),
                credentialsType => onepanel_utils:get_converted(credentials_type, XRootD, binary),
                storagePathType => <<"canonical">>,
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
        <<"someCeph">> => #{
            type => <<"ceph">>,
            monitorHostname => <<"newHostName">>,
            username => <<"changedCephAdmin">>
        },
        <<"someGluster">> => #{
            type => <<"glusterfs">>,
            transport => <<"http">>,
            mountPoint => <<"otherMountPoint">>
        },
        <<"someXRootD">> => #{
            type => <<"xrootd">>,
            url => <<"root://domain.invalid:1094/data/">>
        }
    },

    ExistingStorages = get_storages(Config),
    lists:foreach(fun
        (#{id := Id, type := _Type, name := Name} = Storage) ->
            case maps:find(Name, ChangesByName) of
                {ok, Changes} ->
                    Expected = Storage#{verificationPassed => false},

                    Results = onepanel_test_utils:service_action(Node, op_worker, update_storage, #{
                        hosts => [Host], storage => Changes, id => Id
                    }),
                    onepanel_test_utils:assert_service_action_result(service:get_module(op_worker),
                        update_storage, [Node], Expected, Results
                    );
                error ->
                    skip
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
                    onepanel_test_utils:assert_service_action_result(SModule, Action, [Node], Result, Results)
                end, ActionsWithResults)
            end, Nodes),

            lists:foreach(fun({Action, Result}) ->
                Results = onepanel_test_utils:service_action(hd(Nodes), Service, Action),
                onepanel_test_utils:assert_service_action_result(SModule, Action, Nodes, Result, Results)
            end, ActionsWithResults)
        end, Services),

        lists:foreach(fun({Action, Result}) ->
            Results = onepanel_test_utils:service_action(hd(Nodes), MainService, Action),
            lists:foreach(fun(Service) ->
                SModule = service:get_module(Service),
                onepanel_test_utils:assert_service_action_result(SModule, Action, Nodes, Result, Results)
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
            ?NON_ADMIN_USERNAME, ?NON_ADMIN_PASSWORD, 1, NewConfig2),

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
        [] -> ct:fail("Step ~ts:~ts not found among results:~n~tp", [Module, Function, Results])
    end.


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
