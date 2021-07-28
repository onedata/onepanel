%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of onepanel service.
%%%
%%% This test suites recreates test nodes for each test case
%%% to ensure fresh state for clusterization tests.
%%% @end
%%%--------------------------------------------------------------------
-module(service_onepanel_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("onepanel_test_utils.hrl").
-include("service.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_testcase/2, end_per_testcase/2,
    init_per_suite/1, end_per_suite/1]).

%% tests
-export([
    deploy_should_create_cluster/1,
    join_should_add_node/1,
    join_should_work_after_leave/1,
    join_should_fail_on_clustered_node/1,
    sequential_join_should_create_cluster/1,
    extend_should_add_node_by_hostname/1,
    extend_should_add_node_by_ip/1,
    extend_should_return_hostname_of_new_node/1,
    extend_should_work_in_deployed_cluster/1,
    extend_should_copy_generated_config_to_new_node/1,
    extend_should_copy_certificates_to_new_node/1,
    leave_should_remove_node/1
]).

all() ->
    ?ALL([
        deploy_should_create_cluster,
        join_should_add_node,
        join_should_work_after_leave,
        join_should_fail_on_clustered_node,
        sequential_join_should_create_cluster,
        extend_should_add_node_by_hostname,
        extend_should_add_node_by_ip,
        extend_should_return_hostname_of_new_node,
        extend_should_copy_generated_config_to_new_node,
        extend_should_copy_certificates_to_new_node,
        leave_should_remove_node
    ]).


%% Sample certificate files
-define(TEST_CERT_PATHS, #{
    web_cert_file => "testca/web_cert.pem",
    web_key_file => "testca/web_key.pem",
    web_cert_chain_file => "testca/web_chain.pem"
}).


%%%===================================================================
%%% Test functions
%%%===================================================================


deploy_should_create_cluster(Config) ->
    [Node | _] = Nodes = ?config(onepanel_nodes, Config),
    Hosts = hosts:from_nodes(Nodes),

    onepanel_test_utils:service_action(Node,
        ?SERVICE_PANEL, deploy, #{hosts => Hosts}
    ),
    ?assertEqual(Hosts,
        lists:sort(rpc:call(Node, service_onepanel, get_hosts, []))).


join_should_add_node(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1 | _] = Hosts = hosts:from_nodes(Nodes),

    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => [Host1]}
    ),
    {ok, InviteToken} = rpc:call(Node1, invite_tokens, create, []),
    onepanel_test_utils:service_action(Node2,
        ?SERVICE_PANEL, join_cluster, #{
            invite_token => InviteToken,
            cluster_host => Host1
        }
    ),

    ?assertEqual(Hosts, lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))).


join_should_fail_on_clustered_node(Config) ->
    [Node1 | _] = ?config(onepanel_nodes, Config),
    Cluster1 = ?config(cluster1, Config),
    Cluster2 = ?config(cluster2, Config),
    [Host1 | _] = Cluster1Hosts = hosts:from_nodes(Cluster1),
    Cluster2Hosts = hosts:from_nodes(Cluster2),

    lists:foreach(fun(Node) ->
        {ok, InviteToken} = rpc:call(Node1, invite_tokens, create, []),
        ?assertMatch({error, _}, rpc:call(Node, service, apply,
            [?SERVICE_PANEL, join_cluster, #{hosts => [hosts:from_node(Node)],
                invite_token => InviteToken, cluster_host => Host1}]
        ))
    end, Cluster2),

    % assert no change
    ?assertEqual(Cluster1Hosts, lists:sort(rpc:call(hd(Cluster1), service_onepanel, get_hosts, []))),
    ?assertEqual(Cluster2Hosts, lists:sort(rpc:call(hd(Cluster2), service_onepanel, get_hosts, []))).


join_should_work_after_leave(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1 | _] = hosts:from_nodes(Nodes),

    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => [Host1]}
    ),

    {ok, InviteToken1} = rpc:call(Node1, invite_tokens, create, []),
    onepanel_test_utils:service_action(Node2,
        ?SERVICE_PANEL, join_cluster, #{invite_token => InviteToken1, cluster_host => Host1}
    ),
    onepanel_test_utils:service_action(Node2,
        ?SERVICE_PANEL, leave_cluster, #{}
    ),
    {ok, InviteToken2} = rpc:call(Node1, invite_tokens, create, []),
    onepanel_test_utils:service_action(Node2,
        ?SERVICE_PANEL, join_cluster, #{invite_token => InviteToken2, cluster_host => Host1}
    ).


sequential_join_should_create_cluster(Config) ->
    [Node1, Node2, Node3, Node4, Node5 | _] = Nodes =
        ?config(onepanel_nodes, Config),
    [Host1, Host2, Host3, Host4 | _] = Hosts =
        hosts:from_nodes(Nodes),

    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => [Host1]}
    ),

    {ok, InviteToken1} = rpc:call(Node1, invite_tokens, create, []),
    {ok, InviteToken2} = rpc:call(Node1, invite_tokens, create, []),
    ?assertNotEqual(InviteToken1, InviteToken2),

    % Assert that invite tokens can be used multiple times
    onepanel_test_utils:service_action(Node2,
        ?SERVICE_PANEL, join_cluster, #{invite_token => InviteToken1, cluster_host => Host1}
    ),
    onepanel_test_utils:service_action(Node3,
        ?SERVICE_PANEL, join_cluster, #{invite_token => InviteToken1, cluster_host => Host2}
    ),
    onepanel_test_utils:service_action(Node4,
        ?SERVICE_PANEL, join_cluster, #{invite_token => InviteToken2, cluster_host => Host3}
    ),

    % Assert that invite tokens expires after predefined period of time
    clock_freezer_mock:simulate_seconds_passing(100000),
    ?assertMatch({error, _}, onepanel_test_utils:attempt_service_action(Node5,
        ?SERVICE_PANEL, join_cluster, #{invite_token => InviteToken1, cluster_host => Host4}
    )),
    % Returning back in time to when token was still valid should make it usable
    clock_freezer_mock:simulate_seconds_passing(-100000),
    onepanel_test_utils:service_action(Node5,
        ?SERVICE_PANEL, join_cluster, #{invite_token => InviteToken1, cluster_host => Host4}
    ),

    lists:foreach(fun(Node) ->
        ?assertEqual(Hosts,
            lists:sort(rpc:call(Node, service_onepanel, get_hosts, [])))
    end, Nodes).


% ensure presence of deployed services does not prevent adding more nodes
extend_should_work_in_deployed_cluster(Config) ->
    Cluster1 = ?config(cluster1, Config),
    Cluster1Hosts = hosts:from_nodes(Cluster1),
    [Node1 | _] = Nodes = ?config(onepanel_nodes, Config),
    [_Host1, _Host2, Host3 | _] = hosts:from_nodes(Nodes),

    onepanel_test_utils:service_action(Node1, ?SERVICE_PANEL, extend_cluster,
        #{address => Host3}),

    ?assertEqual(lists:sort([Host3 | Cluster1Hosts]),
        lists:sort(rpc:call(hd(Cluster1), service_onepanel, get_hosts, []))).


leave_should_remove_node(Config) ->
    [Node1, Node2, Node3 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2, Node3],
    [Host1, Host2, Host3] = hosts:from_nodes(Nodes),
    Hosts = [Host2, Host3],

    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => [Host1 | Hosts]}
    ),
    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, leave_cluster, #{}
    ),

    ?assertEqual([Host1], rpc:call(Node1, service_onepanel, get_hosts, [])),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node3, service_onepanel, get_hosts, []))).


extend_should_add_node_by_hostname(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1, Host2] = Hosts = hosts:from_nodes(Nodes),

    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => [Host1]}
    ),
    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, extend_cluster, #{hostname => Host2}
    ),

    ?assertEqual(Hosts, lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))).


extend_should_add_node_by_ip(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1 | _] = Hosts = hosts:from_nodes(Nodes),
    Host2Address = test_utils:get_docker_ip(Node2),

    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => [Host1]}
    ),
    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, extend_cluster, #{address => Host2Address}
    ),

    ?assertEqual(Hosts, lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))).

extend_should_return_hostname_of_new_node(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1 | Host2] = Hosts = hosts:from_nodes(Nodes),
    Host2Binary = onepanel_utils:convert(Host2, binary),
    Host2Address = test_utils:get_docker_ip(Node2),

    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => [Host1]}
    ),
    Results = ?assertMatch([_|_], rpc:call(Node1, service, apply_sync,
        [?SERVICE_PANEL, extend_cluster, #{address => Host2Address}]
    )),
    ?assertMatch([{[{_Node, #{hostname := Host2Binary}}], []}],
        [FunctionResults || #step_end{module = service_onepanel, function = extend_cluster,
            good_bad_results = FunctionResults} <- Results]),

    ?assertEqual(Hosts, lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))).


extend_should_copy_generated_config_to_new_node(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1, Host2] = hosts:from_nodes(Nodes),
    Host2Bin = list_to_binary(Host2),
    V1 = some_value,
    V2 = {complex, value},

    % prepare
    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => [Host1]}),
    ?assertMatch(ok, rpc:call(Node1,
        onepanel_env, write, [[?APP_NAME, some_variable], V1])),
    ?assertMatch(ok, rpc:call(Node1,
        onepanel_env, write, [[ctool, some_variable2], V2])),

    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, extend_cluster, #{hostname => Host2Bin}
    ),

    ?assertEqual({ok, V1}, rpc:call(Node2,
        onepanel_env, read_effective, [[?APP_NAME, some_variable], ?SERVICE_PANEL]
    )),
    ?assertEqual({ok, V1}, rpc:call(Node2,
        onepanel_env, find, [some_variable, ?APP_NAME]
    )),
    ?assertEqual({ok, V2}, rpc:call(Node2,
        onepanel_env, read_effective, [[ctool, some_variable2], ?SERVICE_PANEL]
    )),
    ?assertEqual({ok, V2}, rpc:call(Node2,
        onepanel_env, find, [some_variable2, ctool]
    )).


extend_should_copy_certificates_to_new_node(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1, Host2] = hosts:from_nodes(Nodes),
    Host2Bin = list_to_binary(Host2),

    % prepare
    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => [Host1]}),
    onepanel_test_utils:service_action(Node1,
        ?SERVICE_LE, deploy, #{letsencrypt_plugin => ?SERVICE_OPW}),
    onepanel_test_utils:service_action(Node1,
        % mock will produce cert and key files
        ?SERVICE_LE, update, #{letsencrypt_enabled => true}),
    % sanity check
    verify_certificate_files(Node1, Config),

    % add node
    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, extend_cluster, #{hostname => Host2Bin}
    ),

    verify_certificate_files(Node2, Config).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    [{?CTH_ENV_UP, ?DISABLE}, {?LOAD_MODULES, [onepanel_test_utils]} | Config].

init_per_testcase(join_should_fail_on_clustered_node, Config) ->
    Config2 = init_per_testcase(default, Config),

    [Node1, Node2, Node3 | _] = ?config(onepanel_nodes, Config2),
    Cluster1 = [Node1],
    Cluster2 = lists:sort([Node2, Node3]),
    Cluster1Hosts = hosts:from_nodes(Cluster1),
    Cluster2Hosts = hosts:from_nodes(Cluster2),

    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => Cluster1Hosts}),
    ?assertEqual(Cluster1Hosts, lists:sort(rpc:call(
        Node1, service_onepanel, get_hosts, [])
    )),

    onepanel_test_utils:service_action(Node2,
        ?SERVICE_PANEL, deploy, #{hosts => Cluster2Hosts}),
    ?assertEqual(lists:sort(Cluster2Hosts), lists:sort(rpc:call(
        Node2, service_onepanel, get_hosts, [])
    )),

    [{cluster1, Cluster1}, {cluster2, Cluster2} | Config2];

init_per_testcase(sequential_join_should_create_cluster, Config) ->
    Config2 = init_per_testcase(default, Config),
    Nodes = ?config(onepanel_nodes, Config2),
    clock_freezer_mock:setup_for_ct(Nodes, [global_clock]),
    Config2;

init_per_testcase(extend_should_work_in_deployed_cluster, Config) ->
    Config2 = init_per_testcase(default, Config),

    [Node1, Node2 | _] = ?config(onepanel_nodes, Config2),
    Cluster1 = [Node1, Node2],
    Cluster1Hosts = hosts:from_nodes(Cluster1),
    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => Cluster1Hosts}),
    ?assertEqual(ok, test_utils:mock_new(Cluster1, [service_oneprovider])),
    ?assertEqual(ok, test_utils:mock_expect(Cluster1, service_oneprovider, get_hosts,
        fun() -> Cluster1Hosts end)),

    [{cluster1, Cluster1} | Config2];

init_per_testcase(leave_should_not_remove_used_host, Config) ->
    NewConfig = init_per_testcase(default, Config),
    [Node1 | _] = Nodes = ?config(onepanel_nodes, NewConfig),
    Hosts = hosts:from_nodes(Nodes),

    % simulate deployed cluster
    lists:foreach(fun(Service) ->
        % Node1 will be used to deploy cluster
        ?assertMatch(ok, rpc:call(Node1, service, save, [#service{
            name = Service, hosts = Hosts
        }]))
    end, [couchbase, cluster_manager, op_worker]),
    NewConfig;

init_per_testcase(Case, Config) when
    Case == extend_should_copy_certificates_to_new_node;
    Case == extend_should_copy_generated_config_to_new_node
->
    NewConfig = init_per_testcase(default, Config),
    [Node1 | _] = Nodes = ?config(onepanel_nodes, NewConfig),
    [Host1 | _] = hosts:from_nodes(Nodes),

    [Node1 | _] = ?config(onepanel_nodes, NewConfig),
    ?assertEqual(ok, test_utils:mock_new(Nodes,
        [service_op_worker, letsencrypt_api], [passthrough])),
    test_utils:mock_expect(Nodes, service_op_worker, supports_letsencrypt_challenge,
        fun(_) -> true end),
    test_utils:mock_expect(Nodes, service_op_worker, get_domain,
        fun() -> list_to_binary(Host1) end),
    test_utils:mock_expect(Nodes, service_op_worker, reload_webcert,
        fun(_) -> ok end),
    test_utils:mock_expect(Nodes, letsencrypt_api, run_certification_flow,
        fun(_, _) ->
            deploy_predefined_certs(?TEST_CERT_PATHS, Config),

            KeysDir = filename:join(
                onepanel_env:get(letsencrypt_keys_dir),
                production
            ),
            ?assertEqual(ok, filelib:ensure_dir([KeysDir, "/"])),
            ?assertEqual(ok, file:write_file(KeysDir ++ "/letsencrypt_public_key.pem",
                <<"letsencrypt_public_key">>)),
            ?assertEqual(ok, file:write_file(KeysDir ++ "/letsencrypt_private_key.pem",
                <<"letsencrypt_private_key">>))
        end),
    NewConfig;

init_per_testcase(_Case, Config) ->
    Config2 = onepanel_test_utils:ensure_started(
        test_node_starter:prepare_test_environment(Config, ?MODULE)
    ),
    Nodes = ?config(onepanel_nodes, Config2),
    lists:foreach(fun(Node) ->
        ?assertMatch({ok, _}, rpc:call(Node, mock_manager, start, []))
    end, Nodes),
    kv_utils:put(onepanel_nodes, lists:sort(Nodes), Config2).


end_per_testcase(sequential_join_should_create_cluster, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    clock_freezer_mock:teardown_for_ct(Nodes),
    end_per_testcase(default, Config);

end_per_testcase(_Case, Config) ->
    test_node_starter:clean_environment(Config).


end_per_suite(_Config) ->
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc Writes predefined certificate files on node.
%% @end
%%--------------------------------------------------------------------
-spec deploy_predefined_certs(map(), Config :: proplists:proplist()) -> ok.
deploy_predefined_certs(SourcePaths, Config) ->
    lists:foreach(fun({FileType, Path}) ->
        {ok, Content} = file:read_file(?TEST_FILE(Config, Path)),
        ?assertMatch(ok, file:write_file(onepanel_env:get(FileType), Content))
    end, maps:to_list(SourcePaths)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verifies certificate files have the expected content in
%% extend_should_copy_certificates_to_new_node test case.
%% @end
%%--------------------------------------------------------------------
-spec verify_certificate_files(node(), Config :: proplists:proplist()) -> ok.
verify_certificate_files(Node, Config) ->
    lists:foreach(fun({FileType, ExpContentPath}) ->
        {ok, FilePathOnNode} = test_utils:get_env(Node, ?APP_NAME, FileType),
        ?assertEqual(
            file:read_file(?TEST_FILE(Config, ExpContentPath)),
            rpc:call(Node, file, read_file, [FilePathOnNode])
        )
    end, maps:to_list(?TEST_CERT_PATHS)),

    {ok, LEDir} = test_utils:get_env(Node, ?APP_NAME, letsencrypt_keys_dir),
    LEPrivateKey = filename:join([LEDir, production, "letsencrypt_private_key.pem"]),
    LEPublicKey = filename:join([LEDir, production, "letsencrypt_public_key.pem"]),

    ?assertEqual(
        {ok, <<"letsencrypt_private_key">>},
        rpc:call(Node, file, read_file, [LEPrivateKey])
    ),
    ?assertEqual(
        {ok, <<"letsencrypt_public_key">>},
        rpc:call(Node, file, read_file, [LEPublicKey])
    ).
