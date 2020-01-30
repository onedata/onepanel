%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of onepanel service.
%%% @end
%%%--------------------------------------------------------------------
-module(service_onepanel_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("onepanel_test_utils.hrl").
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
        leave_should_remove_node
    ]).

-define(COOKIE, test_cookie).
-define(COOKIE2, other_cookie).

%%%===================================================================
%%% Test functions
%%%===================================================================


deploy_should_create_cluster(Config) ->
    [Node | _] = Nodes = ?config(onepanel_nodes, Config),
    Hosts = lists:sort(hosts:from_nodes(Nodes)),

    ?assertEqual(ok, rpc:call(Node, service, apply,
        [?SERVICE_PANEL, deploy, #{cookie => ?COOKIE, hosts => Hosts}]
    )),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node, service_onepanel, get_hosts, []))).


join_should_add_node(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1 | _] = Hosts = lists:sort(hosts:from_nodes(Nodes)),

    Ctx = #{cookie => ?COOKIE},
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE_PANEL, deploy, Ctx#{hosts => [Host1]}]
    )),
    ?assertEqual(ok, rpc:call(Node2, service, apply,
        [?SERVICE_PANEL, join_cluster, Ctx#{cluster_host => Host1}]
    )),

    ?assertEqual(Hosts, lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))).


join_should_fail_on_clustered_node(Config) ->
    Cluster1 = ?config(cluster1, Config),
    Cluster2 = ?config(cluster2, Config),
    [Host1 | _] = Cluster1Hosts = hosts:from_nodes(Cluster1),
    Cluster2Hosts = hosts:from_nodes(Cluster2),

    lists:foreach(fun(Node) ->
        ?assertMatch({error, _}, rpc:call(Node, service, apply,
            [?SERVICE_PANEL, join_cluster, #{hosts => [hosts:from_node(Node)],
                cluster_host => Host1}]
        ))
    end, Cluster2),

    % assert no change
    ?assertEqual(Cluster1Hosts, lists:sort(rpc:call(hd(Cluster1), service_onepanel, get_hosts, []))),
    ?assertEqual(Cluster2Hosts, lists:sort(rpc:call(hd(Cluster2), service_onepanel, get_hosts, []))).


join_should_work_after_leave(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1 | _] = lists:sort(hosts:from_nodes(Nodes)),

    Ctx = #{cookie => ?COOKIE},
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE_PANEL, deploy, Ctx#{hosts => [Host1]}]
    )),
    ?assertEqual(ok, rpc:call(Node2, service, apply,
        [?SERVICE_PANEL, join_cluster, Ctx#{cluster_host => Host1}]
    )),
    ?assertEqual(ok, rpc:call(Node2, service, apply,
        [?SERVICE_PANEL, leave_cluster, #{}]
    )),
    ?assertEqual(ok, rpc:call(Node2, service, apply,
        [?SERVICE_PANEL, join_cluster, Ctx#{cluster_host => Host1}]
    )).


sequential_join_should_create_cluster(Config) ->
    [Node1, Node2, Node3, Node4, Node5 | _] = Nodes =
        lists:sort(?config(onepanel_nodes, Config)),
    [Host1, Host2, Host3, Host4 | _] = Hosts =
        lists:sort(hosts:from_nodes(Nodes)),

    Ctx = #{cookie => ?COOKIE},
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE_PANEL, deploy, Ctx#{hosts => [Host1]}])),
    ?assertEqual(ok, rpc:call(Node2, service, apply,
        [?SERVICE_PANEL, join_cluster, Ctx#{cluster_host => Host1}]
    )),
    ?assertEqual(ok, rpc:call(Node3, service, apply,
        [?SERVICE_PANEL, join_cluster, Ctx#{cluster_host => Host2}]
    )),
    ?assertEqual(ok, rpc:call(Node4, service, apply,
        [?SERVICE_PANEL, join_cluster, Ctx#{cluster_host => Host3}]
    )),
    ?assertEqual(ok, rpc:call(Node5, service, apply,
        [?SERVICE_PANEL, join_cluster, Ctx#{cluster_host => Host4}]
    )),

    lists:foreach(fun(Node) ->
        ?assertEqual(Hosts, lists:sort(rpc:call(Node, service_onepanel,
            get_hosts, [])))
    end, Nodes).


% ensure presence of deployed services does not prevent adding more nodes
extend_should_work_in_deployed_cluster(Config) ->
    Cluster1 = ?config(cluster1, Config),
    Cluster1Hosts = hosts:from_nodes(Cluster1),
    [Node1 | _] = Nodes = lists:sort(?config(onepanel_nodes, Config)),
    [_Host1, _Host2, Host3 | _] = Hosts =
        lists:sort(hosts:from_nodes(Nodes)),

    onepanel_test_utils:service_action(Node1, ?SERVICE_PANEL, extend_cluster,
        #{address => Host3}),

    ?assertEqual(lists:sort([Host3 | Cluster1Hosts]),
        lists:sort(rpc:call(hd(Cluster1), service_onepanel, get_hosts, []))).


leave_should_remove_node(Config) ->
    [Node1, Node2, Node3 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2, Node3],
    [Host1, Host2, Host3] = lists:sort(hosts:from_nodes(Nodes)),
    Hosts = [Host2, Host3],

    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE_PANEL, deploy, #{cookie => ?COOKIE, hosts => [Host1 | Hosts]}]
    )),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE_PANEL, leave_cluster, #{}]
    )),

    ?assertEqual([Host1], lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node3, service_onepanel, get_hosts, []))).


extend_should_add_node_by_hostname(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1 | Host2] = Hosts = lists:sort(hosts:from_nodes(Nodes)),

    Ctx = #{cookie => ?COOKIE},
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE_PANEL, deploy, Ctx#{hosts => [Host1]}]
    )),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE_PANEL, extend_cluster, Ctx#{hostname => Host2}]
    )),

    ?assertEqual(Hosts, lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))).


extend_should_add_node_by_ip(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1 | _] = Hosts = lists:sort(hosts:from_nodes(Nodes)),
    Host2Address = test_utils:get_docker_ip(Node2),

    Ctx = #{cookie => ?COOKIE},
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE_PANEL, deploy, Ctx#{hosts => [Host1]}]
    )),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE_PANEL, extend_cluster, Ctx#{address => Host2Address}]
    )),

    ?assertEqual(Hosts, lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))).

extend_should_return_hostname_of_new_node(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    Nodes = [Node1, Node2],
    [Host1 | Host2] = Hosts = lists:sort(hosts:from_nodes(Nodes)),
    Host2Binary = onepanel_utils:convert(Host2, binary),
    Host2Address = test_utils:get_docker_ip(Node2),

    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [?SERVICE_PANEL, deploy, #{hosts => [Host1]}]
    )),
    Results = ?assertMatch([_|_], rpc:call(Node1, service, apply_sync,
        [?SERVICE_PANEL, extend_cluster, #{address => Host2Address}]
    )),
    ?assertMatch([{[{_Node, #{hostname := Host2Binary}}], []}],
        [FunctionResults || {service_onepanel, extend_cluster, FunctionResults} <- Results]),

    ?assertEqual(Hosts, lists:sort(rpc:call(Node1, service_onepanel, get_hosts, []))),
    ?assertEqual(Hosts, lists:sort(rpc:call(Node2, service_onepanel, get_hosts, []))).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    [{?CTH_ENV_UP, ?DISABLE} | Config].

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
    ?assertEqual(Cluster2Hosts, lists:sort(rpc:call(
        Node2, service_onepanel, get_hosts, [])
    )),

    [{cluster1, Cluster1}, {cluster2, Cluster2} | Config2];

init_per_testcase(extend_should_work_in_deployed_cluster, Config) ->
    Config2 = init_per_testcase(default, Config),

    Nodes = ?config(onepanel_nodes, Config2),
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config2),
    Cluster1 = [Node1, Node2],
    Cluster1Hosts = hosts:from_nodes(Cluster1),
    onepanel_test_utils:service_action(Node1,
        ?SERVICE_PANEL, deploy, #{hosts => Cluster1Hosts}),
    test_utils:mock_new(Cluster1, [service_oneprovider]),
    test_utils:mock_expect(Cluster1, service_oneprovider, get_hosts,
        fun() -> Cluster1Hosts end),

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

init_per_testcase(_Case, Config) ->
    onepanel_test_utils:ensure_started(
        test_node_starter:prepare_test_environment(Config, ?MODULE)).


end_per_testcase(_Case, Config) ->
    test_node_starter:clean_environment(Config).


end_per_suite(_Config) ->
    ok.
