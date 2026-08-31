%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Onezone cluster resize.
%%% @end
%%%-------------------------------------------------------------------
-module(cl_resize_oz_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("cluster_deployment_test_utils.hrl").
-include("names.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1,

    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    add_node_test/1,
    deploy_new_worker_test/1
]).

% NOTE: below tests depend on ordering and must be run sequentially!!!
all() -> [
    add_node_test,
    deploy_new_worker_test
].

-define(ATTEMPTS, 10).


%%%===================================================================
%%% Tests
%%%===================================================================


add_node_test(Config) ->
    [PanelNode1, PanelNode2] = PanelNodes = ?config(oz_panel_nodes, Config),
    PanelNode1Details = cluster_management_test_utils:infer_node_details(PanelNode1),
    PanelNode1Hostname = PanelNode1Details#node_details.hostname,
    PanelNode2Hostname = dns_test_utils:get_hostname(PanelNode2),
    OzDomain = dns_test_utils:get_k8s_service_domain(PanelNode1),

    panel_test_rpc:set_emergency_passphrase(PanelNode1, ?ONENV_EMERGENCY_PASSPHRASE),

    cluster_management_test_utils:assert_onedata_service_domain(PanelNodes, undefined),

    OzClusterConfig = #oz_cluster_config{
        nodes = #{1 => PanelNode1Details},
        managers = [1],
        main_manager = 1,
        workers = [1],
        databases = [1],
        name = <<"zone">>,
        domain = OzDomain
    },
    oz_cluster_deployment_test_utils:deploy_batch(OzClusterConfig),

    ?assertEqual([PanelNode1Hostname], cluster_management_test_utils:get_all_hosts(PanelNode1)),
    cluster_management_test_utils:assert_onedata_service_domain([PanelNode1], OzDomain),
    cluster_management_test_utils:assert_onedata_service_domain([PanelNode2], undefined),

    ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:post(PanelNode1, <<"/hosts">>, #{auth => root, json => #{
            <<"address">> => PanelNode2Hostname
        }})
    ),

    %% Assert new host has been added to cluster but no service was automatically started on it
    ?assertEqual(
        lists:usort([PanelNode1Hostname, PanelNode2Hostname]),
        lists:usort(cluster_management_test_utils:get_all_hosts(PanelNode1))
    ),
    ?assertEqual([PanelNode1Hostname], cluster_management_test_utils:get_service_hosts(PanelNode1, ?ONEZONE, worker)),
    ?assertEqual([PanelNode1Hostname], cluster_management_test_utils:get_service_hosts(PanelNode1, ?ONEZONE, manager)),
    ?assertEqual([PanelNode1Hostname], cluster_management_test_utils:get_service_hosts(PanelNode1, ?ONEZONE, database)),
    % Assert added node copied configured service envs
    cluster_management_test_utils:assert_onedata_service_domain(PanelNodes, OzDomain).


deploy_new_worker_test(Config) ->
    [Node1, Node2] = ?config(oz_panel_nodes, Config),

    Domain = dns_test_utils:get_k8s_service_domain(Node1),
    Node1Details = cluster_management_test_utils:infer_node_details(Node1),
    Node1Hostname = Node1Details#node_details.hostname,
    Node2Hostname = dns_test_utils:get_hostname(Node2),

    ?assertEqual(
        #{Node1Hostname => <<"healthy">>},
        cluster_management_test_utils:get_service_status_cluster_wide(Node1, ?ONEZONE, worker)
    ),
    [WorkerNode1] = get_worker_node(Node1),
    ?assertEqual([WorkerNode1], get_worker_chash_nodes(WorkerNode1)),
    % Assert domain set on both panel nodes and single worker node
    cluster_management_test_utils:assert_onedata_service_domain(zone, Domain),

    {ok, _, _, #{<<"taskId">> := TaskId}} = ?assertMatch(
        {ok, ?HTTP_202_ACCEPTED, _, _},
        panel_test_rest:post(Node1, <<"/zone/workers">>, #{
            auth => root,
            json => #{<<"hosts">> => [Node2Hostname]}
        })
    ),
    cluster_management_test_utils:await_task_status(Node1, TaskId, <<"ok">>),
    cluster_management_test_utils:refresh_oct(Config),

    ?assertEqual(
        #{Node1Hostname => <<"healthy">>, Node2Hostname => <<"healthy">>},
        cluster_management_test_utils:get_service_status_cluster_wide(Node1, ?ONEZONE, worker),
        ?ATTEMPTS
    ),
    % Assert all nodes, including new worker node, has domain set (oct was refreshed and is aware of every new node)
    cluster_management_test_utils:assert_onedata_service_domain(zone, Domain),

    [WorkerNode2] = get_worker_node(Node1) -- [WorkerNode1],
    ?assertEqual(
        lists:usort([WorkerNode1, WorkerNode2]),
        lists:usort(get_worker_chash_nodes(WorkerNode1))
    ),
    ?assertEqual(
        panel_test_rpc:call(Node1, service_oz_worker, get_policies, []),
        panel_test_rpc:call(Node2, service_oz_worker, get_policies, [])
    ).


%% @private
get_worker_node(Node) ->
    panel_test_rpc:call(Node, service_oz_worker, get_nodes, []).


%% @private
-spec get_worker_chash_nodes(node()) -> [node()].
get_worker_chash_nodes(WorkerNode) ->
    erpc:call(WorkerNode, consistent_hashing, get_all_nodes, []).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1oz_2nodes_not_deployed",
        envs = [
            {oz_panel, ctool, [
                % Allow Onezone panel to connect with Pebble server
                {force_insecure_connections, true}
            ]}
        ],
        posthook = fun(NewConfig) ->
            panel_test_rest:set_host_address_infer_policy(ip),
            % Requests should be made without cert verification as requests
            % are made using ip to ensure they are directed to specific host
            panel_test_rest:set_insecure_flag(),

            NewConfig
        end
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_, Config) ->
    % Refresh oct to ensure all changes in cluster are visible in background config/cache
    cluster_management_test_utils:refresh_oct(Config).
