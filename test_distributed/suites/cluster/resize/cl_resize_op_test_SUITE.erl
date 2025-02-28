%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Oneprovider cluster resize.
%%% @end
%%%-------------------------------------------------------------------
-module(cl_resize_op_test_SUITE).
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
    [Node1, Node2] = ?config(op_panel_nodes, Config),
    Node1Details = cluster_management_test_utils:infer_node_details(Node1),
    Node1Hostname = Node1Details#node_details.hostname,
    Node2Hostname = dns_test_utils:get_hostname(Node2),

    panel_test_rpc:set_emergency_passphrase(Node1, ?ONENV_EMERGENCY_PASSPHRASE),

    ProviderName = <<"krakow">>,
    OpClusterConfig = #op_cluster_config{
        nodes = #{1 => Node1Details},
        managers = [1],
        main_manager = 1,
        workers = [1],
        databases = [1],
        name = ProviderName,
        register = true,
        registration_token = op_cluster_deployment_test_utils:get_registration_token(),
        domain = dns_test_utils:get_k8s_domain(Node1)
    },
    op_cluster_deployment_test_utils:deploy_batch(OpClusterConfig),

    ?assertEqual([Node1Hostname], cluster_management_test_utils:get_all_hosts(Node1)),

    ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:post(Node1, <<"/hosts">>, #{auth => root, json => #{
            <<"address">> => Node2Hostname
        }})
    ),

    %% Assert new host has been added to cluster but no service was automatically started on it
    ?assertEqual(
        lists:usort([Node1Hostname, Node2Hostname]),
        lists:usort(cluster_management_test_utils:get_all_hosts(Node1))
    ),
    ?assertEqual([Node1Hostname], cluster_management_test_utils:get_service_hosts(Node1, op, worker)),
    ?assertEqual([Node1Hostname], cluster_management_test_utils:get_service_hosts(Node1, op, manager)),
    ?assertEqual([Node1Hostname], cluster_management_test_utils:get_service_hosts(Node1, op, database)),
    ?assertEqual([], cluster_management_test_utils:get_service_hosts(Node1, op, ones3)).


deploy_new_worker_test(Config) ->
    [Node1, Node2] = ?config(op_panel_nodes, Config),

    Node1Details = cluster_management_test_utils:infer_node_details(Node1),
    Node1Hostname = Node1Details#node_details.hostname,
    Node2Hostname = dns_test_utils:get_hostname(Node2),

    TokenFilePath = onepanel_env:get_remote(Node1, op_worker_root_token_path, ?APP_NAME),
    GetTokenFileContentFun = fun(Node) ->
        panel_test_rpc:call(Node, file, read_file, [TokenFilePath])
    end,

    ?assertEqual(
        #{Node1Hostname => <<"healthy">>},
        cluster_management_test_utils:get_service_status_cluster_wide(Node1, op, worker)
    ),
    {ok, CurrentFileContents} = GetTokenFileContentFun(Node1),

    [WorkerNode1] = panel_test_rpc:get_op_worker_nodes(Node1),
    ?assertEqual([WorkerNode1], get_worker_chash_nodes(WorkerNode1)),

    {ok, _, _, #{<<"taskId">> := TaskId}} = ?assertMatch(
        {ok, ?HTTP_202_ACCEPTED, _, _},
        panel_test_rest:post(Node1, <<"/provider/workers">>, #{
            auth => root,
            json => #{<<"hosts">> => [Node2Hostname]}
        })
    ),
    cluster_management_test_utils:await_task_status(Node1, TaskId, <<"ok">>),

    ?assertEqual(
        #{Node1Hostname => <<"healthy">>, Node2Hostname => <<"healthy">>},
        cluster_management_test_utils:get_service_status_cluster_wide(Node1, op, worker),
        ?ATTEMPTS
    ),
    ?assertEqual({ok, CurrentFileContents}, GetTokenFileContentFun(Node2)),

    [WorkerNode2] = panel_test_rpc:get_op_worker_nodes(Node1) -- [WorkerNode1],
    ?assertEqual(
        lists:usort([WorkerNode1, WorkerNode2]),
        lists:usort(get_worker_chash_nodes(WorkerNode1))
    ).


%% @private
-spec get_worker_chash_nodes(node()) -> [node()].
get_worker_chash_nodes(WorkerNode) ->
    opw_test_rpc:call(WorkerNode, consistent_hashing, get_all_nodes, []).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1op_2nodes_not_deployed",
        envs = [
            {op_panel, ctool, [
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
