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
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    add_node_test/1
]).

all() -> [
    add_node_test
].


%%%===================================================================
%%% Tests
%%%===================================================================


add_node_test(Config) ->
    [Node1, Node2] = ?config(op_panel_nodes, Config),
    Node1Details = op_cluster_deployment_test_utils:infer_node_details(Node1),
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
    ?assertEqual([Node1Hostname], cluster_management_test_utils:get_service_hosts(Node1, worker)),
    ?assertEqual([Node1Hostname], cluster_management_test_utils:get_service_hosts(Node1, manager)),
    ?assertEqual([Node1Hostname], cluster_management_test_utils:get_service_hosts(Node1, database)),
    ?assertEqual([], cluster_management_test_utils:get_service_hosts(Node1, ones3)).


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
