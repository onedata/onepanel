%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Oneprovider deployment with adding ones3 before
%%% registration in oz.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_deployment_op_add_ones3_before_registration_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("cluster_deployment_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    deploy_test/1
]).

all() -> [
    deploy_test
].

-define(AWAIT_DEPLOYMENT_READY_ATTEMPTS, 180).

% Time caveat is required in temporary tokens
-define(DEFAULT_TEMP_CAVEAT_TTL, 36000).


%%%===================================================================
%%% Tests
%%%===================================================================


deploy_test(Config) ->
    AdminUserId = oct_background:get_user_id(admin),
    RegistrationToken = tokens_test_utils:create_provider_registration_token(AdminUserId),

    [OpPanelNode1, OpPanelNode2] = ?config(op_panel_nodes, Config),
    OpPanelNode2Details = cluster_deployment_test_utils:infer_node_details(OpPanelNode2),
    OpPanelNode2Host = OpPanelNode2Details#node_details.hostname,
    OpPanelNode2Ip = OpPanelNode2Details#node_details.ip,

    OneS3Port = panel_test_rpc:call(OpPanelNode1, service_ones3, get_port, []),
    panel_test_rpc:set_emergency_passphrase(OpPanelNode1, ?ONENV_EMERGENCY_PASSPHRASE),

    OpClusterConfig = #op_cluster_config{
        nodes = #{
            1 => cluster_deployment_test_utils:infer_node_details(OpPanelNode1),
            2 => OpPanelNode2Details
        },
        managers = [1, 2],
        main_manager = 1,
        workers = [1],
        databases = [1],
        name = <<"krakow">>,
        admin_email = <<"admin@example.eu">>,
        register = true,
        registration_token = RegistrationToken,
        subdomain_delegation = true,
        subdomain = <<"krakow">>,
        lets_encrypt = true
    },

    cluster_deployment_test_utils:deploy_cluster(OpClusterConfig),
    ?assertEqual(#{}, get_ones3_status(OpPanelNode1)),

    cluster_deployment_test_utils:deploy_ones3(OpClusterConfig#op_cluster_config{
        ones3_nodes = [2]
    }),
    ?assertEqual(#{OpPanelNode2Host => <<"stopped">>}, get_ones3_status(OpPanelNode1)),

    cluster_deployment_test_utils:register_provider(OpClusterConfig),
    ?assertEqual(#{OpPanelNode2Host => <<"healthy">>}, get_ones3_status(OpPanelNode1), 60),

    cluster_deployment_test_utils:configure_dns(OpClusterConfig),
    cluster_deployment_test_utils:configure_web_cert(OpClusterConfig),

    ?assertMatch({ok, _}, gen_tcp:connect(OpPanelNode2Ip, OneS3Port, [], 10), ?AWAIT_DEPLOYMENT_READY_ATTEMPTS),
    ok.


%% @private
get_ones3_status(PanelNode) ->
    {ok, _, _, Resp} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(PanelNode, <<"/provider/ones3">>, #{auth => root})
    ),
    Resp.


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
            % Requests should be made without cert verification as provider
            % domain is set/changed during deployment
            panel_test_rest:set_insecure_flag(),

            dns_test_utils:update_zone_subdomain_delegation(true),
            NewConfig
        end
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().
