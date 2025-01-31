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
-include("cert_test_utils.hrl").
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

-define(AWAIT_DEPLOYMENT_READY_ATTEMPTS, 60).


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

    ProviderName = <<"krakow">>,
    OpClusterConfig = #op_cluster_config{
        nodes = #{
            1 => cluster_deployment_test_utils:infer_node_details(OpPanelNode1),
            2 => OpPanelNode2Details
        },
        managers = [1, 2],
        main_manager = 1,
        workers = [1],
        databases = [1],
        name = ProviderName,
        admin_email = <<"admin@example.eu">>,
        register = true,
        registration_token = RegistrationToken,
        subdomain_delegation = true,
        subdomain = ProviderName,
        lets_encrypt = true
    },

    % Cluster deployed without OneS3 should have no host with OneS3
    cluster_deployment_test_utils:deploy_cluster(OpClusterConfig),
    ?assertEqual(#{}, cluster_test_utils:get_ones3_status_all(OpPanelNode1)),
    ExpOnedataTestCertDetails = #{
        <<"issuer">> => ?ONEDATA_TEST_CERT_ISSUER,
        % Domain status cannot be validated for not registered providers
        % resulting in status unknown
        <<"status">> => <<"unknown">>
    },
    cert_test_utils:assert_cert_details(OpPanelNode1, ExpOnedataTestCertDetails),

    % Deploying OneS3 on proper host before provider registration enables the
    % service on selected host but DOES NOT start it!
    % Also, no changes to certificates are made
    cluster_deployment_test_utils:deploy_ones3(OpClusterConfig#op_cluster_config{
        ones3_nodes = [2]
    }),
    ?assertEqual(
        #{OpPanelNode2Host => <<"stopped">>},
        cluster_test_utils:get_ones3_status_all(OpPanelNode1)
    ),
    cert_test_utils:assert_cert_details(OpPanelNode1, ExpOnedataTestCertDetails),

    % Enabled OneS3 are started right after provider is registered in oz.
    % But still, no changes to certificates are made.
    cluster_deployment_test_utils:register_provider(OpClusterConfig),
    ?assertEqual(
        #{OpPanelNode2Host => <<"healthy">>},
        cluster_test_utils:get_ones3_status_all(OpPanelNode1),
        ?AWAIT_DEPLOYMENT_READY_ATTEMPTS
    ),
    cert_test_utils:assert_cert_details(OpPanelNode1, ExpOnedataTestCertDetails#{
        <<"status">> => <<"domain_mismatch">>
    }),

    cluster_deployment_test_utils:configure_dns(OpClusterConfig),

    % Enabling Lets Encrypt should result in new certificates for op domain and s3 subdomain
    cluster_deployment_test_utils:configure_web_cert(OpClusterConfig),
    OzDomain = oct_background:get_zone_domain(),
    ProviderDomain = <<ProviderName/binary, ".", OzDomain/binary>>,
    OneS3Domain = <<"s3.", ProviderDomain/binary>>,
    ExpPebbleCertDetails = #{
        <<"status">> => <<"valid">>,
        <<"letsEncrypt">> => true,
        <<"domain">> => ProviderDomain,
        <<"dnsNames">> => lists:sort([ProviderDomain, OneS3Domain])
    },
    AllPebbleCertDetails = cert_test_utils:assert_cert_details(OpPanelNode1, ExpPebbleCertDetails),
    cert_test_utils:assert_newly_issued_pebble_cert(AllPebbleCertDetails),

    ?assertMatch({ok, _}, gen_tcp:connect(OpPanelNode2Ip, OneS3Port, [], 10), ?AWAIT_DEPLOYMENT_READY_ATTEMPTS),
    ok.


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
