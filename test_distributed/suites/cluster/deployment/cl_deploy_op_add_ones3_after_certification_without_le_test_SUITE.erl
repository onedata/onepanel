%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Oneprovider deployment with adding ones3 after
%%% deployment without Lets Encrypt enabled.
%%% @end
%%%-------------------------------------------------------------------
-module(cl_deploy_op_add_ones3_after_certification_without_le_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("cert_test_utils.hrl").
-include("cluster_deployment_test_utils.hrl").
-include("names.hrl").
-include("onepanel_test_utils.hrl").
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

-define(ATTEMPTS, 60).


%%%===================================================================
%%% Tests
%%%===================================================================


deploy_test(Config) ->
    [Node1, Node2] = ?config(op_panel_nodes, Config),
    Node1Ip = ip_test_utils:get_node_ip(Node1),
    Node2Details = cluster_management_test_utils:infer_node_details(Node2),
    Node2Ip = Node2Details#node_details.ip,

    panel_test_rpc:set_emergency_passphrase(Node1, ?ONENV_EMERGENCY_PASSPHRASE),

    ProviderName = <<"krakow">>,
    ProviderDomain = dns_test_utils:get_k8s_domain(Node1),

    OpClusterConfig = #op_cluster_config{
        nodes = #{
            1 => cluster_management_test_utils:infer_node_details(Node1),
            2 => Node2Details
        },
        managers = [1, 2],
        main_manager = 1,
        workers = [1],
        databases = [1],
        name = ProviderName,
        register = true,
        registration_token = op_cluster_deployment_test_utils:get_registration_token(),
        subdomain_delegation = false,
        domain = ProviderDomain,
        lets_encrypt = false
    },

    % Cluster deployed without OneS3 should have no host with OneS3
    op_cluster_deployment_test_utils:deploy_all_services(OpClusterConfig),
    ?assertEqual(#{}, cluster_management_test_utils:get_ones3_status_cluster_wide(Node1)),
    ExpOnedataTestCertDetails = #{
        <<"issuer">> => ?ONEDATA_TEST_CERT_ISSUER,
        <<"letsEncrypt">> => false,
        <<"domain">> => ProviderDomain,
        <<"dnsNames">> => [ProviderDomain],
        % Domain status cannot be validated for not registered providers
        % resulting in status unknown
        <<"status">> => <<"unknown">>
    },
    cert_test_utils:assert_cert_details(Node1, ExpOnedataTestCertDetails),

    op_cluster_deployment_test_utils:register_provider(OpClusterConfig),
    ?assertEqual(#{}, cluster_management_test_utils:get_ones3_status_cluster_wide(Node1)),
    AllCertDetails = cert_test_utils:assert_cert_details(Node1, ExpOnedataTestCertDetails#{
        <<"status">> => <<"valid">>
    }),

    op_cluster_deployment_test_utils:configure_dns(OpClusterConfig),
    op_cluster_deployment_test_utils:configure_web_cert(OpClusterConfig),
    ?assertEqual(#{}, cluster_management_test_utils:get_ones3_status_cluster_wide(Node1)),
    cert_test_utils:assert_cert_details(Node1, AllCertDetails),

    % Deploying OneS3 on proper host after certification enables the
    % service on selected host and immediately start it BUT DOES NOT regenerates certificate
    % (if lets encrypt is disabled)
    DefaultOneS3Port = cluster_management_test_utils:get_ones3_port(Node1),
    OneS3PortToSet = ?RAND_ELEMENT([undefined, 16666, 17777, 18888, 19999]),
    ExpOneS3Port = utils:ensure_defined(OneS3PortToSet, DefaultOneS3Port),

    op_cluster_deployment_test_utils:deploy_ones3_service(OpClusterConfig#op_cluster_config{
        ones3_nodes = [2],
        ones3_port = OneS3PortToSet
    }),
    ?assertEqual(
        #{Node2Details#node_details.hostname => <<"healthy">>},
        cluster_management_test_utils:get_ones3_status_cluster_wide(Node1),
        ?ATTEMPTS
    ),
    ?assertMatch({error, econnrefused}, gen_tcp:connect(Node1Ip, ExpOneS3Port, [], 10), ?ATTEMPTS),
    ?assertMatch({ok, _}, gen_tcp:connect(Node2Ip, ExpOneS3Port, [], 10), ?ATTEMPTS),

    cert_test_utils:assert_cert_details(Node1, AllCertDetails#{
        % Cert is no longer valid as it contains only op domain in DNS names
        % when also s3 subdomain is expected
        <<"status">> => <<"domain_mismatch">>
    }),

    ok.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1op_2nodes_not_deployed_pebble",
        envs = [
            {op_panel, onepanel, [
                % Do not include s3 subdomain in generated test cert
                {include_s3_subdomain_in_test_cert, false}
            ]},
            {op_panel, ctool, [
                % Allow Oneprovider panel to connect with Pebble server
                {force_insecure_connections, true}
            ]},
            {op_panel, onepanel, [
                {ones3_log_level, 3}
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
