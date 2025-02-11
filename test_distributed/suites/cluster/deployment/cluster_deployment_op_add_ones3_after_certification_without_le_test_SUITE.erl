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
-module(cluster_deployment_op_add_ones3_after_certification_without_le_test_SUITE).
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

    panel_test_rpc:set_emergency_passphrase(OpPanelNode1, ?ONENV_EMERGENCY_PASSPHRASE),

    DefaultOneS3Port = ?rpc(OpPanelNode1, onepanel_env:get(ones3_http_port, ?APP_NAME)),
    OneS3PortToSet = ?RAND_ELEMENT([undefined, 6666, 7777, 8888, 9999]),
    ExpOneS3Port = utils:ensure_defined(OneS3PortToSet, DefaultOneS3Port),

    ProviderName = <<"krakow">>,
    ProviderDomain = get_op_domain(OpPanelNode1),

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
        subdomain_delegation = false,
        domain = ProviderDomain,
        lets_encrypt = false
    },

    % Cluster deployed without OneS3 should have no host with OneS3
    cluster_deployment_test_utils:deploy_cluster(OpClusterConfig),
    ?assertEqual(#{}, cluster_test_utils:get_ones3_status_all(OpPanelNode1)),
    ExpOnedataTestCertDetails = #{
        <<"issuer">> => ?ONEDATA_TEST_CERT_ISSUER,
        <<"letsEncrypt">> => false,
        <<"domain">> => ProviderDomain,
        <<"dnsNames">> => [ProviderDomain],
        % Domain status cannot be validated for not registered providers
        % resulting in status unknown
        <<"status">> => <<"unknown">>
    },
    cert_test_utils:assert_cert_details(OpPanelNode1, ExpOnedataTestCertDetails),

    cluster_deployment_test_utils:register_provider(OpClusterConfig),
    ?assertEqual(#{}, cluster_test_utils:get_ones3_status_all(OpPanelNode1)),
    AllCertDetails = cert_test_utils:assert_cert_details(OpPanelNode1, ExpOnedataTestCertDetails#{
        <<"status">> => <<"valid">>
    }),

    cluster_deployment_test_utils:configure_dns(OpClusterConfig),

    cluster_deployment_test_utils:configure_web_cert(OpClusterConfig),
    ?assertEqual(#{}, cluster_test_utils:get_ones3_status_all(OpPanelNode1)),
    cert_test_utils:assert_cert_details(OpPanelNode1, AllCertDetails),

    % Deploying OneS3 on proper host after certification enables the
    % service on selected host and immediately start it BUT DOES NOT regenerates certificate
    % (if lets encrypt is disabled)
    cluster_deployment_test_utils:deploy_ones3(OpClusterConfig#op_cluster_config{
        ones3_nodes = [2],
        ones3_port = OneS3PortToSet
    }),
    ?assertEqual(
        #{OpPanelNode2Host => <<"healthy">>},
        cluster_test_utils:get_ones3_status_all(OpPanelNode1),
        ?AWAIT_DEPLOYMENT_READY_ATTEMPTS
    ),
    cert_test_utils:assert_cert_details(OpPanelNode1, AllCertDetails#{
        % Cert is no longer valid as it contains only op domain in DNS names
        % when also s3 subdomain is expected
        <<"status">> => <<"domain_mismatch">>
    }),

    ?assertMatch({ok, _}, gen_tcp:connect(OpPanelNode2Ip, ExpOneS3Port, [], 10), ?AWAIT_DEPLOYMENT_READY_ATTEMPTS),
    ok.


%% @private
get_op_domain(OpPanelNode) ->
    {ok, OzDomain} = test_utils:get_env(OpPanelNode, ?APP_NAME, test_web_cert_domain),
    str_utils:to_binary(OzDomain).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1op_2nodes_not_deployed",
        envs = [
            {op_panel, onepanel, [
                % Do not include s3 subdomain in generated test cert
                {include_s3_subdomain_in_test_cert, false}
            ]},
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
