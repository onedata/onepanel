%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Lets Encrypt for Onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(le_oz_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("cert_test_utils.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% exported for CT
-export([
    groups/0, all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]).

% Tests
-export([
    get_certificate_metadata_test/1,
    toggle_lets_encrypt_test/1,
    valid_certificate_should_not_be_replaced_test/1,

    non_lets_encrypt_issued_certificate_should_be_replaced_with_http_challenge_test/1,
    non_lets_encrypt_issued_certificate_should_be_replaced_with_dns_challenge_test/1,

    domain_mismatched_certificate_should_be_replaced_with_http_challenge_test/1,
    domain_mismatched_certificate_should_be_replaced_with_dns_challenge_test/1,

    expired_certificate_should_be_replaced_with_http_challenge_test/1,
    expired_certificate_should_be_replaced_with_dns_challenge_test/1,

    disabling_lets_encrypt_should_do_nothing_to_already_present_certificate_test/1,
    failed_certification_attempt_leaves_lets_encrypt_disabled_test/1,
    failed_certification_attempt_leaves_lets_encrypt_enabled_test/1
]).

groups() -> [
    {http_challenge, [], [
        non_lets_encrypt_issued_certificate_should_be_replaced_with_http_challenge_test,
        domain_mismatched_certificate_should_be_replaced_with_http_challenge_test,
        expired_certificate_should_be_replaced_with_http_challenge_test
    ]},

    {dns_challenge, [], [
        non_lets_encrypt_issued_certificate_should_be_replaced_with_dns_challenge_test,
        domain_mismatched_certificate_should_be_replaced_with_dns_challenge_test,
        expired_certificate_should_be_replaced_with_dns_challenge_test
    ]}
].

all() -> [
    get_certificate_metadata_test,
    toggle_lets_encrypt_test,
    valid_certificate_should_not_be_replaced_test,

    {group, http_challenge},
    {group, dns_challenge},

    disabling_lets_encrypt_should_do_nothing_to_already_present_certificate_test,
    failed_certification_attempt_leaves_lets_encrypt_disabled_test,
    failed_certification_attempt_leaves_lets_encrypt_enabled_test
].

% Increase certification attempts as pebble may fail several times
% (not offering challenge, etc.) which is even better as it lets
% us tests certification retries
-define(CERTIFICATION_ATTEMPTS, 10).

-define(CERTIFICATION_FLOW_ERROR, ?ERROR_LETS_ENCRYPT_RESPONSE(<<>>, <<>>)).

-define(ATTEMPTS, 100).


%%%===================================================================
%%% API
%%%===================================================================


get_certificate_metadata_test(Config) ->
    cert_test_utils:update_lets_encrypt(zone, disable),
    cert_test_utils:deploy_certs(zone, ?PEBBLE_VALID_CERT_DIR_NAME, Config),

    OzDomain = dns_test_utils:get_zone_domain(),

    ScenarioSpec = #scenario_spec{
        test_proxied_onepanel_rest_endpoint = false,
        name = <<"Get Onezone webcertificate metadata using /web_cert endpoint">>,
        type = rest,
        target_nodes = oct_background:get_zone_panels(),
        client_spec = #client_spec{
            correct = [
                root,
                member
            ],
            unauthorized = [
                guest,
                {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_PANEL, <<"onezone">>))}
                | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
            ],
            forbidden = [peer]
        },

        prepare_args_fun = fun(_) ->
            #rest_args{
                method = get,
                path = <<"web_cert">>
            }
        end,

        validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
            ?assertMatch(
                #{
                    <<"letsEncrypt">> := false,
                    <<"expirationTime">> := _,
                    <<"creationTime">> := _,
                    <<"paths">> := #{
                        <<"cert">> := _,
                        <<"key">> := _,
                        <<"chain">> := _
                    },
                    <<"domain">> := OzDomain,
                    <<"dnsNames">> := [OzDomain],
                    <<"issuer">> := _,
                    <<"status">> := <<"valid">>
                },
                Body
            ),
            ?assertEqual(8, length(maps:keys(Body)))
        end)
    },
    ?assert(api_test_runner:run_tests([ScenarioSpec])),

    cert_test_utils:update_lets_encrypt(zone, enable),

    ?assert(api_test_runner:run_tests([ScenarioSpec#scenario_spec{
        validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
            ?assertMatch(
                #{
                    <<"letsEncrypt">> := true,
                    <<"expirationTime">> := _,
                    <<"creationTime">> := _,
                    <<"paths">> := #{
                        <<"cert">> := _,
                        <<"key">> := _,
                        <<"chain">> := _
                    },
                    <<"domain">> := OzDomain,
                    <<"dnsNames">> := [OzDomain],
                    <<"issuer">> := _,
                    <<"status">> := <<"valid">>,
                    <<"lastRenewalFailure">> := _,
                    <<"lastRenewalSuccess">> := _
                },
                Body
            ),
            ?assertEqual(10, length(maps:keys(Body)))
        end)
    }])).


toggle_lets_encrypt_test(Config) ->
    cert_test_utils:update_lets_encrypt(zone, disable),
    cert_test_utils:deploy_certs(zone, ?PEBBLE_VALID_CERT_DIR_NAME, Config),

    ?assert(api_test_runner:run_tests([#scenario_spec{
        test_proxied_onepanel_rest_endpoint = false,
        name = <<"Toggle Onezone Lets Enrytp using /web_cert endpoint">>,
        type = rest,
        target_nodes = oct_background:get_zone_panels(),
        client_spec = #client_spec{
            correct = [
                root,
                {member, [?CLUSTER_UPDATE]}
            ],
            unauthorized = [
                guest,
                {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_PANEL, <<"onezone">>))}
                | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
            ],
            forbidden = [
                peer,
                {member, privileges:cluster_privileges() -- [?CLUSTER_UPDATE]}
            ]
        },
        data_spec = #data_spec{
            required = [<<"letsEncrypt">>],
            correct_values = #{<<"letsEncrypt">> => [true, false]},
            bad_values = [{<<"letsEncrypt">>, bul, ?ERROR_BAD_VALUE_BOOLEAN(<<"letsEncrypt">>)}]
        },
        prepare_args_fun = fun(#api_test_ctx{data = Data}) ->
            #rest_args{
                method = patch,
                path = <<"web_cert">>,
                headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                body = json_utils:encode(Data)
            }
        end,
        validate_result_fun = api_test_validate:http_204_no_content()
    }])).


valid_certificate_should_not_be_replaced_test(Config) ->
    cert_test_utils:update_lets_encrypt(zone, disable),
    cert_test_utils:deploy_certs(zone, ?PEBBLE_VALID_CERT_DIR_NAME, Config),

    OzDomain = dns_test_utils:get_zone_domain(),
    ExpBasicCertDetails = #{
        <<"domain">> => OzDomain,
        <<"dnsNames">> => [OzDomain],
        <<"status">> => <<"valid">>,
        <<"letsEncrypt">> => false
    },
    AllCertDetails = cert_test_utils:assert_cert_details(zone, ExpBasicCertDetails),

    cert_test_utils:update_lets_encrypt(zone, enable),

    ExpAllCertDetails = AllCertDetails#{<<"letsEncrypt">> => true},
    cert_test_utils:assert_cert_details(zone, ExpAllCertDetails).


non_lets_encrypt_issued_certificate_should_be_replaced_with_http_challenge_test(Config) ->
    non_lets_encrypt_issued_certificate_should_be_replaced_test_base(Config).


non_lets_encrypt_issued_certificate_should_be_replaced_with_dns_challenge_test(Config) ->
    non_lets_encrypt_issued_certificate_should_be_replaced_test_base(Config).


%% @private
non_lets_encrypt_issued_certificate_should_be_replaced_test_base(Config) ->
    cert_test_utils:update_lets_encrypt(zone, disable),
    cert_test_utils:deploy_certs(zone, ?ONEDATA_TEST_CERT_DIR_NAME, Config),

    OzDomain = dns_test_utils:get_zone_domain(),
    ExpBasicCertDetails = #{
        <<"domain">> => OzDomain,
        <<"dnsNames">> => [OzDomain],
        <<"status">> => <<"valid">>
    },
    ExpOnedataTestCertDetails = ExpBasicCertDetails#{
        <<"issuer">> => ?ONEDATA_TEST_CERT_ISSUER,
        <<"letsEncrypt">> => false
    },
    cert_test_utils:assert_cert_details(zone, ExpOnedataTestCertDetails),

    cert_test_utils:update_lets_encrypt(zone, enable),

    ExpPebbleCertDetails = ExpBasicCertDetails#{<<"letsEncrypt">> => true},
    AllPebbleCertDetails = cert_test_utils:assert_cert_details(zone, ExpPebbleCertDetails),
    cert_test_utils:assert_newly_issued_pebble_cert(AllPebbleCertDetails).


domain_mismatched_certificate_should_be_replaced_with_http_challenge_test(Config) ->
    domain_mismatched_certificate_should_be_replaced_test_base(Config).


domain_mismatched_certificate_should_be_replaced_with_dns_challenge_test(Config) ->
    domain_mismatched_certificate_should_be_replaced_test_base(Config).


%% @private
domain_mismatched_certificate_should_be_replaced_test_base(Config) ->
    cert_test_utils:update_lets_encrypt(zone, disable),
    cert_test_utils:deploy_certs(zone, ?PEBBLE_DOMAIN_MISMATCH_CERT_DIR_NAME, Config),

    ExpDomainMismatchedCertDetails = #{
        <<"domain">> => ?PEBBLE_DOMAIN_MISMATCH_FAKE_DOMAIN,
        <<"dnsNames">> => [?PEBBLE_DOMAIN_MISMATCH_FAKE_DOMAIN],
        <<"status">> => <<"domain_mismatch">>,
        <<"letsEncrypt">> => false
    },
    cert_test_utils:assert_cert_details(zone, ExpDomainMismatchedCertDetails),

    cert_test_utils:update_lets_encrypt(zone, enable),

    OzDomain = dns_test_utils:get_zone_domain(),
    ExpPebbleCertDetails = #{
        <<"domain">> => OzDomain,
        <<"dnsNames">> => [OzDomain],
        <<"status">> => <<"valid">>,
        <<"letsEncrypt">> => true
    },
    AllPebbleCertDetails = cert_test_utils:assert_cert_details(zone, ExpPebbleCertDetails),
    cert_test_utils:assert_newly_issued_pebble_cert(AllPebbleCertDetails).


expired_certificate_should_be_replaced_with_http_challenge_test(Config) ->
    expired_certificate_should_be_replaced_test_base(Config).


expired_certificate_should_be_replaced_with_dns_challenge_test(Config) ->
    expired_certificate_should_be_replaced_test_base(Config).


%% @private
expired_certificate_should_be_replaced_test_base(Config) ->
    cert_test_utils:update_lets_encrypt(zone, disable),
    cert_test_utils:deploy_certs(zone, ?PEBBLE_EXPIRED_CERT_DIR_NAME, Config),

    OzDomain = dns_test_utils:get_zone_domain(),
    ExpBasicCertDetails = #{
        <<"domain">> => OzDomain,
        <<"dnsNames">> => [OzDomain]
    },
    ExpExpiredCertDetails = ExpBasicCertDetails#{
        <<"status">> => <<"expired">>,
        <<"letsEncrypt">> => false
    },
    cert_test_utils:assert_cert_details(zone, ExpExpiredCertDetails),

    cert_test_utils:update_lets_encrypt(zone, enable),

    ExpPebbleCertDetails = ExpBasicCertDetails#{
        <<"status">> => <<"valid">>,
        <<"letsEncrypt">> => true
    },
    AllPebbleCertDetails = cert_test_utils:assert_cert_details(zone, ExpPebbleCertDetails),
    cert_test_utils:assert_newly_issued_pebble_cert(AllPebbleCertDetails).


disabling_lets_encrypt_should_do_nothing_to_already_present_certificate_test(Config) ->
    % Run previous test code to ensure new certificate was issued
    expired_certificate_should_be_replaced_test_base(Config),
    CertDetails = cert_test_utils:get_cert_details(zone),

    cert_test_utils:update_lets_encrypt(zone, disable),

    ExpCertDetails = maps:without(
        [<<"lastRenewalFailure">>, <<"lastRenewalSuccess">>],
        CertDetails#{<<"letsEncrypt">> => false}
    ),
    ?assertEqual(ExpCertDetails, cert_test_utils:get_cert_details(zone)).


failed_certification_attempt_leaves_lets_encrypt_disabled_test(Config) ->
    failed_certification_attempt_leaves_lets_encrypt_intact_test_base(Config).


failed_certification_attempt_leaves_lets_encrypt_enabled_test(Config) ->
    failed_certification_attempt_leaves_lets_encrypt_intact_test_base(Config).


%% @private
failed_certification_attempt_leaves_lets_encrypt_intact_test_base(_Config) ->
    KeyToRm = <<"lastRenewalFailure">>,
    CertDetails = maps:remove(KeyToRm, cert_test_utils:get_cert_details(zone)),

    {ok, _, _, #{<<"error">> := RespError}} = ?assertMatch(
        {ok, ?HTTP_400_BAD_REQUEST, _, #{<<"error">> := _}},
        cert_test_utils:try_update_lets_encrypt(zone, enable)
    ),
    ?assertMatch(?ERROR_ON_NODES(?CERTIFICATION_FLOW_ERROR, _), errors:from_json(RespError)),

    ?assertEqual(CertDetails, maps:remove(KeyToRm, cert_test_utils:get_cert_details(zone))).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils, cert_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1oz",
        envs = [
            {oz_panel, onepanel, [
                {letsencrypt_issuer_regex, ?RE_PEBBLE_ISSUER},
                % Increase certification attempts as pebble likes to fail from time to time
                {letsencrypt_attempts, ?CERTIFICATION_ATTEMPTS}
            ]}
        ],
        posthook = fun(NewConfig) ->
            % Requests should be made without cert verification due to possibly
            % incorrect certificates (tests will mess with them)
            panel_test_rest:set_insecure_flag(),

            NewConfig
        end
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


init_per_group(Group, Config) ->
    {ChallengeType, EnableSubdomainDelegation} = case Group of
        http_challenge -> {http, false};
        dns_challenge -> {dns, true}
    end,
    PanelNoes = oct_background:get_zone_panels(),
    test_utils:mock_new(PanelNoes, letsencrypt_api),
    test_utils:mock_expect(PanelNoes, letsencrypt_api, challenge_types, fun() ->
        [ChallengeType]
    end),
    dns_test_utils:update_zone_subdomain_delegation(EnableSubdomainDelegation),

    Config.


end_per_group(_Group, Config) ->
    PanelNoes = oct_background:get_zone_panels(),
    test_utils:mock_unload(PanelNoes, [letsencrypt_api]),

    Config.


init_per_testcase(Testcase, Config) when
    Testcase =:= failed_certification_attempt_leaves_lets_encrypt_disabled_test;
    Testcase =:= failed_certification_attempt_leaves_lets_encrypt_enabled_test
->
    % Decrease certification attempts so that test will not hung for longer
    % than it needs (certification fails due to mocked error)
    cert_test_utils:set_certification_attempts(zone, 2),

%%    cert_test_utils:deploy_certs(zone, ?PEBBLE_VALID_CERT_DIR_NAME, Config),
    cert_test_utils:update_lets_encrypt(zone, case Testcase of
        failed_certification_attempt_leaves_lets_encrypt_disabled_test -> disable;
        failed_certification_attempt_leaves_lets_encrypt_enabled_test -> enable
    end),
    cert_test_utils:deploy_certs(zone, ?PEBBLE_EXPIRED_CERT_DIR_NAME, Config),

    PanelNodes = oct_background:get_zone_panels(),
    test_utils:mock_new(PanelNodes, [letsencrypt_api], [passthrough]),
    test_utils:mock_expect(PanelNodes, letsencrypt_api, run_certification_flow, fun(_) ->
        throw(?CERTIFICATION_FLOW_ERROR)
    end),

    init_per_testcase(?DEFAULT_CASE(Testcase), Config);

init_per_testcase(_Testcase, Config) ->
    PanelNodes = oct_background:get_zone_panels(),
    test_utils:mock_new(PanelNodes, [service_oz_worker, service_onepanel], [passthrough]),

    Config.


end_per_testcase(Testcase, Config) when
    Testcase =:= failed_certification_attempt_leaves_lets_encrypt_disabled_test;
    Testcase =:= failed_certification_attempt_leaves_lets_encrypt_enabled_test
->
    PanelNodes = oct_background:get_zone_panels(),
    test_utils:mock_unload(PanelNodes, [letsencrypt_api]),

    end_per_testcase(?DEFAULT_CASE(Testcase), Config);

end_per_testcase(_Testcase, Config) ->
    PanelNodes = oct_background:get_zone_panels(),
    test_utils:mock_unload(PanelNodes, [service_oz_worker, service_onepanel]),

    cert_test_utils:set_certification_attempts(zone, ?CERTIFICATION_ATTEMPTS),
    cert_test_utils:deploy_certs(zone, ?PEBBLE_VALID_CERT_DIR_NAME, Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================


