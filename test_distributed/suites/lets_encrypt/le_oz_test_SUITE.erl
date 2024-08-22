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

-include("cert_test_utils.hrl").
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

    valid_certificate_should_not_be_replaced_test/1,

    non_lets_encrypt_issued_certificate_should_be_replaced_with_http_challenge_test/1,
    non_lets_encrypt_issued_certificate_should_be_replaced_with_dns_challenge_test/1,

    domain_mismatched_certificate_should_be_replaced_with_http_challenge_test/1,
    domain_mismatched_certificate_should_be_replaced_with_dns_challenge_test/1,

    expired_certificate_should_be_replaced_with_http_challenge_test/1,
    expired_certificate_should_be_replaced_with_dns_challenge_test/1
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
    valid_certificate_should_not_be_replaced_test,

    {group, http_challenge},
    {group, dns_challenge}
].


-define(ATTEMPTS, 100).


%%%===================================================================
%%% API
%%%===================================================================


get_certificate_metadata_test(Config) ->
    cert_test_utils:disable_lets_encrypt(zone),
    cert_test_utils:deploy_certs(zone, ?PEBBLE_VALID_CERT_DIR_NAME, Config),

    OzDomain = dns_test_utils:get_zone_domain(),
    CertMetadataWithDisabledLetsEncrypt = ?assertMatch(
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
        cert_test_utils:get_cert_details(zone)
    ),
    ?assertEqual(8, length(maps:keys(CertMetadataWithDisabledLetsEncrypt))),

    cert_test_utils:enable_lets_encrypt(zone),

    CertMetadataWithEnabledLetsEncrypt = ?assertMatch(
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
        cert_test_utils:get_cert_details(zone)
    ),
    ?assertEqual(10, length(maps:keys(CertMetadataWithEnabledLetsEncrypt))).


valid_certificate_should_not_be_replaced_test(Config) ->
    cert_test_utils:disable_lets_encrypt(zone),
    cert_test_utils:deploy_certs(zone, ?PEBBLE_VALID_CERT_DIR_NAME, Config),

    OzDomain = dns_test_utils:get_zone_domain(),
    ExpBasicCertDetails = #{
        <<"domain">> => OzDomain,
        <<"dnsNames">> => [OzDomain],
        <<"status">> => <<"valid">>,
        <<"letsEncrypt">> => false
    },
    AllCertDetails = cert_test_utils:assert_cert_details(zone, ExpBasicCertDetails),

    cert_test_utils:enable_lets_encrypt(zone),

    ExpAllCertDetails = AllCertDetails#{<<"letsEncrypt">> => true},
    cert_test_utils:assert_cert_details(zone, ExpAllCertDetails).


non_lets_encrypt_issued_certificate_should_be_replaced_with_http_challenge_test(Config) ->
    non_lets_encrypt_issued_certificate_should_be_replaced_test_base(Config).


non_lets_encrypt_issued_certificate_should_be_replaced_with_dns_challenge_test(Config) ->
    non_lets_encrypt_issued_certificate_should_be_replaced_test_base(Config).


%% @private
non_lets_encrypt_issued_certificate_should_be_replaced_test_base(Config) ->
    cert_test_utils:disable_lets_encrypt(zone),
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

    cert_test_utils:enable_lets_encrypt(zone),

    ExpPebbleCertDetails = ExpBasicCertDetails#{<<"letsEncrypt">> => true},
    AllPebbleCertDetails = cert_test_utils:assert_cert_details(zone, ExpPebbleCertDetails),
    cert_test_utils:assert_newly_issued_pebble_cert(AllPebbleCertDetails).


domain_mismatched_certificate_should_be_replaced_with_http_challenge_test(Config) ->
    domain_mismatched_certificate_should_be_replaced_test_base(Config).


domain_mismatched_certificate_should_be_replaced_with_dns_challenge_test(Config) ->
    domain_mismatched_certificate_should_be_replaced_test_base(Config).


%% @private
domain_mismatched_certificate_should_be_replaced_test_base(Config) ->
    cert_test_utils:disable_lets_encrypt(zone),
    cert_test_utils:deploy_certs(zone, ?PEBBLE_DOMAIN_MISMATCH_CERT_DIR_NAME, Config),

    ExpDomainMismatchedCertDetails = #{
        <<"domain">> => ?PEBBLE_DOMAIN_MISMATCH_FAKE_DOMAIN,
        <<"dnsNames">> => [?PEBBLE_DOMAIN_MISMATCH_FAKE_DOMAIN],
        <<"status">> => <<"domain_mismatch">>,
        <<"letsEncrypt">> => false
    },
    cert_test_utils:assert_cert_details(zone, ExpDomainMismatchedCertDetails),

    cert_test_utils:enable_lets_encrypt(zone),

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
    cert_test_utils:disable_lets_encrypt(zone),
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

    cert_test_utils:enable_lets_encrypt(zone),

    ExpPebbleCertDetails = ExpBasicCertDetails#{
        <<"status">> => <<"valid">>,
        <<"letsEncrypt">> => true
    },
    AllPebbleCertDetails = cert_test_utils:assert_cert_details(zone, ExpPebbleCertDetails),
    cert_test_utils:assert_newly_issued_pebble_cert(AllPebbleCertDetails).


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
                {letsencrypt_attempts, 10}
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


end_per_group(_Case, Config) ->
    PanelNoes = oct_background:get_zone_panels(),
    test_utils:mock_unload(PanelNoes, [letsencrypt_api]),

    Config.


init_per_testcase(_Case, Config) ->
    PanelNodes = oct_background:get_zone_panels(),
    test_utils:mock_new(PanelNodes, [service_oz_worker, service_onepanel], [passthrough]),

    Config.


end_per_testcase(_Case, Config) ->
    PanelNodes = oct_background:get_zone_panels(),
    test_utils:mock_unload(PanelNodes),

    cert_test_utils:deploy_certs(zone, ?PEBBLE_VALID_CERT_DIR_NAME, Config),

    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
