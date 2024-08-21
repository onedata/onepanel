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

%% API
-export([all/0]).
-export([
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    non_lets_encrypt_issued_certificate_should_be_replaced_test/1,
    domain_mismatched_certificate_should_be_replaced_test/1,
    expired_certificate_should_be_replaced_test/1
]).

all() -> [
    non_lets_encrypt_issued_certificate_should_be_replaced_test,
    domain_mismatched_certificate_should_be_replaced_test,
    expired_certificate_should_be_replaced_test
].


-define(TODO_DUMP_CERT, ct:pal("~p", [cert_test_utils:get_cert_details(zone)])).  %% TODO rm


-define(ATTEMPTS, 100).


%%%===================================================================
%%% API
%%%===================================================================


non_lets_encrypt_issued_certificate_should_be_replaced_test(Config) ->
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


domain_mismatched_certificate_should_be_replaced_test(Config) ->
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


expired_certificate_should_be_replaced_test(Config) ->
    cert_test_utils:disable_lets_encrypt(zone),
    cert_test_utils:deploy_certs(zone, ?PEBBLE_EXPIRED_CERT_DIR_NAME, Config),

    OzDomain = dns_test_utils:get_zone_domain(),
    ExpBasicCertDetails = #{
        <<"domain">> => OzDomain,
        <<"dnsNames">> => [OzDomain]
    },
    ExpExpiredCertDetails = ExpBasicCertDetails#{
        <<"status">> => <<"near_expiration">>,  %% TODO expired
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
        envs = [{oz_panel, onepanel, [
            {letsencrypt_issuer_regex, ?RE_PEBBLE_ISSUER},
            % Increase certification attempts as pebble likes to fail from time to time
            {letsencrypt_attempts, 10}
        ]}],
        posthook = fun(NewConfig) ->
            % Requests should be made without cert verification due to possibly
            % incorrect certificates (tests will mess with them)
            panel_test_rest:set_insecure_flag(),

            NewConfig
        end
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


init_per_testcase(_Case, Config) ->
    Config.


end_per_testcase(_Case, _Config) ->
    % Onepanel's listeners are restarted asynchronously - force it to prevent
    % restart during next test
    cert_test_utils:reload_certs(zone),

    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
