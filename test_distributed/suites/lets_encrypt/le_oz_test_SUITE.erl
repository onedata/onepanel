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


-define(RE_PEBBLE_ISSUER, <<"^Pebble Intermediate CA \\w+$">>).

-define(ONEDATA_TEST_CERTS_REL_DIR, "onedata").

-define(PEBBLE_DOMAIN_MISMATCH_CERT_REL_DIR, "pebble_domain_mismatch").
-define(PEBBLE_DOMAIN_MISMATCH_FAKE_DOMAIN, <<"fake.local">>).

-define(PEBBLE_EXPIRED_CERT_REL_DIR, "pebble_expired").


-define(TODO_DUMP_CERT, ct:pal("~p", [cert_test_utils:get_cert_details(zone)])).  %% TODO rm


-define(ATTEMPTS, 100).


%% TODO między testami może jeszcze nie wstał https_listener - może jakieś sleepy?

%%%===================================================================
%%% API
%%%===================================================================


non_lets_encrypt_issued_certificate_should_be_replaced_test(Config) ->
    cert_test_utils:disable_lets_encrypt(zone),
    OnedataTestCertPaths = build_cert_rel_paths(?ONEDATA_TEST_CERTS_REL_DIR),
    cert_test_utils:deploy_certs(zone, OnedataTestCertPaths, Config),

    OzDomain = dns_test_utils:get_zone_domain(),
    ExpBasicCertDetails = #{
        <<"domain">> => OzDomain,
        <<"dnsNames">> => [OzDomain],
        <<"status">> => <<"valid">>
    },
    ExpOnedataTestCertDetails = ExpBasicCertDetails#{
        <<"issuer">> => <<"OneDataTestWebServerCA">>,
        <<"letsEncrypt">> => false
    },
    assert_cert_details(ExpOnedataTestCertDetails),

    enable_lets_encrypt_and_await_cert_replacement(),

    ExpPebbleCertDetails = ExpBasicCertDetails#{<<"letsEncrypt">> => true},
    AllPebbleCertDetails = assert_cert_details(ExpPebbleCertDetails),
    assert_pebble_issuer(AllPebbleCertDetails).  %% TODO check creationtime?


domain_mismatched_certificate_should_be_replaced_test(Config) ->
    cert_test_utils:disable_lets_encrypt(zone),
    DomainMismatchedCertPaths = build_cert_rel_paths(?PEBBLE_DOMAIN_MISMATCH_CERT_REL_DIR),
    cert_test_utils:deploy_certs(zone, DomainMismatchedCertPaths, Config),

    ExpDomainMismatchedCertDetails = #{
        <<"domain">> => ?PEBBLE_DOMAIN_MISMATCH_FAKE_DOMAIN,
        <<"dnsNames">> => [?PEBBLE_DOMAIN_MISMATCH_FAKE_DOMAIN],
        <<"status">> => <<"domain_mismatch">>,
        <<"letsEncrypt">> => false
    },
    assert_cert_details(ExpDomainMismatchedCertDetails),

    enable_lets_encrypt_and_await_cert_replacement(),

    OzDomain = dns_test_utils:get_zone_domain(),
    ExpPebbleCertDetails = #{
        <<"domain">> => OzDomain,
        <<"dnsNames">> => [OzDomain],
        <<"status">> => <<"valid">>,
        <<"letsEncrypt">> => true
    },
    AllPebbleCertDetails = assert_cert_details(ExpPebbleCertDetails),
    assert_pebble_issuer(AllPebbleCertDetails).  %% TODO check creationtime?


expired_certificate_should_be_replaced_test(Config) ->
    cert_test_utils:disable_lets_encrypt(zone),
    ExpiredCertPaths = build_cert_rel_paths(?PEBBLE_EXPIRED_CERT_REL_DIR),
    cert_test_utils:deploy_certs(zone, ExpiredCertPaths, Config),

    OzDomain = dns_test_utils:get_zone_domain(),
    ExpBasicCertDetails = #{
        <<"domain">> => OzDomain,
        <<"dnsNames">> => [OzDomain]
    },
    ExpExpiredCertDetails = ExpBasicCertDetails#{
        <<"status">> => <<"near_expiration">>,  %% TODO expired
        <<"letsEncrypt">> => false
    },
    assert_cert_details(ExpExpiredCertDetails),

    enable_lets_encrypt_and_await_cert_replacement(),

    ExpPebbleCertDetails = ExpBasicCertDetails#{
        <<"status">> => <<"valid">>,
        <<"letsEncrypt">> => true
    },
    AllPebbleCertDetails = assert_cert_details(ExpPebbleCertDetails),
    assert_pebble_issuer(AllPebbleCertDetails).  %% TODO check creationtime?


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils, cert_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1oz",
        envs = [{oz_panel, onepanel, [
            {letsencrypt_issuer_regex, ?RE_PEBBLE_ISSUER},
            {letsencrypt_attempts, 10}  %% TODO
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
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
build_cert_rel_paths(CertRelDir) ->
    #{
        web_cert_file => str_utils:format("~s/web_cert.pem", [CertRelDir]),
        web_key_file => str_utils:format("~s/web_key.pem", [CertRelDir]),
        web_cert_chain_file => str_utils:format("~s/web_chain.pem", [CertRelDir])
    }.


%% @private
enable_lets_encrypt_and_await_cert_replacement() ->
    CurrentCertDetails = cert_test_utils:get_cert_details(zone),

    cert_test_utils:enable_lets_encrypt(zone),

    ?assertNotMatch(
        CurrentCertDetails,
        maps:remove(<<"letsEncrypt">>, cert_test_utils:get_cert_details(zone)),
        ?ATTEMPTS
    ),

    ok.


%% @private
assert_cert_details(ExpCertDetails) ->
    CheckedKeys = maps:keys(ExpCertDetails),
    GetCertDetailsFun = fun() ->
        Details = cert_test_utils:get_cert_details(zone),
        {maps:with(CheckedKeys, Details), Details}
    end,
    {_, AllCertDetails} = ?assertMatch({ExpCertDetails, _}, GetCertDetailsFun()),
    AllCertDetails.


%% @private
assert_pebble_issuer(CertDetails) ->
    ?assertEqual(
        match,
        re:run(maps:get(<<"issuer">>, CertDetails), ?RE_PEBBLE_ISSUER, [{capture, none}])
    ).
