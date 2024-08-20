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
    non_lets_encrypt_issued_certificate_should_be_replaced_test/1
]).

all() -> [
    non_lets_encrypt_issued_certificate_should_be_replaced_test
].


-define(ONEDATA_TEST_CERTS_REL_DIR, "onedata").

-define(RE_PEBBLE_ISSUER, <<"^Pebble Intermediate CA \\w+$">>).


-define(ATTEMPTS, 100).


%%%===================================================================
%%% API
%%%===================================================================


non_lets_encrypt_issued_certificate_should_be_replaced_test(Config) ->
    ct:pal("~p", [cert_test_utils:get_cert_details(zone)]),

    OzDomain = dns_test_utils:get_zone_domain(),
    ExpBasicCertDetails = #{
        <<"domain">> => OzDomain,
        <<"dnsNames">> => [OzDomain],
        <<"status">> => <<"valid">>
    },

    cert_test_utils:disable_lets_encrypt(zone),
    OnedataTestCertPaths = build_cert_rel_paths(?ONEDATA_TEST_CERTS_REL_DIR),
    cert_test_utils:deploy_certs(zone, OnedataTestCertPaths, Config),

    ExpOnedataTestCertDetails = ExpBasicCertDetails#{
        <<"issuer">> => <<"OneDataTestWebServerCA">>,
        <<"letsEncrypt">> => false
    },
    AllOnedataTestCertDetails = assert_cert_details(ExpOnedataTestCertDetails),

    cert_test_utils:enable_lets_encrypt(zone),

    AllOnedataTestCertDetailsWithEnabledLE = AllOnedataTestCertDetails#{
        <<"letsEncrypt">> => true
    },
    ?assertNotMatch(
        AllOnedataTestCertDetailsWithEnabledLE,
        cert_test_utils:get_cert_details(zone),
        ?ATTEMPTS
    ),

    ExpPebbleCertDetails = ExpBasicCertDetails#{<<"letsEncrypt">> => true},
    AllPebbleCertDetails = assert_cert_details(ExpPebbleCertDetails),
    assert_pebble_issuer(AllPebbleCertDetails).


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
