%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Lets Encrypt.
%%% @end
%%%-------------------------------------------------------------------
-module(le_test_base).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("cert_test_utils.hrl").
-include_lib("ctool/include/privileges.hrl").

% Setup/teardown
-export([
    init_automatic_certification_renewal_test/2,
    teardown_automatic_certification_renewal_test/2,

    init_failed_certification_attempt_test/3,
    teardown_failed_certification_attempt_test/2
]).

% Test bases
-export([
    get_certificate_metadata_test_base/1,
    toggle_lets_encrypt_test_base/1,
    valid_certificate_should_not_be_replaced_test_base/1,
    non_lets_encrypt_issued_certificate_should_be_replaced_test_base/1,
    domain_mismatched_certificate_should_be_replaced_test_base/3,
    expired_certificate_should_be_replaced_test_base/1,
    automatic_certification_renewal_test_base/1,
    disabling_lets_encrypt_should_do_nothing_to_already_present_certificate_test_base/1,
    failed_certification_attempt_leaves_lets_encrypt_intact_test_base/1
]).

-type test_spec() :: #le_test_spec{}.

-export_type([test_spec/0]).

-define(WEB_CERT_RENEWAL_CHECK_SEC, 10).
% With renewal margin bigger than the validity of the cert all certs will be
% near expired as soon as they are issued.
-define(WEB_CERT_RENEW_MARGIN_DAYS, 36500).

% Increase certification attempts as pebble may fail several times
% (not offering challenge, etc.) which is even better as it lets
% us tests certification retries
-define(CERTIFICATION_ATTEMPTS, 10).

-define(CERTIFICATION_FLOW_ERROR, ?ERROR_LETS_ENCRYPT_RESPONSE(<<>>, <<>>)).

-define(ATTEMPTS, 60).


%%%===================================================================
%%% Setup and teardown
%%%===================================================================


-spec init_automatic_certification_renewal_test(oct_background:entity_selector(), test_config:config()) ->
    test_config:config().
init_automatic_certification_renewal_test(EntitySelector, Config) ->
    PrevDays = cert_test_utils:substitute_cert_renewal_days(EntitySelector, ?WEB_CERT_RENEW_MARGIN_DAYS),
    PrevDelay = cert_test_utils:substitute_cert_renewal_check_delay(EntitySelector, ?WEB_CERT_RENEWAL_CHECK_SEC),

    cert_test_utils:update_lets_encrypt(EntitySelector, disable),
    cert_test_utils:deploy_certs(EntitySelector, ?PEBBLE_EXPIRED_CERT_DIR_NAME, Config),

    [{prev_renewal_days, PrevDays}, {prev_renewal_check_delay, PrevDelay} | Config].


-spec teardown_automatic_certification_renewal_test(oct_background:entity_selector(), test_config:config()) ->
    test_config:config().
teardown_automatic_certification_renewal_test(EntitySelector, Config) ->
    cert_test_utils:substitute_cert_renewal_days(EntitySelector, ?config(prev_renewal_days, Config)),
    cert_test_utils:substitute_cert_renewal_check_delay(EntitySelector, ?config(prev_renewal_check_delay, Config)),

    cert_test_utils:update_lets_encrypt(EntitySelector, disable),

    Config.


-spec init_failed_certification_attempt_test(oct_background:entity_selector(), enable | disable, test_config:config()) ->
    test_config:config().
init_failed_certification_attempt_test(EntitySelector, LetsEncryptPolicy, Config) ->
    % Decrease certification attempts so that test will not hung for longer
    % than it needs (certification fails due to mocked error)
    cert_test_utils:set_certification_attempts(EntitySelector, 2),

    cert_test_utils:update_lets_encrypt(EntitySelector, LetsEncryptPolicy),
    cert_test_utils:deploy_certs(EntitySelector, ?PEBBLE_EXPIRED_CERT_DIR_NAME, Config),

    PanelNodes = panel_test_utils:get_panel_nodes(EntitySelector),
    test_utils:mock_new(PanelNodes, [letsencrypt_api], [passthrough]),
    test_utils:mock_expect(PanelNodes, letsencrypt_api, run_certification_flow, fun(_) ->
        throw(?CERTIFICATION_FLOW_ERROR)
    end),

    Config.


-spec teardown_failed_certification_attempt_test(oct_background:entity_selector(), test_config:config()) ->
    test_config:config().
teardown_failed_certification_attempt_test(EntitySelector, Config) ->
    PanelNodes = panel_test_utils:get_panel_nodes(EntitySelector),
    test_utils:mock_unload(PanelNodes, [letsencrypt_api]),

    Config.


%%%===================================================================
%%% Test bases
%%%===================================================================


-spec get_certificate_metadata_test_base(test_spec()) -> ok | no_return().
get_certificate_metadata_test_base(#le_test_spec{
    entity_selector = EntitySelector,
    exp_domain = ExpDomain,
    exp_dns_names = ExpDnsNames,
    service = Service,
    ct_config = CtConfig
}) ->
    ExpSortedDnsNames = lists:sort(ExpDnsNames),

    cert_test_utils:update_lets_encrypt(EntitySelector, disable),
    cert_test_utils:deploy_certs(EntitySelector, ?PEBBLE_VALID_CERT_DIR_NAME, CtConfig),

    ScenarioSpec = #scenario_spec{
        test_proxied_onepanel_rest_endpoint = false,
        name = <<"Get certificate metadata using /web_cert endpoint">>,
        type = rest,
        target_nodes = panel_test_utils:get_panel_nodes(EntitySelector),
        client_spec = #client_spec{
            correct = [
                root,
                member
            ],
            unauthorized = [
                guest,
                {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(Service)}
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
                    <<"domain">> := ExpDomain,
                    <<"dnsNames">> := _,
                    <<"issuer">> := _,
                    <<"status">> := <<"valid">>
                },
                Body
            ),
            ?assertEqual(ExpSortedDnsNames, maps:get(<<"dnsNames">>, Body)),
            ?assertEqual(8, length(maps:keys(Body)))
        end)
    },
    ?assert(api_test_runner:run_tests([ScenarioSpec])),

    cert_test_utils:update_lets_encrypt(EntitySelector, enable),

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
                    <<"domain">> := ExpDomain,
                    <<"dnsNames">> := _,
                    <<"issuer">> := _,
                    <<"status">> := <<"valid">>,
                    <<"lastRenewalFailure">> := _,
                    <<"lastRenewalSuccess">> := _
                },
                Body
            ),
            ?assertEqual(ExpSortedDnsNames, maps:get(<<"dnsNames">>, Body)),
            ?assertEqual(10, length(maps:keys(Body)))
        end)
    }])).


-spec toggle_lets_encrypt_test_base(test_spec()) -> ok | no_return().
toggle_lets_encrypt_test_base(#le_test_spec{
    entity_selector = EntitySelector,
    service = Service,
    ct_config = CtConfig
}) ->
    cert_test_utils:deploy_certs(EntitySelector, ?PEBBLE_VALID_CERT_DIR_NAME, CtConfig),
    cert_test_utils:update_lets_encrypt(EntitySelector, disable),

    ?assert(api_test_runner:run_tests([#scenario_spec{
        test_proxied_onepanel_rest_endpoint = false,
        name = <<"Toggle Lets Enrytps using /web_cert endpoint">>,
        type = rest,
        target_nodes = panel_test_utils:get_panel_nodes(EntitySelector),
        client_spec = #client_spec{
            correct = [
                root,
                {member, [?CLUSTER_UPDATE]}
            ],
            unauthorized = [
                guest,
                {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(Service)}
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


-spec valid_certificate_should_not_be_replaced_test_base(test_spec()) -> ok | no_return().
valid_certificate_should_not_be_replaced_test_base(#le_test_spec{
    entity_selector = EntitySelector,
    exp_domain = ExpDomain,
    exp_dns_names = ExpDnsNames,
    ct_config = CtConfig
}) ->
    cert_test_utils:update_lets_encrypt(EntitySelector, disable),
    cert_test_utils:deploy_certs(EntitySelector, ?PEBBLE_VALID_CERT_DIR_NAME, CtConfig),

    ExpBasicCertDetails = #{
        <<"domain">> => ExpDomain,
        <<"dnsNames">> => lists:sort(ExpDnsNames),
        <<"status">> => <<"valid">>,
        <<"letsEncrypt">> => false
    },
    AllCertDetails = cert_test_utils:assert_cert_details(EntitySelector, ExpBasicCertDetails),

    cert_test_utils:update_lets_encrypt(EntitySelector, enable),

    cert_test_utils:assert_certs_reloaded(EntitySelector),
    ExpAllCertDetails = AllCertDetails#{<<"letsEncrypt">> => true},
    cert_test_utils:assert_cert_details(EntitySelector, ExpAllCertDetails).


-spec non_lets_encrypt_issued_certificate_should_be_replaced_test_base(test_spec()) ->
    ok | no_return().
non_lets_encrypt_issued_certificate_should_be_replaced_test_base(#le_test_spec{
    entity_selector = EntitySelector,
    exp_domain = ExpDomain,
    exp_dns_names = ExpDnsNames,
    ct_config = CtConfig
}) ->
    cert_test_utils:update_lets_encrypt(EntitySelector, disable),
    cert_test_utils:deploy_certs(EntitySelector, ?ONEDATA_TEST_CERT_DIR_NAME, CtConfig),

    ExpBasicCertDetails = #{
        <<"domain">> => ExpDomain,
        <<"dnsNames">> => lists:sort(ExpDnsNames),
        <<"status">> => <<"valid">>
    },
    ExpOnedataTestCertDetails = ExpBasicCertDetails#{
        <<"issuer">> => ?ONEDATA_TEST_CERT_ISSUER,
        <<"letsEncrypt">> => false
    },
    cert_test_utils:assert_cert_details(EntitySelector, ExpOnedataTestCertDetails),

    cert_test_utils:update_lets_encrypt(EntitySelector, enable),

    ExpPebbleCertDetails = ExpBasicCertDetails#{<<"letsEncrypt">> => true},
    AllPebbleCertDetails = cert_test_utils:assert_cert_details(EntitySelector, ExpPebbleCertDetails),
    cert_test_utils:assert_certs_reloaded(EntitySelector),
    cert_test_utils:assert_newly_issued_pebble_cert(AllPebbleCertDetails).


-spec domain_mismatched_certificate_should_be_replaced_test_base(binary(), [binary()], test_spec()) ->
    ok | no_return().
domain_mismatched_certificate_should_be_replaced_test_base(FakeDomain, FakeDnsNames, #le_test_spec{
    entity_selector = EntitySelector,
    exp_domain = ExpDomain,
    exp_dns_names = ExpDnsNames,
    ct_config = CtConfig
}) ->
    cert_test_utils:update_lets_encrypt(EntitySelector, disable),
    cert_test_utils:deploy_certs(EntitySelector, ?PEBBLE_DOMAIN_MISMATCH_CERT_DIR_NAME, CtConfig),

    ExpDomainMismatchedCertDetails = #{
        <<"domain">> => FakeDomain,
        <<"dnsNames">> => lists:sort(FakeDnsNames),
        <<"status">> => <<"domain_mismatch">>,
        <<"letsEncrypt">> => false
    },
    cert_test_utils:assert_cert_details(EntitySelector, ExpDomainMismatchedCertDetails),

    cert_test_utils:update_lets_encrypt(EntitySelector, enable),

    ExpPebbleCertDetails = #{
        <<"domain">> => ExpDomain,
        <<"dnsNames">> => lists:sort(ExpDnsNames),
        <<"status">> => <<"valid">>,
        <<"letsEncrypt">> => true
    },
    AllPebbleCertDetails = cert_test_utils:assert_cert_details(EntitySelector, ExpPebbleCertDetails),
    cert_test_utils:assert_certs_reloaded(EntitySelector),
    cert_test_utils:assert_newly_issued_pebble_cert(AllPebbleCertDetails).


-spec expired_certificate_should_be_replaced_test_base(test_spec()) ->
    ok | no_return().
expired_certificate_should_be_replaced_test_base(#le_test_spec{
    entity_selector = EntitySelector,
    exp_domain = ExpDomain,
    exp_dns_names = ExpDnsNames,
    ct_config = CtConfig
}) ->
    cert_test_utils:update_lets_encrypt(EntitySelector, disable),
    cert_test_utils:deploy_certs(EntitySelector, ?PEBBLE_EXPIRED_CERT_DIR_NAME, CtConfig),

    ExpBasicCertDetails = #{
        <<"domain">> => ExpDomain,
        <<"dnsNames">> => lists:sort(ExpDnsNames)
    },
    ExpExpiredCertDetails = ExpBasicCertDetails#{
        <<"status">> => <<"expired">>,
%%        <<"status">> => <<"near_expiration">>,  %% TODO expired
        <<"letsEncrypt">> => false
    },
    cert_test_utils:assert_cert_details(EntitySelector, ExpExpiredCertDetails),

    cert_test_utils:update_lets_encrypt(EntitySelector, enable),

    ExpPebbleCertDetails = ExpBasicCertDetails#{
        <<"status">> => <<"valid">>,
        <<"letsEncrypt">> => true
    },
    AllPebbleCertDetails = cert_test_utils:assert_cert_details(EntitySelector, ExpPebbleCertDetails),
    cert_test_utils:assert_certs_reloaded(EntitySelector),
    cert_test_utils:assert_newly_issued_pebble_cert(AllPebbleCertDetails).


-spec automatic_certification_renewal_test_base(test_spec()) ->
    ok | no_return().
automatic_certification_renewal_test_base(#le_test_spec{
    entity_selector = EntitySelector,
    exp_domain = ExpDomain,
    exp_dns_names = ExpDnsNames
}) ->
    cert_test_utils:update_lets_encrypt(EntitySelector, enable),

    ExpPebbleCertDetails = #{
        <<"domain">> => ExpDomain,
        <<"dnsNames">> => lists:sort(ExpDnsNames),
        <<"status">> => <<"near_expiration">>,
        <<"letsEncrypt">> => true
    },
    #{<<"creationTime">> := CertCreationTime} = cert_test_utils:assert_cert_details(
        EntitySelector, ExpPebbleCertDetails
    ),
    cert_test_utils:assert_certs_reloaded(EntitySelector),
    ?assertNot(CertCreationTime == get_cert_creation_time(EntitySelector), ?ATTEMPTS),

    #{<<"creationTime">> := CertCreationTime2} = cert_test_utils:assert_cert_details(
        EntitySelector, ExpPebbleCertDetails
    ),
    cert_test_utils:assert_certs_reloaded(EntitySelector),
    ?assertNot(CertCreationTime2 == get_cert_creation_time(EntitySelector), ?ATTEMPTS).


-spec disabling_lets_encrypt_should_do_nothing_to_already_present_certificate_test_base(test_spec()) ->
    ok | no_return().
disabling_lets_encrypt_should_do_nothing_to_already_present_certificate_test_base(TestSpec = #le_test_spec{
    entity_selector = EntitySelector
}) ->
    % Run previous test code to ensure new certificate was issued
    expired_certificate_should_be_replaced_test_base(TestSpec),
    CertDetails = cert_test_utils:get_cert_details(EntitySelector),

    cert_test_utils:update_lets_encrypt(EntitySelector, disable),

    ExpCertDetails = maps:without(
        [<<"lastRenewalFailure">>, <<"lastRenewalSuccess">>],
        CertDetails#{<<"letsEncrypt">> => false}
    ),
    cert_test_utils:assert_certs_reloaded(EntitySelector),
    ?assertEqual(ExpCertDetails, cert_test_utils:get_cert_details(EntitySelector)).


-spec failed_certification_attempt_leaves_lets_encrypt_intact_test_base(test_spec()) ->
    ok | no_return().
failed_certification_attempt_leaves_lets_encrypt_intact_test_base(#le_test_spec{
    entity_selector = EntitySelector
}) ->
    KeyToRm = <<"lastRenewalFailure">>,
    CertDetails = maps:remove(KeyToRm, cert_test_utils:get_cert_details(EntitySelector)),

    {ok, _, _, #{<<"error">> := RespError}} = ?assertMatch(
        {ok, ?HTTP_400_BAD_REQUEST, _, #{<<"error">> := _}},
        cert_test_utils:try_update_lets_encrypt(EntitySelector, enable)
    ),
    ?assertMatch(?ERROR_ON_NODES(?CERTIFICATION_FLOW_ERROR, _), errors:from_json(RespError)),

    cert_test_utils:assert_certs_reloaded(EntitySelector),
    ?assertEqual(CertDetails, maps:remove(KeyToRm, cert_test_utils:get_cert_details(EntitySelector))).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec get_cert_creation_time(oct_background:entity_selector()) -> binary().
get_cert_creation_time(EntitySelector) ->
    maps:get(<<"creationTime">>, cert_test_utils:get_cert_details(EntitySelector)).
