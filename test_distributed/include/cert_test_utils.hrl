%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Definitions of macros and records used in cert tests.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(CERT_TEST_UTILS_HRL).
-define(CERT_TEST_UTILS_HRL, 1).


-define(RE_PEBBLE_ISSUER, <<"^Pebble Intermediate CA \\w+$">>).

-define(ONEDATA_TEST_CERT_DIR_NAME, "onedata").
-define(ONEDATA_TEST_CERT_ISSUER, <<"OneDataTestWebServerCA">>).

-define(PEBBLE_DOMAIN_MISMATCH_CERT_DIR_NAME, "pebble_domain_mismatch").
-define(PEBBLE_DOMAIN_MISMATCH_FAKE_DOMAIN, <<"fake.local">>).

-define(PEBBLE_EXPIRED_CERT_DIR_NAME, "pebble_expired").

-define(PEBBLE_VALID_CERT_DIR_NAME, "pebble_valid").

-define(GARBAGE_CERT_DIR_NAME, "garbage").


-record(le_test_spec, {
    entity_selector :: oct_background:entity_selector(),
    exp_domain :: binary(),
    exp_dns_names :: [binary()],
    service :: aai:service_spec(),
    ct_config :: test_config:config()
}).


-endif.
