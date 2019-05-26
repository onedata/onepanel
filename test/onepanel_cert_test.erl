%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for onepanel_cert module.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_cert_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-define(DATA_DIR, "./test/" ++ ?MODULE_STRING ++ "_data/").
-define(LOCALHOST_CERT, filename:join(?DATA_DIR, "localhost_cert.pem")).
-define(MULTIDOMAIN_CERT, filename:join(?DATA_DIR, "multidomain_cert.pem")).

-include("modules/errors.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test functions
%%%===================================================================

read_should_return_record_test() ->
    ?assertMatch({ok, #'Certificate'{}},
        onepanel_cert:read(?LOCALHOST_CERT)
    ).


read_subject_common_name_test() ->
    Expected = <<"localhost">>,
    {ok, Cert} = onepanel_cert:read(?LOCALHOST_CERT),
    ?assertEqual(Expected, onepanel_cert:get_subject_cn(Cert)).


read_issuer_common_name_test() ->
    Expected = <<"OneDataTestWebServerCA">>,
    {ok, Cert} = onepanel_cert:read(?LOCALHOST_CERT),
    ?assertEqual(Expected, onepanel_cert:get_issuer_cn(Cert)).


read_dates_as_epoch_test() ->
    ExpectedSince = 1518104419, % Feb  8 15:40:19 2018 GMT
    ExpectedUntil = 1833464419, % Feb  6 15:40:19 2028 GMT
    {ok, Cert} = onepanel_cert:read(?LOCALHOST_CERT),
    ?assertEqual({ExpectedSince, ExpectedUntil},
        onepanel_cert:get_times(Cert)).


verify_accepts_domain_test() ->
    {ok, Cert1} = onepanel_cert:read(?LOCALHOST_CERT),
    {ok, Cert2} = onepanel_cert:read(?MULTIDOMAIN_CERT),

    ?assertEqual(valid, onepanel_cert:verify_hostname(Cert1, <<"localhost">>)),
    ?assertEqual(valid, onepanel_cert:verify_hostname(Cert2,
        <<"10-32-10-225.metadata.pod.dev.onedata.uk.to">>)),
    ?assertEqual(valid, onepanel_cert:verify_hostname(Cert2,
        <<"metadata-oneprovider-krakow.metadata.svc.dev.onedata.uk.to">>)).


verify_rejects_domain_test() ->
    {ok, Cert1} = onepanel_cert:read(?LOCALHOST_CERT),
    {ok, Cert2} = onepanel_cert:read(?MULTIDOMAIN_CERT),

    ?assertEqual(invalid, onepanel_cert:verify_hostname(Cert1, <<"not_in_cert.com">>)),
    ?assertEqual(invalid, onepanel_cert:verify_hostname(Cert2, <<"not_in_cert.com">>)).

-endif.
