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

-include("modules/errors.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test functions
%%%===================================================================

read_cert_should_return_record_test() ->
    ?assertMatch({ok, #'Certificate'{}},
        onepanel_cert:read_cert(?LOCALHOST_CERT)
    ).

read_subject_common_name_test() ->
    Expected = <<"localhost">>,
    {ok, Cert} = onepanel_cert:read_cert(?LOCALHOST_CERT),
    ?assertEqual(Expected, onepanel_cert:get_subject_cn(Cert)).

read_issuer_common_name_test() ->
    Expected = <<"OneDataTestWebServerCA">>,
    {ok, Cert} = onepanel_cert:read_cert(?LOCALHOST_CERT),
    ?assertEqual(Expected, onepanel_cert:get_issuer_cn(Cert)).

read_dates_as_epoch_test() ->
    ExpectedSince = 1518104419, % Feb  8 15:40:19 2018 GMT
    ExpectedUntil = 1833464419, % Feb  6 15:40:19 2028 GMT
    {ok, Cert} = onepanel_cert:read_cert(?LOCALHOST_CERT),
    ?assertEqual({ExpectedSince, ExpectedUntil},
        onepanel_cert:get_times(Cert)).

-endif.
