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

% validity dates of the localhost_cert stored in test data
-define(SINCE, 1518104419). % Feb  8 15:40:19 2018 GMT
-define(UNTIL, 1833464419). % Feb  6 15:40:19 2028 GMT

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
    {ok, Cert} = onepanel_cert:read(?LOCALHOST_CERT),
    ?assertEqual({?SINCE, ?UNTIL}, onepanel_cert:get_times(Cert)).


get_seconds_till_expiration_test() ->
    {ok, Cert} = onepanel_cert:read(?LOCALHOST_CERT),
    ?assertEqual(?UNTIL - time_utils:system_time_seconds(),
        onepanel_cert:get_seconds_till_expiration(Cert)).


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


list_certificate_files_test() ->
    utils:run_with_tempdir(fun(TmpDir) ->
        LEDir = filename:join([TmpDir, letsencrypt]),
        LECredentials = filename:join([LEDir, staging, private_key]),
        ok = onepanel_env:set(letsencrypt_keys_dir, LEDir),
        ok = filelib:ensure_dir(LECredentials),
        ok = file:write_file(LECredentials, <<>>),

        Files = lists:map(fun(Var) ->
            Path = filename:join(TmpDir, Var),
            ok = file:write_file(Path, <<>>),
            ok = onepanel_env:set(Var, Path),
            Path
        end, [web_key_file, web_cert_file, web_cert_chain_file]),
        Files2 = lists:sort([LECredentials | Files]),

        ?assertEqual(Files2, lists:sort(onepanel_cert:list_certificate_files()))
    end).



-endif.
