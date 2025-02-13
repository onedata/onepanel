%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for Lets Encrypt tests.
%%% @end
%%%--------------------------------------------------------------------
-module(cert_test_utils).
-author("Bartosz Walkowicz").

-include("cert_test_utils.hrl").
-include("names.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("kernel/src/inet_dns.hrl").


%% API
-export([
    substitute_cert_renewal_days/2,
    substitute_cert_renewal_check_delay/2,

    set_certification_attempts/2,

    get_cert_details/1,
    assert_cert_details/2,
    assert_cert_details/3,
    assert_newly_issued_pebble_cert/1,

    update_lets_encrypt/2,
    try_update_lets_encrypt/2,

    deploy_certs/3,
    reload_certs/1,
    assert_certs_on_disc_and_loaded_matches/1
]).

-define(ATTEMPTS, 30).


%%%===================================================================
%%% API
%%%===================================================================


-spec substitute_cert_renewal_days(oct_background:entity_selector(), non_neg_integer()) ->
    non_neg_integer().
substitute_cert_renewal_days(EntitySelector, Days) ->
    PanelNodes = panel_test_utils:get_panel_nodes(EntitySelector),
    {ok, PrevDays} = test_utils:get_env(?RAND_ELEMENT(PanelNodes), ?APP_NAME, web_cert_renewal_days),
    test_utils:set_env(PanelNodes, ?APP_NAME, web_cert_renewal_days, Days),

    PrevDays.


-spec substitute_cert_renewal_check_delay(oct_background:entity_selector(), time:seconds()) ->
    time:seconds().
substitute_cert_renewal_check_delay(EntitySelector, DelaySec) ->
    PanelNodes = panel_test_utils:get_panel_nodes(EntitySelector),
    {ok, PrevDelaySec} = test_utils:get_env(?RAND_ELEMENT(PanelNodes), ?APP_NAME, web_cert_renewal_check_delay),
    test_utils:set_env(PanelNodes, ?APP_NAME, web_cert_renewal_check_delay, DelaySec),

    PrevDelaySec.


-spec set_certification_attempts(oct_background:entity_selector(), non_neg_integer()) ->
    ok.
set_certification_attempts(EntitySelector, Attempts) ->
    PanelNodes = panel_test_utils:get_panel_nodes(EntitySelector),
    test_utils:set_env(PanelNodes, ?APP_NAME, letsencrypt_attempts, Attempts).


-spec get_cert_details(oct_background:entity_selector()) -> json_utils:json_map().
get_cert_details(EntitySelector) ->
    {ok, _, _, CertDetails} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(EntitySelector, <<"/web_cert">>, #{auth => root}),
        % Call with attempts in case of cert reload after certification
        ?ATTEMPTS
    ),
    CertDetails.


-spec assert_cert_details(oct_background:entity_selector(), json_utils:json_map()) ->
    json_utils:json_map().
assert_cert_details(EntitySelector, ExpCertDetails) ->
    assert_cert_details(EntitySelector, ExpCertDetails, 1).


-spec assert_cert_details(
    oct_background:entity_selector(),
    json_utils:json_map(),
    non_neg_integer()
) ->
    json_utils:json_map().
assert_cert_details(EntitySelector, ExpCertDetails, Attempts) ->
    CheckedKeys = maps:keys(ExpCertDetails),
    GetCertDetailsFun = fun() ->
        Details = get_cert_details(EntitySelector),
        {maps:with(CheckedKeys, Details), Details}
    end,
    {_, AllCertDetails} = ?assertMatch({ExpCertDetails, _}, GetCertDetailsFun(), Attempts),
    AllCertDetails.


-spec assert_newly_issued_pebble_cert(json_utils:json_map()) -> ok.
assert_newly_issued_pebble_cert(#{
    <<"creationTime">> := CreationTimeIso8601,
    <<"issuer">> := Issuer
}) ->
    % Assert certificate was issued within last 5 minutes
    Now = global_clock:timestamp_seconds(),
    CreationTimestamp = time:iso8601_to_seconds(CreationTimeIso8601),
    ?assert(CreationTimestamp > Now - 300),

    ?assertEqual(match, re:run(Issuer, ?RE_PEBBLE_ISSUER, [{capture, none}])),
    ok.


-spec update_lets_encrypt(oct_background:entity_selector(), enable | disable) ->
    ok.
update_lets_encrypt(EntitySelector, State) ->
    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        try_update_lets_encrypt(EntitySelector, State),
        ?ATTEMPTS
    ),
    ok.


-spec try_update_lets_encrypt(oct_background:entity_selector(), enable | disable) ->
    panel_test_rest:response().
try_update_lets_encrypt(EntitySelector, State) ->
    panel_test_rest:patch(EntitySelector, <<"/web_cert">>, #{
        auth => root,
        json => #{<<"letsEncrypt">> => case State of
            enable -> true;
            disable -> false
        end},
        % Enabling lets encrypt may cause (if current cert is not valid)
        % new synchronous certification process. This may take some time
        recv_timeout => timer:minutes(5)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Writes predefined certificate files from suite data dir on all nodes.
%% @end
%%--------------------------------------------------------------------
-spec deploy_certs(oct_background:entity_selector(), string(), test_config:config()) ->
    ok.
deploy_certs(EntitySelector, CertDirName, Config) ->
    Nodes = panel_test_utils:get_panel_nodes(EntitySelector),
    ExpResult = lists:duplicate(length(Nodes), ok),

    lists:foreach(fun
        ({web_cert_full_chain_file, _Path}) when EntitySelector =:= zone ->
            % Onezone does not use web_full_chain - only OneS3 needs it
            ok;

        ({FileType, Path}) ->
            {ok, Content} = file:read_file(?TEST_FILE(Config, Path)),
            {Result, []} = utils:rpc_multicall(Nodes, erlang, apply, [fun() ->
                Dest = onepanel_env:get(FileType),
                file:write_file(Dest, Content)
            end, []]),
            ?assertEqual(ExpResult, Result)

    end, [
        {web_cert_file, str_utils:format("~ts/web_cert.pem", [CertDirName])},
        {web_key_file, str_utils:format("~ts/web_key.pem", [CertDirName])},
        {web_cert_chain_file, str_utils:format("~ts/web_chain.pem", [CertDirName])},
        {web_cert_full_chain_file, str_utils:format("~ts/web_full_chain.pem", [CertDirName])}
    ]),

    reload_certs(EntitySelector).


-spec reload_certs(oct_background:entity_selector()) -> ok.
reload_certs(EntitySelector) ->
    PanelNodes = panel_test_utils:get_panel_nodes(EntitySelector),
    case EntitySelector of
        zone ->
            ok;
        _ ->
            panel_test_rpc:call(?RAND_ELEMENT(PanelNodes), fun() ->
                Ctx = #{hosts => service_ones3:get_hosts()},
                service:apply_sync(?SERVICE_ONES3, reload_webcert, Ctx)
            end)
    end,

    lists:foreach(fun(Nodes) ->
        ExpResult = lists:duplicate(length(Nodes), ok),
        {Result, []} = utils:rpc_multicall(Nodes, https_listener, reload_web_certs, []),
        ?assertEqual(ExpResult, Result)
    end, [
        PanelNodes,
        panel_test_utils:get_worker_nodes(EntitySelector)
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Asserts certificates returned by server (when queried by openssl) are the
%% same as the ones on disc under cert dir location.
%% @end
%%--------------------------------------------------------------------
-spec assert_certs_on_disc_and_loaded_matches(oct_background:entity_selector()) -> ok.
assert_certs_on_disc_and_loaded_matches(EntitySelector) ->
    ExpPems = lists:flatten([
        read_pems(EntitySelector, web_cert_file),
        read_pems(EntitySelector, web_cert_chain_file)
    ]),

    PortsPerDomain0 = [{dns_test_utils:get_domain(EntitySelector), [
        panel_test_rpc:call(EntitySelector, onepanel_env, get, [worker_https_server_port, ?APP_NAME, 443]),
        panel_test_rpc:call(EntitySelector, https_listener, port, [])
    ]}],
    PortsPerDomain1 = PortsPerDomain0 ++ panel_test_rpc:call(EntitySelector, fun() ->
        case service_ones3:exists() of
            true -> [{service_ones3:get_domain(), [service_ones3:get_port()]}];
            false -> []
        end
    end),

    lists:foreach(fun({Domain, Ports}) ->
        lists:foreach(fun(Port) ->
            Cmd = str_utils:format(
                "openssl s_client -showcerts -connect ~ts:~tp -servername ~ts",
                [Domain, Port, Domain]
            ),
            Result = str_utils:to_binary(os:cmd(Cmd)),

            lists:foreach(fun(Pem) ->
                ?assertMatch({_, _}, binary:match(Result, Pem))
            end, ExpPems)
        end, Ports)
    end, PortsPerDomain1).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
read_pems(NodeSelector, FileType) ->
    {ok, Content} = panel_test_rpc:call(NodeSelector, fun() ->
        file:read_file(onepanel_env:get(FileType))
    end),
    [string:trim(cert_utils:ders_to_pem([Der])) || Der <- cert_utils:pem_to_ders(Content)].
