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
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("kernel/src/inet_dns.hrl").


%% API
-export([
    set_certification_attempts/2,

    get_cert_details/1,
    assert_cert_details/2,
    assert_newly_issued_pebble_cert/1,

    update_lets_encrypt/2,
    try_update_lets_encrypt/2,

    deploy_certs/3,
    reload_certs/1
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec set_certification_attempts(oct_background:entity_selector(), non_neg_integer()) ->
    ok.
set_certification_attempts(EntitySelector, Attempts) ->
    PanelNodes = get_panel_nodes(EntitySelector),

    ?assertEqual(
        {lists:duplicate(length(PanelNodes), ok), []},
        utils:rpc_multicall(PanelNodes, onepanel_env, set, [letsencrypt_attempts, Attempts])
    ),
    ok.


-spec get_cert_details(oct_background:entity_selector()) -> json_utils:json_map().
get_cert_details(EntitySelector) ->
    {ok, _, _, CertDetails} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(EntitySelector, <<"/web_cert">>, #{auth => root})
    ),
    CertDetails.


-spec assert_cert_details(oct_background:entity_selector(), json_utils:json_map()) ->
    json_utils:json_map().
assert_cert_details(EntitySelector, ExpCertDetails) ->
    CheckedKeys = maps:keys(ExpCertDetails),
    GetCertDetailsFun = fun() ->
        Details = get_cert_details(EntitySelector),
        {maps:with(CheckedKeys, Details), Details}
    end,
    {_, AllCertDetails} = ?assertMatch({ExpCertDetails, _}, GetCertDetailsFun()),
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
        try_update_lets_encrypt(EntitySelector, State)
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
    Nodes = get_panel_nodes(EntitySelector),
    ExpResult = lists:duplicate(length(Nodes), ok),

    lists:foreach(fun({FileType, Path}) ->

        {ok, Content} = file:read_file(?TEST_FILE(Config, Path)),
        {Result, []} = utils:rpc_multicall(Nodes, erlang, apply, [fun() ->
            Dest = onepanel_env:get(FileType),
            file:write_file(Dest, Content)
        end, []]),
        ?assertEqual(ExpResult, Result)

    end, [
        {web_cert_file, str_utils:format("~ts/web_cert.pem", [CertDirName])},
        {web_key_file, str_utils:format("~ts/web_key.pem", [CertDirName])},
        {web_cert_chain_file, str_utils:format("~ts/web_chain.pem", [CertDirName])}
    ]),

    reload_certs(EntitySelector).


-spec reload_certs(oct_background:entity_selector()) -> ok.
reload_certs(EntitySelector) ->
    Nodes = get_panel_nodes(EntitySelector),
    ExpResult = lists:duplicate(length(Nodes), ok),
    {Result, []} = utils:rpc_multicall(Nodes, https_listener, reload_web_certs, []),
    ?assertEqual(ExpResult, Result),

    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
get_panel_nodes(zone) -> oct_background:get_zone_panels();
get_panel_nodes(ProviderSelector) -> oct_background:get_provider_panels(ProviderSelector).
