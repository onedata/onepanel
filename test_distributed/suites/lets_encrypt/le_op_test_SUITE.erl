%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Lets Encrypt for Oneprovider.
%%% @end
%%%-------------------------------------------------------------------
-module(le_op_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("cert_test_utils.hrl").
-include("deployment_progress.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% exported for CT
-export([
    groups/0, all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]).

% Tests
-export([
    get_certificate_metadata_test/1,
    toggle_lets_encrypt_test/1,
    valid_certificate_should_not_be_replaced_test/1,

    non_lets_encrypt_issued_certificate_should_be_replaced_with_http_challenge_test/1,
    non_lets_encrypt_issued_certificate_should_be_replaced_with_dns_challenge_test/1,

    domain_mismatched_certificate_should_be_replaced_with_http_challenge_test/1,
    domain_mismatched_certificate_should_be_replaced_with_dns_challenge_test/1,

    expired_certificate_should_be_replaced_with_http_challenge_test/1,
    expired_certificate_should_be_replaced_with_dns_challenge_test/1,

    automatic_certification_renewal_test/1,
    disabling_lets_encrypt_should_do_nothing_to_already_present_certificate_test/1,
    failed_certification_attempt_leaves_lets_encrypt_disabled_test/1,
    failed_certification_attempt_leaves_lets_encrypt_enabled_test/1
]).

groups() -> [
    {http_challenge, [], [
        non_lets_encrypt_issued_certificate_should_be_replaced_with_http_challenge_test,
        domain_mismatched_certificate_should_be_replaced_with_http_challenge_test,
        expired_certificate_should_be_replaced_with_http_challenge_test
    ]},

    {dns_challenge, [], [
        non_lets_encrypt_issued_certificate_should_be_replaced_with_dns_challenge_test,
        domain_mismatched_certificate_should_be_replaced_with_dns_challenge_test,
        expired_certificate_should_be_replaced_with_dns_challenge_test
    ]}
].

all() -> [
    get_certificate_metadata_test,
    toggle_lets_encrypt_test,
    valid_certificate_should_not_be_replaced_test,

    {group, http_challenge},
    {group, dns_challenge},

    automatic_certification_renewal_test,
    disabling_lets_encrypt_should_do_nothing_to_already_present_certificate_test,
    failed_certification_attempt_leaves_lets_encrypt_disabled_test,
    failed_certification_attempt_leaves_lets_encrypt_enabled_test
].


% Increase certification attempts as pebble may fail several times
% (not offering challenge, etc.) which is even better as it lets
% us tests certification retries
-define(CERTIFICATION_ATTEMPTS, 10).

-define(AWAIT_DEPLOYMENT_READY_ATTEMPTS, 180).

-define(PROVIDER_NAME, <<"krakow">>).


%%%===================================================================
%%% API
%%%===================================================================


get_certificate_metadata_test(Config) ->
    le_test_base:get_certificate_metadata_test_base(build_test_spec(Config)).


toggle_lets_encrypt_test(Config) ->
    le_test_base:toggle_lets_encrypt_test_base(build_test_spec(Config)).


valid_certificate_should_not_be_replaced_test(Config) ->
    le_test_base:valid_certificate_should_not_be_replaced_test_base(build_test_spec(Config)).


non_lets_encrypt_issued_certificate_should_be_replaced_with_http_challenge_test(Config) ->
    le_test_base:non_lets_encrypt_issued_certificate_should_be_replaced_test_base(build_test_spec(Config)).


non_lets_encrypt_issued_certificate_should_be_replaced_with_dns_challenge_test(Config) ->
    le_test_base:non_lets_encrypt_issued_certificate_should_be_replaced_test_base(build_test_spec(Config)).


domain_mismatched_certificate_should_be_replaced_with_http_challenge_test(Config) ->
    OpDomain = get_domain(),

    le_test_base:domain_mismatched_certificate_should_be_replaced_test_base(
        OpDomain, [OpDomain, ?PEBBLE_DOMAIN_MISMATCH_FAKE_DOMAIN], build_test_spec(Config)
    ).


domain_mismatched_certificate_should_be_replaced_with_dns_challenge_test(Config) ->
    OpDomain = get_domain(),

    le_test_base:domain_mismatched_certificate_should_be_replaced_test_base(
        OpDomain, [OpDomain, ?PEBBLE_DOMAIN_MISMATCH_FAKE_DOMAIN], build_test_spec(Config)
    ).


expired_certificate_should_be_replaced_with_http_challenge_test(Config) ->
    le_test_base:expired_certificate_should_be_replaced_test_base(build_test_spec(Config)).


expired_certificate_should_be_replaced_with_dns_challenge_test(Config) ->
    le_test_base:expired_certificate_should_be_replaced_test_base(build_test_spec(Config)).


automatic_certification_renewal_test(Config) ->
    le_test_base:automatic_certification_renewal_test_base(build_test_spec(Config)).


disabling_lets_encrypt_should_do_nothing_to_already_present_certificate_test(Config) ->
    le_test_base:disabling_lets_encrypt_should_do_nothing_to_already_present_certificate_test_base(
        build_test_spec(Config)
    ).


failed_certification_attempt_leaves_lets_encrypt_disabled_test(Config) ->
    le_test_base:failed_certification_attempt_leaves_lets_encrypt_intact_test_base(build_test_spec(Config)).


failed_certification_attempt_leaves_lets_encrypt_enabled_test(Config) ->
    le_test_base:failed_certification_attempt_leaves_lets_encrypt_intact_test_base(build_test_spec(Config)).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, le_test_base, ip_test_utils, cert_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1op_2nodes_not_deployed",
        envs = [
            {op_panel, ctool, [
                % Allow Oneprovider panel to connect with Pebble server
                {force_insecure_connections, true}
            ]},
            {op_panel, onepanel, [
                {letsencrypt_issuer_regex, ?RE_PEBBLE_ISSUER},
                % Increase certification attempts as pebble likes to fail from time to time
                {letsencrypt_attempts, ?CERTIFICATION_ATTEMPTS},

                {ones3_verbose_log_level, 3}
            ]}
        ],
        posthook = fun(NewConfig) ->
            % Requests should be made without cert verification due to possibly
            % incorrect certificates (tests will mess with them)
            panel_test_rest:set_insecure_flag(),
            dns_test_utils:update_zone_subdomain_delegation(true),

            add_op_etc_hosts_entries_on_testmaster(NewConfig),
            add_op_etc_hosts_entries_on_ones3_node(NewConfig),
            NewConfig2 = perhaps_deploy(NewConfig),
            NewConfig2
        end
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


init_per_group(Group, Config) ->
    ChallengeType = case Group of
        http_challenge -> http;
        dns_challenge -> dns
    end,
    PanelNoes = panel_test_utils:get_panel_nodes(krakow),
    test_utils:mock_new(PanelNoes, letsencrypt_api),
    test_utils:mock_expect(PanelNoes, letsencrypt_api, challenge_types, fun() ->
        [ChallengeType]
    end),

    Config.


end_per_group(_Group, Config) ->
    PanelNoes = panel_test_utils:get_panel_nodes(krakow),
    test_utils:mock_unload(PanelNoes, [letsencrypt_api]),

    Config.


init_per_testcase(Testcase = automatic_certification_renewal_test, Config) ->
    init_per_testcase(
        ?DEFAULT_CASE(Testcase),
        le_test_base:init_automatic_certification_renewal_test(krakow, Config)
    );

init_per_testcase(Testcase, Config) when
    Testcase =:= failed_certification_attempt_leaves_lets_encrypt_disabled_test;
    Testcase =:= failed_certification_attempt_leaves_lets_encrypt_enabled_test
->
    LetsEncryptPolicy = case Testcase of
        failed_certification_attempt_leaves_lets_encrypt_disabled_test -> disable;
        failed_certification_attempt_leaves_lets_encrypt_enabled_test -> enable
    end,
    init_per_testcase(
        ?DEFAULT_CASE(Testcase),
        le_test_base:init_failed_certification_attempt_test(krakow, LetsEncryptPolicy, Config)
    );

init_per_testcase(_Testcase, Config) ->
    PanelNodes = panel_test_utils:get_panel_nodes(krakow),
    test_utils:mock_new(PanelNodes, [service_oz_worker, service_onepanel], [passthrough]),

    Config.


end_per_testcase(Testcase = automatic_certification_renewal_test, Config) ->
    end_per_testcase(
        ?DEFAULT_CASE(Testcase),
        le_test_base:teardown_automatic_certification_renewal_test(krakow, Config)
    );

end_per_testcase(Testcase, Config) when
    Testcase =:= failed_certification_attempt_leaves_lets_encrypt_disabled_test;
    Testcase =:= failed_certification_attempt_leaves_lets_encrypt_enabled_test
->
    end_per_testcase(
        ?DEFAULT_CASE(Testcase),
        le_test_base:teardown_failed_certification_attempt_test(krakow, Config)
    );

end_per_testcase(_Testcase, Config) ->
    PanelNodes = panel_test_utils:get_panel_nodes(krakow),
    test_utils:mock_unload(PanelNodes, [service_oz_worker, service_onepanel]),

    cert_test_utils:set_certification_attempts(krakow, ?CERTIFICATION_ATTEMPTS),
    cert_test_utils:deploy_certs(krakow, ?PEBBLE_VALID_CERT_DIR_NAME, Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec build_test_spec(test_config:config()) -> le_test_base:test_spec().
build_test_spec(Config) ->
    OpDomain = get_domain(),
    OneS3Domain = get_s3_domain(),

    #le_test_spec{
        entity_selector = krakow,
        exp_domain = OpDomain,
        exp_dns_names = [OpDomain, OneS3Domain],
        service = ?SERVICE(?OP_PANEL, oct_background:get_provider_id(krakow)),
        ct_config = Config
    }.


%% @private
-spec get_s3_domain() -> binary().
get_s3_domain() ->
    OpDomain = get_domain(),
    <<"s3.", OpDomain/binary>>.


%% @private
-spec get_domain() -> binary().
get_domain() ->
    ProviderName = ?PROVIDER_NAME,
    OzDomain = oct_background:get_zone_domain(),
    <<ProviderName/binary, ".", OzDomain/binary>>.


%% @private
perhaps_deploy(Config) ->
    OpPanelNodes = ?config(op_panel_nodes, Config),

    IsDeploymentReadyFun = fun(PanelNode) ->
        panel_test_rpc:call(PanelNode, onepanel_deployment, is_set, [?PROGRESS_READY])
    end,

    case lists:all(IsDeploymentReadyFun, OpPanelNodes) of
        true ->
            Config;
        false ->
            deploy_using_batch_config(Config),
            NewConfig1 = oct_nodes:refresh_config(Config),
            NewConfig2 = oct_nodes:connect_with_nodes(NewConfig1),
            oct_background:update_environment(NewConfig2)
    end.


%% @private
deploy_using_batch_config(Config) ->
    AdminUserId = oct_background:get_user_id(admin),
    RegistrationToken = tokens_test_utils:create_provider_registration_token(AdminUserId),

    [OpPanelNode1, OpPanelNode2] = OpPanelNodes = ?config(op_panel_nodes, Config),
    [_OpIpHost1, OpIpHost2] = OpIps = lists:map(fun ip_test_utils:get_node_ip/1, OpPanelNodes),
    [OpIpHost1Bin, OpIpHost2Bin] = lists:map(fun ip_test_utils:encode_ip/1, OpIps),
    [OpHost1, OpHost2] = hosts:from_nodes(OpPanelNodes),

    panel_test_rpc:set_emergency_passphrase(OpPanelNode1, ?ONENV_EMERGENCY_PASSPHRASE),

    BatchConfig = #{
        <<"cluster">> => #{
            <<"nodes">> => #{
                <<"node-1">> => #{
                    <<"hostname">> => str_utils:to_binary(OpHost1),
                    <<"externalIp">> => OpIpHost1Bin
                },
                <<"node-2">> => #{
                    <<"hostname">> => str_utils:to_binary(OpHost2),
                    <<"externalIp">> => OpIpHost2Bin
                }
            },
            <<"managers">> => #{
                <<"mainNode">> => <<"node-1">>,
                <<"nodes">> => [<<"node-1">>, <<"node-2">>]
            },
            <<"workers">> => #{
                <<"nodes">> => [<<"node-1">>]
            },
            <<"oneS3">> => #{
                <<"nodes">> => [<<"node-2">>]
            },
            <<"databases">> => #{
                <<"nodes">> => [<<"node-1">>]
            }
        },
        <<"oneprovider">> => #{
            <<"register">> => true,
            <<"token">> => RegistrationToken,

            <<"name">> => <<"krakow">>,
            <<"adminEmail">> => <<"admin@example.eu">>,
            <<"subdomainDelegation">> => true,
            <<"subdomain">> => ?PROVIDER_NAME,
            <<"letsEncryptEnabled">> => true
        }
    },

    OpRequestOpts = #{
        auth => root,
        hostname => OpIpHost1Bin
    },
    {ok, ?HTTP_202_ACCEPTED, _, Resp} = panel_test_rest:post(
        OpPanelNode1, <<"/provider/configuration">>, OpRequestOpts#{json => BatchConfig}
    ),
    TaskId = maps:get(<<"taskId">>, Resp),

    ?assertMatch(
        {ok, ?HTTP_200_OK, _, #{<<"status">> := <<"ok">>}},
        panel_test_rest:get(OpPanelNode1, <<"/tasks/", TaskId/binary>>, OpRequestOpts),
        ?AWAIT_DEPLOYMENT_READY_ATTEMPTS
    ),

    OneS3Port = panel_test_rpc:call(OpPanelNode2, service_ones3, get_port, []),
    ?assertMatch({ok, _}, gen_tcp:connect(OpIpHost2, OneS3Port, [], 10), ?AWAIT_DEPLOYMENT_READY_ATTEMPTS).


%% @private
add_op_etc_hosts_entries_on_testmaster(Config) ->
    OpPanelNodes = ?config(op_panel_nodes, Config),
    [OpIpHost1, OpIpHost2] = lists:map(fun ip_test_utils:get_node_ip/1, OpPanelNodes),

    add_entries_to_etc_hosts([
        {get_domain(), OpIpHost1},
        {get_domain(), OpIpHost2},
        {get_s3_domain(), OpIpHost2}
    ]).


%% @private
add_op_etc_hosts_entries_on_ones3_node(Config) ->
    [_OpPanelNode1, OpPanelNode2] = OpPanelNodes = ?config(op_panel_nodes, Config),
    [OpIpHost1, _OpIpHost2] = lists:map(fun ip_test_utils:get_node_ip/1, OpPanelNodes),

    EntriesToAdd = [{get_domain(), OpIpHost1}],
    panel_test_rpc:call(OpPanelNode2, fun() -> add_entries_to_etc_hosts(EntriesToAdd) end).


%% @private
-spec add_entries_to_etc_hosts([{binary(), inet:ip_address()}]) -> ok.
add_entries_to_etc_hosts(NewEntries) ->
    LinesToAdd = lists:map(fun({Domain, Ip}) ->
        str_utils:format_bin("~ts\t~ts", [ip_test_utils:encode_ip(Ip), Domain])
    end, NewEntries),

    {ok, RawFile} = file:read_file("/etc/hosts"),
    LinesToPreserve = binary:split(RawFile, <<"\n">>, [global]),

    {ok, File} = file:open("/etc/hosts", [write]),

    lists:foreach(fun(Entry) ->
        io:fwrite(File, "~ts~n", [Entry])
    end, LinesToPreserve ++ LinesToAdd),

    file:close(File).
