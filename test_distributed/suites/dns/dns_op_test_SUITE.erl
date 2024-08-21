%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Oneprovider use of Onezone's internal DNS.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_op_test_SUITE).
-author("Bartosz Walkowicz").

-include("names.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).
-export([
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    configure_dns_for_domain_test/1,
    modify_ips_for_domain_test/1,
    configure_dns_for_subdomain_test/1,
    modify_ips_for_subdomain_test/1,
    modify_subdomain_test/1
]).

all() -> [
    configure_dns_for_domain_test,
    modify_ips_for_domain_test,
    configure_dns_for_subdomain_test,
    modify_ips_for_subdomain_test,
    modify_subdomain_test
].


-define(PROVIDER_SELECTOR, krakow).
-define(SUBDOMAIN_LABEL, <<"krakow">>).

-define(EXP_NONE_IPS, #{worker => [], s3 => []}).

-define(ATTEMPTS, 30).


%%%===================================================================
%%% API
%%%===================================================================


configure_dns_for_domain_test(_Config) ->
    OpDomain = get_op_domain(),
    configure_domain(),
    assert_provider_domain(OpDomain),

    OpWorkerIps = [OneS3Ip | _] = get_op_nodes_ips(),
    ExpIps = #{worker => OpWorkerIps, s3 => [OneS3Ip]},

    % Originally, with no dns servers specified (system defaults will be used)
    % dns check should:
    % - return only domain check
    % - domain check passes as one-env adds domain mappings to /etc/hosts
    %   (default system resolver will check it first)
    InitialDnsConfig = #{
        <<"builtInDnsServer">> => false,
        <<"dnsServers">> => [],
        <<"dnsCheckAcknowledged">> => true
    },
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, InitialDnsConfig),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, InitialDnsConfig),
    assert_oz_dns(OpDomain, ?EXP_NONE_IPS),
    assert_panel_dns_check(OpDomain, ok, ExpIps),

    % Enabling build in dns server does nothing (this option is relevant only for oz)
    DnsConfigDiff1 = #{<<"builtInDnsServer">> => true},
    ExpDnsConfig1 = maps:merge(InitialDnsConfig, DnsConfigDiff1),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff1),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig1),
    assert_oz_dns(OpDomain, ?EXP_NONE_IPS),
    assert_panel_dns_check(OpDomain, ok, ExpIps),

    % With dns server set explicitly to external one dns check should fail
    DnsConfigDiff2 = #{<<"dnsServers">> => [<<"8.8.8.8">>]},
    ExpDnsConfig2 = maps:merge(ExpDnsConfig1, DnsConfigDiff2),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff2),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig2),
    assert_oz_dns(OpDomain, ?EXP_NONE_IPS),
    assert_panel_dns_check(OpDomain, unresolvable, ExpIps),

    % With dns server set explicitly to oz one dns check should also fail
    OzIps = ip_test_utils:get_zone_nodes_ips(),
    DnsConfigDiff3 = #{<<"dnsServers">> => [?RAND_ELEMENT(ip_test_utils:encode_ips(OzIps))]},
    ExpDnsConfig3 = maps:merge(ExpDnsConfig2, DnsConfigDiff3),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff3),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig3),
    assert_oz_dns(OpDomain, ?EXP_NONE_IPS),
    assert_panel_dns_check(OpDomain, unresolvable, ExpIps).


modify_ips_for_domain_test(_Config) ->
    OpDomain = get_op_domain(),
    configure_domain(),
    assert_provider_domain(OpDomain),

    OpWorkerIps = [OneS3Ip | _] = get_op_nodes_ips(),

    ip_test_utils:assert_cluster_ips(?PROVIDER_SELECTOR, OpWorkerIps),
    assert_oz_dns(OpDomain, ?EXP_NONE_IPS),
    assert_panel_dns_check(OpDomain, unresolvable, #{worker => OpWorkerIps, s3 => [OneS3Ip]}),

    NewOpWorkerIps = lists:sort([ip_test_utils:random_ip(), ip_test_utils:random_ip()]),

    ip_test_utils:update_cluster_ips(?PROVIDER_SELECTOR, NewOpWorkerIps),
    ip_test_utils:assert_cluster_ips(?PROVIDER_SELECTOR, NewOpWorkerIps),
    assert_oz_dns(OpDomain, ?EXP_NONE_IPS),
    assert_panel_dns_check(OpDomain, unresolvable, #{worker => NewOpWorkerIps, s3 => [hd(NewOpWorkerIps)]}).


configure_dns_for_subdomain_test(_Config) ->
    OpSubdomain = build_op_subdomain(),
    configure_subdomain(),
    assert_provider_domain(OpSubdomain),

    OpWorkerIps = [OneS3Ip | _] = get_op_nodes_ips(),
    ExpIps = #{worker => OpWorkerIps, s3 => [OneS3Ip]},

    % Originally, with no dns servers specified (system defaults will be used)
    % dns check should:
    % - return only domain check
    % - domain check fails as no mappings are added to /etc/hosts for new subdomain
    InitialDnsConfig = #{
        <<"builtInDnsServer">> => false,
        <<"dnsServers">> => [],
        <<"dnsCheckAcknowledged">> => true
    },
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, InitialDnsConfig),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, InitialDnsConfig),
    assert_oz_dns(OpSubdomain, ExpIps),
    assert_panel_dns_check(OpSubdomain, unresolvable, ExpIps),

    % Enabling build in dns server does nothing (this option is relevant only for oz)
    DnsConfigDiff1 = #{<<"builtInDnsServer">> => true},
    ExpDnsConfig1 = maps:merge(InitialDnsConfig, DnsConfigDiff1),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff1),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig1),
    assert_oz_dns(OpSubdomain, ExpIps),
    assert_panel_dns_check(OpSubdomain, unresolvable, ExpIps),

    % With dns server set explicitly to external one dns check should fail
    DnsConfigDiff2 = #{<<"dnsServers">> => [<<"8.8.8.8">>]},
    ExpDnsConfig2 = maps:merge(ExpDnsConfig1, DnsConfigDiff2),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff2),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig2),
    assert_oz_dns(OpSubdomain, ExpIps),
    assert_panel_dns_check(OpSubdomain, unresolvable, ExpIps),

    % With dns server set explicitly to oz one dns check should also fail
    OzIps = ip_test_utils:get_zone_nodes_ips(),
    DnsConfigDiff3 = #{<<"dnsServers">> => [?RAND_ELEMENT(ip_test_utils:encode_ips(OzIps))]},
    ExpDnsConfig3 = maps:merge(ExpDnsConfig2, DnsConfigDiff3),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff3),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig3),
    assert_oz_dns(OpSubdomain, ExpIps),
    assert_panel_dns_check(OpSubdomain, ok, ExpIps).


modify_ips_for_subdomain_test(_Config) ->
    OpSubdomain = build_op_subdomain(),
    configure_subdomain(),
    assert_provider_domain(OpSubdomain),

    OpWorkerIps = [OneS3Ip | _] = get_op_nodes_ips(),
    ExpIps = #{worker => OpWorkerIps, s3 => [OneS3Ip]},

    ip_test_utils:assert_cluster_ips(?PROVIDER_SELECTOR, OpWorkerIps),
    assert_oz_dns(OpSubdomain, ExpIps),
    assert_panel_dns_check(OpSubdomain, ok, ExpIps),

    NewOpWorkerIps = lists:sort([ip_test_utils:random_ip(), ip_test_utils:random_ip()]),
    NewExpIps = #{worker => NewOpWorkerIps, s3 => [hd(NewOpWorkerIps)]},

    ip_test_utils:update_cluster_ips(?PROVIDER_SELECTOR, NewOpWorkerIps),
    ip_test_utils:assert_cluster_ips(?PROVIDER_SELECTOR, NewOpWorkerIps),
    assert_oz_dns(OpSubdomain, NewExpIps),
    assert_panel_dns_check(OpSubdomain, ok, NewExpIps).


modify_subdomain_test(_Config) ->
    CurrentOpSubdomain = build_op_subdomain(),
    configure_subdomain(),
    assert_provider_domain(CurrentOpSubdomain),

    OpWorkerIps = [OneS3Ip | _] = get_op_nodes_ips(),
    ExpIps = #{worker => OpWorkerIps, s3 => [OneS3Ip]},

    ip_test_utils:assert_cluster_ips(?PROVIDER_SELECTOR, OpWorkerIps),
    assert_oz_dns(CurrentOpSubdomain, ExpIps),
    assert_panel_dns_check(CurrentOpSubdomain, ok, ExpIps),

    NewOpSubdomainLabel = ?RAND_STR(),
    NewOpSubdomain = build_op_subdomain(NewOpSubdomainLabel),
    configure_subdomain(NewOpSubdomainLabel),
    assert_provider_domain(NewOpSubdomain),
    assert_oz_dns(CurrentOpSubdomain, ?EXP_NONE_IPS),
    assert_oz_dns(NewOpSubdomain, ExpIps),
    assert_panel_dns_check(NewOpSubdomain, ok, ExpIps).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1op_2nodes",
        posthook = fun(NewConfig) ->
            dns_test_utils:update_zone_subdomain_delegation(true),
            NewConfig
        end
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


init_per_testcase(_Case, Config) ->
    % Ensure dns servers are set to oz dns after tests messing with dns config
    OzIps = ip_test_utils:get_zone_nodes_ips(),
    DnsConfigDiff = #{<<"dnsServers">> => [?RAND_ELEMENT(ip_test_utils:encode_ips(OzIps))]},
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff),

    Config.


end_per_testcase(Testcase, _Config) when
    Testcase =:= modify_ips_for_subdomain_test;
    Testcase =:= modify_ips_for_domain_test
->
    ip_test_utils:update_cluster_ips(?PROVIDER_SELECTOR, get_op_nodes_ips());

end_per_testcase(_Case, _Config) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
configure_domain() ->
    update_provider_details(#{
        <<"subdomainDelegation">> => false,
        <<"domain">> => get_op_domain()
    }).


%% @private
configure_subdomain() ->
    configure_subdomain(?SUBDOMAIN_LABEL).


%% @private
configure_subdomain(SubdomainLabel) ->
    update_provider_details(#{
        <<"subdomainDelegation">> => true,
        <<"subdomain">> => SubdomainLabel
    }).


%% @private
update_provider_details(JsonData) ->
    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(?PROVIDER_SELECTOR, <<"/provider">>, #{auth => root, json => JsonData})
    ),
    ok.


%% @private
assert_provider_domain(ExpDomain) ->
    ?assertMatch(
        {ok, ?HTTP_200_OK, _, #{<<"domain">> := ExpDomain}},
        panel_test_rest:get(?PROVIDER_SELECTOR, <<"/provider">>, #{auth => root})
    ),
    ok.


%% @private
get_op_domain() ->
    oct_background:get_provider_domain(?PROVIDER_SELECTOR).


%% @private
build_op_subdomain() ->
    build_op_subdomain(?SUBDOMAIN_LABEL).


%% @private
build_op_subdomain(SubdomainLabel) ->
    OzDomain = dns_test_utils:get_zone_domain(),
    str_utils:format_bin("~ts.~ts", [SubdomainLabel, OzDomain]).


%% @private
get_op_nodes_ips() ->
    ip_test_utils:get_provider_nodes_ips(?PROVIDER_SELECTOR).


%% @private
assert_oz_dns(OpSubdomain, ExpIps) ->
    DnsServerIps = ip_test_utils:get_zone_nodes_ips(),

    maps:foreach(fun
        (worker, ExpWorkerIps) ->
            SortedExpOpWorkerIps = lists:sort(ExpWorkerIps),
            dns_test_utils:assert_dns_answer(DnsServerIps, OpSubdomain, a, SortedExpOpWorkerIps);

        % TODO VFS-12241 s3
    %%    OneS3Subdomain = <<"s3.", OpSubdomain/binary>>,
    %%    SortedExpOneS3Ips = lists:sort(ExpOneS3Ips),
    %%    dns_test_utils:assert_dns_answer(DnsServerIps, OneS3Subdomain, a, SortedExpOneS3Ips).
        (_, _) ->
            ok
    end, ExpIps).


%% @private
assert_panel_dns_check(ExpOpSubdomain, ExpSummary, ExpIps) ->
    ExpCheckResults = build_exp_dns_check_results(ExpOpSubdomain, ExpSummary, ExpIps),
    PerformFun = fun() ->
        dns_test_utils:invalidate_dns_check_cache(?PROVIDER_SELECTOR),
        maps:without([<<"timestamp">>], dns_test_utils:perform_dns_check(?PROVIDER_SELECTOR))
    end,
    ?assertEqual(ExpCheckResults, PerformFun(), ?ATTEMPTS).


%% @private
build_exp_dns_check_results(ExpOpSubdomain, ExpSummary, ExpIps) ->
    maps:fold(fun
        (worker, ExpWorkerIps, Acc) ->
            Acc#{<<"domain">> => build_exp_dns_check_result(
                ExpOpSubdomain, ExpWorkerIps, ExpSummary
            )};

        %% TODO VFS-12241 s3
%%        (s3, ExpOneS3Ips, Acc) ->
%%            Acc#{<<"oneS3Subdomain">> => build_exp_dns_check_result(
%%                <<"s3.", ExpOpSubdomain/binary>>, ExpOneS3Ips, ExpOneS3Summary
%%            )};
        (_, _, Acc) ->
            Acc
    end, #{}, ExpIps).


%% @private
build_exp_dns_check_result(Subdomain, ExpIps, ExpSubdomainCheckSummary) ->
    ExpIpsBin = ip_test_utils:encode_ips(ExpIps),

    #{
        <<"summary">> => str_utils:to_binary(ExpSubdomainCheckSummary),
        <<"expected">> => ExpIpsBin,
        <<"got">> => case ExpSubdomainCheckSummary of
            ok -> ExpIpsBin;
            unresolvable -> []
        end,
        <<"recommended">> => lists:sort(lists:map(fun(IpBin) ->
            str_utils:format_bin("~ts. IN A ~ts", [Subdomain, IpBin])
        end, ExpIpsBin))
    }.
