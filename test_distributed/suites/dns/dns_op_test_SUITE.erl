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
-include_lib("ctool/include/test/assertions.hrl").
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

-define(ATTEMPTS, 30).

% TODO test s3

%%%===================================================================
%%% API
%%%===================================================================


configure_dns_for_domain_test(_Config) ->
    configure_domain(),
    OpDomain = get_op_domain(),
    OpIps = get_op_ips(),

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
    assert_oz_dns(OpDomain, [], []),
    assert_panel_dns_check(OpDomain, OpIps, ok, [], none),

    % Enabling build in dns server does nothing (this option is relevant only for oz)
    DnsConfigDiff1 = #{<<"builtInDnsServer">> => true},
    ExpDnsConfig1 = maps:merge(InitialDnsConfig, DnsConfigDiff1),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff1),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig1),
    assert_oz_dns(OpDomain, [], []),
    assert_panel_dns_check(OpDomain, OpIps, ok, [], none),

    % With dns server set explicitly to external one dns check should fail
    DnsConfigDiff2 = #{<<"dnsServers">> => [<<"8.8.8.8">>]},
    ExpDnsConfig2 = maps:merge(ExpDnsConfig1, DnsConfigDiff2),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff2),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig2),
    assert_oz_dns(OpDomain, [], []),
    assert_panel_dns_check(OpDomain, OpIps, unresolvable, [], none),

    % With dns server set explicitly to oz one dns check should also fail
    OzIps = ip_test_utils:get_zone_nodes_ips(),
    DnsConfigDiff3 = #{<<"dnsServers">> => [?RAND_ELEMENT(ip_test_utils:encode_ips(OzIps))]},
    ExpDnsConfig3 = maps:merge(ExpDnsConfig2, DnsConfigDiff3),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff3),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig3),
    assert_oz_dns(OpDomain, [], []),
    assert_panel_dns_check(OpDomain, OpIps, unresolvable, [], none).


modify_ips_for_domain_test(_Config) ->
    configure_domain(),
    OpDomain = get_op_domain(),
    OpIps = get_op_ips(),

    ip_test_utils:assert_cluster_ips(?PROVIDER_SELECTOR, OpIps),
    assert_oz_dns(OpDomain, [], []),
    assert_panel_dns_check(OpDomain, OpIps, unresolvable, [], none),

    NewOpIps = lists:sort([ip_test_utils:random_ip(), ip_test_utils:random_ip()]),
    ip_test_utils:update_cluster_ips(?PROVIDER_SELECTOR, NewOpIps),
    ip_test_utils:assert_cluster_ips(?PROVIDER_SELECTOR, NewOpIps),
    assert_oz_dns(OpDomain, [], []),
    assert_panel_dns_check(OpDomain, NewOpIps, unresolvable, [], none).


configure_dns_for_subdomain_test(_Config) ->
    configure_subdomain(),
    OpSubdomain = get_op_subdomain(),
    OpIps = get_op_ips(),

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
    assert_oz_dns(OpSubdomain, OpIps, []),
    assert_panel_dns_check(OpSubdomain, OpIps, unresolvable, [], none),

    % Enabling build in dns server does nothing (this option is relevant only for oz)
    DnsConfigDiff1 = #{<<"builtInDnsServer">> => true},
    ExpDnsConfig1 = maps:merge(InitialDnsConfig, DnsConfigDiff1),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff1),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig1),
    assert_oz_dns(OpSubdomain, OpIps, []),
    assert_panel_dns_check(OpSubdomain, OpIps, unresolvable, [], none),

    % With dns server set explicitly to external one dns check should fail
    DnsConfigDiff2 = #{<<"dnsServers">> => [<<"8.8.8.8">>]},
    ExpDnsConfig2 = maps:merge(ExpDnsConfig1, DnsConfigDiff2),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff2),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig2),
    assert_oz_dns(OpSubdomain, OpIps, []),
    assert_panel_dns_check(OpSubdomain, OpIps, unresolvable, [], none),

    % With dns server set explicitly to oz one dns check should also fail
    OzIps = ip_test_utils:get_zone_nodes_ips(),
    DnsConfigDiff3 = #{<<"dnsServers">> => [?RAND_ELEMENT(ip_test_utils:encode_ips(OzIps))]},
    ExpDnsConfig3 = maps:merge(ExpDnsConfig2, DnsConfigDiff3),
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff3),
    dns_test_utils:assert_panel_dns_config(?PROVIDER_SELECTOR, ExpDnsConfig3),
    assert_oz_dns(OpSubdomain, OpIps, []),
    assert_panel_dns_check(OpSubdomain, OpIps, ok, [], none).


modify_ips_for_subdomain_test(_Config) ->
    configure_subdomain(),
    OpSubdomain = get_op_subdomain(),
    OpIps = get_op_ips(),

    ip_test_utils:assert_cluster_ips(?PROVIDER_SELECTOR, OpIps),
    assert_oz_dns(OpSubdomain, OpIps, []),
    assert_panel_dns_check(OpSubdomain, OpIps, ok, [], none),

    NewOpIps = lists:sort([ip_test_utils:random_ip(), ip_test_utils:random_ip()]),
    ip_test_utils:update_cluster_ips(?PROVIDER_SELECTOR, NewOpIps),
    ip_test_utils:assert_cluster_ips(?PROVIDER_SELECTOR, NewOpIps),
    assert_oz_dns(OpSubdomain, NewOpIps, []),
    assert_panel_dns_check(OpSubdomain, NewOpIps, ok, [], none).


modify_subdomain_test(_Config) ->
    configure_subdomain(),
    OpSubdomain = get_op_subdomain(),
    OpIps = get_op_ips(),

    ip_test_utils:assert_cluster_ips(?PROVIDER_SELECTOR, OpIps),
    assert_oz_dns(OpSubdomain, OpIps, []),
    assert_panel_dns_check(OpSubdomain, OpIps, ok, [], none),

    NewOpSubdomainLabel = ?RAND_STR(),
    configure_subdomain(NewOpSubdomainLabel),
    NewOpSubdomain = get_op_subdomain(NewOpSubdomainLabel),
    assert_oz_dns(OpSubdomain, [], []),
    assert_oz_dns(NewOpSubdomain, OpIps, []),
    assert_panel_dns_check(NewOpSubdomain, OpIps, ok, [], none).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1op_2nodes",
        posthook = fun(NewConfig) ->
            enable_subdomain_delegation(),
            NewConfig
        end
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


init_per_testcase(_Case, Config) ->
    Config.


end_per_testcase(Testcase, _Config) when
    Testcase =:= configure_dns_for_domain_test;
    Testcase =:= configure_dns_for_subdomain_test
->
    % Ensure dns servers are set to oz dns after tests messing with dns config
    OzIps = ip_test_utils:get_zone_nodes_ips(),
    DnsConfigDiff = #{<<"dnsServers">> => [?RAND_ELEMENT(ip_test_utils:encode_ips(OzIps))]},
    dns_test_utils:update_panel_dns_config(?PROVIDER_SELECTOR, DnsConfigDiff);

end_per_testcase(Testcase, _Config) when
    Testcase =:= modify_ips_for_subdomain_test;
    Testcase =:= modify_ips_for_domain_test
->
    ip_test_utils:update_cluster_ips(?PROVIDER_SELECTOR, get_op_ips());

end_per_testcase(_Case, _Config) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
enable_subdomain_delegation() ->
    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(zone, <<"/zone/policies">>, #{auth => root, json => #{
            <<"subdomainDelegation">> => true
        }})
    ),
    ok.


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
get_op_domain() ->
    oct_background:get_provider_domain(?PROVIDER_SELECTOR).


%% @private
get_op_subdomain() ->
    get_op_subdomain(?SUBDOMAIN_LABEL).


%% @private
get_op_subdomain(SubdomainLabel) ->
    OzDomain = dns_test_utils:get_zone_domain(),
    str_utils:format_bin("~s.~s", [SubdomainLabel, OzDomain]).


%% @private
get_op_ips() ->
    ip_test_utils:get_provider_nodes_ips(?PROVIDER_SELECTOR).


%% @private
assert_oz_dns(OpSubdomain, ExpOpWorkerIps, ExpOneS3Ips) ->
    DnsServerIps = ip_test_utils:get_zone_nodes_ips(),

    SortedExpOpWorkerIps = lists:sort(ExpOpWorkerIps),
    dns_test_utils:assert_dns_answer(DnsServerIps, OpSubdomain, a, SortedExpOpWorkerIps),

    OneS3Subdomain = <<"s3.", OpSubdomain/binary>>,
    SortedExpOneS3Ips = lists:sort(ExpOneS3Ips),
    dns_test_utils:assert_dns_answer(DnsServerIps, OneS3Subdomain, a, SortedExpOneS3Ips).


%% @private
assert_panel_dns_check(ExpOpSubdomain, ExpOpWorkerIps, ExpOpWorkerCheckSummary, ExpOneS3Ips, ExpOneS3CheckSummary) ->
    ExpCheckResult = build_exp_dns_check(
        ExpOpSubdomain, ExpOpWorkerIps, ExpOpWorkerCheckSummary, ExpOneS3Ips, ExpOneS3CheckSummary
    ),
    PerformFun = fun() ->
        dns_test_utils:invalidate_dns_check_cache(?PROVIDER_SELECTOR),

        Check = dns_test_utils:perform_dns_check(?PROVIDER_SELECTOR),
        maps:without([<<"timestamp">>], Check)
    end,
    ?assertEqual(ExpCheckResult, PerformFun(), ?ATTEMPTS).


%% @private
build_exp_dns_check(
    ExpOpSubdomain, ExpOpWorkerIps, ExpOpWorkerCheckSummary, ExpOneS3Ips, ExpOneS3CheckSummary
) ->
    ExpDomainCheck = #{<<"domain">> => build_exp_dns_subdomain_check(
        ExpOpSubdomain, ExpOpWorkerIps, ExpOpWorkerCheckSummary
    )},
    case ExpOneS3CheckSummary of
        none ->
            ExpDomainCheck;
        _ ->
            ExpDomainCheck#{<<"oneS3Subdomain">> => build_exp_dns_subdomain_check(
                <<"s3.", ExpOpSubdomain/binary>>, ExpOneS3Ips, ExpOneS3CheckSummary
            )}
    end.


%% @private
build_exp_dns_subdomain_check(Subdomain, ExpIps, ExpSubdomainCheckSummary) ->
    ExpIpsBin = ip_test_utils:encode_ips(ExpIps),

    #{
        <<"summary">> => str_utils:to_binary(ExpSubdomainCheckSummary),
        <<"expected">> => ExpIpsBin,
        <<"got">> => case ExpSubdomainCheckSummary of
            ok -> ExpIpsBin;
            unresolvable -> []
        end,
        <<"recommended">> => lists:sort(lists:map(fun(IpBin) ->
            str_utils:format_bin("~s. IN A ~s", [Subdomain, IpBin])
        end, ExpIpsBin))
    }.
