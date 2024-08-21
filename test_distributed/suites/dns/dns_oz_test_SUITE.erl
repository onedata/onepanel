%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Onezone use of Onezone's internal DNS.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_oz_test_SUITE).
-author("Bartosz Walkowicz").

-include("names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).
-export([
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    configure_dns_test/1,
    modify_ips_test/1
]).

all() -> [
    configure_dns_test,
    modify_ips_test
].


-define(NS_DOMAIN_LABELS, [<<"ns1">>, <<"ns2">>]).

-define(ATTEMPTS, 30).


%%%===================================================================
%%% API
%%%===================================================================


configure_dns_test(_Config) ->
    OzDomain = dns_test_utils:get_zone_domain(),
    OzIps = ip_test_utils:get_zone_nodes_ips(),

    % Originally, with build in dns server disabled and no dns servers specified
    % (system defaults will be used) dns check should:
    % - return only domain check
    % - domain check passes as one-env adds domain mappings to /etc/hosts
    %   (default system resolver will check it first)
    InitialDnsConfig = #{
        <<"builtInDnsServer">> => false,
        <<"dnsServers">> => [],
        <<"dnsCheckAcknowledged">> => true
    },
    dns_test_utils:update_panel_dns_config(zone, InitialDnsConfig),
    dns_test_utils:assert_panel_dns_config(zone, InitialDnsConfig),
    assert_oz_dns(OzDomain, OzIps),
    assert_panel_dns_check(OzDomain, OzIps, ok, none),

    % With built in dns server enabled dns check should:
    % - return domain and dns zone checks
    % - domain check passes as one-env adds domain mappings to /etc/hosts
    %   (default system resolver will check it first)
    % - dns zone fails as there are no such mapping in /etc/hosts and no "real"
    %   dns server contains them
    DnsConfigDiff1 = #{<<"builtInDnsServer">> => true},
    ExpDnsConfig1 = maps:merge(InitialDnsConfig, DnsConfigDiff1),
    dns_test_utils:update_panel_dns_config(zone, DnsConfigDiff1),
    dns_test_utils:assert_panel_dns_config(zone, ExpDnsConfig1),
    assert_oz_dns(OzDomain, OzIps),
    assert_panel_dns_check(OzDomain, OzIps, ok, unresolvable),

    % With dns server set explicitly to external one dns check should fail
    DnsConfigDiff2 = #{<<"dnsServers">> => [<<"8.8.8.8">>]},
    ExpDnsConfig2 = maps:merge(ExpDnsConfig1, DnsConfigDiff2),
    dns_test_utils:update_panel_dns_config(zone, DnsConfigDiff2),
    dns_test_utils:assert_panel_dns_config(zone, ExpDnsConfig2),
    assert_oz_dns(OzDomain, OzIps),
    assert_panel_dns_check(OzDomain, OzIps, unresolvable, unresolvable),

    % With explicitly set dns servers to oz dns both domain and dns zone checks
    % should succeed
    DnsConfigDiff3 = #{<<"dnsServers">> => [?RAND_ELEMENT(ip_test_utils:encode_ips(OzIps))]},
    ExpDnsConfig3 = maps:merge(ExpDnsConfig2, DnsConfigDiff3),
    dns_test_utils:update_panel_dns_config(zone, DnsConfigDiff3),
    dns_test_utils:assert_panel_dns_config(zone, ExpDnsConfig3),
    assert_oz_dns(OzDomain, OzIps),
    assert_panel_dns_check(OzDomain, OzIps, ok, ok).


modify_ips_test(_Config) ->
    OzDomain = dns_test_utils:get_zone_domain(),
    OzIps = ip_test_utils:get_zone_nodes_ips(),

    DnsConfig = #{
        <<"builtInDnsServer">> => true,
        <<"dnsServers">> => [?RAND_ELEMENT(ip_test_utils:encode_ips(OzIps))]
    },
    dns_test_utils:update_panel_dns_config(zone, DnsConfig),
    ip_test_utils:assert_cluster_ips(zone, OzIps),
    assert_oz_dns(OzDomain, OzIps),
    assert_panel_dns_check(OzDomain, OzIps, ok, ok),

    NewOzIps = lists:sort([ip_test_utils:random_ip(), ip_test_utils:random_ip()]),
    ip_test_utils:update_cluster_ips(zone, NewOzIps),
    ip_test_utils:assert_cluster_ips(zone, NewOzIps),
    assert_oz_dns(OzDomain, NewOzIps),
    assert_panel_dns_check(OzDomain, NewOzIps, ok, ok).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1oz_2nodes"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


init_per_testcase(_Case, Config) ->
    Config.


end_per_testcase(configure_dns_test, _Config) ->
    % Ensure dns servers are set to oz dns after tests messing with dns config
    OzIps = ip_test_utils:get_zone_nodes_ips(),
    DnsConfigDiff = #{
        <<"builtInDnsServer">> => false,
        <<"dnsServers">> => [?RAND_ELEMENT(ip_test_utils:encode_ips(OzIps))]
    },
    dns_test_utils:update_panel_dns_config(zone, DnsConfigDiff);

end_per_testcase(modify_ips_test, _Config) ->
    ip_test_utils:update_cluster_ips(zone, ip_test_utils:get_zone_nodes_ips());

end_per_testcase(_Case, _Config) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
assert_oz_dns(OzDomain, ExpOzIps) ->
    DnsServerIps = ip_test_utils:get_zone_nodes_ips(),
    SortedExpOzIps = lists:sort(ExpOzIps),

    dns_test_utils:assert_dns_answer(DnsServerIps, OzDomain, a, SortedExpOzIps),

    % Even if oz dns is not enabled entries for ns are created
    NsDomains = lists:map(fun(NsDomainLabel) ->
        str_utils:format("~ts.~ts", [NsDomainLabel, OzDomain])
    end, ?NS_DOMAIN_LABELS),

    dns_test_utils:assert_dns_answer(DnsServerIps, OzDomain, ns, NsDomains),

    lists:foreach(fun({NsDomain, ExpOzIp}) ->
        dns_test_utils:assert_dns_answer(DnsServerIps, NsDomain, a, [ExpOzIp])
    end, lists:zip(NsDomains, SortedExpOzIps)).


%% @private
assert_panel_dns_check(ExpOzDomain, ExpOzIps, ExpDomainCheckSummary, ExpDnsZoneCheckSummary) ->
    ExpCheckResults = build_exp_dns_check_results(
        ExpOzDomain, ExpOzIps, ExpDomainCheckSummary, ExpDnsZoneCheckSummary
    ),
    PerformFun = fun() ->
        dns_test_utils:invalidate_dns_check_cache(zone),
        maps:without([<<"timestamp">>], dns_test_utils:perform_dns_check(zone))
    end,
    ?assertEqual(ExpCheckResults, PerformFun(), ?ATTEMPTS).


%% @private
build_exp_dns_check_results(ExpOzDomain, ExpOzIps, ExpDomainCheckSummary, ExpDnsZoneCheckSummary) ->
    ExpOzIpsBin = ip_test_utils:encode_ips(ExpOzIps),

    ExpDomainRecommended = lists:sort(lists:map(fun(OzIpBin) ->
        str_utils:format_bin("~ts. IN A ~ts", [ExpOzDomain, OzIpBin])
    end, ExpOzIpsBin)),

    ExpDomainCheck = #{<<"domain">> => build_exp_dns_check_result(
        ExpDomainCheckSummary, ExpOzIpsBin, ExpDomainRecommended
    )},
    case ExpDnsZoneCheckSummary of
        none ->
            ExpDomainCheck;
        _ ->
            ExpDnsZoneRecommended = lists:flatten([
                lists:map(fun(NsDomainLabel) ->
                    str_utils:format_bin("~ts. IN NS ~ts.~ts", [ExpOzDomain, NsDomainLabel, ExpOzDomain])
                end, ?NS_DOMAIN_LABELS),
                lists:map(fun({NsDomainLabel, OzIpBin}) ->
                    str_utils:format_bin("~ts.~ts. IN A ~ts", [NsDomainLabel, ExpOzDomain, OzIpBin])
                end, lists:zip(?NS_DOMAIN_LABELS, ExpOzIpsBin))
            ]),
            ExpDomainCheck#{<<"dnsZone">> => build_exp_dns_check_result(
                ExpDnsZoneCheckSummary, ExpOzIpsBin, ExpDnsZoneRecommended
            )}
    end.


%% @private
build_exp_dns_check_result(ExpSummary, ExpIpsBin, ExpRecommended) ->
    #{
        <<"summary">> => str_utils:to_binary(ExpSummary),
        <<"expected">> => ExpIpsBin,
        <<"got">> => case ExpSummary of
            ok -> ExpIpsBin;
            unresolvable -> []
        end,
        <<"recommended">> => ExpRecommended
    }.
