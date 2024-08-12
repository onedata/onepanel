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
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    configure_dns_test/1
]).

all() -> [
    configure_dns_test
].


-define(NS_DOMAIN_LABELS, [<<"ns1">>, <<"ns2">>, <<"ns3">>]).


%%%===================================================================
%%% API
%%%===================================================================


configure_dns_test(_Config) ->
    OzDomain = get_zone_domain(),
    OzIps = get_zone_ips(),
    OzIpsBin = lists:map(fun(Ip) ->
        {ok, IpBin} = ip_utils:to_binary(Ip),
        IpBin
    end, OzIps),

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
    update_panel_dns_config(InitialDnsConfig),
    assert_panel_dns_config(InitialDnsConfig),
    assert_panel_dns_check(OzDomain, OzIpsBin, ok, none),
    assert_oz_dns(OzDomain, OzIps),

    % With build in dns server enabled dns check should:
    % - return domain and dns zone checks
    % - domain check passes as one-env adds domain mappings to /etc/hosts
    %   (default system resolver will check it first)
    % - dns zone fails as there are no such mapping in /etc/hosts and no "real"
    %   dns server contains them
    DnsConfigDiff1 = #{<<"builtInDnsServer">> => true},
    ExpDnsConfig1 = maps:merge(InitialDnsConfig, DnsConfigDiff1),
    update_panel_dns_config(DnsConfigDiff1),
    assert_panel_dns_config(ExpDnsConfig1),
    assert_panel_dns_check(OzDomain, OzIpsBin, ok, unresolvable),
    assert_oz_dns(OzDomain, OzIps),

    % With build in dns server enabled dns check should fail for both domain
    % and dns zone
    DnsConfigDiff2 = #{<<"dnsServers">> => [<<"8.8.8.8">>]},
    ExpDnsConfig2 = maps:merge(ExpDnsConfig1, DnsConfigDiff2),
    update_panel_dns_config(DnsConfigDiff2),
    assert_panel_dns_config(ExpDnsConfig2),
    assert_panel_dns_check(OzDomain, OzIpsBin, unresolvable, unresolvable),
    assert_oz_dns(OzDomain, OzIps),

    % With explicitly set dns servers to oz dns both domain and dns zone checks
    % should succeed
    DnsConfigDiff3 = #{<<"dnsServers">> => ?RAND_SUBLIST(OzIpsBin, 1, length(OzIpsBin))},
    ExpDnsConfig3 = maps:merge(ExpDnsConfig2, DnsConfigDiff3),
    update_panel_dns_config(DnsConfigDiff3),
    assert_panel_dns_config(ExpDnsConfig3),
    assert_panel_dns_check(OzDomain, OzIpsBin, ok, ok),
    assert_oz_dns(OzDomain, OzIps).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1oz_3nodes"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
get_zone_domain() ->
    OzNode = ?RAND_ELEMENT(oct_background:get_zone_panels()),
    {ok, OzDomain} = test_utils:get_env(OzNode, ?APP_NAME, test_web_cert_domain),
    str_utils:to_binary(OzDomain).


%% @private
get_zone_ips() ->
    lists:map(fun get_node_ip/1, oct_background:get_zone_panels()).


%% @private
-spec get_node_ip(node()) -> inet:ip_address().
get_node_ip(Node) ->
    panel_test_rpc:insecure_call(Node, fun() ->
        {ok, IpAddresses} = inet:getifaddrs(),
        hd([
            Addr || {_, Opts} <- IpAddresses, {addr, Addr} <- Opts,
            size(Addr) == 4, Addr =/= {127, 0, 0, 1}
        ])
    end).


%% @private
assert_panel_dns_config(ExpConfig) ->
    ?assertMatch(
        {ok, ?HTTP_200_OK, _, ExpConfig},
        panel_test_rest:get(zone, <<"/dns_check/configuration">>, #{auth => root})
    ).


%% @private
update_panel_dns_config(JsonData) ->
    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(zone, <<"/dns_check/configuration">>, #{auth => root, json => JsonData})
    ),
    ok.


%% @private
assert_panel_dns_check(ExpOzDomain, ExpOzIpsBin, ExpDomainCheckSummary, ExpDnsZoneCheckSummary) ->
    ExpCheckResult = build_exp_dns_check(
        ExpOzDomain, ExpOzIpsBin, ExpDomainCheckSummary, ExpDnsZoneCheckSummary
    ),
    PerformFun = fun() ->
        Check = perform_dns_check(),
        maps:without([<<"timestamp">>], Check)
    end,
    ?assertEqual(ExpCheckResult, PerformFun()).


%% @private
build_exp_dns_check(ExpOzDomain, ExpOzIpsBin, ExpDomainCheckSummary, ExpDnsZoneCheckSummary) ->
    SortedOzIpsBin = lists:sort(ExpOzIpsBin),

    ExpDomainCheck = #{
        <<"domain">> => #{
            <<"summary">> => str_utils:to_binary(ExpDomainCheckSummary),
            <<"expected">> => SortedOzIpsBin,
            <<"got">> => case ExpDomainCheckSummary of
                ok -> SortedOzIpsBin;
                unresolvable -> []
            end,
            <<"recommended">> => lists:map(fun(OzIpBin) ->
                str_utils:format_bin("~s. IN A ~s", [ExpOzDomain, OzIpBin])
            end, SortedOzIpsBin)
        }
    },
    case ExpDnsZoneCheckSummary of
        none ->
            ExpDomainCheck;
        _ ->
            Recommended = lists:flatten([
                lists:map(fun(NsDomainLabel) ->
                    str_utils:format_bin("~s. IN NS ~s.~s", [ExpOzDomain, NsDomainLabel, ExpOzDomain])
                end, ?NS_DOMAIN_LABELS),
                lists:map(fun({NsDomainLabel, OzIpBin}) ->
                    str_utils:format_bin("~s.~s. IN A ~s", [NsDomainLabel, ExpOzDomain, OzIpBin])
                end, lists:zip(?NS_DOMAIN_LABELS, SortedOzIpsBin))
            ]),
            ExpDomainCheck#{
                <<"dnsZone">> => #{
                    <<"summary">> => str_utils:to_binary(ExpDnsZoneCheckSummary),
                    <<"expected">> => SortedOzIpsBin,
                    <<"got">> => case ExpDnsZoneCheckSummary of
                        ok -> SortedOzIpsBin;
                        unresolvable -> []
                    end,
                    <<"recommended">> => Recommended
                }
            }
    end.


%% @private
perform_dns_check() ->
    {ok, _, _, Check} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(zone, <<"/dns_check">>, #{auth => root})
    ),
    Check.


%% @private
assert_oz_dns(OzDomain, ExpOzIps) ->
    DnsServerIps = get_zone_ips(),
    SortedExpOzIps = lists:sort(ExpOzIps),

    dns_test_utils:assert_dns_answer(DnsServerIps, OzDomain, a, SortedExpOzIps),

    % Even if oz dns is not enabled entries for ns are created
    NsDomains = lists:map(fun(NsDomainLabel) ->
        str_utils:format("~s.~s", [NsDomainLabel, OzDomain])
    end, ?NS_DOMAIN_LABELS),

    dns_test_utils:assert_dns_answer(DnsServerIps, OzDomain, ns, NsDomains),

    lists:foreach(fun({NsDomain, ExpOzIp}) ->
        dns_test_utils:assert_dns_answer(DnsServerIps, NsDomain, a, [ExpOzIp])
    end, lists:zip(NsDomains, SortedExpOzIps)).
