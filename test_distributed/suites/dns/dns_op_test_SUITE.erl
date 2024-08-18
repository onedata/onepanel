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
    configure_subdomain_test/1
]).

all() -> [
    configure_subdomain_test
].


-define(PROVIDER_SELECTOR, krakow).
-define(SUBDOMAIN_LABEL, <<"krakow">>).

-define(ATTEMPTS, 30).

% TODO test s3

%%%===================================================================
%%% API
%%%===================================================================


configure_subdomain_test(_Config) ->
    OpIps = get_op_ips(),

    % Oz dns do not store Oneprovider domain so querying it should not return anything
    configure_domain(),
    OpDomain = get_op_domain(),
    assert_oz_dns(OpDomain, [], []),

    configure_subdomain(),
    OpSubDomain = get_op_subdomain(),
    assert_oz_dns(OpSubDomain, OpIps, []).  %% TODO s3


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
    update_provider_details(#{
        <<"subdomainDelegation">> => true,
        <<"subdomain">> => ?SUBDOMAIN_LABEL
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
    OzDomain = dns_test_utils:get_zone_domain(),
    str_utils:format_bin("~s.~s", [?SUBDOMAIN_LABEL, OzDomain]).


%% @private
get_op_ips() ->
    ip_test_utils:get_provider_nodes_ips(?PROVIDER_SELECTOR).


%% @private
assert_oz_dns(OpSubDomain, ExpOpWorkerIps, ExpOneS3Ips) ->
    DnsServerIps = ip_test_utils:get_zone_nodes_ips(),

    SortedExpOpWorkerIps = lists:sort(ExpOpWorkerIps),
    dns_test_utils:assert_dns_answer(DnsServerIps, OpSubDomain, a, SortedExpOpWorkerIps),

    OneS3Subdomain = <<"s3.", OpSubDomain/binary>>,
    SortedExpOneS3Ips = lists:sort(ExpOneS3Ips),
    dns_test_utils:assert_dns_answer(DnsServerIps, OneS3Subdomain, a, SortedExpOneS3Ips).
