%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for ip tests.
%%% @end
%%%--------------------------------------------------------------------
-module(ip_test_utils).
-author("Bartosz Walkowicz").

-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("kernel/src/inet_dns.hrl").


%% API
-export([
    random_ip/0,

    encode_ips/1,
    encode_ip/1,

    get_zone_nodes_ips/0,
    get_provider_nodes_ips/1,
    get_node_ip/1,

    assert_cluster_ips/2,
    update_cluster_ips/2
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec random_ip() -> inet:ip_address().
random_ip() ->
    {?RAND_INT(1, 255), ?RAND_INT(1, 255), ?RAND_INT(1, 255), ?RAND_INT(1, 255)}.


-spec encode_ips([ip_utils:ip()]) -> [binary()].
encode_ips(Ips) ->
    lists:map(fun encode_ip/1, Ips).


-spec encode_ip(ip_utils:ip()) -> binary().
encode_ip(Ip) ->
    {ok, IpBin} = ip_utils:to_binary(Ip),
    IpBin.


-spec get_zone_nodes_ips() -> [inet:ip_address()].
get_zone_nodes_ips() ->
    lists:sort(lists:map(fun get_node_ip/1, oct_background:get_zone_panels())).


-spec get_provider_nodes_ips(oct_background:entity_selector()) -> [inet:ip_address()].
get_provider_nodes_ips(ProviderSelector) ->
    lists:sort(lists:map(fun get_node_ip/1, oct_background:get_provider_panels(ProviderSelector))).


-spec get_node_ip(node()) -> inet:ip_address().
get_node_ip(Node) ->
    panel_test_rpc:insecure_call(Node, fun() ->
        {ok, IpAddresses} = inet:getifaddrs(),
        hd([
            Addr || {_, Opts} <- IpAddresses, {addr, Addr} <- Opts,
            size(Addr) == 4, Addr =/= {127, 0, 0, 1}
        ])
    end).


-spec assert_cluster_ips(oct_background:entity_selector(), [inet:ip_address()]) ->
    ok.
assert_cluster_ips(EntitySelector, ExpIps) ->
    Path = get_cluster_ips_rest_path(EntitySelector),

    ExpClusterIps = #{
        <<"isConfigured">> => true,
        <<"hosts">> => build_host_ips_map(EntitySelector, ExpIps)
    },
    ?assertMatch(
        {ok, ?HTTP_200_OK, _, ExpClusterIps},
        panel_test_rest:get(EntitySelector, Path, #{auth => root})
    ),
    ok.


-spec update_cluster_ips(oct_background:entity_selector(), [inet:ip_address()]) ->
    ok.
update_cluster_ips(EntitySelector, NewIps) ->
    Path = get_cluster_ips_rest_path(EntitySelector),
    JsonData = #{<<"hosts">> => build_host_ips_map(EntitySelector, NewIps)},

    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(EntitySelector, Path, #{auth => root, json => JsonData})
    ),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
get_cluster_ips_rest_path(zone) -> <<"/zone/cluster_ips">>;
get_cluster_ips_rest_path(_) -> <<"/provider/cluster_ips">>.


%% @private
build_host_ips_map(EntitySelector, Ips) ->
    lists:foldl(fun({Host, Ip}, Acc) ->
        Acc#{Host => Ip}
    end, #{}, lists:zip(get_hosts(EntitySelector), encode_ips(lists:sort(Ips)))).


%% @private
get_hosts(EntitySelector) ->
    lists:map(
        fun(Node) -> str_utils:to_binary(?GET_HOSTNAME(Node)) end,
        lists:sort(get_nodes(EntitySelector))
    ).


%% @private
get_nodes(zone) -> oct_background:get_zone_nodes();
get_nodes(ProviderSelector) -> oct_background:get_provider_nodes(ProviderSelector).
