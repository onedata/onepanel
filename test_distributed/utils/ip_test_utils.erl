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

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("kernel/src/inet_dns.hrl").


%% API
-export([
    get_zone_nodes_ips/0,
    get_provider_nodes_ips/1,
    encode_ips/1
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec get_zone_nodes_ips() -> [inet:ip_address()].
get_zone_nodes_ips() ->
    lists:sort(lists:map(fun get_node_ip/1, oct_background:get_zone_panels())).


-spec get_provider_nodes_ips(oct_background:entity_selector()) -> [inet:ip_address()].
get_provider_nodes_ips(ProviderSelector) ->
    lists:sort(lists:map(fun get_node_ip/1, oct_background:get_provider_panels(ProviderSelector))).


-spec encode_ips([ip_utils:ip()]) -> [binary()].
encode_ips(Ips) ->
    lists:map(fun(Ip) ->
        {ok, IpBin} = ip_utils:to_binary(Ip),
        IpBin
    end, Ips).


%%%===================================================================
%%% Internal functions
%%%===================================================================


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
