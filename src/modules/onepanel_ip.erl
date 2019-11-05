%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles parsing and detecing IP address.
%%% @end
%%%-------------------------------------------------------------------
-module(onepanel_ip).
-author("Wojciech Geisler").

-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([determine_ip/0, ip4_to_binary/1, parse_ip4/1, is_ip/1, hostname_ips/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Attempts to determine IP of current node.
%% @end
%%--------------------------------------------------------------------
-spec determine_ip() -> inet:ip4_address().
determine_ip() ->
    % use first working method of getting IP
    onepanel_lists:foldl_while(fun(IpSupplier, PrevResult) ->
        try
            {ok, IP} = IpSupplier(),
            {halt, IP}
        catch  _:_ -> {cont, PrevResult} end
    end, undefined, [
        fun determine_ip_by_oz/0,
        fun determine_ip_by_domain/0,
        fun determine_ip_by_external_service/0,
        fun determine_ip_by_shell/0,
        fun() -> {ok, {127, 0, 0, 1}} end
    ]).


%%--------------------------------------------------------------------
%% @doc Attempts to parse given argument as IP string or tuple.
%% @end
%%--------------------------------------------------------------------
-spec parse_ip4(inet:ip4_address() | binary() | string()) ->
    {ok, inet:ip4_address()} | {error, einval} | no_return().
parse_ip4({_, _, _, _} = IP) ->
    {ok, IP};
parse_ip4(Value) ->
    List = onepanel_utils:convert(Value, list),
    inet:parse_ipv4strict_address(List).


%%--------------------------------------------------------------------
%% @doc Converts IP tuple to binary.
%% @end
%%--------------------------------------------------------------------
-spec ip4_to_binary(inet:ip4_address()) -> binary().
ip4_to_binary(IPTuple) ->
    list_to_binary(inet:ntoa(IPTuple)).


%%--------------------------------------------------------------------
%% @doc Detects IP address in a string, binary or tuple form.
%% @end
%%--------------------------------------------------------------------
-spec is_ip(term()) -> boolean().
is_ip(Value) ->
    try parse_ip4(Value) of
        {ok, _} -> true;
        _ -> false
    catch _:_ -> false
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns list of IPv4 addresses returned by shell command 'hostname i'.
%% @end
%%--------------------------------------------------------------------
-spec hostname_ips() -> [inet:ip4_address()].
hostname_ips() ->
    {_, Result, _} = onepanel_shell:execute(["hostname", "-i"]),
    Words = string:split(Result, " ", all),
    % filter out IPv6 addresses
    lists:filtermap(fun(Word) ->
        case parse_ip4(Word) of
            {ok, IP} -> {true, IP};
            _ -> false
        end
    end, Words).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to use Onezone endpoint to determine the public IP of this node.
%% Will work only in onepanel of registered oneprovider.
%% @end
%%--------------------------------------------------------------------
-spec determine_ip_by_oz() -> {ok, inet:ip4_address()} | {error, term()}.
determine_ip_by_oz() ->
    case service:exists(?SERVICE_OPW)
        andalso service_oneprovider:is_registered(#{}) of
        true ->
            {ok, IPBin} = oz_providers:check_ip_address(none),
            parse_ip4(IPBin);
        _ -> {error, not_registered}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to resolve cluster's domain.
%% If the query returns single IP address, it is used as the determined IP.
%% If more than one value is returned, attempts to match it to
%% hostname -i in order to differentiate between nodes.
%% If none match, the first IP returned by DNS is used.
%% @end
%%--------------------------------------------------------------------
-spec determine_ip_by_domain() -> {ok, inet:ip4_address()} | {error, term()}.
determine_ip_by_domain() ->
    Domain = case onepanel_env:get_cluster_type() of
        oneprovider -> service_op_worker:get_domain();
        onezone -> service_oz_worker:get_domain()
    end,
    case inet_res:lookup(binary_to_list(Domain), in, a) of
        [] -> {error, not_found};
        [{_, _, _, _} = IP] -> {ok, IP};
        ManyIPs ->
            HostnameIPs = hostname_ips(),
            case onepanel_lists:intersect(HostnameIPs, ManyIPs) of
                [] -> {ok, hd(ManyIPs)};
                [IP | _] -> {ok, IP}
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to use external service to determine the public IP of current node.
%% @end
%%--------------------------------------------------------------------
-spec determine_ip_by_external_service() ->
    {ok, inet:ip4_address()} | {error, term()}.
determine_ip_by_external_service() ->
    URL = onepanel_env:get(ip_check_url),
    case http_client:get(URL) of
        {ok, _, _, Body} ->
            Trimmed = onepanel_utils:trim(Body, both),
            parse_ip4(Trimmed);
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Uses shell command "hostname" to determine current IP.
%% @end
%%--------------------------------------------------------------------
-spec determine_ip_by_shell() -> {ok, inet:ip4_address()} | {error, term()}.
determine_ip_by_shell() ->
    case hostname_ips() of
        [Head | _] -> {ok, Head};
        [] -> {error, no_address}
    end.
