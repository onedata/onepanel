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

-include_lib("ctool/include/logging.hrl").

-export([determine_ip/0, parse_ip4/1]).

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
    {ok, IP} = lists:foldl(fun(IpSupplier, PrevResult) ->
        case PrevResult of
            {ok, IP} -> PrevResult;
            _ -> catch IpSupplier()
        end
    end, undefined, [
        fun determine_ip_by_oz/0,
        fun determine_ip_by_external_service/0,
        fun determine_ip_by_shell/0,
        fun () -> {ok, {127,0,0,1}} end
    ]),
    IP.


%%--------------------------------------------------------------------
%% @doc Attempts to parse a string as an IPv4 address.
%% @end
%%--------------------------------------------------------------------
-spec parse_ip4(binary() | string()) ->
    {ok, inet:ip4_address()} | {error, einval}.
parse_ip4(Value) ->
    List = onepanel_utils:convert(Value, list),
    inet:parse_ipv4strict_address(List).


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
    case lists:member(service_op_worker:name(), service:list())
        andalso service_oneprovider:is_registered() of
        true ->
            {ok, IPBin} = oz_providers:check_ip_address(none),
            parse_ip4(IPBin);
        _ -> {error, not_registered}
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
%% Uses shell command `hostname` to determine current IP.
%% @end
%%--------------------------------------------------------------------
-spec determine_ip_by_shell() -> {ok, inet:ip4_address()} | {error, term()}.
determine_ip_by_shell() ->
    Result = onepanel_shell:check_output(["hostname", "-i"]),
    parse_ip4(Result).

