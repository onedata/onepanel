%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Shared helpers for service create middleware handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(service_middleware_handler_utils).
-author("Bartosz Walkowicz").

-include("middleware/middleware.hrl").

-export([
    extract_new_hosts/1,
    validate_hosts_not_existing/2,
    parse_service_name/1,
    is_service_on_cluster_supported/1,
    ensure_has_host/2,
    supported_interfaces_for_service/1
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec extract_new_hosts(map()) -> [service:host()].
extract_new_hosts(Data) ->
    onepanel_utils:get_converted(hosts, Data, {seq, list}).


-spec validate_hosts_not_existing(service:name(), [service:host()]) -> ok | no_return().
validate_hosts_not_existing(Service, Hosts) ->
    ExistingHosts = service:get_hosts(Service),
    case lists:any(fun(H) -> lists:member(H, ExistingHosts) end, Hosts) of
        true -> throw(?ERROR_ALREADY_EXISTS);
        false -> ok
    end.


-spec ensure_has_host(service:name(), service:host()) -> ok | no_return().
ensure_has_host(Service, Host) ->
    case service:has_host(Service, Host) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end.


supported_interfaces_for_service(ServiceBin) ->
    case parse_service_name(ServiceBin) of
        {ok, Service} ->
            case is_service_on_cluster_supported(Service) of
                true -> {true, [rest]};
                false -> false
            end;
        error ->
            false
    end.


-spec parse_service_name(binary()) -> {ok, service:name()} | error.
parse_service_name(<<"cluster_manager">>) -> ?SERVICE_CM;
parse_service_name(<<"couchbase">>) -> ?SERVICE_CB;
parse_service_name(<<"ones3">>) -> ?SERVICE_ONES3;
parse_service_name(<<"op_worker">>) -> ?SERVICE_OPW;
parse_service_name(<<"oz_worker">>) -> ?SERVICE_OZW;
parse_service_name(_) -> error.


-spec is_service_on_cluster_supported(service:name()) -> boolean().
is_service_on_cluster_supported(Service) ->
    case onepanel_env:get_cluster_type() of
        ?ONEPROVIDER -> lists:member(Service, [?SERVICE_CB, ?SERVICE_CM, ?SERVICE_ONES3, ?SERVICE_OPW]);
        ?ONEZONE -> lists:member(Service, [?SERVICE_CB, ?SERVICE_CM, ?SERVICE_OZW])
    end.
