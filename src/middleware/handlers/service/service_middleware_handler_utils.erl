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
    ensure_has_host/2,

    set_started_all/2,
    set_started_on_host/3,
    status_on_host/2,
    status_all_hosts/1
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


-spec set_started_all(service:name(), boolean()) -> ok | no_return().
set_started_all(Service, Started) ->
    Action = case Started of true -> start; false -> stop end,
    middleware_handler_utils:service_exec(Service, Action, #{}).


-spec set_started_on_host(service:name(), service:host(), boolean()) -> ok | no_return().
set_started_on_host(Service, Host, Started) ->
    Action = case Started of true -> start; false -> stop end,
    middleware_handler_utils:service_exec(Service, Action, #{hosts => [Host]}).


-spec status_on_host(service:name(), service:host()) -> {ok, atom()} | errors:error().
status_on_host(Service, Host) ->
    Module = service:get_module(Service),
    middleware_handler_utils:service_call(Service, status, #{hosts => [Host]}, Module, status).


-spec status_all_hosts(service:name()) -> {ok, map()} | errors:error().
status_all_hosts(Service) ->
    Module = service:get_module(Service),
    Results = service:apply_sync(Service, status, #{}),
    case service_utils:results_contain_error(Results) of
        {true, ?ERR_NO_SERVICE_NODES(_)} -> {ok, #{}};
        {true, Error} -> Error;
        false ->
            {HostsResults, []} = service_utils:select_service_step(Module, status, Results),
            Map = lists:foldl(fun({Node, NodeStatus}, Acc) ->
                Host = onepanel_utils:convert(hosts:from_node(Node), binary),
                Acc#{Host => NodeStatus}
            end, #{}, HostsResults),
            {ok, Map}
    end.
