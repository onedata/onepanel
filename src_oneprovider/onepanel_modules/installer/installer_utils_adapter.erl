%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains utility installation functions for provider.
%% @end
%% ===================================================================
-module(installer_utils_adapter).

-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([get_workers/0, get_nagios_report/0]).
-export([check_ip_address/0, check_ip_addresses/0, finalize_installation/1]).

%% Defines how many times onepanel will try to verify software start
-define(FINALIZE_INSTALLATION_ATTEMPTS, 24).

%% Defines how long onepanel will wait before next attempt to verify software start
-define(NEXT_ATTEMPT_DELAY, 5000).

%% ====================================================================
%% API functions
%% ====================================================================

%% get_workers/0
%% ====================================================================
%% @doc Returns configured workers list.
%% @end
-spec get_workers() -> Result when
    Result :: [string()].
%% ====================================================================
get_workers() ->
    case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
        {ok, #?GLOBAL_CONFIG_RECORD{workers = Workers}} ->
            Workers;
        _ ->
            []
    end.


%% check_ip_address/0
%% ====================================================================
%% @doc Checks local host IP address that is visible for Global Registry
%% and saves it in database.
%% @end
-spec check_ip_address() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
check_ip_address() ->
    Host = onepanel_utils:get_host(node()),
    try
        {ok, IpAddress} = gr_providers:check_ip_address(provider),
        ok = dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{ip_address, IpAddress}]),
        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot check IP address: ~p", [Reason]),
            {error, Host}
    end.


%% check_ip_addresses/0
%% ====================================================================
%% @doc Checks IP addresses of all worker hosts.
%% @end
-spec check_ip_addresses() -> Result when
    Result :: ok| {error, Reason :: term()}.
%% ====================================================================
check_ip_addresses() ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{workers = Workers}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {_, HostsError} = onepanel_utils:apply_on_hosts(Workers, ?MODULE, check_ip_address, [], ?RPC_TIMEOUT),
        case HostsError of
            [] -> ok;
            _ -> {error, {hosts, HostsError}}
        end
    catch
        _:Reason ->
            ?error("Cannot check IP addresses: ~p", [Reason]),
            {error, Reason}
    end.


%% get_nagios_report/0
%% ====================================================================
%% @doc Returns results of software nagios health check
%% @end
-spec get_nagios_report() -> Result when
    Result :: {ok, Status, NodesReport :: {Name, Status}, WorkersReport :: [{Node, Name, Status}]} | {error, Reason :: term()},
    Node :: binary(),
    Name :: binary(),
    Status :: binary().
%% ====================================================================
get_nagios_report() ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{workers = [Worker | _]}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        URL = "https://" ++ Worker ++ "/nagios",
        Headers = [{"content-type", "application/json"}],
        {ok, "200", _ResponseHeaders, ResponseBody} = ibrowse:send_req(URL, Headers, get),
        {Xml, _} = xmerl_scan:string(ResponseBody),
        [Status] = [X#xmlAttribute.value || X <- Xml#xmlElement.attributes, X#xmlAttribute.name == status],

        GetDetails = fun
            (oneprovider_node, X) ->
                [NodeName] = [Y#xmlAttribute.value || Y <- X, Y#xmlAttribute.name == name],
                [NodeStatus] = [Y#xmlAttribute.value || Y <- X, Y#xmlAttribute.name == status],
                {list_to_binary(NodeName), list_to_binary(NodeStatus)};
            (worker, X) ->
                [WorkerName] = [Y#xmlAttribute.value || Y <- X, Y#xmlAttribute.name == name],
                [WorkerNode] = [Y#xmlAttribute.value || Y <- X, Y#xmlAttribute.name == node],
                [WorkerStatus] = [Y#xmlAttribute.value || Y <- X, Y#xmlAttribute.name == status],
                {list_to_binary(WorkerName), list_to_binary(WorkerNode), list_to_binary(WorkerStatus)}
        end,

        GetReport = fun(Name) ->
            [GetDetails(Name, X#xmlElement.attributes) || X <- Xml#xmlElement.content, X#xmlElement.name == Name]
        end,

        NodesReport = GetReport(oneprovider_node),
        WorkersReport = GetReport(worker),
        {ok, list_to_binary(Status), NodesReport, WorkersReport}
    catch
        _:Reason ->
            ?error("Cannot get nagios details: ~p", [Reason]),
            {error, Reason}
    end.


%% finalize_installation/1
%% ====================================================================
%% @doc Waits until cluster control panel nodes are up and running.
%% Returns an error after ?FINALIZE_INSTALLATION_ATTEMPTS limit.
%% @end
-spec finalize_installation(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
finalize_installation(_Args) ->
    try
        ok = installer_utils:set_timestamp(),
        ok = finalize_installation_loop(?FINALIZE_INSTALLATION_ATTEMPTS),
        ok
    catch
        _:Reason ->
            ?error("Cannot finalize installation: ~p", [Reason]),
            {error, Reason}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% finalize_installation/2
%% ====================================================================
%% @doc Waits until cluster control panel nodes are up and running.
%% Returns an error after ?FINALIZE_INSTALLATION_ATTEMPTS limit.
%% Should not be used directly, use finalize_installation/1 instead.
%% @end
-spec finalize_installation_loop(Attempts :: integer()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
finalize_installation_loop(0) ->
    ?error("Cannot finalize installation: attempts limit exceeded."),
    {error, <<"Attempts limit exceeded.">>};

finalize_installation_loop(Attempts) ->
    case get_nagios_report() of
        {ok, <<"ok">>, _, _} ->
            ok;
        _ ->
            timer:sleep(?NEXT_ATTEMPT_DELAY),
            finalize_installation_loop(Attempts - 1)
    end.