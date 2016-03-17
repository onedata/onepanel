%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains utility installation functions.
%% @end
%% ===================================================================
-module(installer_utils).

-include("registered_names.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([set_system_limit/2, get_system_limits_cmd/1]).
-export([get_global_config/0, get_timestamp/0, set_timestamp/0]).
-export([overwrite_config_args/4]).
-export([check_port/1, check_port/2, check_ports/2, check_host_domain_name/1]).
-export([get_workers/0, get_nagios_report/0]).
-export([check_ip_address/0, check_ip_addresses/0, finalize_installation/1]).

%% Defines how many times onepanel will try to verify software start
-define(FINALIZE_INSTALLATION_ATTEMPTS, 24).

%% Defines how long onepanel will wait before next attempt to verify software start
-define(NEXT_ATTEMPT_DELAY, 5000).

%% ====================================================================
%% API functions
%% ====================================================================

%% set_system_limit/2
%% ====================================================================
%% @doc Sets system limit on local host.
%% @end
-spec set_system_limit(Type :: atom(), Value :: integer()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
set_system_limit(Type, Value) ->
    try
        Host = onepanel_utils:get_host(node()),
        ok = dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{Type, Value}])
    catch
        _:Reason ->
            ?error_stacktrace("Cannot set ulimits: ~p", [Reason]),
            {error, Reason}
    end.


%% get_system_limits_cmd/0
%% ====================================================================
%% @doc Returns ulimits command required during database or oneprovider
%% node installation.
%% @end
-spec get_system_limits_cmd(Host :: string()) -> Result when
    Result :: string().
%% ====================================================================
get_system_limits_cmd(Host) ->
    try
        {ok, #?LOCAL_CONFIG_RECORD{open_files = OpenFiles, process_limit = Processes}} = dao:get_record(?LOCAL_CONFIG_TABLE, Host),
        "ulimit -n " ++ integer_to_list(OpenFiles) ++ " ; ulimit -u " ++ integer_to_list(Processes)
    catch
        _:Reason ->
            ?error_stacktrace("Cannot get ulimits configuration: ~p. Returning default values.", [Reason]),
            "ulimit -n " ++ integer_to_list(?OPEN_FILES) ++ " ; ulimit -u " ++ integer_to_list(?PROCESSES)
    end.


%% overwrite_config_args/4
%% ====================================================================
%% @doc Overwrites a parameter in config.args.
%% @end
-spec overwrite_config_args(Path :: string(), Parameter :: binary(), Pattern :: binary(), NewValue :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
overwrite_config_args(Path, Parameter, Pattern, NewValue) ->
    try
        {ok, FileContent} = file:read_file(Path),
        {match, [{From, Through}]} = re:run(FileContent, <<Parameter/binary, Pattern/binary>>),
        Beginning = binary:part(FileContent, 0, From),
        End = binary:part(FileContent, From + Through, size(FileContent) - From - Through),
        file:write_file(Path, <<Beginning/binary, Parameter/binary, NewValue/binary, End/binary>>),
        ok
    catch
        _:Reason ->
            ?error_stacktrace("Cannot overwrite config args: ~p", [Reason]),
            {error, Reason}
    end.


%% get_global_config/0
%% ====================================================================
%% @doc Returns global installation configuration.
%% @end
-spec get_global_config() -> Result when
    Result :: list().
%% ====================================================================
get_global_config() ->
    case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
        {ok, Record} ->
            Fields = record_info(fields, ?GLOBAL_CONFIG_RECORD),
            [_ | Values] = tuple_to_list(Record),
            lists:zip(Fields, Values);
        _ -> []
    end.


%% get_timestamp/0
%% ====================================================================
%% @doc Returns timestamp (microseconds since epoch) of last global 
%% configuration change.
%% @end
-spec get_timestamp() -> Result when
    Result :: integer().
%% ====================================================================
get_timestamp() ->
    case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
        #?GLOBAL_CONFIG_RECORD{timestamp = Timestamp} ->
            Timestamp;
        _ ->
            0
    end.


%% set_timestamp/0
%% ====================================================================
%% @doc Sets timestamp for global configuration to microsecond since 
%% epoch.
%% @end
-spec set_timestamp() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
set_timestamp() ->
    Timestamp = erlang:system_time(micro_seconds),
    dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{timestamp, Timestamp}]).


%% check_port/0
%% ====================================================================
%% @doc Check wheter given port if free on local host.
%% @end
-spec check_port(Port :: integer()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
check_port(Port) ->
    case gen_tcp:listen(Port, [{reuseaddr, true}]) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


%% check_port/2
%% ====================================================================
%% @doc Check wheter given port if free on host.
%% @end
-spec check_port(Host :: string(), Port :: integer()) -> Result when
    Result :: ok | error.
%% ====================================================================
check_port(Host, Port) ->
    case rpc:call(onepanel_utils:get_node(Host), ?MODULE, check_port, [Port]) of
        ok ->
            ok;
        {error, Reason} ->
            ?error("Port ~p on host ~p is in use: ~p", [Port, Host, Reason]),
            error
    end.


%% check_ports/2
%% ====================================================================
%% @doc Check whether ports on given hosts are free. Returns lists of
%% pairs: host and status of all ports on this host and overall status,
%% that is 'ok' if all ports are free on all hosts or 'error' otherwise.
%% @end
-spec check_ports(Hosts :: [string()], Ports :: [integer()]) -> Result when
    Result :: {[{Host :: string(), [PortStatus :: ok | error]}], Status :: ok | error}.
%% ====================================================================
check_ports(Hosts, Ports) ->
    lists:mapfoldl(fun(Host, Status) ->
        {HostPorts, HostStatus} = lists:mapfoldl(fun(Port, PortStatus) ->
            case check_port(Host, Port) of
                ok -> {ok, PortStatus};
                _ -> {error, error}
            end
        end, Status, Ports),
        {{Host, HostPorts}, HostStatus}
    end, ok, Hosts).


%% check_host_domain_name/1
%% ====================================================================
%% @doc Check whether domain name for host is fully qualified. For
%% host with fully qualified domain name returns 'ok', otherwise 'error'.
%% @end
-spec check_host_domain_name(Hosts :: [string()]) -> Result when
    Result :: {[IsFullyQualified :: ok | error], Status :: ok | error}.
%% ====================================================================
check_host_domain_name(Host) ->
    case string:str(Host, ".") > 0 of
        true -> ok;
        _ ->
            ?error("Host ~p does not have fully qualified domain name.", [Host]),
            error
    end.

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
        {ok, IpAddress} = oz_providers:check_ip_address(provider),
        ok = dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{ip_address, IpAddress}]),
        {ok, Host}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot check IP address: ~p", [Reason]),
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
            ?error_stacktrace("Cannot check IP addresses: ~p", [Reason]),
            {error, Reason}
    end.


%% get_nagios_report/0
%% ====================================================================
%% @doc Returns results of software nagios health check
%% @end
-spec get_nagios_report() -> Result when
    Result :: {ok, Status :: binary()} | {error, Reason :: term()}.
%% ====================================================================
get_nagios_report() ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{workers = [Worker | _]}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, Protocol} = application:get_env(?APP_NAME, application_nagios_protocol),
        {ok, Port} = application:get_env(?APP_NAME, application_nagios_port),
        URL = Protocol ++ "://" ++ Worker ++ ":" ++ integer_to_list(Port) ++ "/nagios",
        Headers = [{<<"content-type">>, <<"application/json">>}],
        {ok, 200, _ResponseHeaders, ResponseBody} = http_client:get(URL, Headers),

        {Xml, _} = xmerl_scan:string(binary_to_list(ResponseBody)),
        [Status] = [X#xmlAttribute.value || X <- Xml#xmlElement.attributes, X#xmlAttribute.name == status],

        {ok, list_to_binary(Status)}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot get nagios details: ~p", [Reason]),
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
            ?error_stacktrace("Cannot finalize installation: ~p", [Reason]),
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
        {ok, <<"ok">>} ->
            ok;
        _ ->
            timer:sleep(?NEXT_ATTEMPT_DELAY),
            finalize_installation_loop(Attempts - 1)
    end.