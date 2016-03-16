%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements {@link installer_behaviour} callbacks and
%% provides API methods for CM nodes installation.
%% @end
%% ===================================================================
-module(installer_cm).
-behaviour(installer_behaviour).

-include("registered_names.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% install_behaviour callbacks
-export([install/1, uninstall/1, start/1, stop/1, restart/1]).

%% API
-export([local_start/3, local_stop/0, local_restart/0]).

%% ====================================================================
%% Behaviour callback functions
%% ====================================================================

%% install/1
%% ====================================================================
%% @doc Installs CM nodes on given hosts. Arguments list should contain
%% list of hosts where to install CM nodes.
%% @end
-spec install(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(_) ->
    ok.

%% uninstall/1
%% ====================================================================
%% @doc Uninstalls CM nodes on given hosts. Arguments list should
%% contain list of hosts where CM nodes where installed.
%% @end
-spec uninstall(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
uninstall(_) ->
    ok.

%% start/1
%% ====================================================================
%% @doc Starts CM nodes on given hosts. Arguments list should contain
%% list of hosts where CM nodes where installed with main CM node
%% pointed out.
%% @end
-spec start(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
start(Args) ->
    try
        CMs = case proplists:get_value(cms, Args, []) of
            [] -> throw(nothing_to_start);
            Hosts -> Hosts
        end,

        MainCM = case proplists:get_value(main_cm, Args) of
            undefined ->
                throw("Main CM node not found in arguments list.");
            Host -> Host
        end,

        OptCMs = case lists:member(MainCM, CMs) of
            true -> lists:delete(MainCM, CMs);
            _ -> throw("Main CM node not found among CM nodes.")
        end,

        Workers = proplists:get_value(workers, Args, []),

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(CMs, ?MODULE, local_start,
            [MainCM, OptCMs, Workers], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{main_cm, MainCM}, {cms, CMs}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update CM nodes configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts(CMs, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                        {error, {hosts, CMs}}
                end;
            _ ->
                ?error("Cannot start CM nodes on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:nothing_to_start -> ok;
        _:Reason ->
            ?error("Cannot start CM nodes: ~p", [Reason]),
            {error, Reason}
    end.

%% stop/1
%% ====================================================================
%% @doc Stops all CM nodes.
%% @end
-spec stop(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
stop(_) ->
    try
        {ConfiguredMainCM, ConfiguredCMs, ConfiguredWorkers} =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{cms = []}} ->
                    throw("CM nodes not configured.");
                {ok, #?GLOBAL_CONFIG_RECORD{
                    main_cm = MainCM, cms = CMs, workers = Workers}
                } ->
                    {MainCM, CMs, Workers};
                _ -> throw("Cannot get CM nodes configuration.")
            end,

        ConfiguredOptCMs = lists:delete(ConfiguredMainCM, ConfiguredCMs),

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(ConfiguredCMs, ?MODULE, local_stop, [], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{main_cm, undefined}, {cms, []}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update CM nodes configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts(ConfiguredCMs, ?MODULE, local_start,
                            [ConfiguredMainCM, ConfiguredOptCMs, ConfiguredWorkers], ?RPC_TIMEOUT),
                        {error, {hosts, ConfiguredCMs}}
                end;
            _ ->
                ?error("Cannot stop CM nodes on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_start,
                    [ConfiguredMainCM, ConfiguredOptCMs, ConfiguredWorkers], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:Reason ->
            ?error("Cannot stop CM nodes: ~p", [Reason]),
            {error, Reason}
    end.

%% restart/1
%% ====================================================================
%% @doc Restarts all CM nodes.
%% @end
-spec restart(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
restart(_) ->
    try
        {ConfiguredMainCM, ConfiguredCMs} =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{cms = []}} ->
                    throw("CM nodes not configured.");
                {ok, #?GLOBAL_CONFIG_RECORD{main_cm = MainCM, cms = CMs}} ->
                    {MainCM, CMs};
                _ -> throw("Cannot get CM nodes configuration.")
            end,

        ConfiguredOptCMs = lists:delete(ConfiguredMainCM, ConfiguredCMs),

        case stop([]) of
            ok ->
                start([{main_cm, ConfiguredMainCM}, {cms, ConfiguredOptCMs}]);
            Other -> Other
        end
    catch
        _:Reason ->
            ?error("Cannot restart CM nodes: ~p", [Reason]),
            {error, Reason}
    end.

%% ====================================================================
%% API functions
%% ====================================================================

%% local_start/5
%% ====================================================================
%% @doc Starts CM node on local host.
%% @end
-spec local_start(MainCM :: string(), OptCMs :: [string()],
    Workers :: [string()]) -> {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_start(MainCM, OptCMs, Workers) ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Starting CM node: ~p"),

        release_configurator:configure_release(
            ?CM_APP_NAME,
            default,
            [
                {?CM_APP_NAME, [
                    {cm_nodes, [list_to_atom(?CM_NAME ++ "@" ++ CM) || CM <- [MainCM | OptCMs]]},
                    {worker_num, length(Workers)}
                ]}
            ],
            [
                {name, ?CM_NAME ++ "@" ++ Host},
                {setcookie, atom_to_list(erlang:get_cookie())}
            ]
        ),

        ServiceStart = "service " ++ atom_to_list(?CM_APP_NAME) ++ " start 2>1 1>/dev/null",
        SetUlimitsCmd = installer_utils:get_system_limits_cmd(Host),
        "0" = os:cmd("bash -c \"" ++ SetUlimitsCmd ++ " ; " ++ ServiceStart ++ " ; echo -n $?\""),

        {ok, Host}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot start CM node: ~p", [Reason]),
            {error, Host}
    end.

%% local_stop/0
%% ====================================================================
%% @doc Stops CM node on local host.
%% @end
-spec local_stop() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_stop() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Stopping CM node"),

        ServiceStop = "service " ++ atom_to_list(?CM_APP_NAME) ++ " stop 2>1 1>/dev/null",
        "0" = os:cmd(ServiceStop ++ " ; echo -n $?"),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot stop CM node: ~p", [Reason]),
            {error, Host}
    end.

%% local_restart/0
%% ====================================================================
%% @doc Restarts CM node on local host.
%% @end
-spec local_restart() -> Result when
    Result :: {ok, Host :: string()} | {error, Reason :: term()}.
%% ====================================================================
local_restart() ->
    Host = onepanel_utils:get_host(node()),
    try
        case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
            {ok, #?GLOBAL_CONFIG_RECORD{cms = []}} ->
                throw("CM nodes not configured.");
            {ok, _} -> ok;
            _ -> throw("Cannot get CM nodes configuration.")
        end,

        ServiceRestart = "service " ++ atom_to_list(?CM_APP_NAME) ++ " restart 2>1 1>/dev/null",
        "0" = os:cmd(ServiceRestart ++ " ; echo -n $?"),
        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot restart CM node: ~p", [Reason]),
            {error, Host}
    end.