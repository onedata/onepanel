%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements {@link installer_behaviour} callbacks and
%% provides API methods for CCM nodes installation.
%% @end
%% ===================================================================
-module(installer_ccm).
-behaviour(installer_behaviour).

-include("registered_names.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% install_behaviour callbacks
-export([install/1, uninstall/1, start/1, stop/1, restart/1]).

%% API
-export([local_start/5, local_stop/0, local_restart/0]).

%% ====================================================================
%% Behaviour callback functions
%% ====================================================================

%% install/1
%% ====================================================================
%% @doc Installs CCM nodes on given hosts. Arguments list should contain
%% list of hosts where to install CCM nodes.
%% @end
-spec install(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(_) ->
    ok.

%% uninstall/1
%% ====================================================================
%% @doc Uninstalls CCM nodes on given hosts. Arguments list should 
%% contain list of hosts where CCM nodes where installed.
%% @end
-spec uninstall(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
uninstall(_) ->
    ok.

%% start/1
%% ====================================================================
%% @doc Starts CCM nodes on given hosts. Arguments list should contain
%% list of hosts where CCM nodes where installed with main CCM node
%% pointed out.
%% @end
-spec start(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
start(Args) ->
    try
        CCMs = case proplists:get_value(ccms, Args, []) of
                   [] -> throw(nothing_to_start);
                   Hosts -> Hosts
               end,

        MainCCM = case proplists:get_value(main_ccm, Args) of
                      undefined ->
                          throw("Main CCM node not found in arguments list.");
                      Host -> Host
                  end,

        OptCCMs = case lists:member(MainCCM, CCMs) of
                      true -> lists:delete(MainCCM, CCMs);
                      _ -> throw("Main CCM node not found among CCM nodes.")
                  end,

        ConfiguredDbs = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                            {ok, #?GLOBAL_CONFIG_RECORD{dbs = []}} ->
                                throw("Database nodes not configured.");
                            {ok, #?GLOBAL_CONFIG_RECORD{ccms = [], dbs = Dbs}} ->
                                Dbs;
                            {ok, #?GLOBAL_CONFIG_RECORD{ccms = _}} ->
                                throw("CCM nodes already configured.");
                            _ -> throw("Cannot get CCM nodes configuration.")
                        end,

        Workers = proplists:get_value(workers, Args, []),
        StoragePaths = proplists:get_value(storage_paths, Args, []),

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(CCMs, ?MODULE, local_start,
            [MainCCM, OptCCMs, Workers, ConfiguredDbs, StoragePaths], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{main_ccm, MainCCM}, {ccms, CCMs}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update CCM nodes configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts(CCMs, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                        {error, {hosts, CCMs}}
                end;
            _ ->
                ?error("Cannot start CCM nodes on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:nothing_to_start -> ok;
        _:Reason ->
            ?error("Cannot start CCM nodes: ~p", [Reason]),
            {error, Reason}
    end.

%% stop/1
%% ====================================================================
%% @doc Stops all CCM nodes.
%% @end
-spec stop(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
stop(_) ->
    try
        {ConfiguredMainCCM, ConfiguredCCMs, ConfiguredWorkers,
            ConfiguredDbs, ConfiguredStoragePaths} =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{ccms = []}} ->
                    throw("CCM nodes not configured.");
                {ok, #?GLOBAL_CONFIG_RECORD{
                    main_ccm = MainCCM, ccms = CCMs, dbs = Dbs,
                    workers = Workers, storage_paths = StoragePaths}
                } ->
                    {MainCCM, CCMs, Workers, Dbs, StoragePaths};
                _ -> throw("Cannot get CCM nodes configuration.")
            end,

        ConfiguredOptCCMs = lists:delete(ConfiguredMainCCM, ConfiguredCCMs),

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(ConfiguredCCMs, ?MODULE, local_stop, [], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{main_ccm, undefined}, {ccms, []}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update CCM nodes configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts(ConfiguredCCMs, ?MODULE, local_start,
                            [ConfiguredMainCCM, ConfiguredOptCCMs, ConfiguredWorkers,
                                ConfiguredDbs, ConfiguredStoragePaths], ?RPC_TIMEOUT),
                        {error, {hosts, ConfiguredCCMs}}
                end;
            _ ->
                ?error("Cannot stop CCM nodes on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_start,
                    [ConfiguredMainCCM, ConfiguredOptCCMs, ConfiguredWorkers,
                        ConfiguredDbs, ConfiguredStoragePaths], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:Reason ->
            ?error("Cannot stop CCM nodes: ~p", [Reason]),
            {error, Reason}
    end.

%% restart/1
%% ====================================================================
%% @doc Restarts all CCM nodes.
%% @end
-spec restart(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
restart(_) ->
    try
        {ConfiguredMainCCM, ConfiguredCCMs} =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{ccms = []}} ->
                    throw("CCM nodes not configured.");
                {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCM, ccms = CCMs}} ->
                    {MainCCM, CCMs};
                _ -> throw("Cannot get CCM nodes configuration.")
            end,

        ConfiguredOptCCMs = lists:delete(ConfiguredMainCCM, ConfiguredCCMs),

        case stop([]) of
            ok ->
                start([{main_ccm, ConfiguredMainCCM}, {ccms, ConfiguredOptCCMs}]);
            Other -> Other
        end
    catch
        _:Reason ->
            ?error("Cannot restart CCM nodes: ~p", [Reason]),
            {error, Reason}
    end.

%% ====================================================================
%% API functions
%% ====================================================================

%% local_start/5
%% ====================================================================
%% @doc Starts CCM node on local host.
%% @end
-spec local_start(MainCCM :: string(), OptCCMs :: [string()],
    Workers :: [string()], Dbs :: [string()], StoragePaths :: [string()]) ->
    {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_start(MainCCM, OptCCMs, Workers, Dbs, StoragePaths) ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Starting CCM node: ~p"),

        release_configurator:configure_release(
            ?CCM_APP_NAME,
            default,
            [
                {ccm_nodes, [list_to_atom(?CCM_NAME ++ "@" ++ CCM) || CCM <- [MainCCM | OptCCMs]]},
                {db_nodes, [list_to_atom(Db ++ ":" ++ integer_to_list(?DB_PORT)) || Db <- Dbs]},
                {worker_num, length(Workers)},
                {storage_paths, StoragePaths}
            ],
            [
                {name, ?CCM_NAME ++ "@" ++ Host},
                {setcookie, ?COOKIE}
            ]
        ),

        ServiceStart = "/etc/init.d/ " ++ atom_to_list(?CCM_APP_NAME) ++ " start > /dev/null",
        SetUlimitsCmd = installer_utils:get_system_limits_cmd(Host),
        "" = os:cmd("bash -c \"" ++ SetUlimitsCmd ++ " ; " ++ ServiceStart ++ "\""),

        {ok, Host}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot start CCM node: ~p", [Reason]),
            {error, Host}
    end.

%% local_stop/0
%% ====================================================================
%% @doc Stops CCM node on local host.
%% @end
-spec local_stop() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_stop() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Stopping CCM node"),

        ServiceStop = "service " ++ atom_to_list(?CCM_APP_NAME) ++ " stop > /dev/null",
        "" = os:cmd(ServiceStop),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot stop CCM node: ~p", [Reason]),
            {error, Host}
    end.

%% local_restart/0
%% ====================================================================
%% @doc Restarts CCM node on local host.
%% @end
-spec local_restart() -> Result when
    Result :: {ok, Host :: string()} | {error, Reason :: term()}.
%% ====================================================================
local_restart() ->
    Host = onepanel_utils:get_host(node()),
    try
        case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
            {ok, #?GLOBAL_CONFIG_RECORD{ccms = []}} ->
                throw("CCM nodes not configured.");
            {ok, _} -> ok;
            _ -> throw("Cannot get CCM nodes configuration.")
        end,

        ServiceRestart = "service " ++ atom_to_list(?CCM_APP_NAME) ++ " restart > /dev/null",
        "" = os:cmd(ServiceRestart),
        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot restart CCM node: ~p", [Reason]),
            {error, Host}
    end.