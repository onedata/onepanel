%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements {@link installer_behaviour} callbacks and
%% provides API methods for worker nodes installation.
%% @end
%% ===================================================================
-module(installer_worker).
-behaviour(installer_behaviour).

-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% install_behaviour callbacks
-export([install/1, uninstall/1, start/1, stop/1, restart/1]).

%% API
-export([local_install/0, local_uninstall/0, local_start/4, local_stop/1, local_restart/0]).

%% ====================================================================
%% Behaviour callback functions
%% ====================================================================

%% install/1
%% ====================================================================
%% @doc Installs worker nodes on given hosts.
%% @end
-spec install(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(Args) ->
    Workers = proplists:get_value(workers, Args, []),

    {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(Workers, ?MODULE, local_install, [], ?RPC_TIMEOUT),

    case HostsError of
        [] -> ok;
        _ ->
            ?error("Cannot install worker nodes on following hosts: ~p", [HostsError]),
            onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_uninstall, [], ?RPC_TIMEOUT),
            {error, {hosts, HostsError}}
    end.


%% uninstall/1
%% ====================================================================
%% @doc Uninstalls worker nodes on given hosts.
%% @end
-spec uninstall(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
uninstall(Args) ->
    Workers = proplists:get_value(workers, Args, []),

    {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(Workers, ?MODULE, local_uninstall, [], ?RPC_TIMEOUT),

    case HostsError of
        [] -> ok;
        _ ->
            ?error("Cannot uninstall worker nodes on following hosts: ~p", [HostsError]),
            onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_install, [], ?RPC_TIMEOUT),
            {error, {hosts, HostsError}}
    end.


%% start/1
%% ====================================================================
%% @doc Starts worker nodes on given hosts. Argument list should contain
%% host where main CCM node was configured and also list of hosts where
%% database configured and worker nodes installed.
%% @end
-spec start(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
start(Args) ->
    try
        NewWorkers = case proplists:get_value(workers, Args, []) of
                         [] -> throw(nothing_to_start);
                         Hosts -> Hosts
                     end,

        {ConfiguredMainCCM, ConfiguredCCMs, ConfiguredDbs, ConfiguredWorkers, ConfiguredStoragePaths} =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{ccms = []}} -> throw("CCM nodes not configured.");
                {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCM, ccms = CCMs, dbs = Dbs, workers = Workers, storage_paths = StoragePaths}} ->
                    {MainCCM, CCMs, Dbs, Workers, StoragePaths};
                _ -> throw("Cannot get CCM nodes configuration.")
            end,

        lists:foreach(fun(Worker) ->
            case lists:member(Worker, ConfiguredWorkers) of
                true -> throw("Worker " ++ Worker ++ " already configured.");
                _ -> ok
            end
        end, NewWorkers),

        ConfiguredOptCCMs = lists:delete(ConfiguredMainCCM, ConfiguredCCMs),

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(NewWorkers, ?MODULE, local_start,
            [ConfiguredMainCCM, ConfiguredOptCCMs, ConfiguredDbs, ConfiguredStoragePaths], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{workers, ConfiguredWorkers ++ NewWorkers}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update worker nodes configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts(NewWorkers, ?MODULE, local_stop, [ConfiguredStoragePaths], ?RPC_TIMEOUT),
                        {error, {hosts, NewWorkers}}
                end;
            _ ->
                ?error("Cannot start worker nodes on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_stop, [ConfiguredStoragePaths], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:nothing_to_start -> ok;
        _:Reason ->
            ?error("Cannot start worker nodes: ~p", [Reason]),
            {error, Reason}
    end.


%% stop/1
%% ====================================================================
%% @doc Stops worker nodes on given hosts.
%% @end
-spec stop(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
stop(Args) ->
    try
        {ConfiguredMainCCM, ConfiguredCCMs, ConfiguredDbs, ConfiguredWorkers, ConfiguredStoragePaths} =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCM, ccms = CCMs, dbs = Dbs, workers = Workers, storage_paths = StoragePaths}} ->
                    {MainCCM, CCMs, Dbs, Workers, StoragePaths};
                _ -> throw("Cannot get CCM nodes configuration.")
            end,

        WorkersToStop = case proplists:get_value(workers, Args) of
                            undefined -> ConfiguredWorkers;
                            Hosts ->
                                lists:foreach(fun(Host) ->
                                    case lists:member(Host, ConfiguredWorkers) of
                                        false -> throw("Worker " ++ Host ++ " is not configured.");
                                        _ -> ok
                                    end
                                end, Hosts),
                                Hosts
                        end,

        ConfiguredOptCCMs = lists:delete(ConfiguredMainCCM, ConfiguredCCMs),

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(WorkersToStop, ?MODULE, local_stop, [ConfiguredStoragePaths], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{workers, ConfiguredWorkers -- WorkersToStop}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update worker nodes configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts(WorkersToStop, ?MODULE, local_start,
                            [ConfiguredMainCCM, ConfiguredOptCCMs, ConfiguredDbs, ConfiguredStoragePaths], ?RPC_TIMEOUT),
                        {error, {hosts, WorkersToStop}}
                end;
            _ ->
                ?error("Cannot stop worker nodes on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_start,
                    [ConfiguredMainCCM, ConfiguredOptCCMs, ConfiguredDbs, ConfiguredStoragePaths], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:Reason ->
            ?error("Cannot stop worker nodes: ~p", [Reason]),
            {error, Reason}
    end.


%% restart/1
%% ====================================================================
%% @doc Restarts worker nodes on given hosts.
%% @end
-spec restart(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
restart(Args) ->
    try
        ConfiguredWorkers = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                                {ok, #?GLOBAL_CONFIG_RECORD{workers = Workers}} -> Workers;
                                _ -> throw("Cannot get CCM nodes configuration.")
                            end,

        WorkersToRestart = case proplists:get_value(workers, Args) of
                               undefined -> ConfiguredWorkers;
                               Hosts -> Hosts
                           end,

        case stop([{workers, WorkersToRestart}]) of
            ok -> start([{workers, WorkersToRestart}]);
            Other -> Other
        end
    catch
        _:Reason ->
            ?error("Cannot restart worker nodes: ~p", [Reason]),
            {error, Reason}
    end.


%% ====================================================================
%% API functions
%% ====================================================================

%% local_install/0
%% ====================================================================
%% @doc Installs worker node on local host.
%% @end
-spec local_install() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_install() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Installing worker node"),
        WorkerPath = filename:join([?NODES_INSTALL_PATH, ?WORKER_NAME]),

        "" = os:cmd("rm -rf " ++ WorkerPath),
        "" = os:cmd("mkdir -p " ++ WorkerPath),
        "" = os:cmd("cp -R " ++ filename:join([?ONEPROVIDER_RELEASE, "* "]) ++ WorkerPath),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot install worker node: ~p", [Reason]),
            {error, Host}
    end.


%% local_uninstall/0
%% ====================================================================
%% @doc Uninstalls worker node on local host.
%% @end
-spec local_uninstall() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_uninstall() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Uninstalling worker node"),
        WorkerPath = filename:join([?NODES_INSTALL_PATH, ?WORKER_NAME]),

        "" = os:cmd("rm -rf " ++ WorkerPath),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot uninstall worker node: ~p", [Reason]),
            {error, Host}
    end.


%% local_start/4
%% ====================================================================
%% @doc Starts worker node on local host.
%% @end
-spec local_start(MainCCM :: string(), OptCCMs :: [string()], Dbs :: [string()], StoragePaths :: [string()]) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_start(MainCCM, OptCCMs, Dbs, StoragePaths) ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Starting worker node: ~p"),

        Name = <<(list_to_binary(?WORKER_NAME))/binary, "@", (list_to_binary(Host))/binary>>,

        MainCCMName = <<(list_to_binary(?CCM_NAME))/binary, "@", (list_to_binary(MainCCM))/binary>>,

        OptCCMNames = lists:foldl(fun(OptCCM, Acc) ->
            <<Acc/binary, (list_to_binary(?CCM_NAME))/binary, "@", (list_to_binary(OptCCM))/binary, " ">>
        end, <<>>, OptCCMs),

        DbNames = lists:foldl(fun(Db, Acc) ->
            <<Acc/binary, (list_to_binary(?DB_NAME))/binary, "@", (list_to_binary(Db))/binary, " ">>
        end, <<>>, Dbs),


        NodeConfigPath = filename:join([?NODES_INSTALL_PATH, ?WORKER_NAME, ?CONFIG_ARGS_PATH]),
        StorageConfigPath = list_to_binary("\"" ++ filename:join([?NODES_INSTALL_PATH, ?WORKER_NAME, ?STORAGE_CONFIG_PATH]) ++ "\""),
        OverwriteCommand = filename:join([?NODES_INSTALL_PATH, ?WORKER_NAME, ?ONEPROVIDER_SCRIPT_PATH]),
        StartCommand = filename:join([?NODES_INSTALL_PATH, ?WORKER_NAME, ?START_COMMAND_SUFFIX]),

        ok = installer_utils:overwrite_config_args(NodeConfigPath, <<"name: ">>, <<"[^\n]*">>, Name),
        ok = installer_utils:overwrite_config_args(NodeConfigPath, <<"main_ccm: ">>, <<"[^\n]*">>, MainCCMName),
        ok = installer_utils:overwrite_config_args(NodeConfigPath, <<"opt_ccms: ">>, <<"[^\n]*">>, OptCCMNames),
        ok = installer_utils:overwrite_config_args(NodeConfigPath, <<"db_nodes: ">>, <<"[^\n]*">>, DbNames),
        ok = installer_utils:overwrite_config_args(NodeConfigPath, <<"storage_config_path: ">>, <<"[^\n]*">>, StorageConfigPath),
        ok = installer_utils:add_node_to_config(worker_node, list_to_atom(?WORKER_NAME), ?NODES_INSTALL_PATH),
        ok = installer_storage:add_storage_paths_on_host(StoragePaths),

        os:cmd(OverwriteCommand),
        SetUlimitsCmd = installer_utils:get_system_limits_cmd(Host),
        "" = os:cmd("bash -c \"" ++ SetUlimitsCmd ++ " ; " ++ StartCommand ++ "\""),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot start worker node: ~p", [Reason]),
            {error, Host}
    end.


%% local_stop/1
%% ====================================================================
%% @doc Stops worker node on local host.
%% @end
-spec local_stop(StoragePaths :: [string()]) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_stop(StoragePaths) ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Stopping worker node on host: ~p", [Host]),
        WorkerPath = filename:join([?NODES_INSTALL_PATH, ?WORKER_NAME]),

        "" = os:cmd("kill -TERM `ps aux | grep beam | grep " ++ WorkerPath ++ " | awk '{print $2}'`"),
        ok = installer_utils:remove_node_from_config(worker_node),
        ok = installer_storage:remove_storage_paths_on_host(StoragePaths),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot stop worker node: ~p", [Reason]),
            {error, Host}
    end.


%% local_restart/0
%% ====================================================================
%% @doc Restarts worker node on local host.
%% @end
-spec local_restart() -> Result when
    Result :: {ok, Host :: string()} | {error, Reason :: term()}.
%% ====================================================================
local_restart() ->
    Host = onepanel_utils:get_host(node()),
    case restart([{workers, [Host]}]) of
        ok -> {ok, Host};
        _ -> {error, Host}
    end.