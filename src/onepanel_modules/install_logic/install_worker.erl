%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module implements {@link install_behaviour} callbacks and
%% provides API methods for worker nodes installation.
%% @end
%% ===================================================================
-module(install_worker).
-behaviour(install_behaviour).

-include("onepanel_modules/install_logic.hrl").
-include("onepanel_modules/db_logic.hrl").
-include_lib("ctool/include/logging.hrl").

%% install_behaviour callbacks
-export([install/1, uninstall/1, start/1, stop/1, restart/1]).

%% API
-export([install/0, uninstall/0, start/3, stop/0]).

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
    Hosts = proplists:get_value(hosts, Args, []),

    {HostsOk, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, install, [], ?RPC_TIMEOUT),

    case HostsError of
        [] -> ok;
        _ ->
            ?error("Cannot install worker nodes on following hosts: ~p", [HostsError]),
            install_utils:apply_on_hosts(HostsOk, ?MODULE, uninstall, [], ?RPC_TIMEOUT),
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
    Hosts = proplists:get_value(hosts, Args, []),

    {HostsOk, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),

    case HostsError of
        [] -> ok;
        _ ->
            ?error("Cannot uninstall worker nodes on following hosts: ~p", [HostsError]),
            install_utils:apply_on_hosts(HostsOk, ?MODULE, install, [], ?RPC_TIMEOUT),
            {error, {hosts, HostsError}}
    end.


%% start/1
%% ====================================================================
%% @doc Starts worker nodes on given hosts. Argument list should contain
%% host where main CCM node was installed and also list of hosts where
%% database and worker nodes where installed.
%% @end
-spec start(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
start(Args) ->
    try
        {MainCCM, OptCCMs, Dbs, Workers} =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = undefined}} ->
                    throw("CCM nodes not configured.");
                {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCMHost, opt_ccms = OptCCMHosts, dbs = DbHosts, workers = WorkerHosts}} ->
                    {MainCCMHost, OptCCMHosts, DbHosts, WorkerHosts};
                _ -> throw("Cannot get CCM nodes configuration.")
            end,

        NewWorkers = proplists:get_value(workers, Args, []),

        lists:foreach(fun(Worker) ->
            case lists:member(Worker, Workers) of
                true -> throw("Worker " ++ Worker ++ " already configured.");
                _ -> ok
            end
        end, NewWorkers),

        {HostsOk, HostsError} = install_utils:apply_on_hosts(NewWorkers, ?MODULE, start, [MainCCM, OptCCMs, Dbs], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{workers, Workers ++ NewWorkers}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update worker nodes configuration: ~p", [Other]),
                        install_utils:apply_on_hosts(NewWorkers, ?MODULE, stop, [], ?RPC_TIMEOUT),
                        {error, {hosts, NewWorkers}}
                end;
            _ ->
                ?error("Cannot start worker nodes on following hosts: ~p", [HostsError]),
                install_utils:apply_on_hosts(HostsOk, ?MODULE, stop, [], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
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
stop(_) ->
    try
        {MainCCM, OptCCMs, Dbs, Workers} =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{workers = []}} ->
                    throw("Worker nodes not configured.");
                {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCMHost, opt_ccms = OptCCMHosts, dbs = DbHosts, workers = WorkerHosts}} ->
                    {MainCCMHost, OptCCMHosts, DbHosts, WorkerHosts};
                _ -> throw("Cannot get worker nodes configuration.")
            end,

        {HostsOk, HostsError} = install_utils:apply_on_hosts(Workers, ?MODULE, stop, [], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{workers, []}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update worker nodes configuration: ~p", [Other]),
                        install_utils:apply_on_hosts(Workers, ?MODULE, start, [MainCCM, OptCCMs, Dbs], ?RPC_TIMEOUT),
                        {error, {hosts, Workers}}
                end;
            _ ->
                ?error("Cannot stop worker nodes on following hosts: ~p", [HostsError]),
                install_utils:apply_on_hosts(HostsOk, ?MODULE, start, [MainCCM, OptCCMs, Dbs], ?RPC_TIMEOUT),
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
restart(_) ->
    try
        {MainCCM, OptCCMs, Dbs, Workers} =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{workers = []}} ->
                    throw("Worker nodes not configured.");
                {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCMHost, opt_ccms = OptCCMHosts, dbs = DbHosts, workers = WorkerHosts}} ->
                    {MainCCMHost, OptCCMHosts, DbHosts, WorkerHosts};
                _ -> throw("Cannot get worker nodes configuration.")
            end,

        case stop([]) of
            ok -> start([{main_ccm, MainCCM}, {opt_ccms, OptCCMs}, {dbs, Dbs}, {workers, Workers}]);
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

%% install/0
%% ====================================================================
%% @doc Installs worker node on local host.
%% @end
-spec install() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
install() ->
    Host = install_utils:get_host(node()),
    try
        ?debug("Installing worker node."),

        "" = os:cmd("mkdir -p " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME),
        "" = os:cmd("cp -R " ++ ?VEIL_RELEASE ++ "/* " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot install worker node: ~p.", [Reason]),
            {error, Host}
    end.


%% uninstall/0
%% ====================================================================
%% @doc Uninstalls worker node on local host.
%% @end
-spec uninstall() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
uninstall() ->
    Host = install_utils:get_host(node()),
    try
        ?debug("Uninstalling worker node."),

        "" = os:cmd("rm -rf " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot uninstall worker node: ~p.", [Reason]),
            {error, Host}
    end.


%% start/3
%% ====================================================================
%% @doc Starts worker node on local host.
%% @end
-spec start(MainCCM :: string(), OptCCMs :: [string()], Dbs :: [string()]) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
start(MainCCM, OptCCMs, Dbs) ->
    Host = install_utils:get_host(node()),
    try
        ?debug("Starting worker node: ~p."),

        Name = ?DEFAULT_WORKER_NAME ++ "@" ++ Host,

        MainCCMName = ?DEFAULT_CCM_NAME ++ "@" ++ MainCCM,

        OptCCMNames = lists:foldl(fun(OptCCM, Acc) ->
            Acc ++ ?DEFAULT_CCM_NAME ++ "@" ++ OptCCM ++ " "
        end, [], OptCCMs),

        DbNames = lists:foldl(fun(Db, Acc) ->
            Acc ++ ?DEFAULT_DB_NAME ++ "@" ++ Db ++ " "
        end, [], Dbs),

        NodeConfigPath = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_WORKER_NAME, ?CONFIG_ARGS_PATH]),
        StorageConfigPath = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_WORKER_NAME, ?STORAGE_CONFIG_PATH]),
        OverwriteCommand = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_WORKER_NAME, ?VEIL_CLUSTER_SCRIPT_PATH]),
        StartCommand = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_WORKER_NAME, ?START_COMMAND_SUFFIX]),

        ok = install_utils:overwrite_config_args(NodeConfigPath, "name", Name),
        ok = install_utils:overwrite_config_args(NodeConfigPath, "main_ccm", MainCCMName),
        ok = install_utils:overwrite_config_args(NodeConfigPath, "opt_ccms", OptCCMNames),
        ok = install_utils:overwrite_config_args(NodeConfigPath, "db_nodes", DbNames),
        ok = install_utils:overwrite_config_args(NodeConfigPath, "storage_config_path", StorageConfigPath),
        ok = install_utils:add_node_to_config(worker, list_to_atom(?DEFAULT_WORKER_NAME), ?DEFAULT_NODES_INSTALL_PATH),

        os:cmd(OverwriteCommand),
        SetUlimitsCmd = install_utils:get_ulimits_cmd(Host),
        "" = os:cmd(SetUlimitsCmd ++ " ; " ++ StartCommand),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot start worker node: ~p", [Reason]),
            {error, Host}
    end.


%% stop/0
%% ====================================================================
%% @doc Stops worker node on local host.
%% @end
-spec stop() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
stop() ->
    Host = install_utils:get_host(node()),
    try
        ?debug("Stopping worker node on host: ~p.", [Host]),

        "" = os:cmd("kill -TERM `ps aux | grep beam | grep " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ " | awk '{print $2}'`"),
        ok = install_utils:remove_node_from_config(worker),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot stop worker node: ~p", [Reason]),
            {error, Host}
    end.