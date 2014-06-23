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

% install_behaviour callbacks
-export([install/2, uninstall/2, start/2, stop/2, restart/2]).

% API
-export([install/3, uninstall/0, start/0, stop/0, restart/0]).

%% ====================================================================
%% Behaviour callback functions
%% ====================================================================

%% install/2
%% ====================================================================
%% @doc Installs worker nodes.
%% @end
-spec install(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Hosts :: [string()] | binary()},
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
install(Hosts, Args) ->
    try
        MainCCM = proplists:get_value(main_ccm, Args),
        OptCCMs = proplists:get_value(opt_ccms, Args, []),
        Dbs = proplists:get_value(dbs, Args),

        case MainCCM of
            undefined -> throw("Main CCM node not found in arguments list.");
            _ -> ok
        end,

        case Dbs of
            undefined -> throw("Database nodes not found in arguments list.");
            _ -> ok
        end,

        InstalledWorkers = case dao:get_record(?CONFIG_TABLE, ?CONFIG_ID) of
                               {ok, #?CONFIG_TABLE{workers = Workers}} -> Workers;
                               _ -> []
                           end,

        lists:foreach(fun(Host) ->
            case lists:member(Host, InstalledWorkers) of
                true -> throw("Host: " ++ Host ++ " is already installed.");
                _ -> ok
            end
        end, Hosts),

        {HostsOk, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, install, [MainCCM, OptCCMs, Dbs], ?RPC_TIMEOUT),

        case dao:update_record(?CONFIG_TABLE, ?CONFIG_ID, [{workers, InstalledWorkers ++ HostsOk}]) of
            ok ->
                case HostsError of
                    [] -> ok;
                    _ ->
                        lager:error("Cannot install worker nodes on following hosts: ~p", [HostsError]),
                        {error, HostsError}
                end;
            Other ->
                lager:error("Cannot update worker nodes configuration: ~p", Other),
                rpc:multicall(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),
                {error, Hosts}
        end
    catch
        _:Reason ->
            lager:error("Cannot install worker nodes: ~p", [Reason]),
            {error, Reason}
    end.


%% uninstall/2
%% ====================================================================
%% @doc Uninstalls worker nodes.
%% @end
-spec uninstall(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
uninstall(Hosts, _) ->
    try
        InstalledWorkers = case dao:get_record(?CONFIG_TABLE, ?CONFIG_ID) of
                               {ok, #?CONFIG_TABLE{workers = Workers}} -> Workers;
                               _ -> throw("Cannot get worker nodes configuration.")
                           end,

        lists:foreach(fun(Host) ->
            case lists:member(Host, InstalledWorkers) of
                true -> ok;
                _ -> throw("Host: " ++ Host ++ " is not installed.")
            end
        end, Hosts),

        {HostsOk, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),

        NewWorkers = lists:filter(fun(Worker) ->
            not lists:member(Worker, HostsOk)
        end, InstalledWorkers),

        case dao:update_record(?CONFIG_TABLE, ?CONFIG_ID, [{workers, NewWorkers}]) of
            ok ->
                case HostsError of
                    [] -> ok;
                    _ ->
                        lager:error("Cannot uninstall worker nodes on following hosts: ~p", [HostsError]),
                        {error, HostsError}
                end;
            Other ->
                lager:error("Cannot update worker nodes configuration: ~p", Other),
                {error, Hosts}
        end
    catch
        _:Reason ->
            lager:error("Cannot uninstall worker nodes: ~p", [Reason]),
            {error, Reason}
    end.


%% start/2
%% ====================================================================
%% @doc Starts worker nodes.
%% @end
-spec start(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
start(Hosts, _) ->
    {_, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, start, [], ?RPC_TIMEOUT),
    case HostsError of
        [] -> ok;
        _ ->
            lager:error("Cannot start worker nodes on following hosts: ~p", [HostsError]),
            {error, HostsError}
    end.


%% stop/2
%% ====================================================================
%% @doc Stops worker nodes.
%% @end
-spec stop(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
stop(Hosts, _) ->
    {_, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, stop, [], ?RPC_TIMEOUT),
    case HostsError of
        [] -> ok;
        _ ->
            lager:error("Cannot stop worker nodes on following hosts: ~p", [HostsError]),
            {error, HostsError}
    end.


%% restart/2
%% ====================================================================
%% @doc Restarts worker nodes.
%% @end
-spec restart(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
restart(Hosts, _) ->
    {_, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, restart, [], ?RPC_TIMEOUT),
    case HostsError of
        [] -> ok;
        _ ->
            lager:error("Cannot restart worker nodes on following hosts: ~p", [HostsError]),
            {error, HostsError}
    end.


%% ====================================================================
%% API functions
%% ====================================================================

%% install/3
%% ====================================================================
%% @doc Installs worker node. All function arguments are string form of
%% hostname, eg. "127.0.0.1".
%% @end
-spec install(MainCCM :: string(), OptCCMs :: [string()], Dbs :: [string()]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(MainCCM, OptCCMs, Dbs) ->
    Host = install_utils:get_host(node()),
    try
        LongName = ?DEFAULT_WORKER_NAME ++ "@" ++ Host,
        MainCCMLongName = ?DEFAULT_CCM_NAME ++ "@" ++ MainCCM,
        OptCCMsLongNames = lists:foldl(fun(OptCCM, Acc) ->
            Acc ++ ?DEFAULT_CCM_NAME ++ "@" ++ OptCCM ++ " " end, [], OptCCMs),
        DbsLongNames = lists:foldl(fun(Db, Acc) -> Acc ++ ?DEFAULT_DB_NAME ++ "@" ++ Db ++ " " end, [], Dbs),
        ConfigPath = ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/" ++ ?CONFIG_ARGS_PATH,
        StorageConfigPath = ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/" ++ ?STORAGE_CONFIG_PATH,

        lager:info("Installing worker node on host: ~p.", [Host]),

        "" = os:cmd("mkdir -p " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME),
        "" = os:cmd("cp -R " ++ ?VEIL_RELEASE ++ "/ * " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME),
        ok = install_utils:overwrite_config_args(ConfigPath, "name", LongName),
        ok = install_utils:overwrite_config_args(ConfigPath, "main_ccm", MainCCMLongName),
        ok = install_utils:overwrite_config_args(ConfigPath, "opt_ccms", OptCCMsLongNames),
        ok = install_utils:overwrite_config_args(ConfigPath, "db_nodes", DbsLongNames),
        ok = install_utils:overwrite_config_args(ConfigPath, "storage_config_path", StorageConfigPath),
        ok = install_utils:add_node_to_config(worker, list_to_atom(?DEFAULT_WORKER_NAME), ?DEFAULT_NODES_INSTALL_PATH),

        ok
    catch
        _:Reason ->
            lager:error("Cannot install worker node on host ~s: ~p.", [Host, Reason]),
            {error, Reason}
    end.


%% uninstall/0
%% ====================================================================
%% @doc Uninstalls worker node.
%% @end
-spec uninstall() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
uninstall() ->
    Host = install_utils:get_host(node()),
    try
        lager:info("Uninstalling worker node on host: ~p.", [Host]),

        ok = install_utils:remove_node_from_config(worker),
        "" = os:cmd("rm -rf " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME),

        ok
    catch
        _:Reason ->
            lager:error("Cannot uninstall worker node on host ~s: ~p.", [Host, Reason]),
            {error, Reason}
    end.


%% start/0
%% ====================================================================
%% @doc Starts worker node.
%% @end
-spec start() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
start() ->
    Host = install_utils:get_host(node()),
    try
        lager:info("Starting worker node on host: ~p.", [Host]),

        os:cmd(?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/" ++ ?VEIL_CLUSTER_SCRIPT_PATH),
        SetUlimitsCmd = install_utils:get_ulimits_cmd(),
        "" = os:cmd(SetUlimitsCmd ++ "; " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/" ++ ?START_COMMAND_SUFFIX),

        ok
    catch
        _:Reason ->
            lager:error("Cannot start worker node on host ~s: ~p.", [Host, Reason]),
            {error, Reason}
    end.


%% stop/0
%% ====================================================================
%% @doc Stops worker node.
%% @end
-spec stop() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
stop() ->
    Host = install_utils:get_host(node()),
    lager:info("Stopping cmm node on host: ~p.", [Host]),

    case os:cmd("kill -TERM `ps aux | grep beam | grep " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ " | cut -d'\t' -f2 | awk '{print $2}'`") of
        "" -> ok;
        Other ->
            lager:error("Cannot stop worker node on host ~s: ~p.", [Host, Other]),
            {error, Other}
    end.


%% restart/0
%% ====================================================================
%% @doc Restarts worker node.
%% @end
-spec restart() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
restart() ->
    Host = install_utils:get_host(node()),
    try
        lager:info("Restarting worker node on host: ~p.", [Host]),

        ok = stop(),
        ok = start(),

        ok
    catch
        _:Reason ->
            lager:error("Cannot restart worker node on host ~s: ~p.", [Host, Reason]),
            {error, Reason}
    end.