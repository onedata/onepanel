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
-author("Krzysztof Trzepla").

-include("spanel_modules/install.hrl").
-include("spanel_modules/db.hrl").

% install_behaviour callbacks
-export([install/2, uninstall/2, start/2, stop/2, restart/2]).

% API
-export([install/3, uninstall/0, start/0, stop/0, restart/0]).

%% ===================================================================
%% Behaviour callback functions
%% ===================================================================

%% install/2
%% ====================================================================
%% @doc Installs worker nodes.
%% @end
-spec install(Hosts :: [string()], Args) -> ok | {error, Hosts :: [string()] | binary()} when
  Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
install(Hosts, Args) ->
  try
    MainCCM = proplists:get_value(main_ccm, Args),
    OptCCMs = proplists:get_value(opt_ccms, Args, []),
    Dbs = proplists:get_value(dbs, Args),

    case MainCCM of
      undefined -> throw(<<"Main CCM node not found in arguments list.">>);
      _ -> ok
    end,

    case Dbs of
      undefined -> throw(<<"Database nodes not found in arguments list.">>);
      _ -> ok
    end,

    InstalledWorkers = case dao:get_record(configurations, last) of
                         {ok, #configuration{workers = Workers}} -> Workers;
                         _ -> []
                       end,

    lists:foreach(fun(Host) ->
      case lists:member(Host, InstalledWorkers) of
        true -> throw(<<"Host: ", (list_to_binary(Host))/binary, " is already installed.">>);
        _ -> ok
      end
    end, Hosts),

    {InstallOk, InstallError} = install_utils:apply_on_hosts(Hosts, ?MODULE, install, [MainCCM, OptCCMs, Dbs], ?RPC_TIMEOUT),

    case dao:update_record(configurations, #configuration{id = last, workers = InstalledWorkers ++ InstallOk}) of
      ok ->
        case InstallError of
          [] -> ok;
          _ ->
            rpc:multicall(InstallError, ?MODULE, uninstall, [], ?RPC_TIMEOUT),
            {error, InstallError}
        end;
      _ ->
        lager:error("Error while updating worker configuration."),
        rpc:multicall(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),
        {error, Hosts}
    end
  catch
    _:Reason -> {error, Reason}
  end.


%% uninstall/2
%% ====================================================================
%% @doc Uninstalls worker nodes.
%% @end
-spec uninstall(Hosts :: [string()], Args) -> ok | {error, Hosts :: [string()] | binary()} when
  Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
uninstall(Hosts, _) ->
  try
    InstalledWorkers = case dao:get_record(configurations, last) of
                         {ok, #configuration{workers = Workers}} -> Workers;
                         _ -> []
                       end,

    lists:foreach(fun(Host) ->
      case lists:member(Host, InstalledWorkers) of
        true -> ok;
        _ -> throw(<<"Host: ", (list_to_binary(Host))/binary, " is not installed.">>)
      end
    end, Hosts),

    {UninstallOk, UninstallError} = install_utils:apply_on_hosts(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),

    NewWorkers = lists:filter(fun(Worker) -> not lists:member(Worker, UninstallOk) end, InstalledWorkers),

    case dao:update_record(configurations, #configuration{id = last, workers = NewWorkers}) of
      ok ->
        case UninstallError of
          [] -> ok;
          _ -> {error, UninstallError}
        end;
      _ ->
        lager:error("Error while updating worker configuration."),
        {error, Hosts}
    end
  catch
    _:Reason -> {error, Reason}
  end.


%% start/2
%% ====================================================================
%% @doc Starts worker nodes.
%% @end
-spec start(Hosts :: [string()], Args) -> ok | {error, Hosts :: [string()]} when
  Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
start(Hosts, _) ->
  {_, StartError} = install_utils:apply_on_hosts(Hosts, ?MODULE, start, [], ?RPC_TIMEOUT),
  case StartError of
    [] -> ok;
    _ -> {error, StartError}
  end.


%% stop/2
%% ====================================================================
%% @doc Stops worker nodes.
%% @end
-spec stop(Hosts :: [string()], Args) -> ok | {error, Hosts :: [string()]} when
  Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
stop(Hosts, _) ->
  {_, StartError} = install_utils:apply_on_hosts(Hosts, ?MODULE, stop, [], ?RPC_TIMEOUT),
  case StartError of
    [] -> ok;
    _ -> {error, StartError}
  end.

%% restart/2
%% ====================================================================
%% @doc Restarts worker nodes.
%% @end
-spec restart(Hosts :: [string()], Args) -> ok | {error, Hosts :: [string()]} when
  Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
restart(Hosts, _) ->
  {_, StartError} = install_utils:apply_on_hosts(Hosts, ?MODULE, restart, [], ?RPC_TIMEOUT),
  case StartError of
    [] -> ok;
    _ -> {error, StartError}
  end.


%% ===================================================================
%% API functions
%% ===================================================================

%% install/3
%% ====================================================================
%% @doc Installs worker node. All function arguments are string form of
%% hostname, eg. "127.0.0.1".
%% @end
-spec install(MainCCM :: string(), OptCCMs :: [string()], Dbs :: [string()]) -> ok | {error, Reason :: term()}.
%% ====================================================================
install(MainCCM, OptCCMs, Dbs) ->
  Host = install_utils:get_host(node()),
  try
    LongName = ?DEFAULT_WORKER_NAME ++ "@" ++ Host,
    MainCCMLongName = ?DEFAULT_WORKER_NAME ++ "@" ++ MainCCM,
    OptCCMsLongNames = lists:foldl(fun(OptCCM, Acc) ->
      Acc ++ ?DEFAULT_WORKER_NAME ++ "@" ++ OptCCM ++ " " end, [], OptCCMs),
    DbsLongNames = lists:foldl(fun(Db, Acc) -> Acc ++ ?DEFAULT_DB_NAME ++ "@" ++ Db ++ " " end, [], Dbs),
    ConfigPath = ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/" ++ ?CONFIG_ARGS_PATH,
    StorageConfigPath = ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/" ++ ?STORAGE_CONFIG_PATH,

    lager:info("Installing worker node on host: ~p.", [Host]),

    "" = os:cmd("mkdir -p " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME),
    "" = os:cmd("cp -R " ++ ?VEIL_RELEASE ++ "/* " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME),
    ok = install_utils:overwrite_config_args(ConfigPath, "name", LongName),
    ok = install_utils:overwrite_config_args(ConfigPath, "main_ccm", MainCCMLongName),
    ok = install_utils:overwrite_config_args(ConfigPath, "opt_ccms", OptCCMsLongNames),
    ok = install_utils:overwrite_config_args(ConfigPath, "db_nodes", DbsLongNames),
    ok = install_utils:overwrite_config_args(ConfigPath, "storage_config_path", StorageConfigPath),
    ok = install_utils:add_node_to_config(worker, list_to_atom(?DEFAULT_WORKER_NAME), ?DEFAULT_NODES_INSTALL_PATH),

    ok
  catch
    _:_ -> {error, <<"Can not install worker node on host: ", (list_to_binary(Host))/binary, ".">>}
  end.


%% uninstall/0
%% ====================================================================
%% @doc Uninstalls worker node.
%% @end
-spec uninstall() -> ok | {error, Reason :: term()}.
%% ====================================================================
uninstall() ->
  Host = install_utils:get_host(node()),
  try
    lager:info("Uninstalling worker node on host: ~p.", [Host]),

    ok = install_utils:remove_node_from_config(worker),
    "" = os:cmd("rm -rf " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME),

    ok
  catch
    _:_ -> {error, <<"Can not uninstall worker node on host: ", (list_to_binary(Host))/binary, ".">>}
  end.


%% start/0
%% ====================================================================
%% @doc Starts worker node.
%% @end
-spec start() -> ok | {error, Reason :: term()}.
%% ====================================================================
start() ->
  Host = install_utils:get_host(node()),
  try
    lager:info("Starting worker node on host: ~p.", [Host]),

    SetUlimitsCmd = install_utils:get_ulimits_cmd(),
    "" = os:cmd(SetUlimitsCmd ++ " ; " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/" ++ ?START_COMMAND_SUFFIX),

    ok
  catch
    _:_ -> {error, <<"Can not start worker node on host: ", (list_to_binary(Host))/binary, ".">>}
  end.


%% stop/0
%% ====================================================================
%% @doc Stops worker node.
%% @end
-spec stop() -> ok | {error, Reason :: term()}.
%% ====================================================================
stop() ->
  Host = install_utils:get_host(node()),
  lager:info("Stopping cmm node on host: ~p.", [Host]),

  case os:cmd("kill -TERM `ps aux | grep beam | grep " ++ ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ " | cut -d'\t' -f2 | awk '{print $2}'`") of
    "" -> ok;
    _ -> {error, <<"Can not stop worker node on host: ", (list_to_binary(Host))/binary, ".">>}
  end.


%% restart/0
%% ====================================================================
%% @doc Restarts worker node.
%% @end
-spec restart() -> ok | {error, Reason :: term()}.
%% ====================================================================
restart() ->
  Host = install_utils:get_host(node()),
  try
    lager:info("Restarting worker node on host: ~p.", [Host]),

    ok = stop(),
    ok = start(),

    ok
  catch
    _:_ -> {error, <<"Can not restart worker node on host: ", (list_to_binary(Host))/binary, ".">>}
  end.