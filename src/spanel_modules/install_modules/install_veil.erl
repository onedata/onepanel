%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains VeilCluster nodes installation functions
%% @end
%% ===================================================================
-module(install_veil).

-include("spanel_modules/install.hrl").
-include("spanel_modules/db.hrl").

%% API
-export([install_veil_node/4, uninstall_veil_node/1, install_veil_nodes/5]).

%% install_veil_node/4
%% ====================================================================
%% @doc Installs veil node of specified type on host.
%% @end
-spec install_veil_node(Type, MainCCM, OptCCMs, Dbs) -> ok | error when
  Type :: ccm | worker,
  MainCCM :: string(),
  OptCCMs :: [string()],
  Dbs :: [string()].
%% ====================================================================
install_veil_node(Type, MainCCM, OptCCMs, Dbs) ->
  try
    Name = case Type of
             ccm -> ?DEFAULT_CCM_NAME;
             worker -> ?DEFAULT_WORKER_NAME
           end,
    Host = install_utils:get_host(node()),
    LongName = Name ++ "@" ++ Host,
    Path = ?DEFAULT_NODES_INSTALL_PATH,
    lager:info("Installing " ++ LongName ++ "..."),
    "" = os:cmd("mkdir -p " ++ Path ++ Name),
    "" = os:cmd("cp -R " ++ ?VEIL_RELEASE ++ "/* " ++ Path ++ Name),

    MainCCMLongName = ?DEFAULT_CCM_NAME ++ "@" ++ MainCCM,
    OptCCMsLongNames = lists:foldl(fun(OptCCM, Acc) ->
      Acc ++ ?DEFAULT_CCM_NAME ++ "@" ++ OptCCM ++ " " end, [], OptCCMs),
    DbsLongNames = lists:foldl(fun(Db, Acc) -> Acc ++ ?DEFAULT_DB_NAME ++ "@" ++ Db ++ " " end, [], Dbs),
    StorageConfigPath = Path ++ Name ++ "/" ++ ?STORAGE_CONFIG_PATH,

    ok = install_utils:overwrite_config_args(Path ++ Name ++ "/" ++ ?CONFIG_ARGS_PATH, "name", LongName),
    ok = install_utils:overwrite_config_args(Path ++ Name ++ "/" ++ ?CONFIG_ARGS_PATH, "main_ccm", MainCCMLongName),
    ok = install_utils:overwrite_config_args(Path ++ Name ++ "/" ++ ?CONFIG_ARGS_PATH, "opt_ccms", OptCCMsLongNames),
    ok = install_utils:overwrite_config_args(Path ++ Name ++ "/" ++ ?CONFIG_ARGS_PATH, "db_nodes", DbsLongNames),
    ok = install_utils:overwrite_config_args(Path ++ Name ++ "/" ++ ?CONFIG_ARGS_PATH, "storage_config_path", StorageConfigPath),

    os:cmd(Path ++ Name ++ "/" ++ ?VEIL_CLUSTER_SCRIPT_PATH),
    ok = install_utils:add_node_to_config(Type, list_to_atom(Name), Path),
    SetUlimitsCmd = install_utils:get_ulimits_cmd(),
    "" = os:cmd(SetUlimitsCmd ++ " ; " ++ Path ++ Name ++ "/" ++ ?START_COMMAND_SUFFIX),
    ok
  catch
    _:_ -> error
  end.

%% uninstall_veil_node/1
%% ====================================================================
%% @doc Uninstalls veil node of specified type on host.
%% @end
-spec uninstall_veil_node(Type :: ccm | worker) -> ok | error.
%% ====================================================================
uninstall_veil_node(Type) ->
  try
    Path = case Type of
             ccm -> ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_CCM_NAME;
             _ -> ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME
           end,
    os:cmd("kill -TERM `ps aux | grep beam | grep " ++ Path ++ " | cut -d'\t' -f2 | awk '{print $2}'`"),
    os:cmd("rm -rf " ++ Path),
    ok
  catch
    _:_ -> error
  end.

%% install_veil_nodes/5
%% ====================================================================
%% @doc Installs veil nodes of specified type on hosts.
%% @end
-spec install_veil_nodes(Hosts, Type, MainCCM, OptCCMs, Dbs) -> ok | {error, HostsError} when
  Hosts :: [string()],
  Type :: ccm | worker,
  MainCCM :: string(),
  OptCCMs :: [string()],
  Dbs :: [string()],
  HostsError :: [string()].
%% ====================================================================
install_veil_nodes(Hosts, Type, MainCCM, OptCCMs, Dbs) ->
  {HostsOk, HostsFailed} = install_utils:apply_on_hosts(Hosts, ?MODULE, install_veil_node, [Type, MainCCM, OptCCMs, Dbs], ?RPC_TIMEOUT),
  Workers = case dao:get_record(configurations, last) of
              {ok, #configuration{workers = InstalledWorkers}} -> InstalledWorkers;
              _ -> []
            end,
  Configuration = case Type of
                    worker ->
                      #configuration{id = last, main_ccm = MainCCM, opt_ccms = OptCCMs, workers = Workers ++ HostsOk};
                    _ -> #configuration{id = last, main_ccm = MainCCM, opt_ccms = OptCCMs}
                  end,
  case dao:update_record(configurations, Configuration) of
    ok ->
      case HostsFailed of
        [] -> ok;
        _ -> {error, HostsFailed}
      end;
    _ ->
      lager:error("Error while updating ~p configuration.", [Type]),
      rpc:multicall(Hosts, ?MODULE, uninstall_veil_node, [Type], ?RPC_TIMEOUT),
      {error, Hosts}
  end.