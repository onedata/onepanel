%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains worker installation functions
%% @end
%% ===================================================================
-module(install_worker).

-include("spanel_modules/install_common.hrl").
-include("spanel_modules/db_logic.hrl").

%% API
-export([install_worker_node/3, uninstall_worker_node/0, install_worker_nodes/4]).

install_worker_node(CCM, CCMs, Databases) ->
  try
    Name = ?DEFAULT_WORKER_NAME,
    Hostname = install_utils:get_hostname(node()),
    Path = ?DEFAULT_NODES_INSTALL_PATH,
    ok = install_utils:install_veil_node(worker, Name, Hostname, Path, CCM, CCMs, Databases),
    SetUlimitCmd = install_utils:get_ulimit_cmd(),
    os:cmd(SetUlimitCmd ++ " ; " ++ Path ++ Name ++ "/" ++ ?START_COMMAND_SUFFIX),
    {node(), ok}
  catch
    _:_ -> {node(), error}
  end.

uninstall_worker_node() ->
  try
    Path = ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME,
    os:cmd("kill -TERM `ps aux | grep beam | grep " ++ Path ++ " | cut -d'\t' -f2 | awk '{print $2}'`"),
    os:cmd("rm -rf " ++ Path),
    {node(), ok}
  catch
    _:_ -> {node(), error}
  end.

install_worker_nodes(CCM, CCMs, Workers, Databases) ->
  {HostsOk, HostsFailed} = install_utils:apply_on_hosts(Workers, ?MODULE, install_worker_node, [CCM, CCMs, Databases], ?RPC_TIMEOUT),
  case db_logic:update_record(configurations, #configuration{id = last, workers = HostsOk}) of
    ok ->
      lager:info("Workers configuration successfully updated."),
      case HostsFailed of
        [] -> ok;
        _ -> {error, HostsFailed}
      end;
    _ ->
      lager:error("Error while updating CCM configuration."),
      rpc:multicall(HostsOk, ?MODULE, uninstall_worker_node, [], ?RPC_TIMEOUT),
      {error, Workers}
  end.