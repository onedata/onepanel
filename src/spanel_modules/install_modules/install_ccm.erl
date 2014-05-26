%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains ccm installation functions
%% @end
%% ===================================================================
-module(install_ccm).

-include("spanel_modules/install_common.hrl").
-include("spanel_modules/db_logic.hrl").

%% API
-export([install_ccm_node/3, uninstall_ccm_node/0, install_ccm_nodes/3]).

install_ccm_node(CCM, CCMs, Databases) ->
  try
    Name = ?DEFAULT_CCM_NAME,
    Hostname = install_utils:get_hostname(node()),
    Path = ?DEFAULT_NODES_INSTALL_PATH,
    ok = install_utils:install_veil_node(ccm, Name, Hostname, Path, CCM, CCMs, Databases),
    SetUlimitCmd = install_utils:get_ulimit_cmd(),
    os:cmd(SetUlimitCmd ++ " ; " ++ Path ++ Name ++ "/" ++ ?START_COMMAND_SUFFIX),
    {node(), ok}
  catch
    _:_ -> {node(), error}
  end.

uninstall_ccm_node() ->
  try
    Path = ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_CCM_NAME,
    os:cmd("kill -TERM `ps aux | grep beam | grep " ++ Path ++ " | cut -d'\t' -f2 | awk '{print $2}'`"),
    os:cmd("rm -rf " ++ Path),
    {node(), ok}
  catch
    _:_ -> {node(), error}
  end.

install_ccm_nodes(CCM, CCMs, Databases) ->
  {HostsOk, HostsFailed} = install_utils:apply_on_hosts([CCM | CCMs], ?MODULE, install_ccm_node, [CCM, CCMs, Databases], ?RPC_TIMEOUT),
  Config = case lists:delete(CCM, HostsOk) of
             HostsOk -> #configuration{id = last, ccms = HostsOk};
             HostsOkWithoutCCM -> #configuration{id = last, ccm = CCM, ccms = HostsOkWithoutCCM}
           end,
  case db_logic:update_record(configurations, Config) of
    ok ->
      lager:info("CCM configuration successfully updated."),
      case HostsFailed of
        [] -> ok;
        _ -> {error, HostsFailed}
      end;
    _ ->
      lager:error("Error while updating CCM configuration."),
      rpc:multicall(HostsOk, ?MODULE, uninstall_ccm_node, [], ?RPC_TIMEOUT),
      {error, [CCM | CCMs]}
  end.