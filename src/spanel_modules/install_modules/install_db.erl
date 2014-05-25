%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains database installation functions
%% @end
%% ===================================================================
-module(install_db).

-include("spanel_modules/install_common.hrl").

%% API
-export([install_database_node/0, uninstall_database_node/0, install_database_nodes/1, add_database_node/1, add_database_nodes/2]).

add_database_node(ClusterNode) ->
  try
      "spanel@" ++ ClusterNodeHostname = atom_to_list(ClusterNode),
      "spanel@" ++ NodeHostname = atom_to_list(node()),
    Url = "http://" ++ ClusterNodeHostname ++ ":" ++ ?DEFAULT_PORT ++ "/nodes/" ++ ?DEFAULT_DB_NAME ++ "@" ++ NodeHostname,
    ok = send_http_request(Url, 0),
    {node(), ok}
  catch
    _:_ -> {node(), error}
  end.

send_http_request(_, 10) ->
  error;
send_http_request(Url, Attempts) ->
  try
    timer:sleep(1000),
    {ok, "201", _ResponseHeaders, ResponseBody} = ibrowse:send_req(Url, [{content_type, "application/json"}], put, "{}", ?CURL_OPTS),
    false = (0 =:= string:str(ResponseBody, "\"ok\":true")),
    ok
  catch
    _:_ -> send_http_request(Url, Attempts + 1)
  end.

add_database_nodes(ClusterNode, Nodes) ->
  {_, FailedNodes} = install_utils:apply_on_nodes(Nodes, ?MODULE, add_database_node, [ClusterNode], ?RPC_TIMEOUT),
  FailedNodes.

install_database_node() ->
  lager:info("Installing database node..."),
  try
      "spanel@" ++ Hostname = atom_to_list(node()),
    Path = ?DEFAULT_BIGCOUCH_INSTALL_PATH,
    "" = os:cmd("mkdir -p " ++ Path),
    "" = os:cmd("cp -R " ++ ?DB_RELEASE ++ "/* " ++ Path),
    ok = install_utils:add_node_to_config(db_node, list_to_atom(?DEFAULT_DB_NAME), Path),
    "" = os:cmd("sed -i -e \"s/^\\-setcookie .*/\\-setcookie " ++ atom_to_list(?DEFAULT_COOKIE) ++ "/g\" " ++ Path ++ "/etc/vm.args"),
    "" = os:cmd("sed -i -e \"s/^\\-name .*/\\-name " ++ ?DEFAULT_DB_NAME ++ "@" ++ Hostname ++ "/g\" " ++ Path ++ "/etc/vm.args"),
    open_port({spawn, ?INIT_D_SCRIPT_PATH ++ " start_db 1>/dev/null"}, [out]),
    {node(), ok}
  catch
    Type:Error ->
      lager:error("Database installation error: ~p, ~p", [Type, Error]),
      {node(), error}
  end.

uninstall_database_node() ->
  try
    {db_node, {db_node, Name, Path}} = install_utils:get_nodes_from_config(database),
    os:cmd(?INIT_D_SCRIPT_PATH ++ " stop_db"),
    os:cmd("rm -rf " ++ Path),
    install_utils:remove_node_from_config(Name)
  catch
    _:_ -> error
  end.

install_database_nodes(Nodes) ->
  {InstallationOk, InstallationFailed} = install_utils:apply_on_nodes(Nodes, ?MODULE, install_database_node, [], ?RPC_TIMEOUT),
  AdditionFailed = case InstallationOk of
                     [PrimaryNode | NodesToAdd] -> add_database_nodes(PrimaryNode, NodesToAdd);
                     _ -> []
                   end,
  case {InstallationFailed, AdditionFailed} of
    {[], []} -> ok;
    _ ->
      rpc:multicall(AdditionFailed, ?MODULE, uninstall_database_node, [], ?RPC_TIMEOUT),
      {error, InstallationFailed, AdditionFailed}
  end.
