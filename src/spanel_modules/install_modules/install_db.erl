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

-include("spanel_modules/db_logic.hrl").
-include("spanel_modules/install_common.hrl").

%% API
-export([install_database_node/0, uninstall_database_node/0, install_database_nodes/1, add_database_node/1, add_database_nodes/2]).

add_database_node(ClusterNodeHostname) ->
  try
    NodeHostname = install_utils:get_hostname(node()),
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
    _:_ ->
      lager:error("Adding database node failed. Retring."),
      send_http_request(Url, Attempts + 1)
  end.

add_database_nodes(ClusterNodeHostname, NodeHostnames) ->
  install_utils:apply_on_hosts(NodeHostnames, ?MODULE, add_database_node, [ClusterNodeHostname], ?RPC_TIMEOUT).

install_database_node() ->
  try
    Hostname = install_utils:get_hostname(node()),
    lager:info("Installing db@~s...", [Hostname]),
    Path = ?DEFAULT_BIGCOUCH_INSTALL_PATH,
    "" = os:cmd("mkdir -p " ++ Path),
    "" = os:cmd("cp -R " ++ ?DB_RELEASE ++ "/* " ++ Path),
    "" = os:cmd("sed -i -e \"s/^\\-setcookie .*/\\-setcookie " ++ atom_to_list(?DEFAULT_COOKIE) ++ "/g\" " ++ Path ++ "/etc/vm.args"),
    "" = os:cmd("sed -i -e \"s/^\\-name .*/\\-name " ++ ?DEFAULT_DB_NAME ++ "@" ++ Hostname ++ "/g\" " ++ Path ++ "/etc/vm.args"),
    BigcouchStartScript = Path ++ "/" ++ ?DB_START_COMMAND_SUFFIX,
    NohupOut = Path ++ "/" ++ ?NOHUP_OUTPUT,
    SetUlimitCmd = install_utils:get_ulimit_cmd(),
    open_port({spawn, "sh -c \"" ++ SetUlimitCmd ++ " ; " ++ "nohup " ++ BigcouchStartScript ++ " > " ++ NohupOut ++ " 2>&1 &" ++ "\" 2>&1 &"}, [out]),
    {node(), ok}
  catch
    Type:Error ->
      lager:error("Database installation error: ~p, ~p", [Type, Error]),
      {node(), error}
  end.

uninstall_database_node() ->
  try
    Path = ?DEFAULT_BIGCOUCH_INSTALL_PATH,
    os:cmd("kill -TERM `ps aux | grep beam | grep " ++ Path ++ " | cut -d'\t' -f2 | awk '{print $2}'`"),
    os:cmd("rm -rf " ++ Path),
    {node(), ok}
  catch
    _:_ -> {node(), error}
  end.

install_database_nodes([]) ->
  ok;
install_database_nodes(Hostnames) ->
  try
    {[ClusterNodeHostname | NodeHostnames], InstallationFailed} = install_utils:apply_on_hosts(Hostnames, ?MODULE, install_database_node, [], ?RPC_TIMEOUT),
    {AdditionOk, AdditionFailed} = add_database_nodes(ClusterNodeHostname, NodeHostnames),
    case db_logic:update_record(configurations, #configuration{id = last, databases = [ClusterNodeHostname | AdditionOk]}) of
      ok ->
        lager:info("Database configuration successfully updated."),
        case {InstallationFailed, AdditionFailed} of
          {[], []} -> ok;
          _ ->
            rpc:multicall(AdditionFailed, ?MODULE, uninstall_database_node, [], ?RPC_TIMEOUT),
            {error, InstallationFailed, AdditionFailed}
        end;
      _ ->
        lager:error("Error while updating database configuration."),
        rpc:multicall([ClusterNodeHostname | NodeHostnames], ?MODULE, uninstall_database_node, [], ?RPC_TIMEOUT),
        {error, Hostnames, []}
    end
  catch
    _:_ -> {error, Hostnames, []}
  end.
