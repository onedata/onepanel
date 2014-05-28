%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains database nodes installation functions
%% @end
%% ===================================================================
-module(install_db).

-include("spanel_modules/db.hrl").
-include("spanel_modules/install.hrl").

%% API
-export([install_db/0, uninstall_db/0, install_dbs/1, add_db_to_cluster/1]).

%% add_db_to_cluster/0
%% ====================================================================
%% @doc Adds database node on host to cluster
%% @end
-spec add_db_to_cluster(ClusterHost :: string()) -> ok | error.
%% ====================================================================
add_db_to_cluster(ClusterHost) ->
  add_db_to_cluster(ClusterHost, 0).

add_db_to_cluster(_, 10) ->
  error;
add_db_to_cluster(ClusterHost, Attempts) ->
  try
    timer:sleep(1000),
    Host = install_utils:get_host(node()),
    Url = "http://" ++ ClusterHost ++ ":" ++ ?DEFAULT_PORT ++ "/nodes/" ++ ?DEFAULT_DB_NAME ++ "@" ++ Host,
    {ok, "201", _ResponseHeaders, ResponseBody} = ibrowse:send_req(Url, [{content_type, "application/json"}], put, "{}", ?CURL_OPTS),
    false = (0 =:= string:str(ResponseBody, "\"ok\":true")),
    ok
  catch
    _:_ -> add_db_to_cluster(ClusterHost, Attempts + 1)
  end.

%% install_db/0
%% ====================================================================
%% @doc Installs database node on host
%% @end
-spec install_db() -> ok | error.
%% ====================================================================
install_db() ->
  try
    Host = install_utils:get_host(node()),
    lager:info("Installing db@~s...", [Host]),
    Path = ?DEFAULT_BIGCOUCH_INSTALL_PATH,
    "" = os:cmd("mkdir -p " ++ Path),
    "" = os:cmd("cp -R " ++ ?DB_RELEASE ++ "/* " ++ Path),
    "" = os:cmd("sed -i -e \"s/^\\-setcookie .*/\\-setcookie " ++ atom_to_list(?DEFAULT_COOKIE) ++ "/g\" " ++ Path ++ "/etc/vm.args"),
    "" = os:cmd("sed -i -e \"s/^\\-name .*/\\-name " ++ ?DEFAULT_DB_NAME ++ "@" ++ Host ++ "/g\" " ++ Path ++ "/etc/vm.args"),
    BigcouchStartScript = Path ++ "/" ++ ?DB_START_COMMAND_SUFFIX,
    NohupOut = Path ++ "/" ++ ?NOHUP_OUTPUT,
    SetUlimitsCmd = install_utils:get_ulimits_cmd(),
    ok = install_utils:add_node_to_config(db_node, list_to_atom(?DEFAULT_DB_NAME), Path),
    open_port({spawn, "sh -c \"" ++ SetUlimitsCmd ++ " ; " ++ "nohup " ++ BigcouchStartScript ++ " > " ++ NohupOut ++ " 2>&1 &" ++ "\" 2>&1 &"}, [out]),
    ok
  catch
    _:_ -> error
  end.

%% uninstall_db/0
%% ====================================================================
%% @doc Uninstalls database node on host
%% @end
-spec uninstall_db() -> ok | error.
%% ====================================================================
uninstall_db() ->
  try
    Path = ?DEFAULT_BIGCOUCH_INSTALL_PATH,
    os:cmd("kill -TERM `ps aux | grep beam | grep " ++ Path ++ " | cut -d'\t' -f2 | awk '{print $2}'`"),
    os:cmd("rm -rf " ++ Path),
    ok
  catch
    _:_ -> error
  end.

%% install_dbs/1
%% ====================================================================
%% @doc Installs database nodes on hosts
%% @end
-spec install_dbs(Hosts :: [string()]) -> ok | {error, HostsError} when
  HostsError :: [string()].
%% ====================================================================
install_dbs(Hosts) ->
  {InstallationOk, InstallationError} = install_utils:apply_on_hosts(Hosts, ?MODULE, install_db, [], ?RPC_TIMEOUT),
  {HostsOk, HostsError} = case InstallationOk of
                            [ClusterHost | NodesToAdd] ->
                              {AdditionOk, AdditionError} = install_utils:apply_on_hosts(NodesToAdd, ?MODULE, add_db_to_cluster, [ClusterHost], ?RPC_TIMEOUT),
                              {[ClusterHost | AdditionOk], AdditionError ++ InstallationError};
                            _ -> {InstallationOk, InstallationError}
                          end,
  case dao:update_record(configurations, #configuration{id = last, dbs = HostsOk}) of
    ok ->
      case HostsError of
        [] -> ok;
        _ ->
          rpc:multicall(HostsError, ?MODULE, uninstall_db, [], ?RPC_TIMEOUT),
          {error, HostsError}
      end;
    _ ->
      lager:error("Error while updating database configuration."),
      rpc:multicall(Hosts, ?MODULE, uninstall_db, [], ?RPC_TIMEOUT),
      {error, Hosts}
  end.
