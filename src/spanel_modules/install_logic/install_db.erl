%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module implements {@link install_behaviour} callbacks and
%% provides API methods for database nodes installation.
%% @end
%% ===================================================================
-module(install_db).
-behaviour(install_behaviour).
-author("Krzysztof Trzepla").

-include("spanel_modules/db.hrl").
-include("spanel_modules/install.hrl").

% install_behaviour callbacks
-export([install/2, uninstall/2, start/2, stop/2, restart/2]).

% API
-export([install/0, uninstall/0, start/0, stop/0, restart/0, join_cluster/1]).

%% ===================================================================
%% Behaviour callback functions
%% ===================================================================

%% install/2
%% ====================================================================
%% @doc Installs database nodes on hosts.
%% @end
-spec install(Hosts :: [string()], Args) -> ok | {error, Error :: [string()] | binary()} when
  Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
install(Hosts, _) ->
  try
    case dao:get_record(configurations, last) of
      {ok, #configuration{dbs = []}} -> ok;
      _ -> throw(<<"Database already installed.">>)
    end,

    {InstallOk, InstallError} = install_utils:apply_on_hosts(Hosts, ?MODULE, install_db, [], ?RPC_TIMEOUT),
    {HostsOk, HostsError} = case InstallOk of
                              [ClusterHost | NodesToAdd] ->
                                {JoinOk, JoinError} = install_utils:apply_on_hosts(NodesToAdd, ?MODULE, join_cluster, [ClusterHost], ?RPC_TIMEOUT),
                                {[ClusterHost | JoinOk], JoinError ++ InstallError};
                              _ -> {InstallOk, InstallError}
                            end,
    case dao:update_record(configurations, #configuration{id = last, dbs = HostsOk}) of
      ok ->
        case HostsError of
          [] -> ok;
          _ ->
            rpc:multicall(HostsError, ?MODULE, uninstall, [], ?RPC_TIMEOUT),
            {error, HostsError}
        end;
      _ ->
        lager:error("Error while updating database configuration."),
        rpc:multicall(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),
        {error, Hosts}
    end
  catch
    _:Reason -> {error, Reason}
  end.


%% uninstall/2
%% ====================================================================
%% @doc Uninstalls database nodes on hosts.
%% @end
-spec uninstall(Hosts :: [string()], Args) -> ok | {error, Error :: [string()]} when
  Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
uninstall(Hosts, _) ->
  try
    InstalledDbs = case dao:get_record(configurations, last) of
                     {ok, #configuration{dbs = Dbs}} -> Dbs;
                     _ -> []
                   end,

    lists:foreach(fun(Host) ->
      case lists:member(Host, InstalledDbs) of
        true -> ok;
        _ -> throw(<<"Host: ", (list_to_binary(Host))/binary, " is not installed.">>)
      end
    end, Hosts),

    {UninstallOk, UninstallError} = install_utils:apply_on_hosts(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),

    NewDbs = lists:filter(fun(Db) -> not lists:member(Db, UninstallOk) end, InstalledDbs),

    case dao:update_record(configurations, #configuration{id = last, dbs = NewDbs}) of
      ok ->
        case UninstallError of
          [] -> ok;
          _ -> {error, UninstallError}
        end;
      _ ->
        lager:error("Error while updating database configuration."),
        {error, Hosts}
    end
  catch
    _:Reason -> {error, Reason}
  end


%% start/2
%% ====================================================================
%% @doc Starts database nodes on hosts.
%% @end
    - spec start(Hosts :: [string()], Args) -> ok | {error, Error :: [string()]} when
Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
start(Hosts, _) ->
  {StartOk, StartError} = install_utils:apply_on_hosts(Hosts, ?MODULE, start, [], ?RPC_TIMEOUT),
  {_, JoinError} = case StartOk of
                     [First | Rest] ->
                       install_utils:apply_on_hosts(Rest, ?MODULE, join_cluster, [First], ?RPC_TIMEOUT);
                     _ -> {StartOk, []}
                   end,
  case StartError ++ JoinError of
    [] -> ok;
    _ ->
      install_utils:apply_on_hosts(JoinError, ?MODULE, start, [], ?RPC_TIMEOUT),
      {error, StartError ++ JoinError}
  end.


%% stop/2
%% ====================================================================
%% @doc Stops database nodes on hosts.
%% @end
-spec stop(Hosts :: [string()], Args) -> ok | {error, Error :: [string()]} when
  Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
stop(Hosts, _) ->
  {_, StopError} = install_utils:apply_on_hosts(Hosts, ?MODULE, stop, [], ?RPC_TIMEOUT),
  case StopError of
    [] -> ok;
    _ -> {error, StopError}
  end.


%% restart/2
%% ====================================================================
%% @doc Restarts database nodes on hosts.
%% @end
-spec restart(Hosts :: [string()], Args) -> ok | {error, Error :: [string()]} when
  Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
restart(Hosts, _) ->
  {_, RestartError} = install_utils:apply_on_hosts(Hosts, ?MODULE, stop, [], ?RPC_TIMEOUT),
  case RestartError of
    [] -> ok;
    _ -> {error, RestartError}
  end.


%% ===================================================================
%% API functions
%% ===================================================================

%% install/0
%% ====================================================================
%% @doc Installs database node.
%% @end
-spec install() -> ok | {error, Reason :: term()}.
%% ====================================================================
install() ->
  Host = install_utils:get_host(node()),
  try
    lager:info("Installing database node on host: ~s.", [Host]),

    "" = os:cmd("mkdir -p " ++ ?DEFAULT_DB_INSTALL_PATH),
    "" = os:cmd("cp -R " ++ ?DB_RELEASE ++ "/* " ++ ?DEFAULT_DB_INSTALL_PATH),
    "" = os:cmd("sed -i -e \"s/^\\-setcookie .*/\\-setcookie " ++ atom_to_list(?DEFAULT_COOKIE) ++ "/g\" " ++ ?DEFAULT_DB_INSTALL_PATH ++ "/etc/vm.args"),
    "" = os:cmd("sed -i -e \"s/^\\-name .*/\\-name " ++ ?DEFAULT_DB_NAME ++ "@" ++ Host ++ "/g\" " ++ ?DEFAULT_DB_INSTALL_PATH ++ "/etc/vm.args"),
    ok = install_utils:add_node_to_config(db_node, list_to_atom(?DEFAULT_DB_NAME), ?DEFAULT_DB_INSTALL_PATH),

    ok
  catch
    _:_ -> {error, <<"Can not install database node on host: ", (list_to_binary(Host))/binary, ".">>}
  end.


%% uninstall/0
%% ====================================================================
%% @doc Uninstalls database node.
%% @end
-spec uninstall() -> ok | {error, Reason :: term()}.
%% ====================================================================
uninstall() ->
  Host = install_utils:get_host(node()),
  try
    lager:info("Uninstalling database node on host: ~s.", [Host]),

    ok = install_utils:remove_node_from_config(db_node),
    "" = os:cmd("rm -rf " ++ ?DEFAULT_DB_INSTALL_PATH),

    ok
  catch
    _:_ -> {error, <<"Can not uninstall database node on host: ", (list_to_binary(Host))/binary, ".">>}
  end.


%% start/0
%% ====================================================================
%% @doc Starts database node.
%% @end
-spec start() -> ok | {error, Reason :: term()}.
%% ====================================================================
start() ->
  Host = install_utils:get_host(node()),
  try
    BigcouchStartScript = ?DEFAULT_DB_INSTALL_PATH ++ "/" ++ ?DB_START_COMMAND_SUFFIX,
    NohupOut = ?DEFAULT_DB_INSTALL_PATH ++ "/" ++ ?NOHUP_OUTPUT,
    SetUlimitsCmd = install_utils:get_ulimits_cmd(),
    lager:info("Starting database node on host: ~s.", [Host]),

    open_port({spawn, "sh -c \"" ++ SetUlimitsCmd ++ " ; " ++ "nohup " ++ BigcouchStartScript ++ " > " ++ NohupOut ++ " 2>&1 &" ++ "\" 2>&1 &"}, [out]),

    ok
  catch
    _:_ -> {error, <<"Can not start database node on host: ", (list_to_binary(Host))/binary, ".">>}
  end.


%% stop/0
%% ====================================================================
%% @doc Stops database node.
%% @end
-spec stop() -> ok | {error, Reason :: term()}.
%% ====================================================================
stop() ->
  Host = install_utils:get_host(node()),
  lager:info("Stopping database node on host: ~s.", [Host]),

  case os:cmd("kill -TERM `ps aux | grep beam | grep " ++ ?DEFAULT_DB_INSTALL_PATH ++ " | cut -d'\t' -f2 | awk '{print $2}'`") of
    "" -> ok;
    _ -> {error, <<"Can not stop database node on host: ", (list_to_binary(Host))/binary, ".">>}
  end.


%% restart/0
%% ====================================================================
%% @doc Restarts database node.
%% @end
-spec restart() -> ok | {error, Reason :: term()}.
%% ====================================================================
restart() ->
  Host = install_utils:get_host(node()),
  try
    lager:info("Restarting database node on host: ~s.", [Host]),

    ok = stop(),
    ok = start(),

    ok
  catch
    _:_ -> {error, <<"Can not restart database node on host: ", (list_to_binary(Host))/binary, ".">>}
  end.


%% join_cluster/2
%% ====================================================================
%% @doc Adds database node to cluster. ClusterNode is one of current
%% database cluster nodes.
%% @end
-spec join_cluster(ClusterNode :: node()) -> ok | {error, Reason :: term()}.
%% ====================================================================
join_cluster(ClusterNode) ->
  join_cluster(ClusterNode, 0).


%% join_cluster/2
%% ====================================================================
%% @doc Adds database node to cluster. ClusterNode is one of current
%% database cluster nodes. Should not be used directly, use join_cluster/1
%% instead.
%% @end
-spec join_cluster(ClusterNode :: node(), Attempts :: integer()) -> ok | {error, Reason :: term()}.
%% ====================================================================
join_cluster(_, 10) ->
  Host = install_utils:get_host(node()),
  {error, <<"Can not add database node to cluster on host: ", (list_to_binary(Host))/binary, ".">>};

join_cluster(ClusterNode, Attempts) ->
  timer:sleep(1000),
  Host = install_utils:get_host(node()),
  try
    ClusterHost = install_utils:get_host(ClusterNode),
    Url = "http://" ++ ClusterHost ++ ":" ++ ?DEFAULT_PORT ++ "/nodes/" ++ ?DEFAULT_DB_NAME ++ "@" ++ Host,
    lager:info("Adding database node to cluster on host: ~s.", [Host]),

    {ok, "201", _ResponseHeaders, ResponseBody} = ibrowse:send_req(Url, [{content_type, "application/json"}], put, "{}", ?CURL_OPTS),
    false = (0 =:= string:str(ResponseBody, "\"ok\":true")),

    ok
  catch
    _:_ -> join_cluster(ClusterNode, Attempts + 1)
  end.
