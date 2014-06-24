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

-include("onepanel_modules/db_logic.hrl").
-include("onepanel_modules/install_logic.hrl").

% install_behaviour callbacks
-export([install/2, uninstall/2, start/2, stop/2, restart/2]).

% API
-export([install/0, uninstall/0, start/0, stop/0, restart/0, join_cluster/1]).

%% ====================================================================
%% Behaviour callback functions
%% ====================================================================

%% install/2
%% ====================================================================
%% @doc Installs database nodes on hosts.
%% @end
-spec install(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
install(Hosts, _) ->
    try
        case dao:get_record(?CONFIG_TABLE, ?CONFIG_ID) of
            {ok, #?CONFIG_TABLE{dbs = []}} -> ok;
            {ok, #?CONFIG_TABLE{dbs = _}} -> throw("Database already installed.");
            _ -> throw("Cannot get database nodes configuration.")
        end,

        {HostsOk, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, install, [], ?RPC_TIMEOUT),

        case dao:update_record(?CONFIG_TABLE, ?CONFIG_ID, [{dbs, HostsOk}]) of
            ok ->
                case HostsError of
                    [] -> ok;
                    _ ->
                        lager:error("Cannot install database nodes on following hosts: ~p", [HostsError]),
                        {error, {hosts, HostsError}}
                end;
            UpdateError ->
                lager:error("Cannot update database nodes configuration: ~p", [UpdateError]),
                rpc:multicall(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),
                {error, UpdateError}
        end
    catch
        _:Reason -> {error, Reason}
    end.


%% uninstall/2
%% ====================================================================
%% @doc Uninstalls database nodes on hosts.
%% @end
-spec uninstall(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
uninstall(Hosts, _) ->
    try
        InstalledDbs = case dao:get_record(?CONFIG_TABLE, ?CONFIG_ID) of
                           {ok, #?CONFIG_TABLE{dbs = Dbs}} -> Dbs;
                           _ -> throw("Cannot get database nodes configuration.")
                       end,

        lists:foreach(fun(Host) ->
            case lists:member(Host, InstalledDbs) of
                true -> ok;
                _ -> throw("Host: " ++ Host ++ " is not installed.")
            end
        end, Hosts),

        {HostsOk, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),

        NewDbs = lists:filter(fun(Db) ->
            not lists:member(Db, HostsOk)
        end, InstalledDbs),

        case dao:update_record(?CONFIG_TABLE, ?CONFIG_ID, [{dbs, NewDbs}]) of
            ok ->
                case HostsError of
                    [] -> ok;
                    _ ->
                        lager:error("Cannot uninstall database nodes on following hosts: ~p", [HostsError]),
                        {error, {hosts, HostsError}}
                end;
            UpdateError ->
                lager:error("Cannot update database nodes configuration: ~p", [UpdateError]),
                {error, UpdateError}
        end
    catch
        _:Reason ->
            lager:error("Cannot uninstall ccm nodes: ~p", [Reason]),
            {error, Reason}
    end.


%% start/2
%% ====================================================================
%% @doc Starts database nodes on hosts.
%% @end
-spec start(Hosts :: [string()], Args) -> Result when
    Result :: ok | {error, Reason :: term()},
    Args :: [{Name :: atom(), Value :: term()}].
%% ====================================================================
start(Hosts, _) ->
    {StartOk, StartError} = install_utils:apply_on_hosts(Hosts, ?MODULE, start, [], ?RPC_TIMEOUT),
    {_, JoinError} = case StartOk of
                         [First | Rest] ->
                             install_utils:apply_on_hosts(Rest, ?MODULE, join_cluster, [First], ?RPC_TIMEOUT);
                         _ -> {StartOk, []}
                     end,

    case JoinError of
        [] -> ok;
        _ -> lager:error("Cannot add following hosts to database cluster: ~p", [JoinError])
    end,

    case StartError ++ JoinError of
        [] -> ok;
        _ ->
            lager:error("Cannot start database nodes on following hosts: ~p", [StartError]),
            install_utils:apply_on_hosts(JoinError, ?MODULE, stop, [], ?RPC_TIMEOUT),
            {error, StartError ++ JoinError}
    end.


%% stop/2
%% ====================================================================
%% @doc Stops database nodes on hosts.
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
            lager:error("Cannot stop database nodes on following hosts: ~p", [HostsError]),
            {error, HostsError}
    end.


%% restart/2
%% ====================================================================
%% @doc Restarts database nodes on hosts.
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
            lager:error("Cannot restart database nodes on following hosts: ~p", [HostsError]),
            {error, HostsError}
    end.


%% ====================================================================
%% API functions
%% ====================================================================

%% install/0
%% ====================================================================
%% @doc Installs database node.
%% @end
-spec install() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install() ->
    Host = install_utils:get_host(node()),
    try
        lager:info("Installing database node on host: ~s.", [Host]),

        "" = os:cmd("mkdir -p " ++ ?DEFAULT_DB_INSTALL_PATH),
        "" = os:cmd("cp -R " ++ ?DB_RELEASE ++ "/ * " ++ ?DEFAULT_DB_INSTALL_PATH),
        "" = os:cmd("sed -i -e \"s/^\\-setcookie .*/\\-setcookie " ++ atom_to_list(?DEFAULT_COOKIE) ++ "/g\" " ++ ?DEFAULT_DB_INSTALL_PATH ++ "/etc/vm.args"),
        "" = os:cmd("sed -i -e \"s/^\\-name .*/\\-name " ++ ?DEFAULT_DB_NAME ++ "@" ++ Host ++ "/g\" " ++ ?DEFAULT_DB_INSTALL_PATH ++ "/etc/vm.args"),
        ok = install_utils:add_node_to_config(db_node, list_to_atom(?DEFAULT_DB_NAME), ?DEFAULT_DB_INSTALL_PATH),

        ok
    catch
        _:Reason ->
            lager:error("Cannot install database node on host ~s: ~p.", [Host, Reason]),
            {error, Reason}
    end.


%% uninstall/0
%% ====================================================================
%% @doc Uninstalls database node.
%% @end
-spec uninstall() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
uninstall() ->
    Host = install_utils:get_host(node()),
    try
        lager:info("Uninstalling database node on host: ~s.", [Host]),

        ok = install_utils:remove_node_from_config(db_node),
        "" = os:cmd("rm -rf " ++ ?DEFAULT_DB_INSTALL_PATH),

        ok
    catch
        _:Reason ->
            lager:error("Cannot uninstall database node on host ~s: ~p.", [Host, Reason]),
            {error, Reason}
    end.


%% start/0
%% ====================================================================
%% @doc Starts database node.
%% @end
-spec start() -> Result when
    Result :: ok | {error, Reason :: term()}.
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
        _:Reason ->
            lager:error("Cannot start database node on host ~s: ~p.", [Host, Reason]),
            {error, Reason}
    end.


%% stop/0
%% ====================================================================
%% @doc Stops database node.
%% @end
-spec stop() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
stop() ->
    Host = install_utils:get_host(node()),
    lager:info("Stopping database node on host: ~s.", [Host]),

    case os:cmd("kill -TERM `ps aux | grep beam | grep " ++ ?DEFAULT_DB_INSTALL_PATH ++ " | cut -d'\t' -f2 | awk '{print $2}'`") of
        "" -> ok;
        Other ->
            lager:error("Cannot stop database node on host ~s: ~p.", [Host, Other]),
            {error, Other}
    end.


%% restart/0
%% ====================================================================
%% @doc Restarts database node.
%% @end
-spec restart() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
restart() ->
    Host = install_utils:get_host(node()),
    try
        lager:info("Restarting database node on host: ~s.", [Host]),

        ok = stop(),
        ok = start(),

        ok
    catch
        _:Reason ->
            lager:error("Cannot start database node on host ~s: ~p.", [Host, Reason]),
            {error, Reason}
    end.


%% join_cluster/2
%% ====================================================================
%% @doc Adds database node to cluster. ClusterNode is one of current
%% database cluster nodes.
%% @end
-spec join_cluster(ClusterNode :: node()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
join_cluster(ClusterNode) ->
    join_cluster(ClusterNode, 0).


%% join_cluster/2
%% ====================================================================
%% @doc Adds database node to cluster. ClusterNode is one of current
%% database cluster nodes. Should not be used directly, use join_cluster/1
%% instead.
%% @end
-spec join_cluster(ClusterHost :: string(), Attempts :: integer()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
join_cluster(_, 10) ->
    Host = install_utils:get_host(node()),
    lager:error("Cannot add database node to cluster on host ~s: attempts limit exceeded.", [Host]),
    {error, "Cannot add database node to cluster: attempts limit exceeded."};

join_cluster(ClusterHost, Attempts) ->
    try
        timer:sleep(1000),
        Host = install_utils:get_host(node()),
        Url = "http://" ++ ClusterHost ++ ":" ++ ?DEFAULT_PORT ++ "/nodes/" ++ ?DEFAULT_DB_NAME ++ "@" ++ Host,
        lager:info("Adding database node to cluster on host: ~s.", [Host]),

        {ok, "201", _ResponseHeaders, ResponseBody} = ibrowse:send_req(Url, [{content_type, "application/json"}], put, "{}", ?CURL_OPTS),
        false = (0 =:= string:str(ResponseBody, "\"ok\":true")),

        ok
    catch
        _:_ -> join_cluster(ClusterHost, Attempts + 1)
    end.
