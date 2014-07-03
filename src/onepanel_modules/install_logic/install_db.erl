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
-include_lib("ctool/include/logging.hrl").

%% install_behaviour callbacks
-export([install/1, uninstall/1, start/1, stop/1, restart/1]).

%% API
-export([install/0, uninstall/0, start/0, stop/0, add_to_cluster/1]).

%% ====================================================================
%% Behaviour callback functions
%% ====================================================================

%% install/1
%% ====================================================================
%% @doc Installs database nodes on given hosts.
%% @end
-spec install(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(Args) ->
    Hosts = proplists:get_value(hosts, Args, []),

    {HostsOk, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, install, [], ?RPC_TIMEOUT),

    case HostsError of
        [] -> ok;
        _ ->
            ?error("Cannot install database nodes on following hosts: ~p", [HostsError]),
            install_utils:apply_on_hosts(HostsOk, ?MODULE, uninstall, [], ?RPC_TIMEOUT),
            {error, {hosts, HostsError}}
    end.


%% uninstall/1
%% ====================================================================
%% @doc Uninstalls database nodes on given hosts.
%% @end
-spec uninstall(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
uninstall(Args) ->
    Hosts = proplists:get_value(hosts, Args, []),

    {HostsOk, HostsError} = install_utils:apply_on_hosts(Hosts, ?MODULE, uninstall, [], ?RPC_TIMEOUT),

    case HostsError of
        [] -> ok;
        _ ->
            ?error("Cannot uninstall database nodes on following hosts: ~p", [HostsError]),
            install_utils:apply_on_hosts(HostsOk, ?MODULE, install, [], ?RPC_TIMEOUT),
            {error, {hosts, HostsError}}
    end.


%% start/1
%% ====================================================================
%% @doc Starts database nodes on given hosts. Argument list should contain
%% list of host where database nodes where installed.
%% @end
-spec start(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
start(Args) ->
    try
        case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
            {ok, #?GLOBAL_CONFIG_RECORD{dbs = []}} -> ok;
            {ok, #?GLOBAL_CONFIG_RECORD{dbs = _}} -> throw("Database nodes already configured.");
            _ -> throw("Cannot get database nodes configuration.")
        end,

        Dbs = proplists:get_value(hosts, Args, []),

        {StartOk, StartError} = install_utils:apply_on_hosts(Dbs, ?MODULE, start, [], ?RPC_TIMEOUT),

        case StartError of
            [] ->
                {_, JoinError} = case StartOk of
                                     [First | Rest] ->
                                         install_utils:apply_on_hosts(Rest, ?MODULE, add_to_cluster, [First], ?RPC_TIMEOUT);
                                     _ -> {StartOk, []}
                                 end,
                case JoinError of
                    [] ->
                        case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{dbs, Dbs}]) of
                            ok -> ok;
                            Other ->
                                ?error("Cannot update database nodes configuration: ~p", [Other]),
                                install_utils:apply_on_hosts(Dbs, ?MODULE, stop, [], ?RPC_TIMEOUT),
                                {error, {hosts, Dbs}}
                        end;
                    _ ->
                        ?error("Cannot add following hosts: ~p to database cluster", [JoinError]),
                        {error, {hosts, JoinError}}
                end;
            _ ->
                ?error("Cannot start database nodes on following hosts: ~p", [StartError]),
                install_utils:apply_on_hosts(StartOk, ?MODULE, stop, [], ?RPC_TIMEOUT),
                {error, {hosts, StartError}}
        end
    catch
        _:Reason ->
            ?error("Cannot start database nodes: ~p", [Reason]),
            {error, Reason}
    end.


%% stop/1
%% ====================================================================
%% @doc Stops database nodes on given hosts.
%% @end
-spec stop(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
stop(_) ->
    try
        Dbs = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                  {ok, #?GLOBAL_CONFIG_RECORD{dbs = []}} ->
                      throw("Database nodes not configured.");
                  {ok, #?GLOBAL_CONFIG_RECORD{dbs = Hosts}} ->
                      Hosts;
                  _ -> throw("Cannot get database nodes configuration.")
              end,

        {HostsOk, HostsError} = install_utils:apply_on_hosts(Dbs, ?MODULE, stop, [], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{dbs, []}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update database nodes configuration: ~p", [Other]),
                        install_utils:apply_on_hosts(Dbs, ?MODULE, start, [], ?RPC_TIMEOUT),
                        {error, {hosts, Dbs}}
                end;
            _ ->
                ?error("Cannot stop database nodes on following hosts: ~p", [HostsError]),
                install_utils:apply_on_hosts(HostsOk, ?MODULE, start, [], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:Reason ->
            ?error("Cannot stop database nodes: ~p", [Reason]),
            {error, Reason}
    end.


%% restart/1
%% ====================================================================
%% @doc Restarts database nodes on given hosts.
%% @end
-spec restart(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
restart(_) ->
    try
        Dbs = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                  {ok, #?GLOBAL_CONFIG_RECORD{dbs = undefined}} ->
                      throw("Database nodes not configured.");
                  {ok, #?GLOBAL_CONFIG_RECORD{dbs = Hosts}} ->
                      Hosts;
                  _ -> throw("Cannot get database nodes configuration.")
              end,

        case stop([]) of
            ok -> start([{hosts, Dbs}]);
            Other -> Other
        end
    catch
        _:Reason ->
            ?error("Cannot restart database nodes: ~p", [Reason]),
            {error, Reason}
    end.


%% ====================================================================
%% API functions
%% ====================================================================

%% install/0
%% ====================================================================
%% @doc Installs database node on local host.
%% @end
-spec install() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
install() ->
    Host = install_utils:get_host(node()),
    try
        ?debug("Installing database node."),

        "" = os:cmd("mkdir -p " ++ ?DEFAULT_DB_INSTALL_PATH),
        "" = os:cmd("cp -R " ++ ?DB_RELEASE ++ "/* " ++ ?DEFAULT_DB_INSTALL_PATH),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot install database node: ~p.", [Reason]),
            {error, Host}
    end.


%% uninstall/0
%% ====================================================================
%% @doc Uninstalls database node on local host.
%% @end
-spec uninstall() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
uninstall() ->
    Host = install_utils:get_host(node()),
    try
        ?debug("Uninstalling database node."),

        "" = os:cmd("rm -rf " ++ ?DEFAULT_DB_INSTALL_PATH),
        ok = file:delete(?ULIMITS_CONFIG_PATH),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot uninstall database node on host ~s: ~p.", [Host, Reason]),
            {error, Host}
    end.


%% start/0
%% ====================================================================
%% @doc Starts database node on local host.
%% @end
-spec start() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
start() ->
    Host = install_utils:get_host(node()),
    try
        ?debug("Starting database node."),
        BigcouchStartScript = filename:join([?DEFAULT_DB_INSTALL_PATH, ?DB_START_COMMAND_SUFFIX]),
        NohupOut = filename:join([?DEFAULT_DB_INSTALL_PATH, ?NOHUP_OUTPUT]),
        SetUlimitsCmd = install_utils:get_ulimits_cmd(Host),

        "" = os:cmd("sed -i -e \"s/^\\-setcookie .*/\\-setcookie " ++ atom_to_list(?DEFAULT_COOKIE) ++ "/g\" " ++ ?DEFAULT_DB_INSTALL_PATH ++ "/etc/vm.args"),
        "" = os:cmd("sed -i -e \"s/^\\-name .*/\\-name " ++ ?DEFAULT_DB_NAME ++ "@" ++ Host ++ "/g\" " ++ ?DEFAULT_DB_INSTALL_PATH ++ "/etc/vm.args"),
        ok = install_utils:add_node_to_config(db_node, list_to_atom(?DEFAULT_DB_NAME), ?DEFAULT_DB_INSTALL_PATH),
        open_port({spawn, "sh -c \"" ++ SetUlimitsCmd ++ " ; " ++ "nohup " ++ BigcouchStartScript ++ " > " ++ NohupOut ++ " 2>&1 &" ++ "\" 2>&1 &"}, [out]),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot start database node: ~p", [Reason]),
            {error, Host}
    end.


%% stop/0
%% ====================================================================
%% @doc Stops database node on local host.
%% @end
-spec stop() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
stop() ->
    Host = install_utils:get_host(node()),
    try
        ?debug("Stopping database node."),

        "" = os:cmd("kill -TERM `ps aux | grep beam | grep " ++ ?DEFAULT_DB_INSTALL_PATH ++ " | awk '{print $2}'`"),
        ok = install_utils:remove_node_from_config(db_node),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot stop database node: ~p", [Reason]),
            {error, Host}
    end.


%% add_to_cluster/1
%% ====================================================================
%% @doc Adds database node to cluster. ClusterNode is one of current
%% database cluster nodes.
%% @end
-spec add_to_cluster(ClusterNode :: node()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
add_to_cluster(ClusterNode) ->
    add_to_cluster(ClusterNode, 0).


%% add_to_cluster/2
%% ====================================================================
%% @doc Adds database node to cluster. ClusterNode is one of current
%% database cluster nodes. Should not be used directly, use add_to_cluster/1
%% instead.
%% @end
-spec add_to_cluster(ClusterHost :: string(), Attempts :: integer()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
add_to_cluster(_, 10) ->
    ?error("Can not add database node to cluster: attempts limit exceeded."),
    Host = install_utils:get_host(node()),
    {error, Host};

add_to_cluster(ClusterHost, Attempts) ->
    Host = install_utils:get_host(node()),
    try
        ?debug("Adding database node to cluster."),
        timer:sleep(1000),
        Url = "http://" ++ ClusterHost ++ ":" ++ ?DEFAULT_PORT ++ "/nodes/" ++ ?DEFAULT_DB_NAME ++ "@" ++ Host,

        {ok, "201", _ResponseHeaders, ResponseBody} = ibrowse:send_req(Url, [{content_type, "application/json"}], put, "{}", ?CURL_OPTS),
        false = (0 =:= string:str(ResponseBody, "\"ok\":true")),

        {ok, Host}
    catch
        _:_ -> add_to_cluster(ClusterHost, Attempts + 1)
    end.
