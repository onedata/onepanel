%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module implements {@link installer_behaviour} callbacks and
%% provides API methods for database nodes installation.
%% @end
%% ===================================================================
-module(installer_db).
-behaviour(installer_behaviour).

-include("onepanel_modules/user_logic.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% install_behaviour callbacks
-export([install/1, uninstall/1, start/1, stop/1, restart/1]).

%% API
-export([local_install/0, local_uninstall/0, local_start/0, local_stop/0, add_to_cluster/2]).

%% ====================================================================
%% Behaviour callback functions
%% ====================================================================

%% install/1
%% ====================================================================
%% @doc Installs database nodes on given hosts. Arguments list should
%% contain list of hosts where to install database nodes.
%% @end
-spec install(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(Args) ->
    Dbs = proplists:get_value(dbs, Args, []),

    {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(Dbs, ?MODULE, local_install, [], ?RPC_TIMEOUT),

    case HostsError of
        [] -> ok;
        _ ->
            ?error("Cannot install database nodes on following hosts: ~p", [HostsError]),
            onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_uninstall, [], ?RPC_TIMEOUT),
            {error, {hosts, HostsError}}
    end.


%% uninstall/1
%% ====================================================================
%% @doc Uninstalls database nodes on given hosts. Arguments list should
%% contain list of hosts where database nodes where installed.
%% @end
-spec uninstall(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
uninstall(Args) ->
    Dbs = proplists:get_value(dbs, Args, []),

    {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(Dbs, ?MODULE, local_uninstall, [], ?RPC_TIMEOUT),

    case HostsError of
        [] -> ok;
        _ ->
            ?error("Cannot uninstall database nodes on following hosts: ~p", [HostsError]),
            onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_install, [], ?RPC_TIMEOUT),
            {error, {hosts, HostsError}}
    end.


%% start/1
%% ====================================================================
%% @doc Starts database nodes on given hosts. Arguments list should
%% contain list of hosts where database nodes where installed.
%% @end
-spec start(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
start(Args) ->
    try
        Dbs = case proplists:get_value(dbs, Args, []) of
                  [] -> throw(nothing_to_start);
                  Hosts -> Hosts
              end,

        case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
            {ok, #?GLOBAL_CONFIG_RECORD{dbs = []}} -> ok;
            {ok, #?GLOBAL_CONFIG_RECORD{dbs = _}} -> throw("Database nodes already configured");
            _ -> throw("Cannot get database nodes configuration")
        end,

        DbPassword = case dao:get_record(?USER_TABLE, <<"admin">>) of
                         {ok, #?USER_RECORD{db_password = Password}} -> binary_to_list(Password);
                         _ -> throw("Cannot get database password")
                     end,

        {StartOk, StartError} = onepanel_utils:apply_on_hosts(Dbs, ?MODULE, local_start, [], ?RPC_TIMEOUT),

        case StartError of
            [] ->
                {_, JoinError} = case StartOk of
                                     [First | Rest] ->
                                         onepanel_utils:apply_on_hosts(Rest, ?MODULE, add_to_cluster, [First, DbPassword], ?RPC_TIMEOUT);
                                     _ -> {StartOk, []}
                                 end,
                case JoinError of
                    [] ->
                        case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{dbs, Dbs}]) of
                            ok -> ok;
                            Other ->
                                ?error("Cannot update database nodes configuration: ~p", [Other]),
                                onepanel_utils:apply_on_hosts(Dbs, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                                {error, {hosts, Dbs}}
                        end;
                    _ ->
                        ?error("Cannot add following hosts: ~p to database cluster", [JoinError]),
                        {error, {hosts, JoinError}}
                end;
            _ ->
                ?error("Cannot start database nodes on following hosts: ~p", [StartError]),
                onepanel_utils:apply_on_hosts(StartOk, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                {error, {hosts, StartError}}
        end
    catch
        _:nothing_to_start -> ok;
        _:Reason ->
            ?error("Cannot start database nodes: ~p", [Reason]),
            {error, Reason}
    end.


%% stop/1
%% ====================================================================
%% @doc Stops all database nodes.
%% @end
-spec stop(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
stop(_) ->
    try
        ConfiguredDbs = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                            {ok, #?GLOBAL_CONFIG_RECORD{dbs = []}} -> throw("Database nodes not configured");
                            {ok, #?GLOBAL_CONFIG_RECORD{dbs = Dbs}} -> Dbs;
                            _ -> throw("Cannot get database nodes configuration")
                        end,

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(ConfiguredDbs, ?MODULE, local_stop, [], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{dbs, []}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update database nodes configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts(ConfiguredDbs, ?MODULE, local_start, [], ?RPC_TIMEOUT),
                        {error, {hosts, ConfiguredDbs}}
                end;
            _ ->
                ?error("Cannot stop database nodes on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_start, [], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:Reason ->
            ?error("Cannot stop database nodes: ~p", [Reason]),
            {error, Reason}
    end.


%% restart/1
%% ====================================================================
%% @doc Restarts all database nodes.
%% @end
-spec restart(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
restart(_) ->
    try
        ConfiguredDbs = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                            {ok, #?GLOBAL_CONFIG_RECORD{dbs = []}} -> throw("Database nodes not configured");
                            {ok, #?GLOBAL_CONFIG_RECORD{dbs = Dbs}} -> Dbs;
                            _ -> throw("Cannot get database nodes configuration")
                        end,

        case stop([]) of
            ok -> start([{dbs, ConfiguredDbs}]);
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

%% local_install/0
%% ====================================================================
%% @doc Installs database node on local host.
%% @end
-spec local_install() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_install() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Installing database node"),

        "" = os:cmd("mkdir -p " ++ ?DEFAULT_DB_INSTALL_PATH),
        "" = os:cmd("cp -R " ++ filename:join([?DB_RELEASE, "* "]) ++ ?DEFAULT_DB_INSTALL_PATH),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot install database node: ~p", [Reason]),
            {error, Host}
    end.


%% local_uninstall/0
%% ====================================================================
%% @doc Uninstalls database node on local host.
%% @end
-spec local_uninstall() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_uninstall() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Uninstalling database node"),

        "" = os:cmd("rm -rf " ++ ?DEFAULT_DB_INSTALL_PATH),
        ok = file:delete(?ULIMITS_CONFIG_PATH),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot uninstall database node on host ~s: ~p", [Host, Reason]),
            {error, Host}
    end.


%% local_start/0
%% ====================================================================
%% @doc Starts database node on local host.
%% @end
-spec local_start() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_start() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Starting database node"),
        BigcouchStartScript = filename:join([?DEFAULT_DB_INSTALL_PATH, ?DB_START_COMMAND_SUFFIX]),
        NohupOut = filename:join([?DEFAULT_DB_INSTALL_PATH, ?NOHUP_OUTPUT]),
        SetUlimitsCmd = installer_utils:get_ulimits_cmd(Host),
        VmArgs = filename:join([?DEFAULT_DB_INSTALL_PATH, "etc", "vm.args"]),

        "" = os:cmd("sed -i -e \"s/^\\-setcookie .*/\\-setcookie " ++ atom_to_list(?DEFAULT_COOKIE) ++ "/g\" " ++ VmArgs),
        "" = os:cmd("sed -i -e \"s/^\\-name .*/\\-name " ++ ?DEFAULT_DB_NAME ++ "@" ++ Host ++ "/g\" " ++ VmArgs),
        ok = installer_utils:add_node_to_config(db_node, list_to_atom(?DEFAULT_DB_NAME), ?DEFAULT_DB_INSTALL_PATH),
        open_port({spawn, "sh -c \"" ++ SetUlimitsCmd ++ " ; " ++ "nohup " ++ BigcouchStartScript ++ " > " ++ NohupOut ++ " 2>&1 &" ++ "\" 2>&1 &"}, [out]),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot start database node: ~p", [Reason]),
            {error, Host}
    end.


%% local_stop/0
%% ====================================================================
%% @doc Stops database node on local host.
%% @end
-spec local_stop() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_stop() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Stopping database node"),

        "" = os:cmd("kill -TERM `ps aux | grep beam | grep " ++ ?DEFAULT_DB_INSTALL_PATH ++ " | awk '{print $2}'`"),
        ok = installer_utils:remove_node_from_config(db_node),

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
-spec add_to_cluster(ClusterNode :: node(), Password :: string()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
add_to_cluster(ClusterNode, Password) ->
    add_to_cluster(ClusterNode, binary_to_list(Password), 0).


%% add_to_cluster/2
%% ====================================================================
%% @doc Adds database node to cluster. ClusterNode is one of current
%% database cluster nodes. Should not be used directly, use add_to_cluster/1
%% instead.
%% @end
-spec add_to_cluster(ClusterHost :: string(), Password :: string(), Attempts :: integer()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
add_to_cluster(_, _, 10) ->
    ?error("Can not add database node to cluster: attempts limit exceeded"),
    Host = onepanel_utils:get_host(node()),
    {error, Host};

add_to_cluster(ClusterHost, Password, Attempts) ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Adding database node to cluster"),
        timer:sleep(1000),
        Url = "http://" ++ ClusterHost ++ ":" ++ ?DEFAULT_PORT ++ "/nodes/" ++ ?DEFAULT_DB_NAME ++ "@" ++ Host,
        Options = [{connect_timeout, ?CONNECTION_TIMEOUT}, {basic_auth, {"admin", Password}}],

        {ok, "201", _ResHeaders, ResBody} = ibrowse:send_req(Url, [{content_type, "application/json"}], put, "{}", Options),
        true = proplists:get_value(<<"ok">>, mochijson2:decode(ResBody, [{format, proplist}])),

        {ok, Host}
    catch
        _:_ -> add_to_cluster(ClusterHost, Password, Attempts + 1)
    end.
