%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements {@link installer_behaviour} callbacks and
%% provides API methods for Global Registry node installation.
%% @end
%% ===================================================================
-module(installer_gr).
-behaviour(installer_behaviour).

-include("registered_names.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% install_behaviour callbacks
-export([install/1, uninstall/1, start/1, stop/1, restart/1]).

%% API
-export([local_install/0, local_uninstall/0, local_start/1, local_stop/0, local_restart/0]).

%% ====================================================================
%% Behaviour callback functions
%% ====================================================================

%% install/1
%% ====================================================================
%% @doc Installs Global Registry nodes on given hosts. Arguments list
%% should contain list of hosts where to install Global Registry nodes.
%% @end
-spec install(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(Args) ->
    case proplists:get_value(gr, Args) of
        undefined -> ok;
        GR ->
            {HostsOk, HostsError} = onepanel_utils:apply_on_hosts([GR], ?MODULE, local_install, [], ?RPC_TIMEOUT),
            case HostsError of
                [] -> ok;
                _ ->
                    ?error("Cannot install Global Registry node on following hosts: ~p", [HostsError]),
                    onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_uninstall, [], ?RPC_TIMEOUT),
                    {error, {hosts, HostsError}}
            end
    end.

%% uninstall/1
%% ====================================================================
%% @doc Uninstalls Global Registry nodes on given hosts. Arguments list
%% should contain list of hosts where Global Registry nodes where installed.
%% @end
-spec uninstall(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
uninstall(Args) ->
    case proplists:get_value(gr, Args) of
        undefined -> ok;
        GR ->
            {HostsOk, HostsError} = onepanel_utils:apply_on_hosts([GR], ?MODULE, local_uninstall, [], ?RPC_TIMEOUT),
            case HostsError of
                [] -> ok;
                _ ->
                    ?error("Cannot uninstall Global Registry node on following hosts: ~p", [HostsError]),
                    onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_install, [], ?RPC_TIMEOUT),
                    {error, {hosts, HostsError}}
            end
    end.

%% start/1
%% ====================================================================
%% @doc Starts Global Registry node on given hosts. Arguments list should contain
%% host where Global Registry node was installed.
%% @end
-spec start(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
start(Args) ->
    try
        GR = case proplists:get_value(gr, Args) of
                 undefined -> throw(nothing_to_start);
                 Host -> Host
             end,

        ConfiguredDbs = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                            {ok, #?GLOBAL_CONFIG_RECORD{dbs = []}} ->
                                throw("Database nodes not configured.");
                            {ok, #?GLOBAL_CONFIG_RECORD{gr = undefined, dbs = Dbs}} ->
                                Dbs;
                            {ok, #?GLOBAL_CONFIG_RECORD{gr = _}} ->
                                throw("Global Registry node already configured.");
                            _ ->
                                throw("Cannot get Global Registry node configuration.")
                        end,

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts([GR], ?MODULE, local_start, [ConfiguredDbs], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{gr, GR}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update Global Registry node configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts([GR], ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                        {error, {hosts, [GR]}}
                end;
            _ ->
                ?error("Cannot start Global Registry node on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:nothing_to_start -> ok;
        _:Reason ->
            ?error("Cannot start Global Registry node: ~p", [Reason]),
            {error, Reason}
    end.

%% stop/1
%% ====================================================================
%% @doc Stops Global Registry node.
%% @end
-spec stop(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
stop(_) ->
    try
        {ConfiguredGR, ConfiguredDbs} = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                                            {ok, #?GLOBAL_CONFIG_RECORD{gr = undefined}} ->
                                                throw("Global Registry node not configured.");
                                            {ok, #?GLOBAL_CONFIG_RECORD{gr = GR, dbs = Dbs}} ->
                                                {GR, Dbs};
                                            _ ->
                                                throw("Cannot get Global Registry node configuration.")
                                        end,

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts([ConfiguredGR], ?MODULE, local_stop, [], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{gr, undefined}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update Global Registry node configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts([ConfiguredGR], ?MODULE, local_start, [ConfiguredDbs], ?RPC_TIMEOUT),
                        {error, {hosts, [ConfiguredGR]}}
                end;
            _ ->
                ?error("Cannot stop Global Registry node on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_start, [ConfiguredDbs], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:Reason ->
            ?error("Cannot stop Global Registry node: ~p", [Reason]),
            {error, Reason}
    end.

%% restart/1
%% ====================================================================
%% @doc Restarts Global Registry node.
%% @end
-spec restart(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
restart(_) ->
    try
        ConfiguredGR = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                           {ok, #?GLOBAL_CONFIG_RECORD{gr = undefined}} ->
                               throw("Global Registry node not configured.");
                           {ok, #?GLOBAL_CONFIG_RECORD{gr = GR}} -> GR;
                           _ ->
                               throw("Cannot get Global Registry node configuration.")
                       end,

        case stop([]) of
            ok -> start([{gr, ConfiguredGR}]);
            Other -> Other
        end
    catch
        _:Reason ->
            ?error("Cannot restart Global Registry node: ~p", [Reason]),
            {error, Reason}
    end.

%% ====================================================================
%% API functions
%% ====================================================================

%% local_install/0
%% ====================================================================
%% @doc Installs Global Registry node on local host.
%% @end
-spec local_install() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_install() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Installing Global Registry node"),
        GRPath = filename:join([?NODES_INSTALL_PATH, ?GLOBALREGISTRY_NAME]),

        "" = os:cmd("rm -rf " ++ GRPath),
        "" = os:cmd("mkdir -p " ++ GRPath),
        "" = os:cmd("cp -R " ++ filename:join([?GLOBALREGISTRY_RELEASE, "* "]) ++ GRPath),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot install Global Registry node: ~p", [Reason]),
            {error, Host}
    end.

%% local_uninstall/0
%% ====================================================================
%% @doc Uninstalls Global Registry node on local host.
%% @end
-spec local_uninstall() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_uninstall() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Uninstalling Global Registry node"),
        GRPath = filename:join([?NODES_INSTALL_PATH, ?GLOBALREGISTRY_NAME]),

        "" = os:cmd("rm -rf " ++ GRPath),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot uninstall Global Registry node: ~p", [Reason]),
            {error, Host}
    end.

%% local_start/1
%% ====================================================================
%% @doc Starts Global Registry node on local host.
%% @end
-spec local_start(Dbs :: [string()]) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_start(Dbs) ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Starting Global Registry node: ~p"),

        release_configurator:configure_release(
            ?SOFTWARE_NAME,
            filename:join([?NODES_INSTALL_PATH, ?GLOBALREGISTRY_NAME]),
            [
                {db_nodes, [list_to_atom(?DB_NAME ++ "@" ++ Db) || Db <- Dbs]},
                {grpcert_domain, ?GLOBALREGISTRY_CERT_DOMAIN}
            ],
            [
                {name, ?GLOBALREGISTRY_NAME ++ "@" ++ Host},
                {setcookie, ?COOKIE}
            ]
        ),

        Daemon = filename:join([?NODES_INSTALL_PATH, ?GLOBALREGISTRY_NAME, ?GLOBALREGISTRY_DAEMON]),
        SetUlimitsCmd = installer_utils:get_system_limits_cmd(Host),
        "" = os:cmd("bash -c \"" ++ SetUlimitsCmd ++ " ; " ++ Daemon ++ " start\""),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot start Global Registry node: ~p", [Reason]),
            {error, Host}
    end.

%% local_stop/0
%% ====================================================================
%% @doc Stops Global Registry node on local host.
%% @end
-spec local_stop() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_stop() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Stopping Global Registry node"),

        "0" = os:cmd("service " ++ ?GLOBALREGISTRY_SERVICE ++ " stop_globalregistry 1>/dev/null 2>&1 ; echo -n $?"),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot stop Global Registry node: ~p", [Reason]),
            {error, Host}
    end.

%% local_restart/0
%% ====================================================================
%% @doc Restarts Global Registry node on local host.
%% @end
-spec local_restart() -> Result when
    Result :: {ok, Host :: string()} | {error, Reason :: term()}.
%% ====================================================================
local_restart() ->
    Host = onepanel_utils:get_host(node()),
    try
        ConfiguredDbs = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                            {ok, #?GLOBAL_CONFIG_RECORD{gr = undefined}} ->
                                throw("Global Registry node not configured.");
                            {ok, #?GLOBAL_CONFIG_RECORD{gr = _, dbs = Dbs}} ->
                                Dbs;
                            _ ->
                                throw("Cannot get Global Registry node configuration.")
                        end,

        case local_stop() of
            {ok, _} -> local_start(ConfiguredDbs);
            Other -> Other
        end
    catch
        _:Reason ->
            ?error("Cannot restart Global Registry node: ~p", [Reason]),
            {error, Host}
    end.