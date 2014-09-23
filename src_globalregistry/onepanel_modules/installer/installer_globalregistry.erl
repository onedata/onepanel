%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements {@link installer_behaviour} callbacks and
%% provides API methods for CCM nodes installation.
%% @end
%% ===================================================================
-module(installer_globalregistry).

%%
%% -behaviour(installer_behaviour).
%%
%% -include("onepanel_modules/installer/state.hrl").
%% -include("onepanel_modules/installer/internals.hrl").
%% -include_lib("ctool/include/logging.hrl").
%%
%% %% install_behaviour callbacks
%% -export([install/1, uninstall/1, start/1, stop/1, restart/1]).
%%
%% %% API
%% -export([local_install/0, local_uninstall/0, local_start/3, local_stop/0, local_restart/0]).
%%
%% %% ====================================================================
%% %% Behaviour callback functions
%% %% ====================================================================
%%
%% %% install/1
%% %% ====================================================================
%% %% @doc Installs CCM nodes on given hosts. Arguments list should contain
%% %% list of hosts where to install CCM nodes.
%% %% @end
%% -spec install(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
%%     Result :: ok | {error, Reason :: term()}.
%% %% ====================================================================
%% install(Args) ->
%%     CCMs = proplists:get_value(ccms, Args, []),
%%
%%     {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(CCMs, ?MODULE, local_install, [], ?RPC_TIMEOUT),
%%
%%     case HostsError of
%%         [] -> ok;
%%         _ ->
%%             ?error("Cannot install CCM nodes on following hosts: ~p", [HostsError]),
%%             onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_uninstall, [], ?RPC_TIMEOUT),
%%             {error, {hosts, HostsError}}
%%     end.
%%
%%
%% %% uninstall/1
%% %% ====================================================================
%% %% @doc Uninstalls CCM nodes on given hosts. Arguments list should
%% %% contain list of hosts where CCM nodes where installed.
%% %% @end
%% -spec uninstall(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
%%     Result :: ok | {error, Reason :: term()}.
%% %% ====================================================================
%% uninstall(Args) ->
%%     CCMs = proplists:get_value(ccms, Args, []),
%%
%%     {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(CCMs, ?MODULE, local_uninstall, [], ?RPC_TIMEOUT),
%%
%%     case HostsError of
%%         [] -> ok;
%%         _ ->
%%             ?error("Cannot uninstall CCM nodes on following hosts: ~p", [HostsError]),
%%             onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_install, [], ?RPC_TIMEOUT),
%%             {error, {hosts, HostsError}}
%%     end.
%%
%%
%% %% start/1
%% %% ====================================================================
%% %% @doc Starts CCM nodes on given hosts. Arguments list should contain
%% %% list of hosts where CCM nodes where installed with main CCM node
%% %% pointed out.
%% %% @end
%% -spec start(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
%%     Result :: ok | {error, Reason :: term()}.
%% %% ====================================================================
%% start(Args) ->
%%     try
%%         CCMs = case proplists:get_value(ccms, Args, []) of
%%                    [] -> throw(nothing_to_start);
%%                    Hosts -> Hosts
%%                end,
%%
%%         MainCCM = case proplists:get_value(main_ccm, Args) of
%%                       undefined -> throw("Main CCM node not found in arguments list.");
%%                       Host -> Host
%%                   end,
%%
%%         OptCCMs = case lists:member(MainCCM, CCMs) of
%%                       true -> lists:delete(MainCCM, CCMs);
%%                       _ -> throw("Main CCM node not found among CCM nodes.")
%%                   end,
%%
%%         ConfiguredDbs = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
%%                             {ok, #?GLOBAL_CONFIG_RECORD{dbs = []}} -> throw("Database nodes not configured.");
%%                             {ok, #?GLOBAL_CONFIG_RECORD{ccms = [], dbs = Dbs}} -> Dbs;
%%                             {ok, #?GLOBAL_CONFIG_RECORD{ccms = _}} -> throw("CCM nodes already configured.");
%%                             _ -> throw("Cannot get CCM nodes configuration.")
%%                         end,
%%
%%         {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(CCMs, ?MODULE, local_start, [MainCCM, OptCCMs, ConfiguredDbs], ?RPC_TIMEOUT),
%%
%%         case HostsError of
%%             [] ->
%%                 case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{main_ccm, MainCCM}, {ccms, CCMs}]) of
%%                     ok -> ok;
%%                     Other ->
%%                         ?error("Cannot update CCM nodes configuration: ~p", [Other]),
%%                         onepanel_utils:apply_on_hosts(CCMs, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
%%                         {error, {hosts, CCMs}}
%%                 end;
%%             _ ->
%%                 ?error("Cannot start CCM nodes on following hosts: ~p", [HostsError]),
%%                 onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
%%                 {error, {hosts, HostsError}}
%%         end
%%     catch
%%         _:nothing_to_start -> ok;
%%         _:Reason ->
%%             ?error("Cannot start CCM nodes: ~p", [Reason]),
%%             {error, Reason}
%%     end.
%%
%%
%% %% stop/1
%% %% ====================================================================
%% %% @doc Stops all CCM nodes.
%% %% @end
%% -spec stop(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
%%     Result :: ok | {error, Reason :: term()}.
%% %% ====================================================================
%% stop(_) ->
%%     try
%%         {ConfiguredMainCCM, ConfiguredCCMs, ConfiguredDbs} =
%%             case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
%%                 {ok, #?GLOBAL_CONFIG_RECORD{ccms = []}} -> throw("CCM nodes not configured.");
%%                 {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCM, ccms = CCMs, dbs = Dbs}} -> {MainCCM, CCMs, Dbs};
%%                 _ -> throw("Cannot get CCM nodes configuration.")
%%             end,
%%
%%         ConfiguredOptCCMs = lists:delete(ConfiguredMainCCM, ConfiguredCCMs),
%%
%%         {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(ConfiguredCCMs, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
%%
%%         case HostsError of
%%             [] ->
%%                 case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{main_ccm, undefined}, {ccms, []}]) of
%%                     ok -> ok;
%%                     Other ->
%%                         ?error("Cannot update CCM nodes configuration: ~p", [Other]),
%%                         onepanel_utils:apply_on_hosts(ConfiguredCCMs, ?MODULE, local_start, [ConfiguredMainCCM, ConfiguredOptCCMs, ConfiguredDbs], ?RPC_TIMEOUT),
%%                         {error, {hosts, ConfiguredCCMs}}
%%                 end;
%%             _ ->
%%                 ?error("Cannot stop CCM nodes on following hosts: ~p", [HostsError]),
%%                 onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_start, [ConfiguredMainCCM, ConfiguredOptCCMs, ConfiguredDbs], ?RPC_TIMEOUT),
%%                 {error, {hosts, HostsError}}
%%         end
%%     catch
%%         _:Reason ->
%%             ?error("Cannot stop CCM nodes: ~p", [Reason]),
%%             {error, Reason}
%%     end.
%%
%%
%% %% restart/1
%% %% ====================================================================
%% %% @doc Restarts all CCM nodes.
%% %% @end
%% -spec restart(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
%%     Result :: ok | {error, Reason :: term()}.
%% %% ====================================================================
%% restart(_) ->
%%     try
%%         {ConfiguredMainCCM, ConfiguredCCMs} =
%%             case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
%%                 {ok, #?GLOBAL_CONFIG_RECORD{ccms = []}} -> throw("CCM nodes not configured.");
%%                 {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCM, ccms = CCMs}} -> {MainCCM, CCMs};
%%                 _ -> throw("Cannot get CCM nodes configuration.")
%%             end,
%%
%%         ConfiguredOptCCMs = lists:delete(ConfiguredMainCCM, ConfiguredCCMs),
%%
%%         case stop([]) of
%%             ok -> start([{main_ccm, ConfiguredMainCCM}, {ccms, ConfiguredOptCCMs}]);
%%             Other -> Other
%%         end
%%     catch
%%         _:Reason ->
%%             ?error("Cannot restart CCM nodes: ~p", [Reason]),
%%             {error, Reason}
%%     end.
%%
%%
%% %% ====================================================================
%% %% API functions
%% %% ====================================================================
%%
%% %% local_install/0
%% %% ====================================================================
%% %% @doc Installs CCM node on local host.
%% %% @end
%% -spec local_install() -> Result when
%%     Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% %% ====================================================================
%% local_install() ->
%%     Host = onepanel_utils:get_host(node()),
%%     try
%%         ?debug("Installing CCM node"),
%%         CCMPath = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_CCM_NAME]),
%%
%%         "" = os:cmd("mkdir -p " ++ CCMPath),
%%         "" = os:cmd("cp -R " ++ filename:join([?VEIL_RELEASE, "* "]) ++ CCMPath),
%%
%%         {ok, Host}
%%     catch
%%         _:Reason ->
%%             ?error("Cannot install CCM node: ~p", [Reason]),
%%             {error, Host}
%%     end.
%%
%%
%% %% local_uninstall/0
%% %% ====================================================================
%% %% @doc Uninstalls CCM node on local host.
%% %% @end
%% -spec local_uninstall() -> Result when
%%     Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% %% ====================================================================
%% local_uninstall() ->
%%     Host = onepanel_utils:get_host(node()),
%%     try
%%         ?debug("Uninstalling CCM node"),
%%         CCMPath = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_CCM_NAME]),
%%
%%         "" = os:cmd("rm -rf " ++ CCMPath),
%%
%%         {ok, Host}
%%     catch
%%         _:Reason ->
%%             ?error("Cannot uninstall CCM node: ~p", [Reason]),
%%             {error, Host}
%%     end.
%%
%%
%% %% local_start/3
%% %% ====================================================================
%% %% @doc Starts CCM node on local host.
%% %% @end
%% -spec local_start(MainCCM :: string(), OptCCMs :: [string()], Dbs :: [string()]) -> Result when
%%     Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% %% ====================================================================
%% local_start(MainCCM, OptCCMs, Dbs) ->
%%     Host = onepanel_utils:get_host(node()),
%%     try
%%         ?debug("Starting CCM node: ~p"),
%%
%%         Name = ?DEFAULT_CCM_NAME ++ "@" ++ Host,
%%
%%         MainCCMName = ?DEFAULT_CCM_NAME ++ "@" ++ MainCCM,
%%
%%         OptCCMNames = lists:foldl(fun(OptCCM, Acc) ->
%%             Acc ++ ?DEFAULT_CCM_NAME ++ "@" ++ OptCCM ++ " "
%%         end, [], OptCCMs),
%%
%%         DbNames = lists:foldl(fun(Db, Acc) ->
%%             Acc ++ ?DEFAULT_DB_NAME ++ "@" ++ Db ++ " "
%%         end, [], Dbs),
%%
%%         NodeConfigPath = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_CCM_NAME, ?CONFIG_ARGS_PATH]),
%%         StorageConfigPath = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_CCM_NAME, ?STORAGE_CONFIG_PATH]),
%%         OverwriteCommand = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_CCM_NAME, ?VEIL_CLUSTER_SCRIPT_PATH]),
%%         StartCommand = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_CCM_NAME, ?START_COMMAND_SUFFIX]),
%%
%%         ok = installer_utils:overwrite_config_args(NodeConfigPath, "name", Name),
%%         ok = installer_utils:overwrite_config_args(NodeConfigPath, "main_ccm", MainCCMName),
%%         ok = installer_utils:overwrite_config_args(NodeConfigPath, "opt_ccms", OptCCMNames),
%%         ok = installer_utils:overwrite_config_args(NodeConfigPath, "db_nodes", DbNames),
%%         ok = installer_utils:overwrite_config_args(NodeConfigPath, "storage_config_path", StorageConfigPath),
%%         ok = installer_utils:add_node_to_config(ccm_node, list_to_atom(?DEFAULT_CCM_NAME), ?DEFAULT_NODES_INSTALL_PATH),
%%
%%         os:cmd(OverwriteCommand),
%%         SetUlimitsCmd = installer_utils:get_system_limits_cmd(Host),
%%         "" = os:cmd(SetUlimitsCmd ++ " ; " ++ StartCommand),
%%
%%         {ok, Host}
%%     catch
%%         _:Reason ->
%%             ?error("Cannot start CCM node: ~p", [Reason]),
%%             {error, Host}
%%     end.
%%
%%
%% %% local_stop/0
%% %% ====================================================================
%% %% @doc Stops CCM node on local host.
%% %% @end
%% -spec local_stop() -> Result when
%%     Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% %% ====================================================================
%% local_stop() ->
%%     Host = onepanel_utils:get_host(node()),
%%     try
%%         ?debug("Stopping CCM node"),
%%         CCMPath = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_CCM_NAME]),
%%
%%         "" = os:cmd("kill -TERM `ps aux | grep beam | grep " ++ CCMPath ++ " | awk '{print $2}'`"),
%%         ok = installer_utils:remove_node_from_config(ccm_node),
%%
%%         {ok, Host}
%%     catch
%%         _:Reason ->
%%             ?error("Cannot stop CCM node: ~p", [Reason]),
%%             {error, Host}
%%     end.
%%
%%
%% %% local_restart/0
%% %% ====================================================================
%% %% @doc Restarts CCM node on local host.
%% %% @end
%% -spec local_restart() -> Result when
%%     Result :: {ok, Host :: string()} | {error, Reason :: term()}.
%% %% ====================================================================
%% local_restart() ->
%%     Host = onepanel_utils:get_host(node()),
%%     try
%%         {ConfiguredMainCCM, ConfiguredCCMs, ConfiguredDbs} =
%%             case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
%%                 {ok, #?GLOBAL_CONFIG_RECORD{ccms = []}} -> throw("CCM nodes not configured.");
%%                 {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCM, ccms = CCMs, dbs = Dbs}} -> {MainCCM, CCMs, Dbs};
%%                 _ -> throw("Cannot get CCM nodes configuration.")
%%             end,
%%
%%         ConfiguredOptCCMs = lists:delete(ConfiguredMainCCM, ConfiguredCCMs),
%%
%%
%%         case local_stop() of
%%             {ok, _} -> local_start(ConfiguredMainCCM, ConfiguredOptCCMs, ConfiguredDbs);
%%             Other -> Other
%%         end
%%     catch
%%         _:Reason ->
%%             ?error("Cannot restart CCM node: ~p", [Reason]),
%%             {error, Host}
%%     end.