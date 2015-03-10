%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements {@link installer_behaviour} callbacks and
%% provides API methods for database nodes installation.
%% @end
%% ===================================================================
-module(installer_db).
-behaviour(installer_behaviour).

-include("registered_names.hrl").
-include("onepanel_modules/logic/user_logic.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% install_behaviour callbacks
-export([install/1, uninstall/1, start/1, stop/1, restart/1]).

%% API
-export([local_install/0, local_uninstall/0, local_start/2, local_stop/0, join_cluster/3]).
-export([change_username/3, local_change_username/3, change_password/4, local_change_password/3]).

%% Defines how many times onepanel will try to verify database node start
-define(FINALIZE_START_ATTEMPTS, 10).

%% Defines how long onepanel will wait before next attempt to verify database node start
-define(NEXT_ATTEMPT_DELAY, 1000).

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
            {ok, #?GLOBAL_CONFIG_RECORD{dbs = _}} ->
                throw("Database nodes already configured.");
            _ -> throw("Cannot get database nodes configuration.")
        end,

        Username = proplists:get_value(username, Args),
        Password = proplists:get_value(password, Args),

        {StartOk, StartError} = onepanel_utils:apply_on_hosts(Dbs, ?MODULE, local_start, [Username, Password], ?RPC_TIMEOUT),

        case StartError of
            [] ->
                {_, JoinError} = onepanel_utils:apply_on_hosts(Dbs, ?MODULE, join_cluster, [Username, Password, hd(Dbs)], ?RPC_TIMEOUT),
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
                            {ok, #?GLOBAL_CONFIG_RECORD{dbs = []}} ->
                                throw("Database nodes not configured.");
                            {ok, #?GLOBAL_CONFIG_RECORD{dbs = Dbs}} -> Dbs;
                            _ ->
                                throw("Cannot get database nodes configuration.")
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
                            {ok, #?GLOBAL_CONFIG_RECORD{dbs = []}} ->
                                throw("Database nodes not configured.");
                            {ok, #?GLOBAL_CONFIG_RECORD{dbs = Dbs}} -> Dbs;
                            _ ->
                                throw("Cannot get database nodes configuration.")
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

        "" = os:cmd("rm -rf " ++ ?DB_PREFIX),
        "" = os:cmd("mkdir -p " ++ ?DB_PREFIX),
        "" = os:cmd("cp -R " ++ filename:join([?DB_RELEASE, "* "]) ++ ?DB_PREFIX),

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

        "" = os:cmd("rm -rf " ++ ?DB_PREFIX),
        ok = file:delete(?ULIMITS_CONFIG_PATH),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot uninstall database node on host ~s: ~p", [Host, Reason]),
            {error, Host}
    end.

%% local_start/2
%% ====================================================================
%% @doc Starts database node on local host.
%% @end
-spec local_start(Username :: binary(), Password :: binary()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_start(Username, Password) ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Starting database node"),

        Name = <<(list_to_binary(?DB_NAME))/binary, "@", (list_to_binary(Host))/binary>>,
        Cookie = list_to_binary(?COOKIE),

        ok = installer_utils:overwrite_config_args(?DB_CONFIG, <<"\n-name ">>, <<"[^\n]*">>, Name),
        ok = installer_utils:overwrite_config_args(?DB_CONFIG, <<"\n-setcookie ">>, <<"[^\n]*">>, Cookie),
        ok = installer_utils:add_node_to_config(db_node, list_to_atom(?DB_NAME), ?DB_PREFIX),

        Daemon = filename:join([?DB_PREFIX, ?DB_DAEMON]),
        SetUlimitsCmd = installer_utils:get_system_limits_cmd(Host),
        "" = os:cmd("bash -c \"" ++ SetUlimitsCmd ++ " ; nohup " ++ Daemon ++ " start & 1>/dev/null 2>&1\""),

        {ok, DefaultUsername} = application:get_env(?APP_NAME, default_username),
        {ok, DefaultPassword} = application:get_env(?APP_NAME, default_password),
        ok = finalize_local_start(DefaultUsername, DefaultPassword, ?FINALIZE_START_ATTEMPTS),
        {ok, _} = local_change_username(DefaultUsername, Username, DefaultPassword),
        {ok, _} = local_change_password(Username, DefaultPassword, Password),

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

        "" = os:cmd("kill -TERM `ps aux | grep beam | grep " ++ ?DB_PREFIX ++ " | awk '{print $2}'`"),
        ok = installer_utils:remove_node_from_config(db_node),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot stop database node: ~p", [Reason]),
            {error, Host}
    end.

%% join_cluster/3
%% ====================================================================
%% @doc Adds database host to cluster. ClusterHost is one of current
%% database cluster hosts.
%% @end
-spec join_cluster(Username :: binary(), Password :: binary(), ClusterHost :: string()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
join_cluster(Username, Password, ClusterHost) ->
    Host = onepanel_utils:get_host(node()),
    try
        case ClusterHost of
            Host -> throw(cluster_host);
            _ -> ok
        end,
        URL = "http://" ++ ClusterHost ++ ":" ++ integer_to_list(?DB_PORT) ++ "/nodes/" ++ ?DB_NAME ++ "@" ++ Host,

        {ok, "201", _, ResponseBody} = request(Username, Password, URL, put, <<"{}">>),
        true = proplists:get_value(<<"ok">>, mochijson2:decode(ResponseBody, [{format, proplist}])),

        {ok, Host}
    catch
        _:cluster_host ->
            {ok, Host};
        _:Reason ->
            ?error("Cannot join database cluster: ~p", [Reason]),
            {error, Host}
    end.

%% change_username/3
%% ====================================================================
%% @doc Changes username in administrative database on given hosts.
%% @end
-spec change_username(Hosts :: [string()], Username :: binary(), NewUsername :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
change_username(_, Username, Username) ->
    ok;

change_username(Hosts, Username, NewUsername) ->
    case gen_server:call(?ONEPANEL_SERVER, {get_password, Username}) of
        {ok, Password} when is_binary(Password) ->
            {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(Hosts, ?MODULE, local_change_username, [Username, NewUsername, Password], ?RPC_TIMEOUT),
            case HostsError of
                [] -> ok;
                _ ->
                    ?error("Cannot change username in administrative database for user ~p on hosts: ~p", [Username, HostsError]),
                    onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_change_username, [NewUsername, Username, Password], ?RPC_TIMEOUT),
                    {error, <<"Cannot change username in administrative database.">>}
            end;
        Other ->
            ?error("Cannot get password to administrative database for user ~p: ~p", [Username, Other]),
            {error, <<"Cannot get password to administrative database for user: ", Username/binary>>}
    end.

%% local_change_username/3
%% ====================================================================
%% @doc Changes username in administrative database on local hosts.
%% @end
-spec local_change_username(Username :: binary(), NewUsername :: binary(), Password :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
local_change_username(Username, Username, _) ->
    Host = onepanel_utils:get_host(node()),
    {ok, Host};

local_change_username(Username, NewUsername, Password) ->
    Host = onepanel_utils:get_host(node()),
    try
        URL = "http://" ++ Host ++ ":" ++ integer_to_list(?DB_PORT) ++ "/_config/admins/",

        {ok, "404", _, ResponseBody} = request(Username, Password, URL ++ binary_to_list(NewUsername), get, []),
        <<"not_found">> = proplists:get_value(<<"error">>, mochijson2:decode(ResponseBody, [{format, proplist}])),
        {ok, "200", _, _} = request(Username, Password, URL ++ binary_to_list(NewUsername), put, mochijson2:encode(Password)),
        {ok, "200", _, _} = request(Username, Password, URL ++ binary_to_list(Username), delete, []),

        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot change username in administrative database for user ~p: ~p", [Username, Reason]),
            {error, Reason}
    end.

%% change_password/4
%% ====================================================================
%% @doc Changes password to administrative database on given hosts.
%% @end
-spec change_password(Hosts :: [string()], Username :: binary(), CurrentPassword :: binary(), NewPassword :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
change_password(Hosts, Username, CurrentPassword, NewPassword) ->
    {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(Hosts, ?MODULE, local_change_password, [Username, CurrentPassword, NewPassword], ?RPC_TIMEOUT),
    case HostsError of
        [] -> ok;
        _ ->
            ?error("Cannot change password to administrative database for user ~p on hosts: ~p", [Username, HostsError]),
            onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_change_password, [Username, NewPassword, CurrentPassword], ?RPC_TIMEOUT),
            {error, <<"Cannot change password to administrative database.">>}
    end.

%% local_change_password/3
%% ====================================================================
%% @doc Changes password to administrative database on local host.
%% @end
-spec local_change_password(Username :: binary(), CurrentPassword :: binary(), NewPassword :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
local_change_password(Username, CurrentPassword, NewPassword) ->
    Host = onepanel_utils:get_host(node()),
    URL = "http://" ++ Host ++ ":" ++ integer_to_list(?DB_PORT) ++ "/_config/admins/" ++ binary_to_list(Username),
    case request(Username, CurrentPassword, URL, put, mochijson2:encode(NewPassword)) of
        {ok, "200", _, _} -> {ok, Host};
        Other ->
            ?error("Cannot change password to administrative database for user ~p: ~p", [Username, Other]),
            {error, Host}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% finalize_local_start/3
%% ====================================================================
%% @doc Waits maximally FINALIZE_START_ATTEMPTS
%% @end
-spec finalize_local_start(Username :: binary(), Password :: binary(), Attempts :: integer()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
finalize_local_start(_, _, 0) ->
    ?error("Cannot finalize database node start: attempts limit exceeded."),
    {error, <<"Attempts limit exceeded.">>};

finalize_local_start(Username, Password, Attempts) ->
    Host = onepanel_utils:get_host(node()),
    URL = "http://" ++ Host ++ ":" ++ integer_to_list(?DB_PORT),
    case request(Username, Password, URL, get, []) of
        {ok, "200", _, _} ->
            ok;
        _ ->
            timer:sleep(?NEXT_ATTEMPT_DELAY),
            finalize_local_start(Username, Password, Attempts - 1)
    end.

%% request/5
%% ====================================================================
%% @doc Sends request to database node with default headers and options
%% using REST API.
%% @end
-spec request(Username :: binary(), Password :: binary(), URL :: string(), Method :: atom(), Body :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
request(Username, Password, URL, Method, Body) ->
    Headers = [{"content-type", "application/json"}],
    Options = [{connect_timeout, ?DB_CONNECTION_TIMEOUT}, {basic_auth, {binary_to_list(Username), binary_to_list(Password)}}],
    ibrowse:send_req(URL, Headers, Method, Body, Options).