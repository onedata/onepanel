%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements {@link installer_behaviour} callbacks and
%% provides API methods for worker nodes installation.
%% @end
%% ===================================================================
-module(installer_worker).
-behaviour(installer_behaviour).

-include("registered_names.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% install_behaviour callbacks
-export([install/1, uninstall/1, start/1, stop/1, restart/1]).

%% API
-export([local_start/4, local_stop/0, local_restart/0]).

%% ====================================================================
%% Behaviour callback functions
%% ====================================================================

%% install/1
%% ====================================================================
%% @doc Installs worker nodes on given hosts.
%% @end
-spec install(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
install(_) ->
    ok.

%% uninstall/1
%% ====================================================================
%% @doc Uninstalls worker nodes on given hosts.
%% @end
-spec uninstall(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
uninstall(_) ->
    ok.

%% start/1
%% ====================================================================
%% @doc Starts worker nodes on given hosts. Argument list should contain
%% host where main CM node was configured and also list of hosts where
%% database configured and worker nodes installed.
%% @end
-spec start(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
start(Args) ->
    try
        NewWorkers = case proplists:get_value(workers, Args, []) of
            [] -> throw(nothing_to_start);
            Hosts -> Hosts
        end,

        {ConfiguredMainCM, ConfiguredCMs, ConfiguredDbs, ConfiguredWorkers, DefaultArgs} =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{cms = []}} ->
                    throw("CM nodes not configured.");
                {ok, #?GLOBAL_CONFIG_RECORD{main_cm = MainCM, cms = CMs, dbs = Dbs, workers = Workers, args = Defaults}} ->
                    {MainCM, CMs, Dbs, Workers, Defaults};
                _ -> throw("Cannot get CM nodes configuration.")
            end,

        lists:foreach(fun(Worker) ->
            case lists:member(Worker, ConfiguredWorkers) of
                true -> throw("Worker " ++ Worker ++ " already configured.");
                _ -> ok
            end
        end, NewWorkers),

        ConfiguredOptCMs = lists:delete(ConfiguredMainCM, ConfiguredCMs),
        MergedArgs = maps:to_list(maps:merge(maps:from_list(DefaultArgs), maps:from_list(Args))),

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(NewWorkers, ?MODULE, local_start,
            [ConfiguredMainCM, ConfiguredOptCMs, ConfiguredDbs, MergedArgs], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID,
                    [{workers, ConfiguredWorkers ++ NewWorkers}, {args, MergedArgs}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update worker nodes configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts(NewWorkers, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                        {error, {hosts, NewWorkers}}
                end;
            _ ->
                ?error("Cannot start worker nodes on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:nothing_to_start -> ok;
        _:Reason ->
            ?error_stacktrace("Cannot start worker nodes: ~p", [Reason]),
            {error, Reason}
    end.

%% stop/1
%% ====================================================================
%% @doc Stops worker nodes on given hosts.
%% @end
-spec stop(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
stop(Args) ->
    try
        {ConfiguredMainCM, ConfiguredCMs, ConfiguredDbs, ConfiguredWorkers} =
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, #?GLOBAL_CONFIG_RECORD{main_cm = MainCM, cms = CMs, dbs = Dbs, workers = Workers}} ->
                    {MainCM, CMs, Dbs, Workers};
                _ -> throw("Cannot get CM nodes configuration.")
            end,

        WorkersToStop = case proplists:get_value(workers, Args) of
            undefined -> ConfiguredWorkers;
            Hosts ->
                lists:foreach(fun(Host) ->
                    case lists:member(Host, ConfiguredWorkers) of
                        false ->
                            throw("Worker " ++ Host ++ " is not configured.");
                        _ -> ok
                    end
                end, Hosts),
                Hosts
        end,

        ConfiguredOptCMs = lists:delete(ConfiguredMainCM, ConfiguredCMs),

        {HostsOk, HostsError} = onepanel_utils:apply_on_hosts(WorkersToStop, ?MODULE, local_stop, [], ?RPC_TIMEOUT),

        case HostsError of
            [] ->
                case dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{workers, ConfiguredWorkers -- WorkersToStop}]) of
                    ok -> ok;
                    Other ->
                        ?error("Cannot update worker nodes configuration: ~p", [Other]),
                        onepanel_utils:apply_on_hosts(WorkersToStop, ?MODULE, local_start,
                            [ConfiguredMainCM, ConfiguredOptCMs, ConfiguredDbs, Args], ?RPC_TIMEOUT),
                        {error, {hosts, WorkersToStop}}
                end;
            _ ->
                ?error("Cannot stop worker nodes on following hosts: ~p", [HostsError]),
                onepanel_utils:apply_on_hosts(HostsOk, ?MODULE, local_start,
                    [ConfiguredMainCM, ConfiguredOptCMs, ConfiguredDbs, Args], ?RPC_TIMEOUT),
                {error, {hosts, HostsError}}
        end
    catch
        _:Reason ->
            ?error_stacktrace("Cannot stop worker nodes: ~p", [Reason]),
            {error, Reason}
    end.

%% restart/1
%% ====================================================================
%% @doc Restarts worker nodes on given hosts.
%% @end
-spec restart(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
restart(Args) ->
    try
        ConfiguredWorkers = case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
            {ok, #?GLOBAL_CONFIG_RECORD{workers = Workers}} ->
                Workers;
            _ ->
                throw("Cannot get CM nodes configuration.")
        end,

        WorkersToRestart = case proplists:get_value(workers, Args) of
            undefined -> ConfiguredWorkers;
            Hosts -> Hosts
        end,

        case stop([{workers, WorkersToRestart}]) of
            ok -> start([{workers, WorkersToRestart}]);
            Other -> Other
        end
    catch
        _:Reason ->
            ?error_stacktrace("Cannot restart worker nodes: ~p", [Reason]),
            {error, Reason}
    end.

%% ====================================================================
%% API functions
%% ====================================================================

%% local_start/4
%% ====================================================================
%% @doc Starts worker node on local host.
%% @end
-spec local_start(MainCM :: string(), OptCMs :: [string()],
    Dbs :: [string()], Args :: list()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_start(MainCM, OptCMs, Dbs, Args) ->
    Host = onepanel_utils:get_host(node()),
    {ok, AppName} = application:get_env(?APP_NAME, application_name),
    try
        ?debug("Starting worker node: ~p"),

        OzDomain = proplists:get_value(oz_domain, Args, "onedata.org"),

        release_configurator:configure_release(
            ?SOFTWARE_NAME,
            default,
            [
                {?SOFTWARE_NAME, [
                    {cm_nodes, [list_to_atom(?CM_NAME ++ "@" ++ CM) || CM <- [MainCM | OptCMs]]},
                    {db_nodes, [list_to_atom(Db ++ ":" ++ integer_to_list(?DB_PORT)) || Db <- Dbs]},
                    {verify_oz_cert, application:get_env(?APP_NAME, verify_oz_cert, true)}
                ] ++ case AppName of
                    oneprovider ->
                        [
                            {provider_domain, proplists:get_value(op_domain, Args, "localhost.local")},
                            {oz_domain, OzDomain}
                        ];
                    onezone ->
                        [
                            {oz_name, proplists:get_value(oz_name, Args, "unknown")},
                            {http_domain, OzDomain}
                        ];
                    _ -> []
                end}
            ],
            [
                {name, ?WORKER_NAME ++ "@" ++ Host},
                {setcookie, atom_to_list(erlang:get_cookie())}
            ]
        ),

        OzUrl = "https://" ++ OzDomain ++ ":8443",
        application:set_env(?APP_NAME, onezone_url, OzUrl),
        ok = app_config:set(?APP_NAME, onezone_url, OzUrl),

        copy_certs(AppName, Args),
        copy_dns_config(AppName, Args),
        copy_auth_config(AppName, Args),

        ServiceStart = "service " ++ atom_to_list(?SOFTWARE_NAME) ++ " start 2>1 1>/dev/null",
        SetUlimitsCmd = installer_utils:get_system_limits_cmd(Host),
        "0" = os:cmd("bash -c \"" ++ SetUlimitsCmd ++ " ; " ++ ServiceStart ++ " ; echo -n $?\""),
        {ok, Host}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot start worker node: ~p", [Reason]),
            {error, Host}
    end.

%% local_stop/0
%% ====================================================================
%% @doc Stops worker node on local host.
%% @end
-spec local_stop() -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
local_stop() ->
    Host = onepanel_utils:get_host(node()),
    try
        ?debug("Stopping worker node on host: ~p", [Host]),

        ServiceStop = "service " ++ atom_to_list(?SOFTWARE_NAME) ++ " stop 2>1 1>/dev/null",
        "0" = os:cmd(ServiceStop ++ " ; echo -n $?"),

        {ok, Host}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot stop worker node: ~p", [Reason]),
            {error, Host}
    end.

%% local_restart/0
%% ====================================================================
%% @doc Restarts worker node on local host.
%% @end
-spec local_restart() -> Result when
    Result :: {ok, Host :: string()} | {error, Reason :: term()}.
%% ====================================================================
local_restart() ->
    Host = onepanel_utils:get_host(node()),
    case restart([{workers, [Host]}]) of
        ok -> {ok, Host};
        _ -> {error, Host}
    end.

%%--------------------------------------------------------------------
%% @doc Tries to copy certificates.
%% @end
%%--------------------------------------------------------------------
-spec copy_certs(AppName :: atom(), Args :: list()) -> ok.
copy_certs(oneprovider, Args) ->
    copy_file(web_cert, "/etc/op_worker/certs/onedataServerWeb.pem", Args);
copy_certs(onezone, Args) ->
    copy_file(web_key, "/etc/op_worker/certs/gui_key.pem", Args),
    copy_file(web_cert, "/etc/op_worker/certs/gui_cert.pem", Args),
    copy_file(web_ca_cert, "/etc/op_worker/cacerts/gui_cacert.pem", Args);
copy_certs(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Tries to copy DNS config.
%% @end
%%--------------------------------------------------------------------
-spec copy_dns_config(AppName :: atom(), Args :: list()) -> ok.
copy_dns_config(onezone, Args) ->
    copy_file(dns_config, "/var/lib/oz_worker/dns.config", Args);
copy_dns_config(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Tries to copy auth config.
%% @end
%%--------------------------------------------------------------------
-spec copy_auth_config(AppName :: atom(), Args :: list()) -> ok.
copy_auth_config(onezone, Args) ->
    copy_file(auth_config, "/var/lib/oz_worker/auth.config", Args);
copy_auth_config(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Tries to download/copy file to given destination.
%% @end
%%--------------------------------------------------------------------
-spec copy_file(AppName :: atom(), Path :: string(), Args :: list()) -> ok.
copy_file(Name, Path, Args) ->
    case proplists:get_value(Name, Args) of
        undefined -> ok;
        Value ->
            ?info("Copying file ~p from ~p to ~p", [Name, Value, Path]),
            try
                {ok, 200, _, Content} = http_client:get(Value),
                file:write_file(Path, Content)
            catch
                _:Reason ->
                    Ans = os:cmd("cp " ++ Value ++ " " ++ Path),
                    ?error_stacktrace("File download failed: ~p. Copy attempt "
                    "returned: ~s", [Reason, Ans])
            end,
            ok
    end.