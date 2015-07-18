%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2015 ACK CYFRONET AGH
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

-define(COUCHBASE_PASSWORD, "lkj4n2klfsd90uchn1kadk290").

%% install_behaviour callbacks
-export([install/1, uninstall/1, start/1, stop/1, restart/1, commit/1]).

%% API
-export([local_start/0, local_stop/0, join_cluster/1, local_commit/0, init_cluster/1]).

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
-spec install(Args :: [{Name :: atom(), Value :: term()}]) -> ok.
%% ====================================================================
install(_Args) ->
    ok.

%% uninstall/1
%% ====================================================================
%% @doc Uninstalls database nodes on given hosts. Arguments list should
%% contain list of hosts where database nodes where installed.
%% @end
-spec uninstall(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
uninstall(_Args) ->
    ok.

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
            {error, Reason} ->
                ?error("Cannot get database nodes configuration: ~p", [Reason]),
                throw("Cannot get database nodes configuration.")
        end,

        {StartOk, StartError} = onepanel_utils:apply_on_hosts(Dbs, ?MODULE, local_start, [], ?RPC_TIMEOUT),

        case StartError of
            [] ->
                {_InitOk, InitError} = onepanel_utils:apply_on_hosts([hd(Dbs)], ?MODULE, init_cluster, [hd(Dbs)], ?RPC_TIMEOUT),
                case InitError of
                    [] ->
                        {_, JoinError} = onepanel_utils:apply_on_hosts(Dbs, ?MODULE, join_cluster, [hd(Dbs)], ?RPC_TIMEOUT),
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
                        ?error("Cannot init database nodes on following hosts: ~p", [InitError]),
                        {error, {hosts, InitError}}
                end;
            _ ->
                ?error("Cannot start database nodes on following hosts: ~p", [StartError]),
                onepanel_utils:apply_on_hosts(StartOk, ?MODULE, local_stop, [], ?RPC_TIMEOUT),
                {error, {hosts, StartError}}
        end
    catch
        _:nothing_to_start -> ok;
        _:Error ->
            ?error_stacktrace("Cannot start database nodes: ~p", [Error]),
            {error, Error}
    end.

%% commit/1
%% ====================================================================
%% @doc Performs database cluster commit using one of cluster nodes.
%% @end
-spec commit(Args :: [{Name :: atom(), Value :: term()}]) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
commit(Args) ->
    case proplists:get_value(dbs, Args, []) of
        [] ->
            ok;
        [Db | _] ->
            rpc:call(onepanel_utils:get_node(Db), ?MODULE, local_commit, [])
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
                            {error, Reason} ->
                                ?error("Cannot get database nodes configuration: ~p", [Reason]),
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
        _:Error ->
            ?error_stacktrace("Cannot stop database nodes: ~p", [Error]),
            {error, Error}
    end.

%% restart/1
%% ====================================================================
%% @doc Restarts all database nodes.
%% @end
-spec restart(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok.
%% ====================================================================
restart(_) ->
    ok.

%% ====================================================================
%% API functions
%% ====================================================================

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

        Name = <<(list_to_binary(?DB_NAME))/binary, "@", (list_to_binary(Host))/binary>>,

        ok = installer_utils:overwrite_config_args(?DB_CONFIG,
            <<"nodename = ">>, <<"[^\n]*">>, Name),
        ok = installer_utils:overwrite_config_args(?DB_CONFIG,
            <<"listener.http.internal = ">>, <<"[^\n]*">>, <<"0.0.0.0:8098">>),
        ok = installer_utils:overwrite_config_args(?DB_CONFIG,
            <<"listener.protobuf.internal = ">>, <<"[^\n]*">>, <<"0.0.0.0:8087">>),

        "0" = os:cmd("/etc/init.d/couchbase start 1>/dev/null 2>&1 ; echo -n $?"),
        ok = wait_until("/etc/init.d/couchbase status 1>/dev/null 2>&1 ; echo -n $?", "0"),

        {ok, Host}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot start database node: ~p", [Reason]),
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

        "0" = os:cmd("/etc/init.d/couchbase stop 1>/dev/null 2>&1 ; echo -n $?"),

        {ok, Host}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot stop database node: ~p", [Reason]),
            {error, Host}
    end.

%% local_commit/0
%% ====================================================================
%% @doc Performs database cluster commit on local node.
%% @end
-spec local_commit() -> ok | {error, Reason :: term()}.
%% ====================================================================
local_commit() ->
    try
        ok
    catch
        _:Reason ->
            ?error_stacktrace("Cannot commit database cluster: ~p", [Reason]),
            {error, Reason}
    end.

%% join_cluster/1
%% ====================================================================
%% @doc Adds database host to cluster. ClusterHost is one of current
%% database cluster hosts.
%% @end
-spec join_cluster(ClusterHost :: string()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
join_cluster(ClusterHost) ->
    Host = onepanel_utils:get_host(node()),
    try
        case ClusterHost of
            Host -> throw(cluster_host);
            _ -> ok
        end,

        JoinCommand = "/opt/couchbase/couchbase-cli rebalance -c " ++ ClusterHost ++ ":8091 -u admin -p " ++ ?COUCHBASE_PASSWORD ++
            " --server-add=" ++ Host ++ ":8091 --server-add-username=admin --server-add-password=" ++ ?COUCHBASE_PASSWORD
            ++ " 1>/dev/null 2>&1 ; echo -n $?",
        ?info("Running couchbase command ~p", [JoinCommand]),
        "0" = os:cmd(JoinCommand),

        {ok, Host}
    catch
        _:cluster_host ->
            {ok, Host};
        _:Reason ->
            ?error_stacktrace("Cannot join database cluster: ~p", [Reason]),
            {error, Host}
    end.


%% init_cluster/1
%% ====================================================================
%% @doc Init database cluster. ClusterHost is one of current
%% database cluster hosts.
%% @end
-spec init_cluster(ClusterHost :: string()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
init_cluster(ClusterHost) ->
    Host = onepanel_utils:get_host(node()),
    try
        catch memsup:start_link(),
        {_, Mem} = proplists:lookup(system_total_memory, memsup:get_system_memory_data()),
        MemMB = erlang:round(Mem / 1024 / 1024),
        MemToAllocate = erlang:round(MemMB / 2),

        InitCommand = "/opt/couchbase/couchbase-cli cluster-init -c " ++ ClusterHost ++ ":8091 --cluster-init-username=admin" ++
            " --cluster-init-password=" ++ ?COUCHBASE_PASSWORD ++ " --cluster-init-ramsize=" ++ integer_to_list(MemToAllocate) ++
            " 1>/dev/null 2>&1 ; echo -n $?",
        ?info("Running couchbase command ~p", [InitCommand]),
        "0" = os:cmd(InitCommand),

        BucketCommand = "/opt/couchbase/couchbase-cli bucket-create -c " ++ ClusterHost ++ ":8091" ++
            " -u admin -p " ++ ?COUCHBASE_PASSWORD ++ " --bucket=default --bucket-ramsize=" ++ integer_to_list(MemToAllocate) ++
            " 1>/dev/null 2>&1 ; echo -n $?",
        ?info("Running couchbase command ~p", [BucketCommand]),
        "0" = os:cmd(BucketCommand),

        {ok, Host}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot init database cluster: ~p", [Reason]),
            {error, Host}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% wait_until/2
%% ====================================================================
%% @private
%% @doc
%% @equiv wait_until(Command, Output, ?FINALIZE_START_ATTEMPTS)
%% @end
-spec wait_until(Command :: string(), Output :: string()) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
wait_until(Command, Output) ->
    wait_until(Command, Output, ?FINALIZE_START_ATTEMPTS).

%% wait_until/3
%% ====================================================================
%% @private
%% @doc
%% Waits given number of attempts for a 'Command' to return 'Output' value.
%% @end
-spec wait_until(Command :: string(), Output :: string(), Attempt :: integer()) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
wait_until(_, _, 0) ->
    ?error("Cannot finalize database node start: attempts limit exceeded."),
    {error, <<"Attempts limit exceeded.">>};

wait_until(Command, Output, N) ->
    case re:run(os:cmd(Command), Output, [{capture, first, list}]) of
        {match, [_]} ->
            ok;
        _ ->
            timer:sleep(?NEXT_ATTEMPT_DELAY),
            wait_until(Command, Output, N - 1)
    end.
