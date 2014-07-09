#!/usr/bin/env escript
%% -*- erlang -*-

%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This script interacts with Onepanel nodes and provides
%% management functions for VeilCluster nodes.
%% @end
%% ===================================================================

%% String version of applicaton name
-define(APP_STR, "onepanel").

%% Default cookie used for communication with cluster
-define(DEFAULT_COOKIE, veil_cluster_node).

% Default system limit values
-define(DEFAULT_OPEN_FILES, 65535).
-define(DEFAULT_PROCESSES, 65535).

% Installation directory of veil RPM
-define(PREFIX, "/opt/veil/").

%% Location of erl_launcher
-define(ERL_LAUNCHER_SCRIPT_PATH, ?PREFIX ++ "scripts/erl_launcher").

%% Timeout for each RPC call
-define(RPC_TIMEOUT, 60000).

%% Exit codes
-define(EXIT_SUCCESS, 0).
-define(EXIT_FAILURE, 1).

%% Local Onepanel node
-define(NODE, local_node).


%% ====================================================================
%% API functions
%% ====================================================================

%% main/1
%% ====================================================================
%% @doc Script entry function.
%% @end
-spec main(Args :: [string()]) -> no_return().
%% ====================================================================
main(Args) ->
    init(),
    case Args of
        ["--install", Config] -> install(Config);
        ["--config"] -> config();
        ["--uninstall"] -> uninstall();
        _ -> print_usage()
    end,
    halt(?EXIT_SUCCESS).


%% init/0
%% ====================================================================
%% @doc Sets up net kernel and establishes connection to VeilCluster.
%% @end
-spec init() -> ok.
%% ====================================================================
init() ->
    Hostname = "@" ++ os:cmd("hostname -f") -- "\n",
    put(?NODE, erlang:list_to_atom(?APP_STR ++ Hostname)),
    os:cmd(?ERL_LAUNCHER_SCRIPT_PATH ++ " epmd"),
    {A, B, C} = erlang:now(),
    NodeName = "onepanel_setup_" ++ integer_to_list(A, 32) ++ integer_to_list(B, 32) ++ integer_to_list(C, 32) ++ "@127.0.0.1",
    net_kernel:start([list_to_atom(NodeName), longnames]),
    erlang:set_cookie(node(), ?DEFAULT_COOKIE).


%% install/1
%% ====================================================================
%% @doc Applies installation preferences read from configuration file.
%% @end
-spec install(Config :: string()) -> ok.
%% ====================================================================
install(Config) ->
    try
        {MainCCM, CCMs, Workers, Dbs, StoragePaths, OpenFiles, Processes} = parse(Config),
        Node = get(?NODE),
        AllHosts = lists:usort(CCMs ++ Workers ++ Dbs),

        print_info("Checking configuration..."),
        check_hosts(Node, AllHosts),
        print_ok(),

        print_info("Checking storage availability..."),
        check_storage_paths(Node, StoragePaths, Workers),
        print_ok(),

        print_info("Setting ulimits..."),
        lists:foreach(fun(Host) ->
            HostOpenFiles = proplists:get_value(Host, OpenFiles, ?DEFAULT_OPEN_FILES),
            HostProcesses = proplists:get_value(Host, Processes, ?DEFAULT_PROCESSES),
            rpc:call(erlang:list_to_atom(?APP_STR ++ "@" ++ Host), installer_utils, set_ulimits, [HostOpenFiles, HostProcesses])
        end, AllHosts),
        print_ok(),

        print_info("Installing database nodes..."),
        execute(Node, installer_db, install, [[{dbs, Dbs}]]),
        print_ok(),

        print_info("Starting database nodes..."),
        execute(Node, installer_db, start, [[{dbs, Dbs}]]),
        print_ok(),

        print_info("Installing ccm nodes..."),
        execute(Node, installer_ccm, install, [[{ccms, CCMs}]]),
        print_ok(),

        print_info("Starting ccm nodes..."),
        execute(Node, installer_ccm, start, [[{main_ccm, MainCCM}, {ccms, CCMs}]]),
        print_ok(),

        print_info("Installing worker nodes..."),
        execute(Node, installer_worker, install, [[{workers, Workers}]]),
        print_ok(),

        print_info("Adding storage paths..."),
        execute(Node, installer_storage, add_storage_paths_to_db, [[{storage_paths, StoragePaths}]]),
        print_ok(),

        print_info("Starting worker nodes..."),
        execute(Node, installer_worker, start, [[{workers, Workers}]]),
        print_ok()
    catch
        _:{config, Reason} when is_list(Reason) ->
            print_error("Configuration error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        _:{hosts, Hosts} when is_list(Hosts) ->
            io:format("[FAIL]\n"),
            format_hosts("Operation failed on following hosts:", Hosts),
            halt(?EXIT_FAILURE);
        _:{exec, Reason} when is_list(Reason) ->
            print_error("Operation error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        _:_ ->
            print_error("An error occurred during operation.\n", []),
            halt(?EXIT_FAILURE)
    end.


%% config/0
%% ====================================================================
%% @doc Displays current installation configuration.
%% @end
-spec config() -> ok.
%% ====================================================================
config() ->
    try
        Node = get(?NODE),

        Terms = rpc:call(Node, installer_utils, get_global_config, []),

        format_host("Main CCM node:", proplists:get_value(main_ccm, Terms)),
        format_hosts("CCM nodes:", proplists:get_value(ccms, Terms)),
        format_hosts("Worker nodes:", proplists:get_value(workers, Terms)),
        format_hosts("Database nodes:", proplists:get_value(dbs, Terms)),
        format_hosts("Storage paths:", proplists:get_value(storage_paths, Terms))
    catch
        _:_ ->
            io:format("Cannot get current installation configuration.\n"),
            halt(?EXIT_FAILURE)
    end.


%% uninstall/0
%% ====================================================================
%% @doc Uninstalls all currently configured components.
%% @end
-spec uninstall() -> ok.
%% ====================================================================
uninstall() ->
    try
        Node = get(?NODE),

        Terms = rpc:call(Node, installer_utils, get_global_config, []),

        CCMs = proplists:get_value(ccms, Terms, []),
        Workers = proplists:get_value(workers, Terms, []),
        Dbs = proplists:get_value(dbs, Terms, []),
        StoragePaths = proplists:get_value(storage_paths, Terms, []),

        print_info("Stopping worker nodes..."),
        execute(Node, installer_worker, stop, [[]]),
        print_ok(),

        print_info("Removing storage paths..."),
        execute(Node, installer_storage, remove_storage_paths_from_db, [[{storage_paths, StoragePaths}]]),
        print_ok(),

        print_info("Uninstalling worker nodes..."),
        execute(Node, installer_worker, uninstall, [[{workers, Workers}]]),
        print_ok(),

        print_info("Stopping ccm nodes..."),
        execute(Node, installer_ccm, stop, [[]]),
        print_ok(),

        print_info("Uninstalling ccm nodes..."),
        execute(Node, installer_ccm, uninstall, [[{ccms, CCMs}]]),
        print_ok(),

        print_info("Stopping database nodes..."),
        execute(Node, installer_db, stop, [[]]),
        print_ok(),

        print_info("Uninstalling database nodes..."),
        execute(Node, installer_db, uninstall, [[{dbs, Dbs}]]),
        print_ok()
    catch
        _:{hosts, Hosts} when is_list(Hosts) ->
            io:format("[FAIL]\n"),
            format_hosts("Operation failed on following hosts:", Hosts),
            halt(?EXIT_FAILURE);
        _:{exec, Reason} when is_list(Reason) ->
            print_error("Operation error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        _:_ ->
            print_error("An error occurred during operation.\n", []),
            halt(?EXIT_FAILURE)
    end.


%% parse/1
%% ====================================================================
%% @doc Parses installation preferences read from configuration file.
%% @end
-spec parse(Config :: string()) -> Result when
    Result :: {
        MainCCM :: string(),
        OptCCMs :: [string()],
        Workers :: [string()],
        Dbs :: [string()],
        StoragePaths :: [string()],
        OpenFiles :: [{Host :: string(), Value :: integer()}],
        Processes :: [{Host :: string(), Value :: integer()}]
    }.
%% ====================================================================
parse(Config) ->
    {ok, Terms} = file:consult(Config),

    MainCCM = proplists:get_value("Main CCM host", Terms),
    CCMs = proplists:get_value("CCM hosts", Terms, []),
    Workers = proplists:get_value("Worker hosts", Terms, []),
    Dbs = proplists:get_value("Database hosts", Terms, []),
    StoragePaths = proplists:get_value("Storage paths", Terms, []),
    OpenFiles = proplists:get_value("Open files limit", Terms, []),
    Processes = proplists:get_value("Processes limit", Terms, []),

    {MainCCM, CCMs, Workers, Dbs, StoragePaths, OpenFiles, Processes}.


%% execute/4
%% ====================================================================
%% @doc Executes given function on given node via RPC call. Returns 'ok'
%% if function returns 'ok', otherwise throws an exception.
%% @end
-spec execute(Node :: atom(), Module :: module(), Function :: atom(), Args :: term()) -> ok | no_return().
%% ====================================================================
execute(Node, Module, Function, Args) ->
    case rpc:call(Node, Module, Function, Args, ?RPC_TIMEOUT) of
        ok -> ok;
        {error, {hosts, Hosts}} -> throw({hosts, Hosts});
        {error, Error} when is_list(Error) -> throw({exec, Error});
        _ -> throw({exec, "Unknow error."})
    end.


%% check_hosts/2
%% ====================================================================
%% @doc Checks whether all hosts mentioned in configuration file are
%% available for further operations.
%% @end
-spec check_hosts(Node :: atom(), Hosts :: [string()]) -> ok | no_return().
%% ====================================================================
check_hosts(Node, Hosts) ->
    ValidHosts = rpc:call(Node, installer_utils, get_hosts, []),
    lists:foreach(fun(Host) ->
        case lists:member(Host, ValidHosts) of
            true -> ok;
            false -> throw({config, io_lib:fwrite("Host ~p was not found among available hosts.", [Host])})
        end
    end, Hosts).


%% check_storage_paths/3
%% ====================================================================
%% @doc Checks whether all storage paths are available for all workers.
%% @end
-spec check_storage_paths(Node :: atom(), StoragePaths :: [string()], Workers :: [string()]) -> ok | no_return().
%% ====================================================================
check_storage_paths(Node, StoragePaths, Workers) when is_list(StoragePaths) ->
    lists:foreach(fun(StoragePath) ->
        case rpc:call(Node, installer_storage, check_storage_path_on_hosts, [Workers, StoragePath]) of
            ok -> ok;
            {error, Hosts} ->
                throw({config, io_lib:fwrite("Storage ~p in not available on following hosts: ~s", [StoragePath, string:join(Hosts, ", ")])})
        end
    end, StoragePaths);
check_storage_paths(_, _, _) ->
    throw({config, "Wrong storage paths format."}).


%% format/2
%% ====================================================================
%% @doc Prints indented string message with given prefix.
%% @end
-spec format(Prefix :: string(), String :: string()) -> ok.
%% ====================================================================
format(Prefix, String) ->
    io:format("~-40s~s", [Prefix, String]).


%% formatln/2
%% ====================================================================
%% @doc Same as format/2, but adds new line add the end of string.
%% @end
-spec formatln(Prefix :: string(), String :: string()) -> ok.
%% ====================================================================
formatln(Prefix, String) ->
    format(Prefix, String),
    io:format("~n", []).


%% format_host/2
%% ====================================================================
%% @doc Formats information about host.
%% @end
-spec format_host(Prefix :: string(), String :: string()) -> ok.
%% ====================================================================
format_host(Prefix, undefined) ->
    formatln(Prefix, "undefined");
format_host(Prefix, Host) ->
    formatln(Prefix, Host).


%% format_hosts/2
%% ====================================================================
%% @doc Formats information about hosts.
%% @end
-spec format_hosts(Prefix :: string(), String :: string()) -> ok.
%% ====================================================================
format_hosts(Prefix, []) ->
    format_host(Prefix, undefined);
format_hosts(Prefix, [Host | Hosts]) ->
    format_host(Prefix, Host),
    lists:foreach(fun(H) ->
        format_host("", H)
    end, Hosts).


%% print_usage/0
%% ====================================================================
%% @doc Prints available script options.
%% @end
-spec print_usage() -> ok.
%% ====================================================================
print_usage() ->
    io:format("Usage: onepanel_setup.escript [options]\n", []),
    io:format("Options:\n"),
    io:format("\t--install <config file>\n"),
    io:format("\t--config\n"),
    io:format("\t--uninstall\n").


%% print_info/1
%% ====================================================================
%% @doc Prints information for given step.
%% @end
-spec print_info(Message :: string()) -> ok.
%% ====================================================================
print_info(Message) ->
    format(Message, "").


%% print_ok/0
%% ====================================================================
%% @doc Prints 'ok' for given step.
%% @end
-spec print_ok() -> ok.
%% ====================================================================
print_ok() ->
    io:format("[ OK ]\n").


%% print_error/0
%% ====================================================================
%% @doc Prints error message for given step.
%% @end
-spec print_error(Format :: string(), Args :: [term()]) -> ok.
%% ====================================================================
print_error(Format, Args) ->
    io:format("[FAIL]\n"),
    io:format(Format, Args).