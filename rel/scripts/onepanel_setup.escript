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
-define(DEFAULT_OPEN_FILES, "65535").
-define(DEFAULT_PROCESSES, "65535").

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
        ["--info"] -> info();
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
        {MainCCM, OptCCMs, Workers, Dbs, StoragePaths, OpenFiles, Processes} = parse(Config),
        Node = get(?NODE),
        Hosts = lists:usort(MainCCM ++ OptCCMs ++ Workers ++ Dbs),

        io:format("Checking configuration...       "),
        check_hosts(Node, Hosts),
        print_ok(),

        case StoragePaths of
            [] -> ok;
            _ ->
                io:format("Checking storage availability..."),
                check_storage_paths(Node, StoragePaths, Workers),
                print_ok()
        end,

        io:format("Setting ulimits...              "),
        lists:foreach(fun(Host) ->
            HostOpenFiles = proplists:get_value(Host, OpenFiles, ?DEFAULT_OPEN_FILES),
            HostProcesses = proplists:get_value(Host, Processes, ?DEFAULT_PROCESSES),
            rpc:call(erlang:list_to_atom(?APP_STR ++ "@" ++ Host), install_utils, set_ulimits, [HostOpenFiles, HostProcesses])
        end, Hosts),
        print_ok(),

        case Dbs of
            [] -> ok;
            _ ->
                io:format("Installing database nodes...    "),
                execute(Node, install_db, install, [[{hosts, Dbs}]]),
                print_ok(),

                io:format("Starting database nodes...      "),
                execute(Node, install_db, start, [[{hosts, Dbs}]]),
                print_ok()
        end,

        case MainCCM ++ OptCCMs of
            [] -> ok;
            _ ->
                io:format("Installing ccm nodes...         "),
                execute(Node, install_ccm, install, [[{hosts, MainCCM ++ OptCCMs}]]),
                print_ok(),

                io:format("Starting ccm nodes...           "),
                case MainCCM of
                    [] ->
                        execute(Node, install_ccm, start, [[{opt_ccms, OptCCMs}]]);
                    _ ->
                        execute(Node, install_ccm, start, [[{main_ccm, erlang:hd(MainCCM)}, {opt_ccms, OptCCMs}]])
                end,
                print_ok()
        end,

        case Workers of
            [] -> ok;
            _ ->
                io:format("Installing worker nodes...      "),
                execute(Node, install_worker, install, [[{hosts, Workers}]]),
                print_ok(),

                io:format("Adding storage paths...         "),
                lists:foreach(fun(StoragePath) ->
                    execute(Node, install_storage, add_storage_path, [Workers, StoragePath])
                end, StoragePaths),
                print_ok(),

                io:format("Starting worker nodes...        "),
                execute(Node, install_worker, start, [[{workers, Workers}]]),
                print_ok()
        end
    catch
        _:{config, Reason} when is_list(Reason) ->
            print_error("Configuration error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        _:{hosts, ErrorHosts} when is_list(ErrorHosts) ->
            print_error("Operation failed on following hosts: ~s\n", [ErrorHosts]),
            halt(?EXIT_FAILURE);
        _:{exec, Reason} when is_list(Reason) ->
            print_error("Operation error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        _:_ ->
            print_error("An error occurred during operation.\n", []),
            halt(?EXIT_FAILURE)
    end.


%% info/0
%% ====================================================================
%% @doc Displays current installation configuration.
%% @end
-spec info() -> ok.
%% ====================================================================
info() ->
    try
        Node = get(?NODE),

        Terms = rpc:call(Node, install_utils, get_global_config, []),

        io:format("Main CCM node:         ~s\n", [format(proplists:get_value(main_ccm, Terms))]),
        io:format("Optional CCM nodes:    ~s\n", [format({hosts, proplists:get_value(opt_ccms, Terms)})]),
        io:format("Worker nodes:          ~s\n", [format({hosts, proplists:get_value(workers, Terms)})]),
        io:format("Database nodes:        ~s\n", [format({hosts, proplists:get_value(dbs, Terms)})]),
        io:format("Storage paths:         ~s\n", [format({hosts, proplists:get_value(storage_paths, Terms)})])
    catch
        _:_ ->
            io:format("An error occurred during information gathering.\n"),
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

        Terms = rpc:call(Node, install_utils, get_global_config, []),

        CCMs = proplists:get_value(opt_ccms, Terms, []) ++
            case proplists:get_value(main_ccm, Terms) of
                undefined -> [];
                MainCCM -> [MainCCM]
            end,
        Workers = proplists:get_value(workers, Terms, []),
        Dbs = proplists:get_value(dbs, Terms, []),
        StoragePaths = proplists:get_value(storage_paths, Terms, []),

        case Workers of
            [] -> ok;
            _ ->
                io:format("Stopping worker nodes...      "),
                execute(Node, install_worker, stop, [[]]),
                print_ok(),

                io:format("Removing storage paths...     "),
                lists:foreach(fun(StoragePath) ->
                    execute(Node, install_storage, remove_storage_path, [Workers, StoragePath])
                end, StoragePaths),
                print_ok(),

                io:format("Uninstalling worker nodes...  "),
                execute(Node, install_worker, uninstall, [[{hosts, Workers}]]),
                print_ok()
        end,

        case CCMs of
            [] -> ok;
            _ ->
                io:format("Stopping ccm nodes...         "),
                execute(Node, install_ccm, stop, [[]]),
                print_ok(),

                io:format("Uninstalling ccm nodes...     "),
                execute(Node, install_ccm, uninstall, [[{hosts, CCMs}]]),
                print_ok()
        end,

        case Dbs of
            [] -> ok;
            _ ->
                io:format("Stopping database nodes...    "),
                execute(Node, install_db, stop, [[]]),
                print_ok(),

                io:format("Uninstalling database nodes..."),
                execute(Node, install_db, uninstall, [[{hosts, Dbs}]]),
                print_ok()
        end
    catch
        _:{hosts, Hosts} when is_list(Hosts) ->
            print_error("Operation failed on following hosts: ~s\n", [Hosts]),
            halt(?EXIT_FAILURE);
        _:{exec, Reason} when is_list(Reason) ->
            print_error("Operation error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        _:_ ->
            print_error("An error occurred during operation.\n", []),
            halt(?EXIT_FAILURE)
    end.


%% format/1
%% ====================================================================
%% @doc Helper function for info/0. In case of list of hosts, converts
%% it comma-delimited string. In case of string returns it. For other
%% cases return "undefined".
%% @end
-spec format(Term :: term()) -> ok.
%% ====================================================================
format({hosts, Hosts}) when is_list(Hosts) ->
    case string:join(Hosts, ", ") of
        "" -> "undefined";
        String -> String
    end;
format(Other) when is_list(Other) ->
    Other;
format(_) ->
    "undefined".


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

    MainCCM = case proplists:get_value("Main CCM host", Terms) of
                  undefined -> [];
                  Host -> [Host]
              end,
    OptCCMs = proplists:get_value("Optional CCM hosts", Terms, []),
    Workers = proplists:get_value("Worker hosts", Terms, []),
    Dbs = proplists:get_value("Database hosts", Terms, []),
    StoragePaths = proplists:get_value("Storage paths", Terms, []),
    OpenFiles = proplists:get_value("Open files limit", Terms, []),
    Processes = proplists:get_value("Processes limit", Terms, []),

    {MainCCM, OptCCMs, Workers, Dbs, StoragePaths, OpenFiles, Processes}.


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
        {error, {hosts, Hosts}} -> throw({hosts, string:join(Hosts, ", ")});
        Other when is_list(Other) -> throw({exec, Other});
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
    ValidHosts = rpc:call(Node, install_utils, get_hosts, []),
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
        case rpc:call(Node, install_storage, check_storage_path_on_hosts, [Workers, StoragePath]) of
            ok -> ok;
            {error, Hosts} ->
                throw({config, io_lib:fwrite("Storage ~p in not available on following hosts: ~s", [StoragePath, string:join(Hosts, ", ")])})
        end
    end, StoragePaths);
check_storage_paths(_, _, _) ->
    throw({config, "Wrong storage paths format."}).


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
    io:format("\t--info\n"),
    io:format("\t--uninstall\n").


%% print_ok/0
%% ====================================================================
%% @doc Prints ok information for given step.
%% @end
-spec print_ok() -> ok.
%% ====================================================================
print_ok() ->
    io:format("\t[ OK ]\n").


%% print_error/0
%% ====================================================================
%% @doc Prints error information for given step.
%% @end
-spec print_error(Format :: string(), Args :: [term()]) -> ok.
%% ====================================================================
print_error(Format, Args) ->
    io:format("\t[FAIL]\n"),
    io:format(Format, Args).