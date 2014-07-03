#!/usr/bin/env escript
%% -*- erlang -*-

-include("registered_names.hrl").
-include("onepanel_modules/install_logic.hrl").

-define(EXIT_SUCCESS, 0).
-define(EXIT_FAILURE, 1).
-define(NODE, node).

main(Args) ->
    init(),
    case Args of
        ["--install", Config] -> install(Config);
        ["--info"] -> info();
        ["--uninstall"] -> uninstall();
        _ -> print_usage()
    end,
    halt(?EXIT_SUCCESS).

init() ->
    Hostname = "@" ++ os:cmd("hostname -f") -- "\n",
    put(?NODE, erlang:list_to_atom(?APP_STR ++ Hostname)),
    os:cmd(?ERL_LAUNCHER_SCRIPT_PATH ++ " epmd"),
    {A, B, C} = erlang:now(),
    NodeName = "onepanel_setup_" ++ integer_to_list(A, 32) ++ integer_to_list(B, 32) ++ integer_to_list(C, 32) ++ "@127.0.0.1",
    net_kernel:start([list_to_atom(NodeName), longnames]),
    erlang:set_cookie(node(), ?DEFAULT_COOKIE).


install(Config) ->
    try
        {MainCCM, OptCCMs, Workers, Dbs, StoragePaths, OpenFiles, Processes} = parse(Config),
        Node = get(?NODE),
        Hosts = lists:usort(MainCCM ++ OptCCMs ++ Workers ++ Dbs),

        io:format("Checking configuration..."),
        check_hosts(Node, Hosts),
        io:format("\t[ OK ]\n"),

        case StoragePaths of
            [] -> ok;
            _ ->
                io:format("Checking storage availability..."),
                check_storage_paths(Node, StoragePaths, Workers),
                io:format("\t[ OK ]\n")
        end,

        io:format("Setting ulimits..."),
        lists:foreach(fun(Host) ->
            HostOpenFiles = proplists:get_value(Host, OpenFiles, ?DEFAULT_OPEN_FILES),
            HostProcesses = proplists:get_value(Host, Processes, ?DEFAULT_PROCESSES),
            rpc:call(erlang:list_to_atom(?APP_STR ++ "@" ++ Host), install_utils, set_ulimits, [HostOpenFiles, HostProcesses])
        end, Hosts),
        io:format("\t[ OK ]\n"),

        case Dbs of
            [] -> ok;
            _ ->
                io:format("Installing database nodes..."),
                execute(Node, install_db, install, [[{hosts, Dbs}]]),
                io:format("\t[ OK ]\n"),

                io:format("Starting database nodes...  "),
                execute(Node, install_db, start, [[{hosts, Dbs}]]),
                io:format("\t[ OK ]\n")
        end,

        case MainCCM ++ OptCCMs of
            [] -> ok;
            _ ->
                io:format("Installing ccm nodes...     "),
                execute(Node, install_ccm, install, [[{hosts, MainCCM ++ OptCCMs}]]),
                io:format("\t[ OK ]\n"),

                io:format("Starting ccm nodes...       "),
                execute(Node, install_ccm, start, [[{main_ccm, MainCCM}, {opt_ccms, OptCCMs}]]),
                io:format("\t[ OK ]\n")
        end,

        case Workers of
            [] -> ok;
            _ ->
                io:format("Installing worker nodes...  "),
                execute(Node, install_worker, install, [[{hosts, Workers}]]),
                io:format("\t[ OK ]\n"),

                io:format("Adding storage paths...     "),
                lists:foreach(fun(StoragePath) ->
                    execute(Node, install_storage, add_storage_path, [Workers, StoragePath])
                end, StoragePaths),
                io:format("\t[ OK ]\n"),

                io:format("Starting worker nodes...    "),
                execute(Node, install_worker, start, [[{workers, Workers}]]),
                io:format("\t[ OK ]\n")
        end
    catch
        _:{config, Reason} when is_list(Reason) ->
            io:format("\t[FAIL]\nConfiguration error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        _:{hosts, Hosts} when is_list(Hosts) ->
            io:format("\t[FAIL]\Operation failed on following hosts: ~s\n", [Hosts]),
            halt(?EXIT_FAILURE);
        _:{exec, Reason} when is_list(Reason) ->
            io:format("\t[FAIL]\Operation error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        _:_ ->
            io:format("\t[FAIL]\nAn error occurred during operation.\n"),
            halt(?EXIT_FAILURE)
    end.

info() ->
    try
        Node = get(?NODE),

        Terms = rpc:call(Node, install_utils, get_global_config, []),

        io:format("Main CCM node:         ~s\n", [format(proplists:get_value(main_ccm, Terms))]),
        io:format("Optional CCM nodes:    ~s\n", [format(proplists:get_value(opt_ccms, Terms))]),
        io:format("Worker nodes:          ~s\n", [format(proplists:get_value(workers, Terms))]),
        io:format("Database nodes:        ~s\n", [format(proplists:get_value(dbs, Terms))]),
        io:format("Storage paths:         ~s\n", [format(proplists:get_value(storage_paths, Terms))])
    catch
        _:_ ->
            io:format("\nAn error occurred during information gathering.\n"),
            halt(?EXIT_FAILURE)
    end.

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
                execute(Node, install_worker, stop, []),
                io:format("\t[ OK ]\n"),

                io:format("Removing storage paths...     "),
                lists:foreach(fun(StoragePath) ->
                    execute(Node, install_storage, add_storage_path, [Workers, StoragePath])
                end, StoragePaths),
                io:format("\t[ OK ]\n"),

                io:format("Uninstalling worker nodes...  "),
                execute(Node, install_worker, uninstall, [[{hosts, Workers}]]),
                io:format("\t[ OK ]\n")
        end,

        case CCMs of
            [] -> ok;
            _ ->
                io:format("Stopping ccm nodes...         "),
                execute(Node, install_ccm, stop, [[]]),
                io:format("\t[ OK ]\n"),

                io:format("Uninstalling ccm nodes...     "),
                execute(Node, install_ccm, uninstall, [[{hosts, CCMs}]]),
                io:format("\t[ OK ]\n")
        end,

        case Dbs of
            [] -> ok;
            _ ->
                io:format("Stopping database nodes...    "),
                execute(Node, install_db, stop, [[]]),
                io:format("\t[ OK ]\n"),

                io:format("Uninstalling database nodes..."),
                execute(Node, install_db, uninstall, [[{hosts, Dbs}]]),
                io:format("\t[ OK ]\n")
        end
    catch
        _:{hosts, Hosts} when is_list(Hosts) ->
            io:format("\t[FAIL]\Operation failed on following hosts: ~s\n", [Hosts]),
            halt(?EXIT_FAILURE);
        _:{exec, Reason} when is_list(Reason) ->
            io:format("\t[FAIL]\Operation error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        _:_ ->
            io:format("\t[FAIL]\nAn error occurred during operation.\n"),
            halt(?EXIT_FAILURE)
    end.

format(List) when is_list(List) ->
    case string:join(List, ", ") of
        "" -> "undefined";
        String -> String
    end;
format(_) ->
    "undefined".

parse(Config) ->
    {ok, Terms} = file:consult(Config),

    MainCCM = lists:flatten([proplists:get_value("Main CCM host", Terms, [])]),
    OptCCMs = proplists:get_value("Optional CCM hosts", Terms, []),
    Workers = proplists:get_value("Worker hosts", Terms, []),
    Dbs = proplists:get_value("Database hosts", Terms, []),
    StoragePaths = proplists:get_value("Storage paths", Terms, []),
    OpenFiles = proplists:get_value("Open files limit", Terms, []),
    Processes = proplists:get_value("Processes limit", Terms, []),

    {MainCCM, OptCCMs, Workers, Dbs, StoragePaths, OpenFiles, Processes}.

execute(Node, Module, Function, Args) ->
    case rpc:call(Node, Module, Function, Args, ?RPC_TIMEOUT) of
        ok -> ok;
        {error, {hosts, Hosts}} -> throw({hosts, string:join(Hosts, ", ")});
        Other when is_list(Other) -> throw({exec, Other});
        _ -> throw({exec, "Unknow error."})
    end.

check_hosts(Node, Hosts) ->
    ValidHosts = rpc:call(Node, install_utils, get_hosts, []),
    lists:foreach(fun(Host) ->
        case lists:member(Host, ValidHosts) of
            true -> ok;
            false -> throw({config, io_lib:fwrite("Host ~p was not found among available hosts.", [Host])})
        end
    end, Hosts).

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

print_usage() ->
    io:format("Usage: onepanel_setup.escript [options]\n", []),
    io:format("Options:\n"),
    io:format("\t--install <config file>\n"),
    io:format("\t--uninstall\n").