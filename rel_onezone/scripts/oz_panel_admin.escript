#!/usr/bin/env escript
%% -*- erlang -*-

%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This script interacts with onepanel nodes and provides
%% management functions for software components.
%% @end
%% ===================================================================

%% String version of applicaton name
-define(APP_STR, "onepanel").

%% Default cookie used for communication with cluster
-define(COOKIE, {{cookie}}).

%% Default system limit values
-define(OPEN_FILES, 65535).
-define(PROCESSES, 65535).

%% Timeout for each RPC call
-define(RPC_TIMEOUT, timer:minutes(5)).

%% Exit codes
-define(EXIT_SUCCESS, 0).
-define(EXIT_FAILURE, 1).

%% Error logs filename
-define(LOG_FILE, os:getenv("RUNNER_LOG_DIR") ++ "op_panel_admin.log").

%% Local onepanel node
-define(NODE, setup_node).

%% config record contains following fields:
%% * main_cm            - hostname of machine where main CM node is configured
%% * cms                - list of hostnames of machines where CM nodes are configured
%% * workers            - list of hostnames of machines where worker nodes are configured
%% * dbs                - list of hostnames of machines where database nodes are configured
%% * open_files         - list of pairs hostname and open files limit on this host
%% * processes          - list of pairs hostname and processes limit on this host
-record(config, {
    main_cm,
    cms,
    workers,
    dbs,
    open_files,
    processes
}).

%% API
-export([main/1]).

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
        ["--install", Path] -> install(Path);
        ["--config"] -> config();
        ["--uninstall"] -> uninstall();
        _ -> print_usage()
    end,
    halt(?EXIT_SUCCESS).

%% init/0
%% ====================================================================
%% @doc Sets up net kernel and establishes connection to oneprovider.
%% @end
-spec init() -> ok.
%% ====================================================================
init() ->
    Hostname = "@" ++ os:cmd("hostname -f") -- "\n",
    put(?NODE, erlang:list_to_atom(?APP_STR ++ Hostname)),
    {A, B, C} = erlang:timestamp(),
    NodeName = "onepanel_admin_" ++ integer_to_list(A, 32) ++
        integer_to_list(B, 32) ++ integer_to_list(C, 32) ++ "@127.0.0.1",
    net_kernel:start([list_to_atom(NodeName), longnames]),
    erlang:set_cookie(node(), ?COOKIE).

%% install/1
%% ====================================================================
%% @doc Applies installation preferences read from configuration file.
%% @end
-spec install(Path :: string()) -> ok.
%% ====================================================================
install(Path) ->
    try
        Node = get(?NODE),
        #config{
            main_cm = MainCM,
            cms = CMs,
            workers = Workers,
            dbs = Dbs,
            open_files = OpenFiles,
            processes = Processes
        } = parse({config, Path}),
        AllHosts = lists:usort(CMs ++ Workers ++ Dbs),

        print_info("Checking configuration..."),
        check_hosts(Node, AllHosts),
        print_ok(),

        print_info("Setting ulimits..."),
        lists:foreach(fun(Host) ->
            HostOpenFiles = proplists:get_value(Host, OpenFiles, ?OPEN_FILES),
            HostProcesses = proplists:get_value(Host, Processes, ?PROCESSES),
            ok = rpc:call(erlang:list_to_atom(?APP_STR ++ "@" ++ Host), installer_utils, set_system_limit, [open_files, HostOpenFiles]),
            ok = rpc:call(erlang:list_to_atom(?APP_STR ++ "@" ++ Host), installer_utils, set_system_limit, [process_limit, HostProcesses])
        end, AllHosts),
        print_ok(),

        ok = execute([
            {Node, installer_db, start, [[{dbs, Dbs}]], "Starting database nodes..."},
            {Node, installer_cm, start, [[{main_cm, MainCM}, {cms, CMs}]], "Starting CM nodes..."},
            {Node, installer_worker, start, [[{workers, Workers}]], "Starting worker nodes..."},
            {Node, installer_utils, finalize_installation, [[]], "Finalizing installation..."}
        ])
    catch
        _:{config, Reason} when is_list(Reason) ->
            print_error("Configuration error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        _:{hosts, Hosts} when is_list(Hosts) ->
            io:format("[FAILED]\n"),
            format_hosts("Operation failed on following hosts:", Hosts),
            halt(?EXIT_FAILURE);
        _:{exec, Reason} when is_list(Reason) ->
            print_error("Operation error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        Error:Reason ->
            Log = io_lib:fwrite("Error: ~p~nReason: ~p~nStacktrace: ~p~n", [Error, Reason, erlang:get_stacktrace()]),
            file:write_file(?LOG_FILE, Log),
            io:format("An error occured. See ~s for more information.~n", [?LOG_FILE]),
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
        #config{
            main_cm = MainCM,
            cms = CMs,
            workers = Workers,
            dbs = Dbs
        } = parse({terms, Terms}),

        format_host("Main CM node:", MainCM),
        format_hosts("CM nodes:", lists:sort(CMs)),
        format_hosts("Worker nodes:", lists:sort(Workers)),
        format_hosts("Database nodes:", lists:sort(Dbs))
    catch
        Error:Reason ->
            Log = io_lib:fwrite("Error: ~p~nReason: ~p~nStacktrace: ~p~n", [Error, Reason, erlang:get_stacktrace()]),
            file:write_file(?LOG_FILE, Log),
            io:format("Cannot get current installation configuration.~nSee ~s for more information.~n", [?LOG_FILE]),
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

        ok = execute([
            {Node, installer_worker, stop, [[]], "Stopping worker nodes..."},
            {Node, installer_cm, stop, [[]], "Stopping CM nodes..."},
            {Node, installer_db, stop, [[]], "Stopping database nodes..."}
        ])
    catch
        _:{hosts, Hosts} when is_list(Hosts) ->
            io:format("[FAILED]\n"),
            format_hosts("Operation failed on following hosts:", Hosts),
            halt(?EXIT_FAILURE);
        _:{exec, Reason} when is_list(Reason) ->
            print_error("Operation error: ~s\n", [Reason]),
            halt(?EXIT_FAILURE);
        Error:Reason ->
            Log = io_lib:fwrite("Error: ~p~nReason: ~p~nStacktrace: ~p~n", [Error, Reason, erlang:get_stacktrace()]),
            file:write_file(?LOG_FILE, Log),
            io:format("An error occured. See ~s for more information.~n", [?LOG_FILE]),
            halt(?EXIT_FAILURE)
    end.

%% parse/1
%% ====================================================================
%% @doc Parses installation preferences read from configuration file.
%% @end
-spec parse({config, Path :: string()} | {terms, Terms :: [term()]}) -> Result when
    Result :: #config{}.
%% ====================================================================
parse({config, Path}) ->
    {ok, Terms} = file:consult(Path),
    #config{
        main_cm = proplists:get_value("Main CM host", Terms),
        cms = proplists:get_value("CM hosts", Terms, []),
        workers = proplists:get_value("Worker hosts", Terms, []),
        dbs = proplists:get_value("Database hosts", Terms, []),
        open_files = proplists:get_value("Open files limit", Terms, []),
        processes = proplists:get_value("Processes limit", Terms, [])
    };

parse({terms, Terms}) ->
    #config{
        main_cm = proplists:get_value(main_cm, Terms),
        cms = proplists:get_value(cms, Terms, []),
        workers = proplists:get_value(workers, Terms, []),
        dbs = proplists:get_value(dbs, Terms, [])
    }.

%% execute/4
%% ====================================================================
%% @doc Executes given function on given node via RPC call. Returns 'ok'
%% if function returns 'ok', otherwise throws an exception.
%% @end
-spec execute([{Node :: atom(), Module :: module(), Function :: atom(), Args :: term(), Description :: string()}]) -> ok | no_return().
%% ====================================================================
execute([]) ->
    ok;

execute([{Node, Module, Function, Args, Description} | Tasks]) ->
    print_info(Description),
    case rpc:call(Node, Module, Function, Args, ?RPC_TIMEOUT) of
        ok ->
            print_ok(),
            execute(Tasks);
        {error, {hosts, Hosts}} ->
            throw({hosts, Hosts});
        {error, Error} when is_list(Error) ->
            throw({exec, Error});
        Error ->
            Log = io_lib:fwrite("Error: ~p~n", [Error]),
            file:write_file(?LOG_FILE, Log),
            throw({exec, "Unknown error."})
    end.

%% check_hosts/2
%% ====================================================================
%% @doc Checks whether all hosts mentioned in configuration file are
%% available for further operations.
%% @end
-spec check_hosts(Node :: atom(), Hosts :: [string()]) -> ok | no_return().
%% ====================================================================
check_hosts(Node, Hosts) ->
    ValidHosts = rpc:call(Node, onepanel_utils, get_hosts, []),
    lists:foreach(fun(Host) ->
        case lists:member(Host, ValidHosts) of
            true -> ok;
            false ->
                throw({config, io_lib:fwrite("Host ~p was not found among available hosts.", [Host])})
        end
    end, Hosts).

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
    io:format("Usage: onepanel_admin [options]\n", []),
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
    io:format("[  OK  ]\n").

%% print_error/0
%% ====================================================================
%% @doc Prints error message for given step.
%% @end
-spec print_error(Format :: string(), Args :: [term()]) -> ok.
%% ====================================================================
print_error(Format, Args) ->
    io:format("[FAILED]\n"),
    io:format(Format, Args).
