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
-define(COOKIE, oneprovider_node).

%% Default system limit values
-define(OPEN_FILES, 65535).
-define(PROCESSES, 65535).

%% Timeout for each RPC call
-define(RPC_TIMEOUT, 120000).

%% Exit codes
-define(EXIT_SUCCESS, 0).
-define(EXIT_FAILURE, 1).

%% Error logs filename
-define(LOG_FILE, "/var/log/op_panel/op_panel_admin.log").

%% Local onepanel node
-define(NODE, setup_node).

%% config record contains following fields:
%% * main_ccm           - hostname of machine where main CCM node is configured
%% * ccms               - list of hostnames of machines where CCM nodes are configured
%% * workers            - list of hostnames of machines where worker nodes are configured
%% * dbs                - list of hostnames of machines where database nodes are configured
%% * storage_paths      - list of paths to storages on every worker node
%% * open_files         - list of pairs hostname and open files limit on this host
%% * processes          - list of pairs hostname and processes limit on this host
%% * register           - yes/no value that describes whether register provider in Global Registry
%% * redirection_point  - url to provider's GUI
%% * client_name        - provider name
-record(config, {
    main_ccm,
    ccms,
    workers,
    dbs,
    storage_paths,
    open_files,
    processes,
    register,
    redirection_point,
    client_name
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
    {A, B, C} = erlang:now(),
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
            main_ccm = MainCCM,
            ccms = CCMs,
            workers = Workers,
            dbs = Dbs,
            storage_paths = StoragePaths,
            open_files = OpenFiles,
            processes = Processes,
            register = Register,
            redirection_point = RedirectionPoint,
            client_name = ClientName
        } = parse({config, Path}),
        AllHosts = lists:usort(CCMs ++ Workers ++ Dbs),

        print_info("Checking configuration..."),
        check_hosts(Node, AllHosts),
        print_ok(),

        print_info("Checking storage availability..."),
        check_storage_paths(Node, StoragePaths, Workers),
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
            {Node, installer_ccm, start, [[{main_ccm, MainCCM}, {ccms, CCMs}]], "Starting ccm nodes..."},
            {Node, installer_storage, add_storage_paths_to_db, [[{storage_paths, StoragePaths}]], "Adding storage paths..."},
            {Node, installer_worker, start, [[{workers, Workers}]], "Starting worker nodes..."},
            {Node, installer_utils_adapter, finalize_installation, [[]], "Finalizing installation..."}
        ]),

        case Register of
            yes ->
                print_info("Connecting to Global Registry..."),
                {ok, _} = rpc:call(Node, gr_providers, check_ip_address, [provider]),
                print_ok(),

                print_info("Checking ports availability..."),
                check_ports(Node),
                print_ok(),

                print_info("Registering..."),
                {ok, _} = rpc:call(Node, provider_logic, register, [RedirectionPoint, ClientName], ?RPC_TIMEOUT),
                print_ok();
            _ ->
                ok
        end
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
            main_ccm = MainCCM,
            ccms = CCMs,
            workers = Workers,
            dbs = Dbs,
            storage_paths = StoragePaths
        } = parse({terms, Terms}),

        format_host("Main CCM node:", MainCCM),
        format_hosts("CCM nodes:", lists:sort(CCMs)),
        format_hosts("Worker nodes:", lists:sort(Workers)),
        format_hosts("Database nodes:", lists:sort(Dbs)),
        format_hosts("Storage paths:", lists:sort(StoragePaths))
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
        Terms = rpc:call(Node, installer_utils, get_global_config, []),
        #config{
            storage_paths = StoragePaths
        } = parse({terms, Terms}),

        ok = execute([
            {Node, installer_worker, stop, [[]], "Stopping worker nodes..."},
            {Node, installer_storage, remove_storage_paths_from_db, [[{storage_paths, StoragePaths}]], "Removing storage paths..."},
            {Node, installer_ccm, stop, [[]], "Stopping ccm nodes..."},
            {Node, installer_db, stop, [[]], "Stopping database nodes..."}
        ]),

        case is_registered() of
            true ->
                ok = execute([{Node, provider_logic, unregister, [], "Unregistering provider..."}]);
            _ ->
                ok
        end
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
        main_ccm = proplists:get_value("Main CCM host", Terms),
        ccms = proplists:get_value("CCM hosts", Terms, []),
        workers = proplists:get_value("Worker hosts", Terms, []),
        dbs = proplists:get_value("Database hosts", Terms, []),
        storage_paths = proplists:get_value("Storage paths", Terms, []),
        open_files = proplists:get_value("Open files limit", Terms, []),
        processes = proplists:get_value("Processes limit", Terms, []),
        register = proplists:get_value("Register in Global Registry", Terms, no),
        redirection_point = list_to_binary(proplists:get_value("Redirection point", Terms, hostname())),
        client_name = list_to_binary(proplists:get_value("Client name", Terms, "provider"))
    };

parse({terms, Terms}) ->
    #config{
        main_ccm = proplists:get_value(main_ccm, Terms),
        ccms = proplists:get_value(ccms, Terms, []),
        workers = proplists:get_value(workers, Terms, []),
        dbs = proplists:get_value(dbs, Terms, []),
        storage_paths = proplists:get_value(storage_paths, Terms, [])
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

%% hostname/0
%% ====================================================================
%% @doc Returns fully-qualified hostname of the machine.
%% @end
-spec hostname() -> Hostname :: string().
%% ====================================================================
hostname() ->
    os:cmd("hostname -f") -- "\n".

%% is_registered/0
%% ====================================================================
%% @doc Returns true if provider is registered, otherwise false.
%% @end
-spec is_registered() -> boolean().
%% ====================================================================
is_registered() ->
    Node = get(?NODE),
    undefined /= rpc:call(Node, provider_logic, get_provider_id, []).

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

%% check_ports/1
%% ====================================================================
%% @doc Checks whether default ports of all control panel nodes are
%% available for Global Registry.
%% @end
-spec check_ports(Node :: atom()) -> ok | no_return().
%% ====================================================================
check_ports(Node) ->
    ControlPanelHosts = case rpc:call(Node, onepanel_utils, get_control_panel_hosts, [], ?RPC_TIMEOUT) of
                            {ok, Hosts} -> Hosts;
                            _ -> []
                        end,
    {DefaultGuiPort, DefaultRestPort} = case rpc:call(Node, provider_logic, get_ports_to_check, [], ?RPC_TIMEOUT) of
                                            {ok, [{<<"gui">>, GuiPort}, {<<"rest">>, RestPort}]} ->
                                                {GuiPort, RestPort};
                                            _ -> {0, 0}
                                        end,
    lists:foreach(fun(ControlPanelHost) ->
        ControlPanelNode = list_to_atom(?APP_STR ++ "@" ++ ControlPanelHost),
        {ok, IpAddress} = rpc:call(ControlPanelNode, gr_providers, check_ip_address, [provider], ?RPC_TIMEOUT),
        ok = rpc:call(Node, gr_providers, check_port, [provider, IpAddress, DefaultGuiPort, <<"gui">>]),
        ok = rpc:call(Node, gr_providers, check_port, [provider, IpAddress, DefaultRestPort, <<"rest">>]),
        ok = rpc:call(Node, dao, update_record,
            [local_configurations, ControlPanelHost, [{gui_port, DefaultGuiPort}, {rest_port, DefaultGuiPort}]], ?RPC_TIMEOUT)
    end, ControlPanelHosts),
    ok.

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