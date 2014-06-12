#!/usr/bin/env escript
%% -*- erlang -*-

% Default cookie used for communication with cluster
-define(default_cookie, veil_cluster_node).

% Installation directory of veil RPM
-define(prefix, "/opt/veil/").

% Location of erl_launcher
-define(erl_launcher_script_path, ?prefix ++ "scripts/erl_launcher").

main(Args) ->
  set_up_net_kernel(),
  case Args of
    ["--install", Config] -> install(Config);
    ["--info"] -> info();
    ["--uninstall"] -> uninstall();
    _ -> print_usage()
  end.

% Ensure EPMD is running and set up net kernel
set_up_net_kernel() ->
  put(hostname, "@" ++ os:cmd("hostname -f") -- "\n"),
  os:cmd(?erl_launcher_script_path ++ " epmd"),
  {A, B, C} = erlang:now(),
  NodeName = "spanel_setup_" ++ integer_to_list(A, 32) ++ integer_to_list(B, 32) ++ integer_to_list(C, 32) ++ "@127.0.0.1",
  net_kernel:start([list_to_atom(NodeName), longnames]),
  erlang:set_cookie(node(), ?default_cookie).

install(Config) ->
  try
    {MainCCM, OptCCMs, Workers, Dbs, StoragePaths} = parse(Config),
    Hostname = os:cmd("hostname -f") -- "\n",
    Node = erlang:list_to_atom("spanel@" ++ Hostname),

    check_hosts(Node, [MainCCM | OptCCMs] ++ Workers ++ Dbs),

    check_storage(Node, StoragePaths, Workers),

    io:format("Installing database nodes..."),
    execute(Node, install_db, install, [Dbs, []]),

    io:format("Starting database nodes...  "),
    execute(Node, install_db, start, [Dbs, []]),

    io:format("Installing ccm nodes...     "),
    execute(Node, install_ccm, install, [[MainCCM | OptCCMs], [{main_ccm, MainCCM}, {opt_ccms, OptCCMs}, {dbs, Dbs}]]),

    io:format("Starting ccm nodes...       "),
    execute(Node, install_ccm, start, [[MainCCM | OptCCMs], []]),

    io:format("Installing worker nodes...  "),
    execute(Node, install_worker, install, [Workers, [{main_ccm, MainCCM}, {opt_ccms, OptCCMs}, {dbs, Dbs}]]),

    io:format("Adding storage paths...     "),
    execute(Node, install_storage, add_storage_paths_on_hosts, [Workers, StoragePaths]),

    io:format("Starting worker nodes...    "),
    execute(Node, install_worker, start, [Workers, []])
  catch
    _:Reason when is_list(Reason) -> io:format("\t[FAIL]\nInstallation failed on following hosts: ~s\n", [Reason]);
    _:_ -> io:format("\nAn error occurred during installation.\n")
  end.

info() ->
  try
    Hostname = os:cmd("hostname -f") -- "\n",
    Node = erlang:list_to_atom("spanel@" ++ Hostname),

    Terms = rpc:call(Node, install_utils, get_installed_hosts, []),

    io:format("Main CCM node:         ~s\n", [proplists:get_value(main_ccm, Terms, "undefined")]),
    io:format("Optional CCM nodes:    ~s\n", [format(proplists:get_value(opt_ccms, Terms, []))]),
    io:format("Worker nodes:          ~s\n", [format(proplists:get_value(workers, Terms, []))]),
    io:format("Database nodes:        ~s\n", [format(proplists:get_value(dbs, Terms, []))]),
    io:format("Storage paths:         ~s\n", [format(proplists:get_value(storage_paths, Terms, []))])
  catch
    _:_ -> io:format("\nAn error occurred during information gathering.\n")
  end.

uninstall() ->
  try
    Hostname = os:cmd("hostname -f") -- "\n",
    Node = erlang:list_to_atom("spanel@" ++ Hostname),

    Terms = rpc:call(Node, install_utils, get_installed_hosts, []),

    CCMs = proplists:get_value(opt_ccms, Terms, []) ++
      case proplists:get_value(main_ccm, Terms) of
        undefined -> [];
        MainCCM -> [MainCCM]
      end,
    Workers = proplists:get_value(workers, Terms, []),
    Dbs = proplists:get_value(dbs, Terms, []),
    StoragePaths = proplists:get_value(storage_paths, Terms, []),

    io:format("Stopping worker nodes...      "),
    execute(Node, install_worker, stop, [Workers, []]),

    io:format("Removing storage paths...     "),
    execute(Node, install_storage, remove_storage_paths_on_hosts, [Workers, StoragePaths]),

    io:format("Uninstalling worker nodes...  "),
    execute(Node, install_worker, uninstall, [Workers, []]),

    io:format("Stopping ccm nodes...         "),
    execute(Node, install_ccm, stop, [CCMs, []]),

    io:format("Uninstalling ccm nodes...     "),
    execute(Node, install_ccm, uninstall, [CCMs, []]),

    io:format("Stopping database nodes...    "),
    execute(Node, install_db, stop, [Dbs, []]),

    io:format("Uninstalling database nodes..."),
    execute(Node, install_db, uninstall, [Dbs, []])
  catch
    _:Reason when is_list(Reason) -> io:format("\t[FAIL]\nUninstallation failed on following hosts: ~s\n", [Reason]);
    _:_ -> io:format("\nAn error occurred during uninstallation.\n")
  end.

format(List) ->
  case string:join(List, ", ") of
    "" -> "undefined";
    String -> String
  end.

parse(Config) ->
  {ok, Terms} = file:consult(Config),
  MainCCM = get_value_from_config("Main CCM host", Terms),
  OptCCMs = get_value_from_config("Optional CCM hosts", Terms),
  Workers = get_value_from_config("Worker hosts", Terms),
  Dbs = get_value_from_config("Database hosts", Terms),
  StoragePaths = get_value_from_config("Storage paths", Terms),
  {MainCCM, OptCCMs, Workers, Dbs, StoragePaths}.

execute(Node, Module, Function, Args) ->
  case rpc:call(Node, Module, Function, Args) of
    ok -> io:format("\t[ OK ]\n");
    {error, Error} when is_binary(Error) -> throw(binary_to_list(Error));
    {error, Error} when is_list(Error) -> throw(Error);
    _ -> throw(error)
  end.

get_value_from_config(Key, Terms) ->
  Value = proplists:get_value(Key, Terms),
  case Value of
    undefined -> throw(io_lib:fwrite("~s not found in configuration file.", [Key]));
    _ -> Value
  end.

check_hosts(Node, Hosts) ->
  ValidHosts = rpc:call(Node, install_utils, get_hosts, []),
  lists:foreach(fun(Host) ->
    case lists:member(Host, ValidHosts) of
      true -> ok;
      false -> throw(io_lib:fwrite("Host ~s was not found among available hosts.", [Host]))
    end
  end, Hosts).

check_storage(_, [], _) ->
  throw("Storage paths can not be empty.");
check_storage(Node, StoragePaths, Workers) when is_list(StoragePaths) ->
  lists:foreach(fun(StoragePath) ->
    case rpc:call(Node, install_storage, check_storage_on_hosts, [Workers, StoragePath]) of
      ok -> ok;
      {error, Hosts} ->
        throw(io_lib:fwrite("Storage ~s in not available on following hosts: ~s", [StoragePath, string:join(Hosts, ", ")]))
    end
  end, StoragePaths);
check_storage(_, _, _) ->
  throw("Wrong storage paths format.").

print_usage() ->
  io:format("Usage: spanel.escript [options]\n", []),
  io:format("Options:\n"),
  io:format("\t--install <config file>\n"),
  io:format("\t--uninstall\n").