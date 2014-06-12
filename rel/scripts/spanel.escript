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
    Ans = rpc:call(Node, install_utils, get_hosts, []),
    io:format("Hosts: ~p", [Ans])
  catch
    _:Error when is_list(Error) -> io:format("Error: ~s", [Error]);
    _:_ -> io:format("An error occurred during installation.")
  end.

uninstall() ->
  io:format("Uninstall.").

parse(Config) ->
  {ok, Terms} = file:consult(Config),
  MainCCM = get_value_from_config("Main CCM host", Terms),
  OptCCMs = get_value_from_config("Optional CCM hosts", Terms),
  Workers = get_value_from_config("Worker hosts", Terms),
  Dbs = get_value_from_config("Database hosts", Terms),
  StoragePaths = get_value_from_config("Storage paths", Terms),
  {MainCCM, OptCCMs, Workers, Dbs, StoragePaths}.

get_value_from_config(Key, Terms) ->
  Value = proplists:get_value(Key, Terms),
  case Value of
    undefined -> throw(io_lib:fwrite("~s not found in configuration file.", [Key]));
    _ -> Value
  end.

print_usage() ->
  io:format("Usage: spanel.escript [options]\n", []),
  io:format("Options:\n"),
  io:format("\t--install <config file>\n"),
  io:format("\t--uninstall\n").