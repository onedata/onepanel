%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains utility installation functions
%% @end
%% ===================================================================
-module(install_utils).

-include("registered_names.hrl").
-include("spanel_modules/db.hrl").
-include("spanel_modules/install.hrl").

%% API
-export([random_ascii_lowercase_sequence/1, apply_on_hosts/5, get_node/1, get_host/1, get_hosts/0]).
-export([set_ulimits_on_hosts/3, set_ulimits/2, get_ulimits_cmd/0]).
-export([add_node_to_config/3, remove_node_from_config/1, overwrite_config_args/3]).
-export([save_file_on_host/3, save_file_on_hosts/3]).

%% random_ascii_lowercase_sequence/1
%% ====================================================================
%% @doc Creates random sequence consisting of lowercase ASCII letters.
-spec random_ascii_lowercase_sequence(Length :: integer()) -> string().
%% ====================================================================
random_ascii_lowercase_sequence(Length) ->
  lists:foldl(fun(_, Acc) -> [random:uniform(26) + 96 | Acc] end, [], lists:seq(1, Length)).

%% apply_on_nodes/5
%% ====================================================================
%% @doc Applies function on specified hosts with timeout in miliseconds.
%% If 'Timeout' equals 'infinity' function waits as long as result is not
%% available. Pair of list where first list contains hosts on which function
%% call was successful. Second list contains remaining hosts.
%% @end
-spec apply_on_hosts(Hosts, Module, Function, Arguments, Timeout) -> {HostsOk, HostsError} when
  Hosts :: [string()],
  HostsOk :: [string()],
  HostsError :: [string()],
  Module :: module(),
  Function :: atom(),
  Arguments :: [term()],
  Timeout :: integer() | infinity.
%% ====================================================================
apply_on_hosts(Hosts, Module, Function, Arguments, Timeout) ->
  Nodes = lists:map(fun(Host) -> get_node(Host) end, Hosts),
  {Results, ErrorNodes} = rpc:multicall(Nodes, Module, Function, Arguments, Timeout),
  OkNodes = sets:to_list(sets:subtract(sets:from_list(Nodes), sets:from_list(ErrorNodes))),
  lists:foldl(fun
    ({ok, Node}, {HostsOk, HostsError}) -> {[get_host(Node) | HostsOk], HostsError};
    ({_, Node}, {HostsOk, HostsError}) -> {HostsOk, [get_host(Node) | HostsError]}
  end, {[], lists:map(fun(ErrorNode) -> get_host(ErrorNode) end, ErrorNodes)}, lists:zip(Results, OkNodes)).

%% get_node/1
%% ====================================================================
%% @doc Returns node from host.
-spec get_node(Host :: string()) -> Node :: node().
%% ====================================================================
get_node(Host) ->
  list_to_atom(?APP_STR ++ "@" ++ Host).

%% get_host/1
%% ====================================================================
%% @doc Returns host from node.
-spec get_host(Node :: node()) -> Host :: string().
%% ====================================================================
get_host(Node) ->
  NodeString = atom_to_list(Node),
  string:substr(NodeString, length(?APP_STR) + 2).

%% get_hosts/0
%% ====================================================================
%% @doc Returns list of hosts' ip addresses.
-spec get_hosts() -> [Host :: string()].
%% ====================================================================
get_hosts() ->
  lists:map(fun(Node) -> install_utils:get_host(Node) end, [node() | nodes(hidden)]).

%% set_ulimits_on_hosts/3
%% ====================================================================
%% @doc Sets system limits for open files and processes on hosts.
%% @end
-spec set_ulimits_on_hosts(Hosts :: [string()], OpenFiles :: integer(), Processes :: integer()) -> ok | error.
%% ====================================================================
set_ulimits_on_hosts(Hosts, OpenFiles, Processes) ->
  {_, HostsError} = apply_on_hosts(Hosts, ?MODULE, set_ulimits, [OpenFiles, Processes], ?RPC_TIMEOUT),
  case HostsError of
    [] -> ok;
    _ -> {error, HostsError}
  end.

%% set_ulimits/2
%% ====================================================================
%% @doc Sets system limits for open files and processes on node.
%% @end
-spec set_ulimits(OpenFiles :: integer(), Processes :: integer()) -> ok | error.
%% ====================================================================
set_ulimits(OpenFiles, Processes) ->
  case file:consult(?ULIMITS_CONFIG_PATH) of
    {ok, []} ->
      file:write_file(?ULIMITS_CONFIG_PATH, io_lib:fwrite("~p.\n~p.\n", [{open_files, OpenFiles}, {process_limit, Processes}]), [append]),
      dao:save_record(configurations, #configuration{id = last, ulimits = {OpenFiles, Processes}});
    {ok, _} ->
      ok;
    Error ->
      lager:error("Cannot parse file ~p, error: ~p", [?ULIMITS_CONFIG_PATH, Error]),
      error
  end.

%% get_ulimits_cmd/0
%% ====================================================================
%% @doc Returns ulimits command required during database or veil node installation.
%% @end
-spec get_ulimits_cmd() -> ok | error.
%% ====================================================================
get_ulimits_cmd() ->
  try
    {ok, #configuration{ulimits = {OpenFiles, Processes}}} = dao:get_record(configurations, last),
    "ulimit -n " ++ OpenFiles ++ " ; ulimit -u " ++ Processes
  catch
    _:_ -> "ulimit -n " ++ ?DEFAULT_OPEN_FILES ++ " ; ulimit -u " ++ ?DEFAULT_PROCESSES
  end.

%% add_node_to_config/3
%% ====================================================================
%% @doc Adds a node to configured_nodes.cfg.
-spec add_node_to_config(Type :: atom(), Name :: string(), Path :: string()) -> ok | error.
%% ====================================================================
add_node_to_config(Type, Name, Path) ->
  try
    {ok, Entries} = file:consult(?CONFIGURED_NODES_PATH),
    save_nodes_in_config(Entries ++ [{Type, Name, Path}])
  catch _:_ ->
    lager:error("Error while adding ~p to ~s", [Name, ?CONFIGURED_NODES_PATH]),
    error
  end.

%% remove_node_from_config/1
%% ====================================================================
%% @doc Removes a node from configured_nodes.cfg.
-spec remove_node_from_config(Name :: string()) -> ok | error.
%% ====================================================================
remove_node_from_config(Name) ->
  try
    {ok, Entries} = file:consult(?CONFIGURED_NODES_PATH),
    ToDelete = case lists:keyfind(Name, 2, Entries) of
                 false -> lager:error("Node ~p not found among configured nodes.", [Name]);
                 Term -> Term
               end,
    save_nodes_in_config(Entries -- [ToDelete])
  catch _:_ ->
    lager:error("Error while deleting ~p from ~s", [Name, ?CONFIGURED_NODES_PATH]),
    error
  end.

%% save_nodes_in_config/1
%% ====================================================================
%% @doc Saves list of nodes in configured_nodes.cfg.
-spec save_nodes_in_config(Name :: string()) -> ok | error.
%% ====================================================================
save_nodes_in_config(NodeList) ->
  try
    file:write_file(?CONFIGURED_NODES_PATH, ""),
    lists:foreach(
      fun(Node) ->
        file:write_file(?CONFIGURED_NODES_PATH, io_lib:fwrite("~p.\n", [Node]), [append])
      end, NodeList),
    ok
  catch _:_ ->
    lager:error("Error while writing to ~s", [?CONFIGURED_NODES_PATH]),
    error
  end.

%% overwrite_config_args/3
%% ====================================================================
%% @doc Overwrites a parameter in config.args.
-spec overwrite_config_args(Path :: string(), Parameter :: string(), NewValue :: string()) -> ok | error.
%% ====================================================================
overwrite_config_args(Path, Parameter, NewValue) ->
  try
    FileContent = case file:read_file(Path) of
                    {ok, DataRead} ->
                      binary_to_list(DataRead);
                    _ ->
                      lager:error("Could not read config.args file"),
                      throw(error)
                  end,

    {match, [{From, Through}]} = re:run(FileContent, Parameter ++ ":.*\n"),
    Beginning = string:substr(FileContent, 1, From),
    End = string:substr(FileContent, From + Through, length(FileContent) - From - Through + 1),
    file:write_file(Path, list_to_binary(Beginning ++ Parameter ++ ": " ++ NewValue ++ End)),
    ok
  catch
    _:_ -> error
  end.

%% save_file_on_hosts/3
%% ====================================================================
%% @doc Saves global registry certificate cert on host.
-spec save_file_on_hosts(Path :: string(), Filename :: string(), Content :: string() | binary()) -> ok | {error, ErrorHosts :: [string()]}.
%% ====================================================================
save_file_on_hosts(Path, Filename, Content) ->
  {_, HostsError} = apply_on_hosts(get_hosts(), ?MODULE, save_file_on_host, [Path, Filename, Content], ?RPC_TIMEOUT),
  case HostsError of
    [] -> ok;
    _ -> {error, HostsError}
  end.

%% save_file_on_host/3
%% ====================================================================
%% @doc Saves global registry certificate cert on all hosts.
-spec save_file_on_host(Path :: string(), Filename :: string(), Content :: string() | binary()) -> ok | error.
%% ====================================================================
save_file_on_host(Path, Filename, Content) ->
  try
    file:make_dir(Path),
    ok = file:write_file(filename:join(Path, Filename), Content),
    ok
  catch
    _:_ -> error
  end.