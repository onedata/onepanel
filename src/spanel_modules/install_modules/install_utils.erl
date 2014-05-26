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
-include("spanel_modules/db_logic.hrl").
-include("spanel_modules/install_common.hrl").

%% API
-export([random_ascii_lowercase_sequence/1, apply_on_hosts/5]).
-export([get_nodes_from_config/1, add_node_to_config/3, remove_node_from_config/1, install_veil_node/7, get_hostname/1, get_ulimit_cmd/0]).

%% random_ascii_lowercase_sequence
%% ====================================================================
%% @doc Create random sequence consisting of lowercase ASCII letters.
-spec random_ascii_lowercase_sequence(Length :: integer()) -> string().
%% ====================================================================
random_ascii_lowercase_sequence(Length) ->
  lists:foldl(fun(_, Acc) -> [random:uniform(26) + 96 | Acc] end, [], lists:seq(1, Length)).

%% apply_on_nodes/5
%% ====================================================================
%% @doc Applies function on specified nodes with timeout in miliseconds.
%% If 'Timeout' equals 'infinity' function waits as long as result is not
%% available. Pair of list where first list contains nodes on which function
%% call was successful. Second list contains remaining nodes.
%% @end
-spec apply_on_hosts(Hostnames, Module, Function, Arguments, Timeout) -> {SuccessfulHosts, FailedHosts} when
  Hostnames :: [node()],
  SuccessfulHosts :: [node()],
  FailedHosts :: [node()],
  Module :: module(),
  Function :: atom(),
  Arguments :: [term()],
  Timeout :: integer() | infinity.
%% ====================================================================
apply_on_hosts(Hostnames, Module, Function, Arguments, Timeout) ->
  Nodes = lists:map(fun(Hostname) -> list_to_atom("spanel@" ++ Hostname) end, Hostnames),
  {OperationResults, OperationErrors} = rpc:multicall(Nodes, Module, Function, Arguments, Timeout),
  {OperationOk, OperationFailed} = lists:foldl(fun
    ({Node, ok}, {Success, Failure}) -> {[install_utils:get_hostname(Node) | Success], Failure};
    ({Node, _}, {Success, Failure}) -> {Success, [install_utils:get_hostname(Node) | Failure]}
  end, {[], []}, OperationResults),
  {OperationOk, OperationFailed ++ lists:map(fun(OperationError) ->
    install_utils:get_hostname(OperationError) end, OperationErrors)}.

install_veil_node(Type, Name, Hostname, Path, CCM, CCMs, Databases) ->
  try
    LongName = Name ++ "@" ++ Hostname,
    lager:info("Installing " ++ LongName ++ "..."),
    "" = os:cmd("mkdir -p " ++ Path ++ Name),
    "" = os:cmd("cp -R " ++ ?VEIL_RELEASE ++ "/* " ++ Path ++ Name),

    MainCCM = ?DEFAULT_CCM_NAME ++ "@" ++ CCM,
    OptCCMs = lists:foldl(fun(CCMHostname, Acc) -> Acc ++ ?DEFAULT_CCM_NAME ++ "@" ++ CCMHostname ++ " " end, [], CCMs),
    DbNodes = lists:foldl(fun(DbHostname, Acc) ->
      Acc ++ ?DEFAULT_DB_NAME ++ "@" ++ DbHostname ++ " " end, [], Databases),
    StorageConfigPath = Path ++ Name ++ "/" ++ ?STORAGE_CONFIG_PATH,

    ok = overwrite_config_args(Path ++ Name ++ "/" ++ ?CONFIG_ARGS_PATH, "name", LongName),
    ok = overwrite_config_args(Path ++ Name ++ "/" ++ ?CONFIG_ARGS_PATH, "main_ccm", MainCCM),
    ok = overwrite_config_args(Path ++ Name ++ "/" ++ ?CONFIG_ARGS_PATH, "opt_ccms", OptCCMs),
    ok = overwrite_config_args(Path ++ Name ++ "/" ++ ?CONFIG_ARGS_PATH, "db_nodes", DbNodes),
    ok = overwrite_config_args(Path ++ Name ++ "/" ++ ?CONFIG_ARGS_PATH, "storage_config_path", StorageConfigPath),

    Ans = os:cmd(Path ++ Name ++ "/" ++ ?VEIL_CLUSTER_SCRIPT_PATH),
    lager:info("Ans: ~p", [Ans]),
    ok = add_node_to_config(Type, list_to_atom(Name), Path),
    ok
  catch
    _:_ -> error
  end.

get_hostname(Node) ->
    "spanel@" ++ Hostname = atom_to_list(Node),
  Hostname.

get_ulimit_cmd() ->
  {ok, #configuration{ulimits = {OpenFiles, Processes}}} = db_logic:get_record(configurations, last),
  "ulimit -n " ++ OpenFiles ++ " ; ulimit -u " ++ Processes.

% Read contigured_nodes.cfg
% WhichCluster = veil | database
get_nodes_from_config(WhichCluster) ->
  try
    {ok, Entries} = file:consult(?CONFIGURED_NODES_PATH),
    case WhichCluster of
      database -> get_database_node_from_config(Entries);
      veil -> get_veil_nodes_from_config(Entries)
    end
  catch _:_ ->
    lager:error("Error while reading ~s", [?CONFIGURED_NODES_PATH])
  end.


% Do not use directly
get_database_node_from_config(Entries) ->
  case lists:keyfind(db_node, 1, Entries) of
    false -> {none, []};
    Node -> {db_node, Node}
  end.


% Do not use directly
get_veil_nodes_from_config(Entries) ->
  case lists:keyfind(worker, 1, Entries) of
    false -> {none, []};
    Worker ->
      case lists:keyfind(ccm, 1, Entries) of
        false -> {worker, Worker};
        CCM -> {ccm_plus_worker, {CCM, Worker}}
      end
  end.

% Add a node to configured_nodes.cfg
add_node_to_config(Type, Name, Path) ->
  try
    {ok, Entries} = file:consult(?CONFIGURED_NODES_PATH),
    save_nodes_in_config(Entries ++ [{Type, Name, Path}])
  catch _:_ ->
    lager:error("Error while adding ~p to ~s", [Name, ?CONFIGURED_NODES_PATH]),
    error
  end.

% Remove a node from configured_nodes.cfg
remove_node_from_config(Name) ->
  try
    {ok, Entries} = file:consult(?CONFIGURED_NODES_PATH),
    ToDelete = case lists:keyfind(Name, 2, Entries) of
                 false -> lager:error("Node ~p not found among configured nodes.", [Name]);
                 Term -> Term
               end,

    save_nodes_in_config(Entries -- [ToDelete])
  catch _:_ ->
    lager:error("Error while deleting ~p from ~s", [Name, ?CONFIGURED_NODES_PATH])
  end.

% Save list of nodes in configured_nodes.cfg
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

% Overwrite a parameter in config.args
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
