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

-include("spanel_modules/install_common.hrl").

%% API
-export([random_ascii_lowercase_sequence/1, apply_on_nodes/5]).
-export([get_nodes_from_config/1, add_node_to_config/3, remove_node_from_config/1]).

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
-spec apply_on_nodes(Nodes, Module, Function, Arguments, Timeout) -> {SuccessfulNodes, FailedNodes} when
  Nodes :: [node()],
  SuccessfulNodes :: [node()],
  FailedNodes :: [node()],
  Module :: module(),
  Function :: atom(),
  Arguments :: [term()],
  Timeout :: integer() | infinity.
%% ====================================================================
apply_on_nodes(Nodes, Module, Function, Arguments, Timeout) ->
  {OperationResults, OperationError} = rpc:multicall(Nodes, Module, Function, Arguments, Timeout),
  {OperationOk, OperationFailed} = lists:foldl(fun
    ({Node, ok}, {Success, Failure}) -> {[Node | Success], Failure};
    ({Node, _}, {Success, Failure}) -> {Success, [Node | Failure]}
  end, {[], []}, OperationResults),
  {OperationOk, OperationFailed ++ OperationError}.

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

