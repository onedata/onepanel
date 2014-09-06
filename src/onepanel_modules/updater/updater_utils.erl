%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Utility functions for updater module.
%% @end
%% ===================================================================
-module(updater_utils).
-author("Rafal Slota").

-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([wait_for_node/2, ping/2, normalize_error_reason/1, flatten_stages/1, select_only_workers/1]).
-export([cast/3, multicast/3, anycast/3, local_cast/1]).

%% ====================================================================
%% API functions
%% ====================================================================


%% wait_for_node/2
%% ====================================================================
%% @doc Actively waits for node's startup no longer then Timeout ms.
%% @end
-spec wait_for_node(Node :: atom(), Timeout :: integer()) -> ok | {error, timeout}.
%% ====================================================================
wait_for_node(_, Timeout) when Timeout < 0 ->
    {error, timeout};
wait_for_node(Node, Timeout) when Timeout >= 0 ->
    timer:sleep(100),
    case ping(Node, 100) of
        pong -> ok;
        _ -> wait_for_node(Node, Timeout - 100)
    end.


%% ping/2
%% ====================================================================
%% @doc Same as net_adm:ping only that is limited by Timeout (ms).
%% @end
-spec ping(Node :: atom(), Timeout :: integer()) -> pong | pang.
%% ====================================================================
ping(Node, Timeout) ->
    Host = self(),
    Pid = spawn(fun() -> Host ! {self(), Node, net_adm:ping(Node)} end),
    receive
        {Pid, Node, Resp} -> Resp
    after Timeout ->
        pang
    end.


%% normalize_error_reason/1
%% ====================================================================
%% @doc Removes {error, _} prefix recursively.
%% @end
-spec normalize_error_reason(Reason :: tuple()) -> Reason :: any().
%% ====================================================================
normalize_error_reason({error, Reason}) ->
    normalize_error_reason(Reason);
normalize_error_reason(Reason) ->
    Reason.


%% flatten_stages/1
%% ====================================================================
%% @doc Flattens given Stage -> Job map into list of {Stage, Job} tuples. See updater_state:get_all_stages/1 for more details.
%% @end
-spec flatten_stages(Stages :: [{StageName :: atom(), [JobName :: atom()]}]) -> [{StageName :: atom(), JobName :: atom()}].
%% ====================================================================
flatten_stages(Stages) ->
    lists:flatten(
        lists:map(
            fun({Stage, Jobs}) ->
                lists:map(fun(Job) -> {Stage, Job} end, Jobs)
            end, Stages)).


%% select_only_workers/1
%% ====================================================================
%% @doc For given node list, return only those that looks like VeilClusters worker nodes.
%% @end
-spec select_only_workers(Nodes :: [atom()]) -> [Workers :: atom()].
%% ====================================================================
select_only_workers([]) ->
    [];
select_only_workers(Node) when is_atom(Node) ->
    case string:tokens(atom_to_list(Node), "@") of
        [?DEFAULT_WORKER_NAME, _] -> [Node];
        _ -> []
    end;
select_only_workers([Node | T]) ->
    select_only_workers(Node) ++ select_only_workers(T).


%% ====================================================================
%% Connectivity
%% ====================================================================

%% cast/3
%% ====================================================================
%% @doc Starts remotely  updater_export:runner(self(), Fun, Args).
%% @end
-spec cast(Node :: atom(), Fun :: atom(), Args :: [term()]) -> RemotePid :: pid().
%% ====================================================================
cast(Node, Fun, Args) ->
    ?debug("Cast: ~p ~p ~p", [Node, Fun, Args]),
    Host = self(),
    spawn_link(Node, updater_export, runner, [Host, Fun, Args]).


%% multicast/3
%% ====================================================================
%% @doc Starts remotely updater_export:runner(self(), Fun, Args) on all given nodes.
%% @end
-spec multicast(Nodes :: [atom()], Fun :: atom(), Args :: [term()]) -> RemotePid :: pid().
%% ====================================================================
multicast(Nodes, Fun, Args) ->
    lists:foreach(fun(Node) -> cast(Node, Fun, Args) end, Nodes).


%% anycast/3
%% ====================================================================
%% @doc Starts remotely updater_export:runner(self(), Fun, Args) on random node.
%% @end
-spec anycast(Nodes :: [atom()], Fun :: atom(), Args :: [term()]) -> RemotePid :: pid().
%% ====================================================================
anycast(Nodes, Fun, Args) ->
    ?debug("Anycast: ~p ~p ~p", [Nodes, Fun, Args]),
    Node = lists:nth(crypto:rand_uniform(1, length(Nodes) + 1), Nodes),
    cast(Node, Fun, Args).


%% local_cast/1
%% ====================================================================
%% @doc Spawns process executing Fun and sends its result to self().
%% @end
-spec local_cast(Fun :: function()) -> Pid :: pid().
%% ====================================================================
local_cast(Fun) ->
    Host = self(),
    spawn_link(fun() -> Host ! {self(), Fun()} end).

%% ====================================================================
%% Internal functions
%% ====================================================================
