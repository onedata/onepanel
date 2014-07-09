%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains utility Onepanel functions.
%% @end
%% ===================================================================
-module(onepanel_utils).

-include("registered_names.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([random_ascii_lowercase_sequence/1]).
-export([get_node/1, get_host/1, get_hosts/0]).
-export([apply_on_hosts/5, save_file_on_host/3, save_file_on_hosts/3]).

%% ====================================================================
%% API functions
%% ====================================================================

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
%% IMPORTANT! Called function must return either {ok, Host} for successful
%% execution or {error, Host} for faulty execution.
%% @end
-spec apply_on_hosts(Hosts, Module, Function, Arguments, Timeout) -> Result when
    Result :: {HostsOk, HostsError},
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
    lists:foldl(fun
        ({ok, Host}, {HostsOk, HostsError}) -> {[Host | HostsOk], HostsError};
        ({_, Host}, {HostsOk, HostsError}) -> {HostsOk, [Host | HostsError]}
    end, {[], lists:map(fun(Node) -> get_host(Node) end, ErrorNodes)}, Results).


%% get_node/1
%% ====================================================================
%% @doc Returns node from host.
-spec get_node(Host :: string()) -> Result when
    Result :: node().
%% ====================================================================
get_node(Host) ->
    list_to_atom(?APP_STR ++ "@" ++ Host).


%% get_host/1
%% ====================================================================
%% @doc Returns host from node.
-spec get_host(Node :: node()) -> Result when
    Result :: string().
%% ====================================================================
get_host(Node) ->
    [_, Host] = string:tokens(atom_to_list(Node), "@"),
    Host.


%% get_hosts/0
%% ====================================================================
%% @doc Returns list of hosts' ip addresses.
-spec get_hosts() -> Result when
    Result :: [Host :: string()].
%% ====================================================================
get_hosts() ->
    lists:foldl(fun(Node, Acc) ->
        NodeString = atom_to_list(Node),
        case string:equal(?APP_STR, string:left(NodeString, length(?APP_STR))) of
            true -> [get_host(Node) | Acc];
            _ -> Acc
        end
    end, [], [node() | nodes(hidden)]).


%% save_file_on_hosts/3
%% ====================================================================
%% @doc Saves Global Registry certificate cert on host.
-spec save_file_on_hosts(Path :: string(), Filename :: string(), Content :: string() | binary()) -> Result when
    Result :: ok | {error, ErrorHosts :: [string()]}.
%% ====================================================================
save_file_on_hosts(Path, Filename, Content) ->
    {_, HostsError} = apply_on_hosts(get_hosts(), ?MODULE, save_file_on_host, [Path, Filename, Content], ?RPC_TIMEOUT),
    case HostsError of
        [] -> ok;
        _ ->
            ?error("Cannot save file ~p at directory ~p on following hosts: ~p", [Filename, Path, HostsError]),
            {error, {hosts, HostsError}}
    end.


%% save_file_on_host/3
%% ====================================================================
%% @doc Saves Global Registry certificate cert on all hosts.
-spec save_file_on_host(Path :: string(), Filename :: string(), Content :: string() | binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
save_file_on_host(Path, Filename, Content) ->
    Host = get_host(node()),
    try
        file:make_dir(Path),
        ok = file:write_file(filename:join(Path, Filename), Content),
        {ok, Host}
    catch
        _:Reason ->
            ?error("Cannot save file ~p at directory ~p: ~p", [Filename, Path, Reason]),
            {error, Host}
    end.

