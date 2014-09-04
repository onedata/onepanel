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
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([get_version/0]).
-export([random_ascii_lowercase_sequence/1]).
-export([get_node/1, get_node/2, get_nodes/0, get_nodes/2, get_host/1, get_hosts/0, get_software_version/0, get_control_panel_hosts/0]).
-export([apply_on_hosts/5, dropwhile_failure/5, save_file_on_host/3, save_file_on_hosts/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% get_version/0
%% ====================================================================
%% @doc Returns application version read from reltool.config file.
-spec get_version() -> Result when
    Result :: binary().
%% ====================================================================
get_version() ->
    case lists:dropwhile(fun
        ({?APP_NAME, _, _}) -> false;
        (_) -> true
    end, application:which_applications()) of
        [{?APP_NAME, _, Version} | _] -> list_to_binary(Version);
        _ -> <<"undefined">>
    end.


%% random_ascii_lowercase_sequence/1
%% ====================================================================
%% @doc Creates random sequence consisting of lowercase ASCII letters.
-spec random_ascii_lowercase_sequence(Length :: integer()) -> Result when
    Result :: string().
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


%% dropwhile_failure/5
%% ====================================================================
%% @doc Applies function sequentially on nodes as long as rpc calls fail
%% with error "badrpc".
%% @end
-spec dropwhile_failure(Nodes, Module, Function, Arguments, Timeout) -> Result when
    Result :: term(),
    Nodes :: [atom()],
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Timeout :: integer() | infinity.
%% ====================================================================
dropwhile_failure([], Module, Function, Arguments, _) ->
    ?error("dropwhile_failure function called as ~p:~p(~p) failed on all nodes.", [Module, Function, Arguments]),
    {error, "Failure on all nodes."};
dropwhile_failure([Node | Nodes], Module, Function, Arguments, Timeout) ->
    case rpc:call(Node, Module, Function, Arguments, Timeout) of
        {badrpc, Reason} ->
            ?error("Cannot execute ~p:~p(~p) on node ~p: ~p", [Module, Function, Arguments, Node, Reason]),
            dropwhile_failure(Nodes, Module, Function, Arguments, Timeout);
        Result -> Result
    end.


%% get_node/1
%% ====================================================================
%% @doc Returns node from host.
-spec get_node(Host :: string()) -> Result when
    Result :: node().
%% ====================================================================
get_node(Host) ->
    get_node(?APP_STR, Host).


%% get_node/2
%% ====================================================================
%% @doc Returns node of given type on provided host.
-spec get_node(Type :: string(), Host :: string()) -> Result when
    Result :: node().
%% ====================================================================
get_node(Type, Host) ->
    list_to_atom(Type ++ "@" ++ Host).


%% get_nodes/0
%% ====================================================================
%% @doc Returns list of all application nodes.
-spec get_nodes() -> Result when
    Result :: node().
%% ====================================================================
get_nodes() ->
    [node() | nodes(hidden)].


%% get_nodes/2
%% ====================================================================
%% @doc Returns list of nodes of given type on provided hosts.
-spec get_nodes(Type :: string(), Hosts :: [string()]) -> Result when
    Result :: node().
%% ====================================================================
get_nodes(Type, Hosts) ->
    lists:map(fun(Host) ->
        get_node(Type, Host)
    end, Hosts).


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
%% @doc Returns list of hostnames of onepanel cluster nodes.
-spec get_hosts() -> Result when
    Result :: [Host :: string()].
%% ====================================================================
get_hosts() ->
    lists:foldl(fun(Node, Acc) ->
        [Name | _] = string:tokens(atom_to_list(Node), "@"),
        case ?APP_STR of
            Name -> [get_host(Node) | Acc];
            _ -> Acc
        end
    end, [], get_nodes()).


%% save_file_on_hosts/3
%% ====================================================================
%% @doc Saves Global Registry certificate cert on all hosts.
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
%% @doc Saves Global Registry certificate cert on host.
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


%% get_software_version/0
%% ====================================================================
%% @doc Returns current software version.
-spec get_software_version() -> Result when
    Result :: string() | undefined.
%% ====================================================================
get_software_version() ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{workers = Workers}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        dropwhile_failure(get_nodes(?DEFAULT_WORKER_NAME, Workers), node_manager, check_vsn, [], ?RPC_TIMEOUT)
    catch
        _:Reason ->
            ?error("Cannot get current software version: ~p", [Reason]),
            undefined
    end.


%% get_control_panel_hosts/0
%% ====================================================================
%% @doc Returns list of control panel hosts
-spec get_control_panel_hosts() -> Result when
    Result :: {ok, Hosts :: [string()]} | {error, Reason :: term()}.
%% ====================================================================
get_control_panel_hosts() ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{ccms = CCMs}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        Nodes = get_nodes(?DEFAULT_CCM_NAME, CCMs),
        {Workers, _} = dropwhile_failure(Nodes, gen_server, call, [{global, central_cluster_manager}, get_workers, 1000], ?RPC_TIMEOUT),
        ControlPanelHosts = lists:foldl(fun
            ({WorkerNode, control_panel}, Acc) -> [onepanel_utils:get_host(WorkerNode) | Acc];
            (_, Acc) -> Acc
        end, [], Workers),
        {ok, ControlPanelHosts}
    catch
        _:Reason ->
            ?error("Cannot get control panel hosts: ~p", [Reason]),
            {error, Reason}
    end.