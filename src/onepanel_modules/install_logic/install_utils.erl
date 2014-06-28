%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains utility installation functions.
%% @end
%% ===================================================================
-module(install_utils).

-include("registered_names.hrl").
-include("onepanel_modules/db_logic.hrl").
-include("onepanel_modules/install_logic.hrl").

%% API
-export([random_ascii_lowercase_sequence/1, apply_on_hosts/5, get_node/1, get_host/1, get_hosts/0]).
-export([set_ulimits_on_hosts/3, set_ulimits/3, get_ulimits_cmd/1, get_ports_to_check/1, get_control_panel_hosts/1]).
-export([add_node_to_config/3, remove_node_from_config/1, overwrite_config_args/3]).
-export([save_file_on_host/3, save_file_on_hosts/3]).

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
        ({ok, Host}, {HostsOk, HostsError}) -> {[Host, HostsOk], HostsError};
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
    NodeString = atom_to_list(Node),
    string:substr(NodeString, length(?APP_STR) + 2).


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


%% set_ulimits_on_hosts/3
%% ====================================================================
%% @doc Sets system limits for open files and processes on hosts.
%% @end
-spec set_ulimits_on_hosts(Hosts :: [string()], OpenFiles :: integer(), Processes :: integer()) -> Result when
    Result :: ok | {error, ErrorHosts :: [string()]}.
%% ====================================================================
set_ulimits_on_hosts(Hosts, OpenFiles, Processes) ->
    {_, HostsError} = apply_on_hosts(Hosts, ?MODULE, set_ulimits, [OpenFiles, Processes], ?RPC_TIMEOUT),
    case HostsError of
        [] -> ok;
        _ ->
            lager:error("Cannot set ulimits on hosts: ~p", [HostsError]),
            {error, HostsError}
    end.


%% set_ulimits/2
%% ====================================================================
%% @doc Sets system limits for open files and processes on host.
%% @end
-spec set_ulimits(Host :: string(), OpenFiles :: integer(), Processes :: integer()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
set_ulimits(Host, OpenFiles, Processes) ->
    case file:consult(?ULIMITS_CONFIG_PATH) of
        {ok, []} ->
            file:write_file(?ULIMITS_CONFIG_PATH, io_lib:fwrite("~p.\n~p.\n", [{open_files, OpenFiles}, {process_limit, Processes}]), [append]),
            dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{open_files_limit, OpenFiles}, {processes_limit, Processes}]);
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
-spec get_ulimits_cmd(Host :: string()) -> Result when
    Result :: string().
%% ====================================================================
get_ulimits_cmd(Host) ->
    try
        {ok, #?LOCAL_CONFIG_RECORD{open_files_limit = OpenFiles, processes_limit = Processes}} = dao:get_record(?LOCAL_CONFIG_TABLE, Host),
        "ulimit -n " ++ OpenFiles ++ " ; ulimit -u " ++ Processes
    catch
        _:Reason ->
            lager:error("Cannot get ulimits configuration: ~p. Returning default values.", [Reason]),
            "ulimit -n " ++ ?DEFAULT_OPEN_FILES ++ " ; ulimit -u " ++ ?DEFAULT_PROCESSES
    end.


%% add_node_to_config/3
%% ====================================================================
%% @doc Adds a node to configured_nodes.cfg.
-spec add_node_to_config(Type :: atom(), Name :: string(), Path :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
add_node_to_config(Type, Name, Path) ->
    try
        {ok, Entries} = file:consult(?CONFIGURED_NODES_PATH),
        save_nodes_in_config(Entries ++ [{Type, Name, Path}])
    catch
        _:Reason ->
            lager:error("Cannot add ~p node to ~s: ~p", [Name, ?CONFIGURED_NODES_PATH, Reason]),
            {error, Reason}
    end.


%% remove_node_from_config/1
%% ====================================================================
%% @doc Removes a node from configured_nodes.cfg.
-spec remove_node_from_config(Name :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove_node_from_config(Name) ->
    try
        {ok, Entries} = file:consult(?CONFIGURED_NODES_PATH),
        ToDelete = case lists:keyfind(Name, 2, Entries) of
                       false -> lager:error("Node ~p not found among configured nodes.", [Name]);
                       Term -> Term
                   end,
        save_nodes_in_config(Entries -- [ToDelete])
    catch
        _:Reason ->
            lager:error("Cannot delete ~p node from ~s: ~p", [Name, ?CONFIGURED_NODES_PATH, Reason]),
            {error, Reason}
    end.


%% save_nodes_in_config/1
%% ====================================================================
%% @doc Saves list of nodes in configured_nodes.cfg.
-spec save_nodes_in_config(Name :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
save_nodes_in_config(NodeList) ->
    try
        file:write_file(?CONFIGURED_NODES_PATH, ""),
        lists:foreach(
            fun(Node) ->
                file:write_file(?CONFIGURED_NODES_PATH, io_lib:fwrite("~p.\n", [Node]), [append])
            end, NodeList),
        ok
    catch
        _:Reason ->
            lager:error("Cannot write to ~s: ~p", [?CONFIGURED_NODES_PATH, Reason]),
            {error, Reason}
    end.


%% overwrite_config_args/3
%% ====================================================================
%% @doc Overwrites a parameter in config.args.
-spec overwrite_config_args(Path :: string(), Parameter :: string(), NewValue :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
overwrite_config_args(Path, Parameter, NewValue) ->
    try
        FileContent = case file:read_file(Path) of
                          {ok, DataRead} ->
                              binary_to_list(DataRead);
                          _ ->
                              throw("Could not read config.args file")
                      end,

        {match, [{From, Through}]} = re:run(FileContent, Parameter ++ ":.*\n"),
        Beginning = string:substr(FileContent, 1, From),
        End = string:substr(FileContent, From + Through, length(FileContent) - From - Through + 1),
        file:write_file(Path, list_to_binary(Beginning ++ Parameter ++ ": " ++ NewValue ++ End)),
        ok
    catch
        _:Reason ->
            lager:error("Cannot overwrite config args: ~p", [Reason]),
            {error, Reason}
    end.


%% save_file_on_hosts/3
%% ====================================================================
%% @doc Saves global registry certificate cert on host.
-spec save_file_on_hosts(Path :: string(), Filename :: string(), Content :: string() | binary()) -> Result when
    Result :: ok | {error, ErrorHosts :: [string()]}.
%% ====================================================================
save_file_on_hosts(Path, Filename, Content) ->
    {_, HostsError} = apply_on_hosts(get_hosts(), ?MODULE, save_file_on_host, [Path, Filename, Content], ?RPC_TIMEOUT),
    case HostsError of
        [] -> ok;
        _ ->
            lager:error("Cannot save file ~p at directory ~p on following hosts: ~p", [Filename, Path, HostsError]),
            {error, HostsError}
    end.


%% save_file_on_host/3
%% ====================================================================
%% @doc Saves global registry certificate cert on all hosts.
-spec save_file_on_host(Path :: string(), Filename :: string(), Content :: string() | binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
save_file_on_host(Path, Filename, Content) ->
    try
        file:make_dir(Path),
        ok = file:write_file(filename:join(Path, Filename), Content),
        ok
    catch
        _:Reason ->
            lager:error("Cannot save file ~p at directory ~p: ~p", [Filename, Path, Reason]),
            {error, Reason}
    end.


%% get_ports_to_check/1
%% ====================================================================
%% @doc Returns default veilcluster ports that will be checked by global registry
-spec get_ports_to_check(MainCCM :: string()) -> Result when
    Result :: {ok, [{Type :: string(), Port :: integer()}]} | {error, Reason :: term()}.
%% ====================================================================
get_ports_to_check(MainCCM) ->
    try
        Node = list_to_atom("ccm@" ++ MainCCM),
        {ok, GuiPort} = rpc:call(Node, application, get_env, [veil_cluster_node, control_panel_port]),
        {ok, RestPort} = rpc:call(Node, application, get_env, [veil_cluster_node, rest_port]),
        {ok, [{"gui", GuiPort}, {"rest", RestPort}]}
    catch
        _:Reason ->
            lager:error("Cannot get ports to check: ~p", [Reason]),
            {error, Reason}
    end.


%% get_control_panel_hosts/1
%% ====================================================================
%% @doc Returns list of control panel hosts
-spec get_control_panel_hosts(MainCCM :: string()) -> Result when
    Result :: {ok, Hosts :: [string()]} | {error, Reason :: term()}.
%% ====================================================================
get_control_panel_hosts(MainCCM) ->
    try
        Node = list_to_atom("ccm@" ++ MainCCM),
        {Workers, _} = rpc:call(Node, gen_server, call, [{global, central_cluster_manager}, get_workers, 1000]),
        ControlPanelHosts = lists:foldl(fun
            ({WorkerNode, control_panel}, Acc) -> [get_host(WorkerNode) | Acc];
            (_, Acc) -> Acc
        end, [], Workers),
        {ok, ControlPanelHosts}
    catch
        _:Reason ->
            lager:error("Cannot get control panel hosts: ~p", [Reason]),
            {error, Reason}
    end.