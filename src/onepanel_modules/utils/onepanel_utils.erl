%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains utility onepanel functions.
%% @end
%% ===================================================================
-module(onepanel_utils).

-include("registered_names.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([random_ascii_lowercase_sequence/1, get_application_version/0,
    get_application_ports/0]).
-export([get_node/1, get_node/2, get_nodes/0, get_nodes/2, get_host/1, get_hosts/0,
    get_host_and_port/1]).
-export([apply_on_hosts/5, apply_on_worker/3, dropwhile_failure/5, save_file_on_host/3,
    save_file_on_hosts/3, delete_file_on_host/2, delete_file_on_hosts/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% random_ascii_lowercase_sequence/1
%% ====================================================================
%% @doc Creates random sequence consisting of lowercase ASCII letters.
%% @end
-spec random_ascii_lowercase_sequence(Length :: integer()) -> Result when
    Result :: string().
%% ====================================================================
random_ascii_lowercase_sequence(Length) ->
    lists:foldl(fun(_, Acc) ->
        [random:uniform(26) + 96 | Acc] end, [], lists:seq(1, Length)).

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
%% @end
-spec get_node(Host :: string()) -> Result when
    Result :: node().
%% ====================================================================
get_node(Host) ->
    get_node(?APP_STR, Host).

%% get_node/2
%% ====================================================================
%% @doc Returns node of given type on provided host.
%% @end
-spec get_node(Type :: string(), Host :: string()) -> Result when
    Result :: node().
%% ====================================================================
get_node(Type, Host) ->
    list_to_atom(Type ++ "@" ++ Host).

%% get_nodes/0
%% ====================================================================
%% @doc Returns list of all application nodes.
%% @end
-spec get_nodes() -> Result when
    Result :: node().
%% ====================================================================
get_nodes() ->
    [node() | nodes(hidden)].

%% get_nodes/2
%% ====================================================================
%% @doc Returns list of nodes of given type on provided hosts.
%% @end
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
%% @end
-spec get_host(Node :: node()) -> Result when
    Result :: string().
%% ====================================================================
get_host(Node) ->
    [_, Host] = string:tokens(atom_to_list(Node), "@"),
    Host.

%% get_hosts/0
%% ====================================================================
%% @doc Returns list of hostnames of onepanel cluster nodes.
%% @end
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
%% @doc Saves file on all hosts.
%% @end
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
%% @doc Saves file on local host.
%% @end
-spec save_file_on_host(Path :: string(), Filename :: string(), Content :: string() | binary()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
save_file_on_host(Path, Filename, Content) ->
    Host = get_host(node()),
    try
        file:make_dir(Path),
        ok = file:write_file(filename:join(Path, Filename), Content),
        {ok, Host}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot save file ~p at directory ~p: ~p", [Filename, Path, Reason]),
            {error, Host}
    end.

%% delete_file_on_hosts/2
%% ====================================================================
%% @doc Deletes file on all hosts.
%% @end
-spec delete_file_on_hosts(Path :: string(), Filename :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
delete_file_on_hosts(Path, Filename) ->
    {_, HostsError} = apply_on_hosts(get_hosts(), ?MODULE, delete_file_on_host, [Path, Filename], ?RPC_TIMEOUT),
    case HostsError of
        [] -> ok;
        _ ->
            ?error("Cannot delete file ~p at directory ~p on following hosts: ~p", [Filename, Path, HostsError]),
            {error, {hosts, HostsError}}
    end.

%% delete_file_on_host/2
%% ====================================================================
%% @doc Deletes file on local host.
%% @end
-spec delete_file_on_host(Path :: string(), Filename :: string()) -> Result when
    Result :: {ok, Host :: string()} | {error, Host :: string()}.
%% ====================================================================
delete_file_on_host(Path, Filename) ->
    Host = get_host(node()),
    try
        ok = file:delete(filename:join(Path, Filename)),
        {ok, Host}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot delete file ~p at directory ~p: ~p", [Filename, Path, Reason]),
            {error, Host}
    end.

%% get_application_version/0
%% ====================================================================
%% @doc Returns version of onepanel application read from reltool.config
%% file.
%% @end
-spec get_application_version() -> Result when
    Result :: binary().
%% ====================================================================
get_application_version() ->
    case lists:dropwhile(fun
        ({?APP_NAME, _, _}) -> false;
        (_) -> true
    end, application:which_applications()) of
        [{?APP_NAME, _, Version} | _] -> list_to_binary(Version);
        _ -> <<"undefined">>
    end.

%% get_application_ports/1
%% ====================================================================
%% @doc Returns ports used by application.
-spec get_application_ports() -> Result when
    Result :: [Port :: integer()].
%% ====================================================================
get_application_ports() ->
    {ok, EtcDir} = application:get_env(?APP_NAME, platform_etc_dir),
    {ok, [Config]} = file:consult(filename:join([EtcDir, ?SOFTWARE_NAME, "app.config"])),
    SysConfig = proplists:get_value(?SOFTWARE_NAME, Config, []),
    lists:usort(proplists:get_value(application_ports, SysConfig, [])).

%% apply_on_worker/3
%% ====================================================================
%% @doc Applies function sequentially on worker components as long as
%% rpc calls fail with error "badrpc".
%% @end
-spec apply_on_worker(Module, Function, Arguments) -> Result when
    Result :: term(),
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()].
%% ====================================================================
apply_on_worker(Module, Function, Arguments) ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{workers = Workers}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        WorkerNodes = onepanel_utils:get_nodes(?WORKER_NAME, Workers),
        onepanel_utils:dropwhile_failure(WorkerNodes, Module, Function, Arguments, ?RPC_TIMEOUT)
    catch
        _:Reason ->
            ?error_stacktrace("Cannot apply ~p on worker: ~p", [{Module, Function, Arguments}, Reason]),
            {error, Reason}
    end.

%% get_host_and_port/1
%% ====================================================================
%% @doc Translates redirection point to pair of host and port.
-spec get_host_and_port(RedirectionPoint :: binary()) -> Result when
    Result :: {ok, Host :: binary(), Port :: integer()} | {error, Reason :: term()}.
%% ====================================================================
get_host_and_port(<<>>) ->
    {error, "Redirection point cannot be empty."};

get_host_and_port(RedirectionPoint) ->
    try
        case binary:split(RedirectionPoint, [<<"://">>, <<":">>], [global]) of
            [<<"http">>, Host] -> {ok, Host, 80};
            [<<"http">>, Host, Port | _] -> {ok, Host, binary_to_integer(Port)};
            [<<"https">>, Host] -> {ok, Host, 443};
            [<<"https">>, Host, Port | _] -> {ok, Host, binary_to_integer(Port)};
            [Host] -> {ok, Host, 443};
            [Host, Port | _] -> {ok, Host, binary_to_integer(Port)};
            _ -> {error, {invalid_refirection_point, RedirectionPoint}}
        end
    catch
        _:Reason ->
            {error, Reason}
    end.