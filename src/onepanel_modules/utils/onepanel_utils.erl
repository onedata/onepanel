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
-include("onepanel_modules/updater/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([random_ascii_lowercase_sequence/1]).
-export([get_application_version/0, get_software_version/0, get_available_software_versions/0, get_software_version_name/1, get_software_version_record/1]).
-export([get_node/1, get_node/2, get_nodes/0, get_nodes/2, get_host/1, get_hosts/0, get_control_panel_hosts/0]).
-export([apply_on_hosts/5, apply_on_worker/3, dropwhile_failure/5, save_file_on_host/3, save_file_on_hosts/3]).

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
        WorkerNodes = get_nodes(?DEFAULT_WORKER_NAME, Workers),
        dropwhile_failure(WorkerNodes, Module, Function, Arguments, ?RPC_TIMEOUT)
    catch
        _:Reason ->
            ?error("Cannot apply ~p on worker: ~p", [{Module, Function, Arguments}, Reason]),
            {error, Reason}
    end.


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
%% @doc Saves Global Registry certificate cert on all hosts.
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
%% @doc Saves Global Registry certificate cert on host.
%% @end
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


%% get_software_version/0
%% ====================================================================
%% @doc Returns installed software version.
%% @end
-spec get_software_version() -> Result when
    Result :: binary() | undefined.
%% ====================================================================
get_software_version() ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{workers = Workers}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        Version = dropwhile_failure(get_nodes(?DEFAULT_WORKER_NAME, Workers), node_manager, check_vsn, [], ?RPC_TIMEOUT),
        list_to_binary(Version)
    catch
        _:Reason ->
            ?error("Cannot get current software version: ~p", [Reason]),
            undefined
    end.


%% get_software_version_name/1
%% ====================================================================
%% @doc Returns version in binary form.
%% @end
-spec get_software_version_name(Version :: #version{}) -> Result when
    Result :: binary().
%% ====================================================================
get_software_version_name(#version{major = Major, minor = Minor, patch = Patch}) ->
    <<(integer_to_binary(Major))/binary, ".", (integer_to_binary(Minor))/binary, ".", (integer_to_binary(Patch))/binary>>.


%% get_software_version_record/1
%% ====================================================================
%% @doc Returns version in record form.
%% @end
-spec get_software_version_record(Version :: binary()) -> Result when
    Result :: #version{}.
%% ====================================================================
get_software_version_record(Version) ->
    [Major, Minor, Patch | _] = binary:split(Version, <<".">>, [global]),
    #version{major = binary_to_integer(Major), minor = binary_to_integer(Minor), patch = binary_to_integer(Patch)}.


%% get_available_software_versions/0
%% ====================================================================
%% @doc Returns available software versions read from remote repository.
%% @end
-spec get_available_software_versions() -> Result when
    Result :: [binary()] | undefined.
%% ====================================================================
get_available_software_versions() ->
    try
        {ok, URL} = application:get_env(?APP_NAME, software_repository_url),
        Options = [{connect_timeout, ?CONNECTION_TIMEOUT}],
        {ok, "200", _ResHeaders, ResBody} = ibrowse:send_req(URL ++ "/get_versions.php", [{content_type, "application/json"}], get, [], Options),
        {_, List} = mochijson2:decode(ResBody),
        sort_versions(proplists:get_value(<<"VeilCluster-Linux.rpm">>, List))
    catch
        _:Reason ->
            ?error("Cannot get available software versions from repository: ~p", [Reason]),
            undefined
    end.


%% get_control_panel_hosts/0
%% ====================================================================
%% @doc Returns list of control panel hosts
%% @end
-spec get_control_panel_hosts() -> Result when
    Result :: {ok, Hosts :: [string()]} | {error, Reason :: term()}.
%% ====================================================================
get_control_panel_hosts() ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{workers = Workers}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, Workers}
    catch
        _:Reason ->
            ?error("Cannot get control panel hosts: ~p", [Reason]),
            {error, Reason}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% sort_versions/1
%% ====================================================================
%% @doc Sorts versions in descending order and eliminates duplicates.
%% @end
-spec sort_versions(Versions :: [#version{}]) -> Result when
    Result :: [#version{}].
%% ====================================================================
sort_versions(Versions) ->
    CmpPatch = fun
        (#version{patch = PatchA}, #version{patch = PatchB}) ->
            PatchA >= PatchB
    end,
    CmpMinor = fun
        (#version{minor = Minor} = A, #version{minor = Minor} = B) ->
            CmpPatch(A, B);
        (#version{minor = MinorA}, #version{minor = MinorB}) ->
            MinorA > MinorB
    end,
    CmpMajor = fun
        (#version{major = Major} = A, #version{major = Major} = B) ->
            CmpMinor(A, B);
        (#version{major = MajorA}, #version{major = MajorB}) ->
            MajorA > MajorB
    end,
    lists:usort(CmpMajor, lists:map(fun(Version) ->
        get_software_version_record(Version)
    end, Versions)).