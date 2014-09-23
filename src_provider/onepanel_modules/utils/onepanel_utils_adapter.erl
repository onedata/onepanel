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
-module(onepanel_utils_adapter).

-include("registered_names.hrl").
-include("onepanel_modules/updater/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([apply_on_worker/3]).
-export([get_software_version/0, get_available_software_versions/0, get_software_version_name/1, get_software_version_record/1]).


%% ====================================================================
%% API functions
%% ====================================================================

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
        WorkerNodes = onepanel_utils:get_nodes(?DEFAULT_WORKER_NAME, Workers),
        onepanel_utils:dropwhile_failure(WorkerNodes, Module, Function, Arguments, ?RPC_TIMEOUT)
    catch
        _:Reason ->
            ?error("Cannot apply ~p on worker: ~p", [{Module, Function, Arguments}, Reason]),
            {error, Reason}
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
        Nodes = onepanel_utils:get_nodes(?DEFAULT_WORKER_NAME, Workers),
        Version = onepanel_utils:dropwhile_failure(Nodes, node_manager, check_vsn, [], ?RPC_TIMEOUT),
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
        {ok, URL} = application:get_env(?APP_NAME, onedata_repository_url),
        Options = [{connect_timeout, ?CONNECTION_TIMEOUT}],
        {ok, "200", _ResHeaders, ResBody} = ibrowse:send_req(URL ++ "/get_versions.php", [{content_type, "application/json"}], get, [], Options),
        {_, List} = mochijson2:decode(ResBody),
        sort_versions(proplists:get_value(<<"VeilCluster-Linux.rpm">>, List))
    catch
        _:Reason ->
            ?error("Cannot get available software versions from repository: ~p", [Reason]),
            undefined
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