%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains helper function used during op_worker service
%%% storage configuration.
%%% @end
%%%--------------------------------------------------------------------
-module(op_worker_storage).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

-include_lib("hackney/include/hackney_lib.hrl").

%% API
-export([add/2, get/0, get/1, update/2]).
-export([add_storage/4]).

-type name() :: binary().
-type storage_params_map() :: #{Key :: atom() | binary() => Value :: binary()}.
-type storage_params_list() :: [{Key :: atom() | binary(), Value :: binary()}].
-type storage_map() :: #{Name :: name() => Params :: storage_params_map()}.
-type storage_list() :: [{Name :: name(), Params :: storage_params_list()}].

-export_type([storage_list/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Verifies that each of provided storages is accessible for all op_worker
%% service nodes. In case of a successful verification proceeds with storage
%% addition.
%% @end
%%--------------------------------------------------------------------
-spec add(Storages :: storage_map(), IgnoreExists :: boolean()) -> ok | no_return().
add(Storages, IgnoreExists) ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    maps:fold(fun(Key, Value, _) ->
        StorageName = onepanel_utils:convert(Key, binary),
        StorageType = onepanel_utils:typed_get(type, Value, binary),
        ReadOnly = onepanel_utils:typed_get(readonly, Value, boolean, false),
        UserCtx = get_storage_user_ctx(Node, StorageType, Value),
        Helper = get_storage_helper(Node, StorageType, UserCtx, Value),
        maybe_verify_storage(Helper, UserCtx, ReadOnly),
        Result = add_storage(Node, StorageName, [Helper], ReadOnly),
        case {Result, IgnoreExists} of
            {{ok, _StorageId}, _} ->
                ok;
            {{error, already_exists}, true} ->
                ok;
            {{error, Reason}, _} ->
                ?throw_error({?ERR_STORAGE_ADDITION, Reason})
        end
    end, [], Storages),
    ok.


%%--------------------------------------------------------------------
%% @doc Returns lists of storages' details currently configured in op_worker
%% service.
%% @end
%%--------------------------------------------------------------------
-spec get() -> storage_list().
get() ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    {ok, Storages} = rpc:call(Node, storage, list, []),
    lists:foldl(fun(Storage, Acc) ->
        Acc ++ get_storage(Node, Storage)
    end, [], Storages).


%%--------------------------------------------------------------------
%% @doc Returns details of a selected storage from op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec get(Name :: name()) -> storage_list().
get(Name) ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    {ok, Storage} = rpc:call(Node, storage, select, [Name]),
    get_storage(Node, Storage).


%%--------------------------------------------------------------------
%% @doc Updates details of a selected storage in op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec update(Name :: name(), Args :: maps:map()) -> ok.
update(Name, Args) ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    [{_, Params}] = op_worker_storage:get(Name),
    {ok, Id} = onepanel_lists:get(id, Params),
    {ok, Type} = onepanel_lists:get(type, Params),
    Args2 = #{<<"timeout">> => onepanel_utils:typed_get(timeout, Args, binary)},
    {ok, _} = rpc:call(Node, storage, update_helper, [Id, Type, Args2]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Returns storage user context record.
%% @end
%%--------------------------------------------------------------------
-spec get_storage_user_ctx(Node :: node(), StorageType :: binary(),
    Params :: storage_params_map()) -> UserCtx :: any().
get_storage_user_ctx(Node, <<"ceph">>, Params) ->
    rpc:call(Node, helper, new_ceph_user_ctx, [
        onepanel_utils:typed_get(username, Params, binary),
        onepanel_utils:typed_get(key, Params, binary)
    ]);

get_storage_user_ctx(Node, <<"posix">>, _Params) ->
    rpc:call(Node, helper, new_posix_user_ctx, [0, 0]);

get_storage_user_ctx(Node, <<"s3">>, Params) ->
    rpc:call(Node, helper, new_s3_user_ctx, [
        onepanel_utils:typed_get(accessKey, Params, binary),
        onepanel_utils:typed_get(secretKey, Params, binary)
    ]);

get_storage_user_ctx(Node, <<"swift">>, Params) ->
    rpc:call(Node, helper, new_swift_user_ctx, [
        onepanel_utils:typed_get(username, Params, binary),
        onepanel_utils:typed_get(password, Params, binary)
    ]).


%%--------------------------------------------------------------------
%% @private @doc Returns storage helper record.
%% @end
%%--------------------------------------------------------------------
-spec get_storage_helper(Node :: node(), StorageType :: binary(), UserCtx :: any(),
    Params :: storage_params_map()) -> Helper :: any().
get_storage_helper(Node, <<"ceph">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_ceph_helper, [
        onepanel_utils:typed_get(monitorHostname, Params, binary),
        onepanel_utils:typed_get(clusterName, Params, binary),
        onepanel_utils:typed_get(poolName, Params, binary),
        get_helper_opt_args([{timeout, binary}], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false)
    ]);
get_storage_helper(Node, <<"posix">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_posix_helper, [
        onepanel_utils:typed_get(mountPoint, Params, binary),
        get_helper_opt_args([{timeout, binary}], Params),
        UserCtx
    ]);
get_storage_helper(Node, <<"s3">>, UserCtx, Params) ->
    #hackney_url{scheme = S3Scheme, host = S3Host, port = S3Port} =
        hackney_url:parse_url(onepanel_utils:typed_get(hostname, Params, binary)),
    rpc:call(Node, helper, new_s3_helper, [
        onepanel_utils:join([S3Host, S3Port], <<":">>),
        onepanel_utils:typed_get(bucketName, Params, binary),
        S3Scheme =:= https,
        get_helper_opt_args([{timeout, binary}, {blockSize, binary}], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false)
    ]);
get_storage_helper(Node, <<"swift">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_swift_helper, [
        onepanel_utils:typed_get(authUrl, Params, binary),
        onepanel_utils:typed_get(containerName, Params, binary),
        onepanel_utils:typed_get(tenantName, Params, binary),
        get_helper_opt_args([{timeout, binary}, {blockSize, binary}], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false)
    ]).


%%--------------------------------------------------------------------
%% @private @doc Returns storage helper optional argument.
%% @end
%%--------------------------------------------------------------------
-spec get_helper_opt_args(KeySpec :: [{Key :: atom(), Type :: onepanel_utils:type()}],
    Params :: storage_params_map()) -> OptArgs :: #{}.
get_helper_opt_args(KeysSpec, Params) ->
    lists:foldl(fun({Key, Type}, OptArgs) ->
        case onepanel_utils:typed_get(Key, Params, Type) of
            #error{} ->
                OptArgs;
            Value ->
                maps:put(onepanel_utils:convert(Key, binary), Value, OptArgs)
        end
    end, #{}, KeysSpec).

%%--------------------------------------------------------------------
%% @private @doc For read-write storage verifies that it is accessible for all
%% op_worker service nodes.
%% @end
%%--------------------------------------------------------------------
-spec maybe_verify_storage(Helper :: any(), UserCtx :: any(), Readonly :: boolean()) ->
    ok | no_return().
maybe_verify_storage(_Helper, _UserCtx, true) ->
    ok;
maybe_verify_storage(Helper, UserCtx, _) ->
    verify_storage(Helper, UserCtx).

%%--------------------------------------------------------------------
%% @private @doc Verifies that storage is accessible for all op_worker
%% service nodes.
%% @end
%%--------------------------------------------------------------------
-spec verify_storage(Helper :: any(), UserCtx :: any()) ->
    ok | no_return().
verify_storage(Helper, UserCtx) ->
    [Node | Nodes] = service_op_worker:get_nodes(),
    {FileId, FileContent} = write_test_file(Node, Helper, UserCtx),
    {FileId2, FileContent2} = verify_test_file(
        Nodes, Helper, UserCtx, FileId, FileContent
    ),
    read_test_file(Node, Helper, UserCtx, FileId2, FileContent2),
    remove_test_file(Node, Helper, UserCtx, FileId2).


%%--------------------------------------------------------------------
%% @private @doc Writes storage test file.
%% @end
%%--------------------------------------------------------------------
-spec write_test_file(Node :: node(), Helper :: any(), UserCtx :: any()) ->
    {FileId :: binary(), FileContent :: binary()} | no_return().
write_test_file(Node, Helper, UserCtx) ->
    FileId = rpc:call(Node, storage_detector, generate_file_id, []),
    Args = [Helper, UserCtx, FileId],
    case rpc:call(Node, storage_detector, create_test_file, Args) of
        <<_/binary>> = FileContent ->
            {FileId, FileContent};
        {badrpc, {'EXIT', {Reason, Stacktrace}}} ->
            ?throw_error({?ERR_STORAGE_TEST_FILE_WRITE, Node, Reason}, Stacktrace)
    end.


%%--------------------------------------------------------------------
%% @private @doc Writes storage test file.
%% @end
%%--------------------------------------------------------------------
-spec read_test_file(Node :: node(), Helper :: any(), UserCtx :: any(),
    FileId :: binary(), FileContent :: binary()) -> ok | no_return().
read_test_file(Node, Helper, UserCtx, FileId, FileContent) ->
    Args = [Helper, UserCtx, FileId],
    ActualFileContent = rpc:call(Node, storage_detector, read_test_file, Args),

    case ActualFileContent of
        FileContent ->
            ok;
        <<_/binary>> ->
            ?throw_error({?ERR_STORAGE_TEST_FILE_READ, Node,
                {invalid_content, FileContent, ActualFileContent}});
        {badrpc, {'EXIT', {Reason, Stacktrace}}} ->
            ?throw_error({?ERR_STORAGE_TEST_FILE_READ, Node, Reason}, Stacktrace)
    end.


%%--------------------------------------------------------------------
%% @private @doc Checks whether storage test file content matches expected one.
%% @end
%%--------------------------------------------------------------------
-spec verify_test_file(Nodes :: [node()], Helper :: any(), UserCtx :: any(),
    FileId :: binary(), FileContent :: binary()) ->
    {FileId :: binary(), FileContent :: binary()} | no_return().
verify_test_file([], _Helper, _UserCtx, FileId, FileContent) ->
    {FileId, FileContent};

verify_test_file([Node | Nodes], Helper, UserCtx, FileId, FileContent) ->
    read_test_file(Node, Helper, UserCtx, FileId, FileContent),
    remove_test_file(Node, Helper, UserCtx, FileId),
    {FileId2, FileContent2} = write_test_file(Node, Helper, UserCtx),
    verify_test_file(Nodes, Helper, UserCtx, FileId2, FileContent2).


%%--------------------------------------------------------------------
%% @private @doc Removes storage test file.
%% @end
%%--------------------------------------------------------------------
-spec remove_test_file(Node :: node(), Helper :: any(), UserCtx :: any(),
    FileId :: binary()) -> ok | no_return().
remove_test_file(Node, Helper, UserCtx, FileId) ->
    Args = [Helper, UserCtx, FileId],
    case rpc:call(Node, storage_detector, remove_test_file, Args) of
        {badrpc, {'EXIT', {Reason, Stacktrace}}} ->
            ?throw_error({?ERR_STORAGE_TEST_FILE_REMOVE, Node, Reason}, Stacktrace);
        _ -> ok
    end.


%%--------------------------------------------------------------------
%% @private @doc Adds storage to the op_worker service configuration.
%% @end
%%--------------------------------------------------------------------
-spec add_storage(Node :: node(), StorageName :: binary(), Helpers :: list(),
    ReadOnly :: boolean()) -> {ok, StorageId :: binary()} | {error, Reason :: term()}.
add_storage(Node, StorageName, Helpers, ReadOnly) ->
    Storage = rpc:call(Node, storage, new, [StorageName, Helpers, ReadOnly]),
    rpc:call(Node, storage, create, [Storage]).


%%--------------------------------------------------------------------
%% @private @doc Returns storage details from op_worker service configuration.
%% @end
%%--------------------------------------------------------------------
-spec get_storage(Node :: node(), Storage :: any()) -> Storage :: storage_list().
get_storage(Node, Storage) ->
    Id = rpc:call(Node, storage, get_id, [Storage]),
    Name = rpc:call(Node, storage, get_name, [Storage]),
    [Helper | _] = rpc:call(Node, storage, get_helpers, [Storage]),
    Type = rpc:call(Node, helper, get_name, [Helper]),
    Args = maps:to_list(rpc:call(Node, helper, get_args, [Helper])),
    [{Name, [{id, Id}, {type, Type} | Args]}].