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
-include_lib("ctool/include/logging.hrl").

%% API
-export([add/2, get/0, get/1, update/2]).
-export([get_supporting_storage/2, get_supporting_storages/2,
    get_file_popularity_details/2, get_autocleaning_details/2, get_soft_quota/1]).
-export([is_mounted_in_root/3]).
-export([add_storage/5, maybe_update_file_popularity/3, maybe_update_autocleaning/3]).

-type id() :: binary().
-type name() :: binary().
-type storage_params_map() :: #{Key :: atom() | binary() => Value :: binary()}.
-type storage_params_list() :: [{Key :: atom() | binary(), Value :: binary()}].
-type storage_map() :: #{Name :: name() => Params :: storage_params_map()}.
-type luma_config() :: {atom(), binary(), non_neg_integer()}.

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
    ?info("Adding ~b storage(s)", [maps:size(Storages)]),
    maps:fold(fun(Key, Value, _) ->
        StorageName = onepanel_utils:convert(Key, binary),
        StorageType = onepanel_utils:typed_get(type, Value, binary),

        ?info("Gathering storage configuration: \"~s\" (~s)", [StorageName, StorageType]),
        ReadOnly = onepanel_utils:typed_get(readonly, Value, boolean, false),
        UserCtx = get_storage_user_ctx(Node, StorageType, Value),
        Helper = get_storage_helper(Node, StorageType, UserCtx, Value),
        LumaConfig = get_luma_config(Node, Value),
        maybe_verify_storage(Helper, UserCtx, ReadOnly),

        ?info("Adding storage: \"~s\" (~s)", [StorageName, StorageType]),
        Result = add_storage(Node, StorageName, [Helper], ReadOnly, LumaConfig),
        case {Result, IgnoreExists} of
            {{ok, _StorageId}, _} ->
                ?info("Successfully added storage: \"~s\" (~s)", [StorageName, StorageType]),
                ok;
            {{error, already_exists}, true} ->
                ?info("Storage already exists, skipping: \"~s\" (~s)", [StorageName, StorageType]),
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
-spec get() -> list().
get() ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    {ok, Storages} = rpc:call(Node, storage, list, []),
    Ids = lists:map(fun(Storage) ->
        rpc:call(Node, storage, get_id, [Storage])
    end, Storages),
    [{ids, Ids}].


%%--------------------------------------------------------------------
%% @doc Returns details of a selected storage from op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec get(Id :: id()) -> storage_params_list().
get(Id) ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    {ok, Storage} = rpc:call(Node, storage, get, [Id]),
    get_storage(Node, Storage).


%%--------------------------------------------------------------------
%% @doc Returns storage supporting given space on given Node.
%% @end
%%--------------------------------------------------------------------
-spec get_supporting_storage(Node :: node(), SpaceId :: id()) -> id().
get_supporting_storage(Node, SpaceId) ->
    hd(get_supporting_storages(Node, SpaceId)).


%%--------------------------------------------------------------------
%% @doc Returns all storages supporting given space on given Node.
%% @end
%%--------------------------------------------------------------------
-spec get_supporting_storages(Node :: node(), SpaceId :: id()) -> [id()].
get_supporting_storages(Node, SpaceId) ->
    {ok, SpaceStorage} = rpc:call(Node, space_storage, get, [SpaceId]),
    rpc:call(Node, space_storage, get_storage_ids, [SpaceStorage]).


%%--------------------------------------------------------------------
%% @doc Checks whether space storage is mounted in root.
%% @end
%%--------------------------------------------------------------------
-spec is_mounted_in_root(Node :: node(), SpaceId :: id(), StorageId :: id()) ->
    boolean().
is_mounted_in_root(Node, SpaceId, StorageId) ->
    {ok, SpaceStorage} = rpc:call(Node, space_storage, get, [SpaceId]),
    MountedInRoot = rpc:call(Node, space_storage, get_mounted_in_root,
        [SpaceStorage]
    ),
    lists:member(StorageId, MountedInRoot).


%%--------------------------------------------------------------------
%% @doc Updates details of a selected storage in op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec update(Name :: name(), Args :: maps:map()) -> ok.
update(Id, Args) ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    Storage = op_worker_storage:get(Id),
    {ok, Id} = onepanel_lists:get(id, Storage),
    {ok, Type} = onepanel_lists:get(type, Storage),
    Args2 = #{<<"timeout">> => onepanel_utils:typed_get(timeout, Args, binary)},
    {ok, _} = rpc:call(Node, storage, update_helper, [Id, Type, Args2]),
    ok.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Enables or disables file popularity.
%% @end
%%-------------------------------------------------------------------
-spec maybe_update_file_popularity(Node :: node(), SpaceId :: id(),
    maps:map()) -> {ok, id()}.
maybe_update_file_popularity(_Node, SpaceId, Args) when map_size(Args) =:= 0 ->
    {ok, SpaceId};
maybe_update_file_popularity(Node, SpaceId, Args) ->
    case onepanel_utils:typed_get(enabled, Args, boolean) of
        true ->
            rpc:call(Node, space_storage, enable_file_popularity, [SpaceId]);
        false ->
            rpc:call(Node, space_storage, disable_file_popularity, [SpaceId])
    end.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Updates autocleaning configuration.
%% @end
%%-------------------------------------------------------------------
-spec maybe_update_autocleaning(Node :: node(), SpaceId :: id(), maps:map()) -> {ok, id()}.
maybe_update_autocleaning(_Node, SpaceId, Args) when map_size(Args) =:= 0 ->
    {ok, SpaceId};
maybe_update_autocleaning(Node, SpaceId, Args) ->
    Settings = #{
        enabled => onepanel_utils:typed_get(enabled, Args, boolean, undefined),
        lower_file_size_limit => onepanel_utils:typed_get([settings, lower_file_size_limit], Args, integer, undefined),
        upper_file_size_limit => onepanel_utils:typed_get([settings, upper_file_size_limit], Args, integer, undefined),
        max_file_not_opened_hours => onepanel_utils:typed_get([settings, max_file_not_opened_hours], Args, integer, undefined),
        target => onepanel_utils:typed_get([settings, target], Args, integer, undefined),
        threshold => onepanel_utils:typed_get([settings, threshold], Args, integer, undefined)
    },
    case rpc:call(Node, space_cleanup_api, configure_autocleaning, [SpaceId, onepanel_maps:remove_undefined(Settings)]) of
        {error, Reason} ->
            ?throw_error({?ERR_CONFIG_AUTOCLEANING, Reason});
        Result -> Result
    end.

%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching file popularity details
%% from provider.
%% @end
%%-------------------------------------------------------------------
-spec get_file_popularity_details(Node :: node(), SpaceId :: id()) -> proplists:proplist().
get_file_popularity_details(Node, SpaceId) ->
    rpc:call(Node, space_storage, get_file_popularity_details, [SpaceId]).

%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching autocleaning details from
%% provider.
%% @end
%%-------------------------------------------------------------------
-spec get_autocleaning_details(Node :: node(), SpaceId :: id()) -> proplists:proplist().
get_autocleaning_details(Node, SpaceId) ->
    Details = rpc:call(Node, space_cleanup_api, get_details, [SpaceId]),
    onepanel_lists:map_undefined_to_null(Details).

%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching soft quota limit from
%% given provider.
%% @end
%%-------------------------------------------------------------------
-spec get_soft_quota(Node :: node()) -> non_neg_integer().
get_soft_quota(Node) ->
    rpc:call(Node, space_storage, get_soft_quota, []).

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
    ]);

get_storage_user_ctx(Node, <<"glusterfs">>, _Params) ->
    rpc:call(Node, helper, new_glusterfs_user_ctx, [0, 0]);

get_storage_user_ctx(Node, <<"nulldevice">>, _Params) ->
    rpc:call(Node, helper, new_nulldevice_user_ctx, [0, 0]).

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
        get_helper_opt_args([
            {signatureVersion, binary},
            {timeout, binary},
            {blockSize, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false)
    ]);
get_storage_helper(Node, <<"swift">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_swift_helper, [
        onepanel_utils:typed_get(authUrl, Params, binary),
        onepanel_utils:typed_get(containerName, Params, binary),
        onepanel_utils:typed_get(tenantName, Params, binary),
        get_helper_opt_args([
            {timeout, binary},
            {blockSize, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false)
    ]);
get_storage_helper(Node, <<"glusterfs">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_glusterfs_helper, [
        onepanel_utils:typed_get(volume, Params, binary),
        onepanel_utils:typed_get(hostname, Params, binary),
        get_helper_opt_args([
            {port, binary},
            {mountPoint, binary},
            {transport, binary},
            {xlatorOptions, binary},
            {timeout, binary},
            {blockSize, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false)
    ]);
get_storage_helper(Node, <<"nulldevice">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_nulldevice_helper, [
        get_helper_opt_args([
            {latencyMin, binary},
            {latencyMax, binary},
            {timeoutProbability, binary},
            {filter, binary},
            {timeout, binary}
        ], Params),
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
    ?info("Verifying write access to storage"),
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
    {FileId, FileContent} = create_test_file(Node, Helper, UserCtx),
    {FileId2, FileContent2} = verify_test_file(
        Nodes, Helper, UserCtx, FileId, FileContent
    ),
    read_test_file(Node, Helper, UserCtx, FileId2, FileContent2),
    remove_test_file(Node, Helper, UserCtx, FileId2).


%%--------------------------------------------------------------------
%% @private @doc Checks whether storage is read/write accessible for all
%% op_worker service nodes by creating, reading and removing test files.
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
    {FileId2, FileContent2} = create_test_file(Node, Helper, UserCtx),
    verify_test_file(Nodes, Helper, UserCtx, FileId2, FileContent2).


%%--------------------------------------------------------------------
%% @private @doc Creates storage test file.
%% @end
%%--------------------------------------------------------------------
-spec create_test_file(Node :: node(), Helper :: any(), UserCtx :: any()) ->
    {FileId :: binary(), FileContent :: binary()} | no_return().
create_test_file(Node, Helper, UserCtx) ->
    FileId = rpc:call(Node, storage_detector, generate_file_id, []),
    Args = [Helper, UserCtx, FileId],
    case rpc:call(Node, storage_detector, create_test_file, Args) of
        <<_/binary>> = FileContent ->
            {FileId, FileContent};
        {badrpc, {'EXIT', {Reason, Stacktrace}}} ->
            ?throw_error({?ERR_STORAGE_TEST_FILE_CREATE, Node, Reason}, Stacktrace)
    end.


%%--------------------------------------------------------------------
%% @private @doc Reads storage test file.
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
    ReadOnly :: boolean(), LumaConfig :: luma_config()) ->
    {ok, StorageId :: binary()} | {error, Reason :: term()}.
add_storage(Node, StorageName, Helpers, ReadOnly, LumaConfig) ->
    Storage = rpc:call(Node, storage, new, [StorageName, Helpers, ReadOnly, LumaConfig]),
    rpc:call(Node, storage, create, [Storage]).


%%--------------------------------------------------------------------
%% @private @doc Returns storage details from op_worker service configuration.
%% @end
%%--------------------------------------------------------------------
-spec get_storage(Node :: node(), Storage :: any()) ->
    Storage :: storage_params_list().
get_storage(Node, Storage) ->
    [Helper | _] = rpc:call(Node, storage, get_helpers, [Storage]),
    AdminCtx = rpc:call(Node, helper, get_admin_ctx, [Helper]),
    AdminCtx2 = maps:with([<<"username">>, <<"accessKey">>], AdminCtx),
    HelperArgs = rpc:call(Node, helper, get_args, [Helper]),
    LumaConfig = rpc:call(Node, storage, get_luma_config_map, [Storage]),
    [
        {id, rpc:call(Node, storage, get_id, [Storage])},
        {name, rpc:call(Node, storage, get_name, [Storage])},
        {type, rpc:call(Node, helper, get_name, [Helper])},
        {readonly, rpc:call(Node, storage, is_readonly, [Storage])},
        {insecure, rpc:call(Node, helper, is_insecure, [Helper])},
        {lumaEnabled, maps:get(enabled, LumaConfig, false)},
        {lumaUrl, maps:get(url, LumaConfig, null)},
        {lumaCacheTimeout, maps:get(cache_timeout, LumaConfig, null)}
    ] ++ maps:to_list(AdminCtx2) ++ maps:to_list(HelperArgs).

%%--------------------------------------------------------------------
%% @private @doc Parses LUMA config arguments if lumaEnabled is set to
%% true in StorageParams and returns luma config acquired from provider.
%% Throws error if lumaEnabled is true and other arguments are missing.
%% @end
%%--------------------------------------------------------------------
-spec get_luma_config(Node :: node(), StorageParams :: storage_params_map()) ->
    undefined | luma_config().
get_luma_config(Node, StorageParams) ->
    case onepanel_utils:typed_get(lumaEnabled, StorageParams, boolean, false) of
        true ->
            Url = get_required_luma_arg(StorageParams, lumaUrl, binary),
            CacheTimeout = get_required_luma_arg(StorageParams, lumaCacheTimeout, integer),
            ApiKey = onepanel_utils:typed_get(lumaApiKey, StorageParams, binary, undefined),
            rpc:call(Node, luma_config, new, [Url, CacheTimeout, ApiKey]);
        false ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @private @doc Returns LUMA argument value associated with Key
%% in StorageParams. Throws error if key is missing
%% @end
%%--------------------------------------------------------------------
-spec get_required_luma_arg(StorageParams :: storage_params_map(), Key ::atom(),
    Type :: onepanel_utils:type()) -> term().
get_required_luma_arg(StorageParams, Key, Type) ->
    case onepanel_utils:typed_get(Key, StorageParams, Type) of
        {error, _} ->
            ?throw_error( ?ERR_LUMA_CONFIG(Key));
        Value ->
            Value
    end.
