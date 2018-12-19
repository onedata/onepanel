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
    get_files_popularity_configuration/2, get_auto_cleaning_configuration/2]).
-export([is_mounted_in_root/3]).
-export([add_storage/5, maybe_update_file_popularity/3,
    maybe_update_auto_cleaning/3, invalidate_luma_cache/1]).

-type id() :: binary().
-type name() :: binary().
-type storage_params_map() :: #{Key :: atom() | binary() => Value :: binary()}.
-type storage_map() :: #{Name :: name() => Params :: storage_params_map()}.
-type luma_config() :: {atom(), binary(), undefined | binary()}.

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
    Node = onepanel_cluster:service_to_node(service_op_worker:name()),
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
    Node = onepanel_cluster:service_to_node(service_op_worker:name()),
    {ok, Storages} = rpc:call(Node, storage, list, []),
    Ids = lists:map(fun(Storage) ->
        rpc:call(Node, storage, get_id, [Storage])
    end, Storages),
    [{ids, Ids}].


%%--------------------------------------------------------------------
%% @doc Returns details of a selected storage from op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec get(Id :: id()) -> storage_params_map().
get(Id) ->
    Node = onepanel_cluster:service_to_node(service_op_worker:name()),
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
    Node = onepanel_cluster:service_to_node(service_op_worker:name()),
    Storage = op_worker_storage:get(Id),
    {ok, Id} = onepanel_maps:get(id, Storage),
    {ok, Type} = onepanel_maps:get(type, Storage),
    Args2 = #{<<"timeout">> => onepanel_utils:typed_get(timeout, Args, binary)},
    {ok, _} = rpc:call(Node, storage, update_helper, [Id, Type, Args2]),
    ok.


%%--------------------------------------------------------------------
%% @doc Checks if storage with given name exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(Node :: node(), StorageName :: name()) -> boolean().
exists(Node, StorageName) ->
    case rpc:call(Node, storage, select, [StorageName]) of
        {error, not_found} -> false;
        {ok, _} -> true
    end.


%%-------------------------------------------------------------------
%% @private
%% @doc
%% Enables or disables file popularity.
%% @end
%%-------------------------------------------------------------------
-spec maybe_update_file_popularity(Node :: node(), SpaceId :: id(),
    maps:map()) -> ok.
maybe_update_file_popularity(_Node, _SpaceId, Args) when map_size(Args) =:= 0 ->
    ok;
maybe_update_file_popularity(Node, SpaceId, Args) ->
    Configuration = parse_files_popularity_configuration(Args),
    case rpc:call(Node, file_popularity_api, configure, [SpaceId, Configuration]) of
        {error, Reason} ->
            ?throw_error({?ERR_CONFIG_FILES_POPULARITY, Reason});
        Result -> Result
    end.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Updates autocleaning configuration.
%% @end
%%-------------------------------------------------------------------
-spec maybe_update_auto_cleaning(Node :: node(), SpaceId :: id(), maps:map()) -> ok.
maybe_update_auto_cleaning(_Node, _SpaceId, Args) when map_size(Args) =:= 0 ->
    ok;
maybe_update_auto_cleaning(Node, SpaceId, Args) ->
    Configuration = parse_auto_cleaning_configuration(Args),
    case rpc:call(Node, autocleaning_api, configure, [SpaceId, Configuration]) of
        {error, Reason} ->
            ?throw_error({?ERR_CONFIG_AUTO_CLEANING, Reason});
        Result -> Result
    end.

%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching files-popularity
%% configuration from provider.
%% @end
%%-------------------------------------------------------------------
-spec get_files_popularity_configuration(Node :: node(), SpaceId :: id()) -> proplists:proplist().
get_files_popularity_configuration(Node, SpaceId) ->
    case rpc:call(Node, file_popularity_api, get_configuration, [SpaceId]) of
        {ok, DetailsMap} ->
            maps:to_list(onepanel_maps:get_store_multiple([
                {[enabled], [enabled]},
                {[example_query], [exampleQuery]},
                {[last_open_hour_weight], [lastOpenHourWeight]},
                {[avg_open_count_per_day_weight], [avgOpenCountPerDayWeight]},
                {[max_avg_open_count_per_day], [maxAvgOpenCountPerDay]}
            ], DetailsMap));
        {error, Reason} ->
            ?throw_error({?ERR_FILES_POPULARITY, Reason})
    end.

%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching autocleaning details from
%% provider.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_configuration(Node :: node(), SpaceId :: id()) -> proplists:proplist().
get_auto_cleaning_configuration(Node, SpaceId) ->
    DetailsMap = rpc:call(Node, autocleaning_api, get_configuration, [SpaceId]),
    DetailsMap2 = onepanel_maps:get_store_multiple([
        {[rules, min_file_size], [rules, minFileSize]},
        {[rules, max_file_size], [rules, maxFileSize]},
        {[rules, min_hours_since_last_open], [rules, minHoursSinceLastOpen]},
        {[rules, max_open_count], [rules, maxOpenCount]},
        {[rules, max_hourly_moving_average], [rules, maxHourlyMovingAverage]},
        {[rules, max_daily_moving_average], [rules, maxDailyMovingAverage]},
        {[rules, max_monthly_moving_average], [rules, maxMonthlyMovingAverage]}
    ], DetailsMap, DetailsMap),
    onepanel_lists:map_undefined_to_null(onepanel_maps:to_list(DetailsMap2)).

%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for invalidating luma cache on given
%% provider for given storage.
%% @end
%%-------------------------------------------------------------------
-spec invalidate_luma_cache(StorageId :: binary) -> ok.
invalidate_luma_cache(StorageId) ->
    Node = onepanel_cluster:service_to_node(service_op_worker:name()),
    ok = rpc:call(Node, luma_cache, invalidate, [StorageId]).

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

get_storage_user_ctx(Node, <<"cephrados">>, Params) ->
    rpc:call(Node, helper, new_cephrados_user_ctx, [
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
    rpc:call(Node, helper, new_nulldevice_user_ctx, [0, 0]);

get_storage_user_ctx(Node, <<"webdav">>, Params) ->
    rpc:call(Node, helper, new_webdav_user_ctx, [
        <<_/binary>> = onepanel_utils:typed_get(credentialsType, Params, binary),
        <<_/binary>> = onepanel_utils:typed_get(credentials, Params, binary, <<>>)
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
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"flat">>)
    ]);
get_storage_helper(Node, <<"cephrados">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_cephrados_helper, [
        onepanel_utils:typed_get(monitorHostname, Params, binary),
        onepanel_utils:typed_get(clusterName, Params, binary),
        onepanel_utils:typed_get(poolName, Params, binary),
        get_helper_opt_args([
            {timeout, binary},
            {blockSize, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"flat">>)
    ]);
get_storage_helper(Node, <<"posix">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_posix_helper, [
        onepanel_utils:typed_get(mountPoint, Params, binary),
        get_helper_opt_args([{timeout, binary}], Params),
        UserCtx,
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"canonical">>)
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
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"flat">>)
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
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"flat">>)
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
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"canonical">>)
    ]);
get_storage_helper(Node, <<"nulldevice">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_nulldevice_helper, [
        get_helper_opt_args([
            {latencyMin, binary},
            {latencyMax, binary},
            {timeoutProbability, binary},
            {filter, binary},
            {simulatedFilesystemParameters, binary},
            {simulatedFilesystemGrowSpeed, binary},
            {timeout, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"canonical">>)
    ]);
get_storage_helper(Node, <<"webdav">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_webdav_helper, [
        onepanel_utils:typed_get(endpoint, Params, binary),
        get_helper_opt_args([
            {verifyServerCertificate, binary},
            {authorizationHeader, binary},
            {rangeWriteSupport, binary},
            {connectionPoolSize, binary},
            {maximumUploadSize, binary},
            {timeout, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"canonical">>)
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
    remove_test_file(Node, Helper, UserCtx, FileId2, size(FileContent2)).


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
    remove_test_file(Node, Helper, UserCtx, FileId, size(FileContent)),
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
            ?throw_stacktrace({?ERR_STORAGE_TEST_FILE_CREATE, Node, Reason}, undefined, Stacktrace)
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
            ?throw_stacktrace({?ERR_STORAGE_TEST_FILE_READ, Node, Reason}, undefined, Stacktrace)
    end.


%%--------------------------------------------------------------------
%% @private @doc Removes storage test file.
%% @end
%%--------------------------------------------------------------------
-spec remove_test_file(Node :: node(), Helper :: any(), UserCtx :: any(),
    FileId :: binary(), Size :: non_neg_integer()) -> ok | no_return().
remove_test_file(Node, Helper, UserCtx, FileId, Size) ->
    Args = [Helper, UserCtx, FileId, Size],
    case rpc:call(Node, storage_detector, remove_test_file, Args) of
        {badrpc, {'EXIT', {Reason, Stacktrace}}} ->
            ?throw_stacktrace({?ERR_STORAGE_TEST_FILE_REMOVE, Node, Reason}, undefined, Stacktrace);
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
    case exists(Node, StorageName) of
        true ->
            {error, already_exists};
        false ->
            Storage = rpc:call(Node, storage, new, [StorageName, Helpers, ReadOnly, LumaConfig]),
            rpc:call(Node, storage, create, [Storage])
    end.


%%--------------------------------------------------------------------
%% @private @doc Returns storage details from op_worker service configuration.
%% @end
%%--------------------------------------------------------------------
-spec get_storage(Node :: node(), Storage :: any()) ->
    Storage :: storage_params_map().
get_storage(Node, Storage) ->
    [Helper | _] = rpc:call(Node, storage, get_helpers, [Storage]),
    AdminCtx = rpc:call(Node, helper, get_admin_ctx, [Helper]),
    AdminCtx2 = maps:with([<<"username">>, <<"accessKey">>], AdminCtx),
    HelperArgs = rpc:call(Node, helper, get_args, [Helper]),
    LumaConfig = rpc:call(Node, storage, get_luma_config_map, [Storage]),

    Params = maps:merge(AdminCtx2, HelperArgs),
    Params#{
        id => rpc:call(Node, storage, get_id, [Storage]),
        name => rpc:call(Node, storage, get_name, [Storage]),
        type => rpc:call(Node, helper, get_name, [Helper]),
        readonly => rpc:call(Node, storage, is_readonly, [Storage]),
        insecure => rpc:call(Node, helper, is_insecure, [Helper]),
        storagePathType => rpc:call(Node, helper, get_storage_path_type, [Helper]),
        lumaEnabled => maps:get(enabled, LumaConfig, false),
        lumaUrl => maps:get(url, LumaConfig, null)
    }.

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
            ApiKey = onepanel_utils:typed_get(lumaApiKey, StorageParams, binary, undefined),
            rpc:call(Node, luma_config, new, [Url, ApiKey]);
        false ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @private @doc Returns LUMA argument value associated with Key
%% in StorageParams. Throws error if key is missing
%% @end
%%--------------------------------------------------------------------
-spec get_required_luma_arg(StorageParams :: storage_params_map(), Key :: atom(),
    Type :: onepanel_utils:type()) -> term().
get_required_luma_arg(StorageParams, Key, Type) ->
    case onepanel_utils:typed_get(Key, StorageParams, Type) of
        {error, _} ->
            ?throw_error(?ERR_LUMA_CONFIG(Key));
        Value ->
            Value
    end.

%%--------------------------------------------------------------------
%% @private @doc Parses and validates auto-cleaning configuration
%% arguments.
%% @end
%%--------------------------------------------------------------------
-spec parse_auto_cleaning_configuration(maps:maps()) -> maps:map().
parse_auto_cleaning_configuration(Args) ->
    onepanel_maps:remove_undefined(#{
        enabled => onepanel_utils:typed_get(enabled, Args, boolean, undefined),
        target => onepanel_utils:typed_get([target], Args, integer, undefined),
        threshold => onepanel_utils:typed_get([threshold], Args, integer, undefined),
        rules => parse_auto_cleaning_rules(Args)
    }).

%%--------------------------------------------------------------------
%% @private @doc Parses and validates auto-cleaning rules
%% configuration arguments.
%% @end
%%--------------------------------------------------------------------
-spec parse_auto_cleaning_rules(maps:maps()) -> maps:map().
parse_auto_cleaning_rules(Args) ->
    ParsedRules = #{
        enabled => onepanel_utils:typed_get([rules, enabled], Args, boolean, undefined)
    },
    ParsedRules2 = lists:foldl(fun(RuleName, AccIn) ->
        RuleSettingConfig = parse_auto_cleaning_rule_setting(RuleName, Args),
        case map_size(RuleSettingConfig) == 0 of
            true ->
                AccIn;
            false ->
                AccIn#{RuleName => RuleSettingConfig}
        end
    end, ParsedRules, [
        min_file_size, max_file_size, min_hours_since_last_open, max_open_count,
        max_hourly_moving_average, max_daily_moving_average, max_monthly_moving_average
    ]),
    onepanel_maps:remove_undefined(ParsedRules2).

%%--------------------------------------------------------------------
%% @private @doc Parses and validates auto-cleaning rule setting.
%% @end
%%--------------------------------------------------------------------
-spec parse_auto_cleaning_rule_setting(atom(), maps:map()) -> maps:map().
parse_auto_cleaning_rule_setting(RuleName, Args) ->
    onepanel_maps:remove_undefined(#{
        enabled => onepanel_utils:typed_get([rules, RuleName, enabled], Args, boolean, undefined),
        value => onepanel_utils:typed_get([rules, RuleName, value], Args, integer, undefined)
    }).

%%-------------------------------------------------------------------
%% @private @doc Parses and validates files-popularity configuration
%% @end
%%-------------------------------------------------------------------
-spec parse_files_popularity_configuration(maps:map()) -> maps:map().
parse_files_popularity_configuration(Args) ->
    onepanel_maps:remove_undefined(#{
        enabled => onepanel_utils:typed_get(enabled, Args, boolean, undefined),
        last_open_hour_weight => onepanel_utils:typed_get(lastOpenHourWeight, Args, float, undefined),
        avg_open_count_per_day_weight => onepanel_utils:typed_get(avgOpenCountPerDayWeight, Args, float, undefined),
        max_avg_open_count_per_day => onepanel_utils:typed_get(maxAvgOpenCountPerDay, Args, float, undefined)
    }).
