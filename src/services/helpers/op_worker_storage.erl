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
    get_file_popularity_details/2, get_autocleaning_details/2]).
-export([is_mounted_in_root/3]).
-export([add_storage/5, maybe_update_file_popularity/3, maybe_update_autocleaning/3,
    invalidate_luma_cache/1]).

-type id() :: binary().
-type name() :: binary().
-type storage_params() :: #{Key :: atom() | binary() => Value :: binary()}.
-type helper_args() :: #{binary() => binary()}.
-type user_ctx() :: #{binary() => binary()}.
-type storages_map() :: #{Name :: name() => Params :: storage_params()}.
-type luma_config() :: {atom(), binary(), undefined | binary()}.

-export_type([storage_params/0, storages_map/0, helper_args/0, user_ctx/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Verifies that each of provided storages is accessible for all op_worker
%% service nodes. In case of a successful verification proceeds with storage
%% addition.
%% @end
%%--------------------------------------------------------------------
-spec add(Storages :: storages_map(), IgnoreExists :: boolean()) -> ok | no_return().
add(Storages, IgnoreExists) ->
    Node = onepanel_cluster:service_to_node(service_op_worker:name()),
    ?info("Adding ~b storage(s)", [maps:size(Storages)]),
    maps:fold(fun(Key, Value, _) ->
        StorageName = onepanel_utils:convert(Key, binary),
        StorageType = onepanel_utils:typed_get(type, Value, binary),
        Result = add(Node, StorageName, Value),

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
    end, ok, Storages).


%%--------------------------------------------------------------------
%% @private
%% @doc Verifies that provide storage is accessible for all op_worker
%% service nodes. In case of a successful verification proceeds with storage
%% addition.
%% Uses given Node for op_worker operations.
%% @end
%%--------------------------------------------------------------------
-spec add(Node :: node(), StorageName :: binary(), Params :: storage_params()) ->
    {ok, StorageId :: binary()} | {error, Reason :: term()}.
add(Node, StorageName, Params) ->
    StorageType = onepanel_utils:typed_get(type, Params, binary),

    ?info("Gathering storage configuration: \"~s\" (~s)", [StorageName, StorageType]),
    ReadOnly = onepanel_utils:typed_get(readonly, Params, boolean, false),

    UserCtx = storage_params:make_user_ctx(Node, StorageType, Params),
    Helper = make_helper(Node, StorageType, UserCtx, Params),

    LumaConfig = get_luma_config(Node, Params),
    maybe_verify_storage(Helper, UserCtx, ReadOnly),

    ?info("Adding storage: \"~s\" (~s)", [StorageName, StorageType]),
    add_storage(Node, StorageName, [Helper], ReadOnly, LumaConfig).


make_helper(Node, StorageType, UserCtx, Params) ->
    Args = storage_params:make_helper_args(Node, StorageType, Params),
    Insecure = onepanel_utils:typed_get(insecure, Params, boolean, false),
    PathType = onepanel_utils:typed_get(storagePathType, Params, binary),

    rpc:call(Node, helper, new_helper,
        [StorageType, Args, UserCtx, PathType, Insecure]).

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

%%    extract helper params

    Args2 = #{<<"timeout">> => onepanel_utils:typed_get(timeout, Args, binary)},


    {ok, _} = rpc:call(Node, storage, update_helper_args, [Id, Type, Args2]),
    ok.


%%--------------------------------------------------------------------
%% @doc Returns lists of storages' details currently configured in op_worker
%% service.
%% @end
%%--------------------------------------------------------------------
-spec get() -> #{ids := [id()]}.
get() ->
    Node = onepanel_cluster:service_to_node(service_op_worker:name()),
    {ok, Storages} = rpc:call(Node, storage, list, []),
    Ids = lists:map(fun(Storage) ->
        rpc:call(Node, storage, get_id, [Storage])
    end, Storages),
    #{ids => Ids}.


%%--------------------------------------------------------------------
%% @doc Returns details of a selected storage from op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec get(Id :: id()) -> storage_params().
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
        lower_file_size_limit => onepanel_utils:typed_get([settings, lower_file_size_limit], Args,
            integer, undefined),
        upper_file_size_limit => onepanel_utils:typed_get([settings, upper_file_size_limit], Args,
            integer, undefined),
        max_file_not_opened_hours => onepanel_utils:typed_get([settings, max_file_not_opened_hours],
            Args, integer, undefined),
        target => onepanel_utils:typed_get([settings, target], Args, integer, undefined),
        threshold => onepanel_utils:typed_get([settings, threshold], Args, integer, undefined)
    },
    case rpc:call(Node, space_cleanup_api, configure_autocleaning,
        [SpaceId, onepanel_maps:remove_undefined(Settings)]) of
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
    storage_tester:verify_storage(Helper, UserCtx).


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
    Storage :: storage_params().
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
-spec get_luma_config(Node :: node(), StorageParams :: storage_params()) ->
    undefined | luma_config().
get_luma_config(Node, StorageParams) ->
    case onepanel_utils:typed_get(lumaEnabled, StorageParams, boolean, false) of
        true ->
            Url = get_required_luma_arg(lumaUrl, StorageParams, binary),
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
-spec get_required_luma_arg(Key :: atom(), StorageParams :: storage_params(),
    Type :: onepanel_utils:type()) -> term().
get_required_luma_arg(Key, StorageParams, Type) ->
    case onepanel_utils:typed_get(Key, StorageParams, Type) of
        #error{} -> ?throw_error(?ERR_LUMA_CONFIG(Key));
        Value -> Value
    end.
