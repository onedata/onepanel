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
-export([add/2, get/0, get/1, update/3]).
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

%% @fixme write rest test for PATCH

%%--------------------------------------------------------------------
%% @doc Verifies that each of provided storages is accessible for all op_worker
%% service nodes. In case of a successful verification proceeds with storage
%% addition.
%% @end
%%--------------------------------------------------------------------
-spec add(Storages :: storages_map(), IgnoreExists :: boolean()) -> ok | no_return().
add(Storages, IgnoreExists) ->
    OpNode = onepanel_cluster:service_to_node(service_op_worker:name()),
    ?info("Adding ~b storage(s)", [maps:size(Storages)]),
    maps:fold(fun(Key, Value, _) ->
        StorageName = onepanel_utils:convert(Key, binary),
        StorageType = onepanel_utils:typed_get(type, Value, binary),
        Result = add(OpNode, StorageName, Value),

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
%% Uses given OpNode for op_worker operations.
%% @end
%%--------------------------------------------------------------------
-spec add(OpNode :: node(), StorageName :: binary(), Params :: storage_params()) ->
    {ok, StorageId :: binary()} | {error, Reason :: term()}.
add(OpNode, StorageName, Params) ->
    StorageType = onepanel_utils:typed_get(type, Params, binary),

    ?info("Gathering storage configuration: \"~s\" (~s)", [StorageName, StorageType]),
    ReadOnly = onepanel_utils:typed_get(readonly, Params, boolean, false),

    UserCtx = storage_params:make_user_ctx(OpNode, StorageType, Params),
    Helper = make_helper(OpNode, StorageType, UserCtx, Params),

    LumaConfig = get_luma_config(OpNode, Params),
    maybe_verify_storage(Helper, UserCtx, ReadOnly),

    ?info("Adding storage: \"~s\" (~s)", [StorageName, StorageType]),
    add_storage(OpNode, StorageName, [Helper], ReadOnly, LumaConfig).


make_helper(OpNode, StorageType, UserCtx, Params) ->
    Args = storage_params:make_helper_args(OpNode, StorageType, Params),
    Insecure = onepanel_utils:typed_get(insecure, Params, boolean, false),
    PathType = onepanel_utils:typed_get(storagePathType, Params, binary),

    rpc:call(OpNode, helper, new_helper,
        [StorageType, Args, UserCtx, Insecure, PathType]).

%%--------------------------------------------------------------------
%% @doc Updates details of a selected storage in op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec update(OpNode :: node(), Name :: name(), Args :: map()) ->
    storage_params().
update(OpNode, Id, Args) ->
    % @fixme receive OpNode as argument

    OpNode = onepanel_cluster:service_to_node(service_op_worker:name()),
    Storage = op_worker_storage:get(Id),
    {ok, Id} = onepanel_maps:get(id, Storage),
    {ok, Type} = onepanel_maps:get(type, Storage),

    ok = maybe_update_name(OpNode, Id, Args),
    ok = maybe_update_admin_ctx(OpNode, Id, Type, Args),
    ok = maybe_update_args(OpNode, Id, Type, Args),
    ok = maybe_update_luma_config(OpNode, Id, Args),
    ok = maybe_update_insecure(OpNode, Id, Type, Args),
    ok = maybe_update_readonly(OpNode, Id, Args),
    make_update_result(OpNode, Id).


%% @private
make_update_result(OpNode, StorageId) ->
    Details = #{name := Name, type := Type, readonly := Readonly} = ?MODULE:get(StorageId),
    try
        {ok, Helper} = rpc:call(OpNode, storage, select_helper, [StorageId, Type]),
        UserCtx = rpc:call(OpNode, helper, get_admin_ctx, [Helper]),
        case maybe_verify_storage(Helper, UserCtx, Readonly) of
            skipped -> Details;
            verified -> Details#{verificationPassed => true}
        end
    catch ErrType:Error ->
        ?warning("Verfication of modified storage ~p (~p) failed:~n~p:~p",
            [Name, StorageId, ErrType, Error]),
        Details#{verificationPassed => false}
    end.


%% @private
-spec maybe_update_name(OpNode :: node(), Id :: id(),
    Params :: map()) -> ok | no_return().
maybe_update_name(OpNode, Id, #{name := Name}) ->
    ok = rpc:call(OpNode, storage, update_name, [Id, Name]);

maybe_update_name(_OpNode, _Id, _Params) ->
    ok.


%% @private
-spec maybe_update_admin_ctx(OpNode :: node(), Id :: id(), Type :: binary(),
    Params :: map()) -> ok | no_return().
maybe_update_admin_ctx(OpNode, Id, Type, Params) ->
    Ctx = storage_params:make_user_ctx(OpNode, Type, Params),
    case maps:size(Ctx) of
        0 -> ok;
        _ ->
            % Note that for some storage types make_user_ctx/3 always
            % returns a nonempty, constant result.
            % This is not a problem as long as those values are
            % never changed by op_worker.
            ok = rpc:call(OpNode, storage, update_admin_ctx, [Id, Type, Ctx])
    end.


%% @private
-spec maybe_update_args(OpNode :: node(), Id :: id(), Type :: binary(),
    Params :: map()) -> ok | no_return().
maybe_update_args(OpNode, Id, Type, Params) ->
    Args = storage_params:make_helper_args(OpNode, Type, Params),
    case maps:size(Args) of
        0 -> ok;
        _ ->
            ok = rpc:call(OpNode, storage, update_helper_args,
                [Id, Type, Args])
    end.


%% @private
-spec maybe_update_insecure(OpNode :: node(), Id :: id(), Type :: binary(),
    Params :: map()) -> ok | no_return().
maybe_update_insecure(OpNode, Id, Type, #{insecure := NewInsecure}) ->
    ok = rpc:call(OpNode, storage, set_insecure,
        [Id, Type, NewInsecure]);

maybe_update_insecure(_OpNode, _Id, _Type, _Params) ->
    ok.


%% @private
-spec maybe_update_readonly(OpNode :: node(), Id :: id(), Params :: map()) ->
    ok | no_return().
maybe_update_readonly(OpNode, Id, #{readonly := NewReadonly}) ->
    ok = rpc:call(OpNode, storage, set_readonly, [Id, NewReadonly]);

maybe_update_readonly(_OpNode, _Id, _Params) ->
    ok.


%% @private
-spec maybe_update_luma_config(OpNode :: node(), Id :: id(),
    Params :: #{atom() => term()}) -> ok | no_return().
maybe_update_luma_config(OpNode, Id, Params) ->
    case storage_params:make_luma_params(Params) of
        Empty when map_size(Empty) == 0 ->
            ok;
        Changes ->
            case rpc:call(OpNode, storage, update_luma_config, [Id, Changes]) of
                ok -> ok;
                {error, luma_disabled} -> ?throw_error(?ERR_LUMA_DISABLED)
            end
    end.


%%--------------------------------------------------------------------
%% @doc Returns lists of storages' details currently configured in op_worker
%% service.
%% @end
%%--------------------------------------------------------------------
-spec get() -> #{ids := [id()]}.
get() ->
    OpNode = onepanel_cluster:service_to_node(service_op_worker:name()),
    {ok, Storages} = rpc:call(OpNode, storage, list, []),
    Ids = lists:map(fun(Storage) ->
        rpc:call(OpNode, storage, get_id, [Storage])
    end, Storages),
    #{ids => Ids}.


%%--------------------------------------------------------------------
%% @doc Returns details of a selected storage from op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec get(Id :: id()) -> storage_params().
get(Id) ->
    OpNode = hd(service_op_worker:get_nodes()),
    {ok, Storage} = rpc:call(OpNode, storage, get, [Id]),
    get_storage(OpNode, Storage).


%%--------------------------------------------------------------------
%% @doc Returns storage supporting given space on given OpNode.
%% @end
%%--------------------------------------------------------------------
-spec get_supporting_storage(OpNode :: node(), SpaceId :: id()) -> id().
get_supporting_storage(OpNode, SpaceId) ->
    hd(get_supporting_storages(OpNode, SpaceId)).


%%--------------------------------------------------------------------
%% @doc Returns all storages supporting given space on given OpNode.
%% @end
%%--------------------------------------------------------------------
-spec get_supporting_storages(OpNode :: node(), SpaceId :: id()) -> [id()].
get_supporting_storages(OpNode, SpaceId) ->
    {ok, SpaceStorage} = rpc:call(OpNode, space_storage, get, [SpaceId]),
    rpc:call(OpNode, space_storage, get_storage_ids, [SpaceStorage]).


%%--------------------------------------------------------------------
%% @doc Checks whether space storage is mounted in root.
%% @end
%%--------------------------------------------------------------------
-spec is_mounted_in_root(OpNode :: node(), SpaceId :: id(), StorageId :: id()) ->
    boolean().
is_mounted_in_root(OpNode, SpaceId, StorageId) ->
    {ok, SpaceStorage} = rpc:call(OpNode, space_storage, get, [SpaceId]),
    MountedInRoot = rpc:call(OpNode, space_storage, get_mounted_in_root,
        [SpaceStorage]
    ),
    lists:member(StorageId, MountedInRoot).


%%--------------------------------------------------------------------
%% @doc Checks if storage with given name exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(OpNode :: node(), StorageName :: name()) -> boolean().
exists(OpNode, StorageName) ->
    case rpc:call(OpNode, storage, select, [StorageName]) of
        {error, not_found} -> false;
        {ok, _} -> true
    end.


%%-------------------------------------------------------------------
%% @private
%% @doc
%% Enables or disables file popularity.
%% @end
%%-------------------------------------------------------------------
-spec maybe_update_file_popularity(OpNode :: node(), SpaceId :: id(),
    maps:map()) -> {ok, id()}.
maybe_update_file_popularity(_OpNode, SpaceId, Args) when map_size(Args) =:= 0 ->
    {ok, SpaceId};
maybe_update_file_popularity(OpNode, SpaceId, Args) ->
    case onepanel_utils:typed_get(enabled, Args, boolean) of
        true ->
            rpc:call(OpNode, space_storage, enable_file_popularity, [SpaceId]);
        false ->
            rpc:call(OpNode, space_storage, disable_file_popularity, [SpaceId])
    end.


%%-------------------------------------------------------------------
%% @private
%% @doc
%% Updates autocleaning configuration.
%% @end
%%-------------------------------------------------------------------
-spec maybe_update_autocleaning(OpNode :: node(), SpaceId :: id(), maps:map()) -> {ok, id()}.
maybe_update_autocleaning(_OpNode, SpaceId, Args) when map_size(Args) =:= 0 ->
    {ok, SpaceId};
maybe_update_autocleaning(OpNode, SpaceId, Args) ->
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
    case rpc:call(OpNode, space_cleanup_api, configure_autocleaning,
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
-spec get_file_popularity_details(OpNode :: node(), SpaceId :: id()) -> proplists:proplist().
get_file_popularity_details(OpNode, SpaceId) ->
    rpc:call(OpNode, space_storage, get_file_popularity_details, [SpaceId]).


%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching autocleaning details from
%% provider.
%% @end
%%-------------------------------------------------------------------
-spec get_autocleaning_details(OpNode :: node(), SpaceId :: id()) -> proplists:proplist().
get_autocleaning_details(OpNode, SpaceId) ->
    Details = rpc:call(OpNode, space_cleanup_api, get_details, [SpaceId]),
    onepanel_lists:map_undefined_to_null(Details).


%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for invalidating luma cache on given
%% provider for given storage.
%% @end
%%-------------------------------------------------------------------
-spec invalidate_luma_cache(StorageId :: binary) -> ok.
invalidate_luma_cache(StorageId) ->
    OpNode = onepanel_cluster:service_to_node(service_op_worker:name()),
    ok = rpc:call(OpNode, luma_cache, invalidate, [StorageId]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc For read-write storage verifies that it is accessible for all
%% op_worker service nodes.
%% @end
%%--------------------------------------------------------------------
-spec maybe_verify_storage(Helper :: any(), UserCtx :: any(), Readonly :: boolean()) ->
    skipped | verified | no_return().
maybe_verify_storage(_Helper, _UserCtx, true) ->
    skipped;
maybe_verify_storage(Helper, UserCtx, _) ->
    ?info("Verifying write access to storage"),
    ok = storage_tester:verify_storage(Helper, UserCtx),
    verified.


%%--------------------------------------------------------------------
%% @private @doc Adds storage to the op_worker service configuration.
%% @end
%%--------------------------------------------------------------------
-spec add_storage(OpNode :: node(), StorageName :: binary(), Helpers :: list(),
    ReadOnly :: boolean(), LumaConfig :: luma_config()) ->
    {ok, StorageId :: binary()} | {error, Reason :: term()}.
add_storage(OpNode, StorageName, Helpers, ReadOnly, LumaConfig) ->
    case exists(OpNode, StorageName) of
        true ->
            {error, already_exists};
        false ->
            Storage = rpc:call(OpNode, storage, new, [StorageName, Helpers, ReadOnly, LumaConfig]),
            rpc:call(OpNode, storage, create, [Storage])
    end.


%%--------------------------------------------------------------------
%% @private @doc Returns storage details from op_worker service configuration.
%% @end
%%--------------------------------------------------------------------
-spec get_storage(OpNode :: node(), Storage :: any()) ->
    Storage :: storage_params().
get_storage(OpNode, Storage) ->
    [Helper | _] = rpc:call(OpNode, storage, get_helpers, [Storage]),
    AdminCtx = rpc:call(OpNode, helper, get_admin_ctx, [Helper]),
    AdminCtx2 = maps:with([<<"username">>, <<"accessKey">>], AdminCtx),
    HelperArgs = rpc:call(OpNode, helper, get_args, [Helper]),
    LumaConfig = rpc:call(OpNode, storage, get_luma_config_map, [Storage]),

    Params = maps:merge(AdminCtx2, HelperArgs),
    Params#{
        id => rpc:call(OpNode, storage, get_id, [Storage]),
        name => rpc:call(OpNode, storage, get_name, [Storage]),
        type => rpc:call(OpNode, helper, get_name, [Helper]),
        readonly => rpc:call(OpNode, storage, is_readonly, [Storage]),
        insecure => rpc:call(OpNode, helper, is_insecure, [Helper]),
        storagePathType => rpc:call(OpNode, helper, get_storage_path_type, [Helper]),
        lumaEnabled => maps:get(enabled, LumaConfig, false),
        lumaUrl => maps:get(url, LumaConfig, null)
    }.


%%--------------------------------------------------------------------
%% @private @doc Parses LUMA config arguments if lumaEnabled is set to
%% true in StorageParams and returns luma config acquired from provider.
%% Throws error if lumaEnabled is true and other arguments are missing.
%% @end
%%--------------------------------------------------------------------
-spec get_luma_config(OpNode :: node(), StorageParams :: storage_params()) ->
    undefined | luma_config().
get_luma_config(OpNode, StorageParams) ->
    case onepanel_utils:typed_get(lumaEnabled, StorageParams, boolean, false) of
        true ->
            Url = get_required_luma_arg(lumaUrl, StorageParams, binary),
            ApiKey = onepanel_utils:typed_get(lumaApiKey, StorageParams, binary, undefined),
            rpc:call(OpNode, luma_config, new, [Url, ApiKey]);
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
