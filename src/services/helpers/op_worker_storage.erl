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
-export([add/2, list/0, get/1, update/3, remove/2]).
-export([get_supporting_storage/2, get_supporting_storages/2,
    get_file_popularity_configuration/2, get_auto_cleaning_configuration/2]).
-export([is_mounted_in_root/3, can_be_removed/1]).
-export([maybe_update_file_popularity/3,
    maybe_update_auto_cleaning/3, invalidate_luma_cache/1]).

% @formatter:off
-type id() :: binary().
-type name() :: binary().

%% specification for updating or modifying storage
-type param() :: binary() | boolean() | integer() | float().
-type storage_params() :: #{
    Key :: atom() => Value :: param()
}.

%% Storage information retrieved from op worker
-type storage_details() :: #{
    readonly | insecure | lumaEnabled := boolean(),
    atom() := binary()
}.

-type helper_args() :: #{binary() => binary()}.
-type user_ctx() :: #{binary() => binary()}.
-type storages_map() :: #{Name :: name() => Params :: storage_params()}.
-type luma_config() :: {atom(), binary(), undefined | binary()}.
-type helper() :: term().
% @formatter:on

-export_type([storage_params/0, storage_details/0, storages_map/0, helper_args/0,
    user_ctx/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds storages specified in a map.
%% Before each addition verifies that given storage is accessible for all
%% op_worker service nodes and aborts upon error.
%% This verification is skipped for readonly storages.
%% @end
%%--------------------------------------------------------------------
-spec add(Storages :: storages_map(), IgnoreExists :: boolean()) -> ok | no_return().
add(Storages, IgnoreExists) ->
    OpNode = onepanel_cluster:service_to_node(service_op_worker:name()),
    ?info("Adding ~b storage(s)", [maps:size(Storages)]),
    maps:fold(fun(Key, Value, _) ->
        StorageName = onepanel_utils:convert(Key, binary),
        StorageType = onepanel_utils:typed_get(type, Value, binary),

        Result = case {exists(OpNode, StorageName), IgnoreExists} of
            {true, true} -> skipped;
            {true, false} -> {error, already_exists};
            _ -> add(OpNode, StorageName, Value)
        end,

        case Result of
            skipped ->
                ?info("Storage already exists, skipping: \"~s\" (~s)",
                    [StorageName, StorageType]),
                ok;
            ok -> ?info("Successfully added storage: \"~s\" (~s)",
                [StorageName, StorageType]),
                ok;
            {error, Reason} ->
                ?throw_error({?ERR_STORAGE_ADDITION, Reason})
        end
    end, ok, Storages).


%%--------------------------------------------------------------------
%% @doc Updates details of a selected storage in op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec update(OpNode :: node(), Id :: id(), Params :: storage_params()) ->
    storage_details().
update(OpNode, Id, Params) ->
    Storage = op_worker_storage:get(Id),
    {ok, Id} = onepanel_maps:get(id, Storage),
    {ok, Type} = onepanel_maps:get(type, Storage),

    ok = maybe_update_name(OpNode, Id, Params),
    ok = maybe_update_admin_ctx(OpNode, Id, Type, Params),
    ok = maybe_update_args(OpNode, Id, Type, Params),
    ok = maybe_update_luma_config(OpNode, Id, Params),
    ok = maybe_update_insecure(OpNode, Id, Type, Params),
    ok = maybe_update_readonly(OpNode, Id, Params),
    make_update_result(OpNode, Id).


%%--------------------------------------------------------------------
%% @doc Removes given storage.
%% Fails if any space is supported by this storage.
%% @end
%%--------------------------------------------------------------------
-spec remove(OpNode :: node(), id()) -> ok | no_return().
remove(OpNode, Id) ->
    case rpc:call(OpNode, storage, safe_remove, [Id]) of
        ok ->
            ?info("Successfully removed storage with id ~p", [Id]),
            ok;
        {error, storage_in_use} -> ?throw_error(?ERR_STORAGE_IN_USE)
    end.


%%--------------------------------------------------------------------
%% @doc Returns lists of storage ids currently configured in op_worker
%% service.
%% @end
%%--------------------------------------------------------------------
-spec list() -> #{ids := [id()]}.
list() ->
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
-spec get(Id :: id()) -> storage_details().
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

%%-------------------------------------------------------------------
%% @doc
%% Enables or disables file popularity.
%% @end
%%-------------------------------------------------------------------
-spec maybe_update_file_popularity(Node :: node(), SpaceId :: id(), maps:map()) ->
    ok.
maybe_update_file_popularity(_Node, _SpaceId, Args) when map_size(Args) =:= 0 ->
    ok;
maybe_update_file_popularity(Node, SpaceId, Args) ->
    Configuration = parse_file_popularity_configuration(Args),
    case rpc:call(Node, file_popularity_api, configure, [SpaceId, Configuration]) of
        {error, Reason} ->
            ?throw_error({?ERR_CONFIG_FILE_POPULARITY, Reason});
        Result -> Result
    end.


%%-------------------------------------------------------------------
%% @doc
%% Updates autocleaning configuration.
%% @end
%%-------------------------------------------------------------------
-spec maybe_update_auto_cleaning(OpNode :: node(), SpaceId :: id(), maps:map()) -> ok.
maybe_update_auto_cleaning(_OpNode, _SpaceId, Args) when map_size(Args) =:= 0 ->
    ok;
maybe_update_auto_cleaning(OpNode, SpaceId, Args) ->
    Configuration = parse_auto_cleaning_configuration(Args),
    case rpc:call(OpNode, autocleaning_api, configure,
        [SpaceId, Configuration]) of
        {error, Reason} ->
            ?throw_error({?ERR_CONFIG_AUTO_CLEANING, Reason});
        Result -> Result
    end.


%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching file-popularity
%% configuration from provider.
%% @end
%%-------------------------------------------------------------------
-spec get_file_popularity_configuration(OpNode :: node(), SpaceId :: id()) -> proplists:proplist().
get_file_popularity_configuration(OpNode, SpaceId) ->
    case rpc:call(OpNode, file_popularity_api, get_configuration, [SpaceId]) of
        {ok, DetailsMap} ->
            maps:to_list(onepanel_maps:get_store_multiple([
                {[enabled], [enabled]},
                {[example_query], [exampleQuery]},
                {[last_open_hour_weight], [lastOpenHourWeight]},
                {[avg_open_count_per_day_weight], [avgOpenCountPerDayWeight]},
                {[max_avg_open_count_per_day], [maxAvgOpenCountPerDay]}
            ], DetailsMap));
        {error, Reason} ->
            ?throw_error({?ERR_FILE_POPULARITY, Reason})
    end.

%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching autocleaning details from
%% provider.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_configuration(OpNode :: node(), SpaceId :: id()) -> proplists:proplist().
get_auto_cleaning_configuration(OpNode, SpaceId) ->
    DetailsMap = rpc:call(OpNode, autocleaning_api, get_configuration, [SpaceId]),
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
-spec invalidate_luma_cache(StorageId :: id()) -> ok.
invalidate_luma_cache(StorageId) ->
    OpNode = onepanel_cluster:service_to_node(service_op_worker:name()),
    ok = rpc:call(OpNode, luma_cache, invalidate, [StorageId]).


%%-------------------------------------------------------------------
%% @doc
%% Checks if given storage can be removed, i.e. does not support
%% any space.
%% @end
%%-------------------------------------------------------------------
-spec can_be_removed(id()) -> boolean().
can_be_removed(StorageId) ->
    Node = hd(service_op_worker:get_nodes()),
    not rpc:call(Node, storage, supports_any_space, [StorageId]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Handles addition of a single storage. Ensures read/write access
%% beforehand.
%% Uses given OpNode for op_worker operations.
%% @end
%%--------------------------------------------------------------------
-spec add(OpNode :: node(), StorageName :: binary(), Params :: storage_params()) ->
    ok | {error, Reason :: term()}.
add(OpNode, StorageName, Params) ->
    StorageType = onepanel_utils:typed_get(type, Params, binary),

    ?info("Gathering storage configuration: \"~s\" (~s)", [StorageName, StorageType]),
    ReadOnly = onepanel_utils:typed_get(readonly, Params, boolean, false),

    UserCtx = storage_params:make_user_ctx(OpNode, StorageType, Params),
    Helper = make_helper(OpNode, StorageType, UserCtx, Params),

    LumaConfig = get_luma_config(OpNode, Params),
    maybe_verify_storage(Helper, UserCtx, ReadOnly),

    ?info("Adding storage: \"~s\" (~s)", [StorageName, StorageType]),
    StorageRecord = rpc:call(OpNode, storage, new,
        [StorageName, [Helper], ReadOnly, LumaConfig]),
    case rpc:call(OpNode, storage, create, [StorageRecord]) of
        {ok, _StorageId} -> ok;
        {error, Reason} -> {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Uses op worker to create a helper record.
%% @end
%%--------------------------------------------------------------------
-spec make_helper(OpNode :: node(), StorageType :: binary(), UserCtx :: user_ctx(),
    Params :: storage_params()) ->
    helper() | {badrpc, term()}.
make_helper(OpNode, StorageType, UserCtx, Params) ->
    Args = storage_params:make_helper_args(OpNode, StorageType, Params),
    Insecure = onepanel_utils:typed_get(insecure, Params, boolean, false),
    PathType = onepanel_utils:typed_get(storagePathType, Params, binary),

    rpc:call(OpNode, helper, new_helper,
        [StorageType, Args, UserCtx, Insecure, PathType]).


%%--------------------------------------------------------------------
%% @private @doc For read-write storage verifies that it is accessible for all
%% op_worker service nodes.
%% @end
%%--------------------------------------------------------------------
-spec maybe_verify_storage(Helper :: helper(), UserCtx :: any(), Readonly :: boolean()) ->
    skipped | verified | no_return().
maybe_verify_storage(_Helper, _UserCtx, true) ->
    skipped;
maybe_verify_storage(Helper, UserCtx, _) ->
    ?info("Verifying write access to storage"),
    ok = storage_tester:verify_storage(Helper, UserCtx),
    verified.


%%--------------------------------------------------------------------
%% @private @doc Returns storage details from op_worker service configuration.
%% @end
%%--------------------------------------------------------------------
-spec get_storage(OpNode :: node(), Storage :: any()) -> storage_details().
get_storage(OpNode, Storage) ->
    [Helper | _] = rpc:call(OpNode, storage, get_helpers, [Storage]),
    AdminCtx = rpc:call(OpNode, helper, get_admin_ctx, [Helper]),
    AdminCtx2 = maps:with([<<"username">>, <<"accessKey">>], AdminCtx),
    HelperArgs = rpc:call(OpNode, helper, get_args, [Helper]),
    LumaConfig = rpc:call(OpNode, storage, get_luma_config_map, [Storage]),

    Params = onepanel_utils:convert(maps:merge(AdminCtx2, HelperArgs), {keys, atom}),
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
    case onepanel_utils:typed_find(Key, StorageParams, Type) of
        #error{} -> ?throw_error(?ERR_LUMA_CONFIG(Key));
        Value -> Value
    end.


%%--------------------------------------------------------------------
%% @private @doc Creates response for modify request
%% by gathering current storage params and performing a write test
%% as when adding new storage.
%% @end
%%--------------------------------------------------------------------
-spec make_update_result(OpNode :: node(), StorageId :: id()) -> storage_details().
make_update_result(OpNode, StorageId) ->
    Details = #{name := Name, type := Type, readonly := Readonly} = ?MODULE:get(StorageId),
    try
        {ok, Helper} = rpc:call(OpNode, storage, select_helper, [StorageId, Type]),
        UserCtx = rpc:call(OpNode, helper, get_admin_ctx, [Helper]),
        maybe_verify_storage(Helper, UserCtx, Readonly)
    of
        skipped -> Details;
        verified -> Details#{verificationPassed => true}
    catch ErrType:Error ->
        ?warning("Verfication of modified storage ~p (~p) failed", [Name, StorageId]),
        % translator will log the error
        onepanel_errors:translate(ErrType, Error),
        Details#{verificationPassed => false}
    end.


%% @private
-spec maybe_update_name(OpNode :: node(), Id :: id(), Params :: map()) ->
    ok | no_return().
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

maybe_update_insecure(_OpNode, _Id, _Type, _Params) -> ok.


%% @private
-spec maybe_update_readonly(OpNode :: node(), Id :: id(), Params :: map()) ->
    ok | no_return().
maybe_update_readonly(OpNode, Id, #{readonly := NewReadonly}) ->
    ok = rpc:call(OpNode, storage, set_readonly, [Id, NewReadonly]);

maybe_update_readonly(_OpNode, _Id, _Params) -> ok.


%% @private
-spec maybe_update_luma_config(OpNode :: node(), Id :: id(),
    Params :: #{atom() => term()}) -> ok | no_return().
maybe_update_luma_config(OpNode, Id, Params) ->
    IsEnabled = rpc:call(OpNode, storage, is_luma_enabled, [Id]),

    case {storage_params:make_luma_params(Params), IsEnabled} of
        {Empty, _} when map_size(Empty) == 0 ->
            ok;
        {#{luma_enabled := false}, false} ->
            ok;
        {#{luma_enabled := New}, Old} when New /= Old ->
            LumaConfig = get_luma_config(OpNode, Params),
            ok = rpc:call(OpNode, storage, set_luma_config, [Id, LumaConfig]),
            invalidate_luma_cache(Id);
        {Changes, true} ->
            ok = rpc:call(OpNode, storage, update_luma_config, [Id, Changes]),
            invalidate_luma_cache(Id)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Checks if storage with given name exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(OpNode :: node(), StorageName :: name()) -> boolean().
exists(OpNode, StorageName) ->
    case rpc:call(OpNode, storage, select, [StorageName]) of
        {error, not_found} -> false;
        {ok, _} -> true
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
%% @private @doc Parses and validates file-popularity configuration
%% @end
%%-------------------------------------------------------------------
-spec parse_file_popularity_configuration(maps:map()) -> maps:map().
parse_file_popularity_configuration(Args) ->
    onepanel_maps:remove_undefined(#{
        enabled => onepanel_utils:typed_get(enabled, Args, boolean, undefined),
        last_open_hour_weight => onepanel_utils:typed_get(last_open_hour_weight, Args, float, undefined),
        avg_open_count_per_day_weight => onepanel_utils:typed_get(avg_open_count_per_day_weight, Args, float, undefined),
        max_avg_open_count_per_day => onepanel_utils:typed_get(max_avg_open_count_per_day, Args, float, undefined)
    }).
