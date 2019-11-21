%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains helper functions used during op_worker service
%%% storage configuration.
%%% @end
%%%--------------------------------------------------------------------
-module(op_worker_storage).
-author("Krzysztof Trzepla").

-include("names.hrl").
-include("modules/errors.hrl").

-include_lib("hackney/include/hackney_lib.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([add/2, list/0, get/1, exists/2, update/3, remove/2]).
-export([get_supporting_storage/2, get_supporting_storages/2,
    get_file_popularity_configuration/2, get_auto_cleaning_configuration/2]).
-export([is_imported_storage/2, can_be_removed/1]).
-export([maybe_update_file_popularity/3,
    maybe_update_auto_cleaning/3, invalidate_luma_cache/1]).

% @formatter:off
-type id() :: binary().
-type name() :: binary().

%% specification for updating or modifying storage
-type param() :: binary() | boolean() | integer() | float() | qos_parameters().
-type storage_params() :: #{
    Key :: atom() => Value :: param()
}.

%% Storage information retrieved from op_worker
-type storage_details() :: #{
    readonly | insecure | lumaEnabled := boolean(),
    qosParameters := qos_parameters(),
    atom() := binary()
}.

-type helper_args() :: op_worker_rpc:helper_args().
-type user_ctx() :: op_worker_rpc:helper_user_ctx().
-type storages_map() :: #{Name :: name() => Params :: storage_params()}.
-type qos_parameters() :: #{binary() => binary()}.

%% Opaque terms from op_worker
-type luma_config() :: op_worker_rpc:luma_config().
-type helper() :: op_worker_rpc:helper().
% @formatter:on

-export_type([id/0, storage_params/0, storage_details/0, storages_map/0,
    qos_parameters/0, helper_args/0, user_ctx/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds specified storage.
%% Before each addition verifies that given storage is accessible for all
%% op_worker service nodes and aborts upon error.
%% This verification is skipped for readonly storages.
%% @end
%%--------------------------------------------------------------------
-spec add(Ctx :: #{name := name(), params := storage_params()},
    IgnoreExists :: boolean()) -> ok | no_return().
add(#{name := Name, params := Params}, IgnoreExists) ->
    {ok, OpNode} = nodes:any(?SERVICE_OPW),
    StorageName = onepanel_utils:convert(Name, binary),
    StorageType = onepanel_utils:typed_get(type, Params, binary),

    Result = case {exists(OpNode, {name, StorageName}), IgnoreExists} of
        {true, true} -> skipped;
        {true, false} -> {error, already_exists};
        _ ->
            {QosParams, StorageParams} = maps:take(qosParameters, Params),
            add(OpNode, StorageName, StorageParams, QosParams)
    end,

    case Result of
        skipped ->
            ?info("Storage already exists, skipping: \"~ts\" (~ts)",
                [StorageName, StorageType]),
            ok;
        ok -> ?info("Successfully added storage: \"~ts\" (~ts)",
            [StorageName, StorageType]),
            ok;
        {error, Reason} ->
            ?throw_error({?ERR_STORAGE_ADDITION, Reason})
    end.


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
    {ok, LumaEnabled} = onepanel_maps:get(lumaEnabled, Storage),
    % remove qosParameters as they are a map and will cause errors
    % when preprocessing arg by conversion to binary
    PlainValues = maps:remove(qosParameters, Params),

    % @TODO VFS-5513 Modify everything in a single datastore operation
    ok = maybe_update_name(OpNode, Id, PlainValues),
    ok = maybe_update_admin_ctx(OpNode, Id, Type, PlainValues),
    ok = maybe_update_args(OpNode, Id, Type, PlainValues),
    ok = maybe_update_luma_config(OpNode, Id, PlainValues, LumaEnabled),
    ok = maybe_update_insecure(OpNode, Id, Type, PlainValues),
    ok = maybe_update_readonly(OpNode, Id, PlainValues),
    ok = maybe_update_qos_parameters(OpNode, Id, Params),
    ok = maybe_update_imported_storage(OpNode, Id, Params),
    make_update_result(OpNode, Id).


%%--------------------------------------------------------------------
%% @doc Removes given storage.
%% Fails if any space is supported by this storage.
%% @end
%%--------------------------------------------------------------------
-spec remove(OpNode :: node(), id()) -> ok | no_return().
remove(OpNode, Id) ->
    case op_worker_rpc:storage_safe_remove(OpNode, Id) of
        ok ->
            ?info("Successfully removed storage with id ~p", [Id]),
            ok;
        {error, storage_in_use} ->
            ?throw_error(?ERR_STORAGE_IN_USE)
    end.


%%--------------------------------------------------------------------
%% @doc Returns lists of storage ids currently configured in op_worker
%% service.
%% @end
%%--------------------------------------------------------------------
-spec list() -> #{ids := [id()]}.
list() ->
    {ok, Ids} = op_worker_rpc:storage_list_ids(),
    #{ids => Ids}.


%%--------------------------------------------------------------------
%% @doc Returns details of a selected storage from op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec get(Id :: id()) -> storage_details().
get(Id) ->
    {ok, Map} = op_worker_rpc:storage_describe(Id),
    onepanel_utils:convert(
        onepanel_maps:undefined_to_null(Map),
        {keys, atom}).


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
    op_worker_rpc:space_logic_get_storage_ids(OpNode, SpaceId).


%%--------------------------------------------------------------------
%% @doc Checks whether space storage is mounted in root.
%% @end
%%--------------------------------------------------------------------
-spec is_imported_storage(OpNode :: node(), StorageId :: id()) ->
    boolean().
is_imported_storage(OpNode, StorageId) ->
    op_worker_rpc:storage_is_imported_storage(OpNode, StorageId).

%%-------------------------------------------------------------------
%% @doc
%% Enables or disables file popularity.
%% @end
%%-------------------------------------------------------------------
-spec maybe_update_file_popularity(Node :: node(), SpaceId :: id(), map()) -> ok.
maybe_update_file_popularity(_Node, _SpaceId, Args) when map_size(Args) =:= 0 ->
    ok;
maybe_update_file_popularity(Node, SpaceId, Args) ->
    Configuration = parse_file_popularity_configuration(Args),
    case op_worker_rpc:file_popularity_api_configure(Node, SpaceId, Configuration) of
        {error, Reason} -> ?throw_error({?ERR_CONFIG_FILE_POPULARITY, Reason});
        Result -> Result
    end.


%%-------------------------------------------------------------------
%% @doc
%% Updates autocleaning configuration.
%% @end
%%-------------------------------------------------------------------
-spec maybe_update_auto_cleaning(OpNode :: node(), SpaceId :: id(), map()) -> ok.
maybe_update_auto_cleaning(_OpNode, _SpaceId, Args) when map_size(Args) =:= 0 ->
    ok;
maybe_update_auto_cleaning(OpNode, SpaceId, Args) ->
    Configuration = parse_auto_cleaning_configuration(Args),
    case op_worker_rpc:autocleaning_configure(OpNode, SpaceId, Configuration) of
        {error, Reason} -> ?throw_error({?ERR_CONFIG_AUTO_CLEANING, Reason});
        Result -> Result
    end.


%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching file-popularity
%% configuration from provider.
%% @end
%%-------------------------------------------------------------------
-spec get_file_popularity_configuration(OpNode :: node(), SpaceId :: id()) -> #{atom() => term()}.
get_file_popularity_configuration(OpNode, SpaceId) ->
    case op_worker_rpc:file_popularity_api_get_configuration(OpNode, SpaceId) of
        {ok, DetailsMap} ->
            onepanel_maps:get_store_multiple([
                {[enabled], [enabled]},
                {[example_query], [exampleQuery]},
                {[last_open_hour_weight], [lastOpenHourWeight]},
                {[avg_open_count_per_day_weight], [avgOpenCountPerDayWeight]},
                {[max_avg_open_count_per_day], [maxAvgOpenCountPerDay]}
            ], DetailsMap);
        {error, Reason} ->
            ?throw_error({?ERR_FILE_POPULARITY, Reason})
    end.

%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching autocleaning details from
%% provider.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_configuration(OpNode :: node(), SpaceId :: id()) -> #{atom() => term()}.
get_auto_cleaning_configuration(OpNode, SpaceId) ->
    DetailsMap = op_worker_rpc:autocleaning_get_configuration(OpNode, SpaceId),
    DetailsMap2 = onepanel_maps:get_store_multiple([
        {[rules, min_file_size], [rules, minFileSize]},
        {[rules, max_file_size], [rules, maxFileSize]},
        {[rules, min_hours_since_last_open], [rules, minHoursSinceLastOpen]},
        {[rules, max_open_count], [rules, maxOpenCount]},
        {[rules, max_hourly_moving_average], [rules, maxHourlyMovingAverage]},
        {[rules, max_daily_moving_average], [rules, maxDailyMovingAverage]},
        {[rules, max_monthly_moving_average], [rules, maxMonthlyMovingAverage]}
    ], DetailsMap, DetailsMap),
    onepanel_maps:undefined_to_null(DetailsMap2).


%%-------------------------------------------------------------------
%% @doc
%% Invalidates luma cache for given storage.
%% @end
%%-------------------------------------------------------------------
-spec invalidate_luma_cache(StorageId :: id()) -> ok.
invalidate_luma_cache(StorageId) ->
    ok = op_worker_rpc:invalidate_luma_cache(StorageId).


%%-------------------------------------------------------------------
%% @doc
%% Checks if given storage can be removed, i.e. does not support
%% any space.
%% @end
%%-------------------------------------------------------------------
-spec can_be_removed(id()) -> boolean().
can_be_removed(StorageId) ->
    not op_worker_rpc:storage_supports_any_space(StorageId).

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
-spec add(OpNode :: node(), StorageName :: binary(), Params :: storage_params(),
    QosParameters :: qos_parameters()) -> ok | {error, Reason :: term()}.
add(OpNode, StorageName, Params, QosParameters) ->
    StorageType = onepanel_utils:typed_get(type, Params, binary),

    ?info("Gathering storage configuration: \"~ts\" (~ts)", [StorageName, StorageType]),
    ReadOnly = onepanel_utils:typed_get(readonly, Params, boolean, false),

    UserCtx = make_user_ctx(OpNode, StorageType, Params),
    {ok, Helper} = make_helper(OpNode, StorageType, UserCtx, Params),

    LumaConfig = make_luma_config(OpNode, Params),
    maybe_verify_storage(Helper, ReadOnly),

    ImportedStorage = onepanel_utils:typed_get(importedStorage, Params, boolean, false),

    ?info("Adding storage: \"~ts\" (~ts)", [StorageName, StorageType]),
    StorageRecord = op_worker_rpc:storage_new(StorageName, [Helper],
        ReadOnly, LumaConfig, ImportedStorage),
    case op_worker_rpc:storage_create(StorageRecord) of
        {ok, StorageId} ->
            update_qos_parameters(OpNode, StorageId, QosParameters),
            ok;
        {error, Reason} -> {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Uses op_worker to create a helper record.
%% @end
%%--------------------------------------------------------------------
-spec make_helper(OpNode :: node(), StorageType :: binary(), UserCtx :: user_ctx(),
    Params :: storage_params()) ->
    {ok, helper()} | {badrpc, term()}.
make_helper(OpNode, StorageType, AdminCtx, Params) ->
    Args = make_helper_args(OpNode, StorageType, Params),
    Insecure = onepanel_utils:typed_get(insecure, Params, boolean, false),
    PathType = onepanel_utils:typed_get(storagePathType, Params, binary),
    op_worker_rpc:new_helper(
        OpNode, StorageType, Args, AdminCtx, Insecure, PathType).


%%--------------------------------------------------------------------
%% @private @doc Parses LUMA config arguments if lumaEnabled is set to
%% true in StorageParams and returns luma config acquired from provider.
%% Throws error if lumaEnabled is true and other arguments are missing.
%% @end
%%--------------------------------------------------------------------
-spec make_luma_config(OpNode :: node(), StorageParams :: storage_params()) ->
    undefined | luma_config().
make_luma_config(OpNode, StorageParams) ->
    case onepanel_utils:typed_get(lumaEnabled, StorageParams, boolean, false) of
        true ->
            Url = get_required_luma_arg(lumaUrl, StorageParams, binary),
            ApiKey = onepanel_utils:typed_get(lumaApiKey, StorageParams, binary, undefined),
            op_worker_rpc:new_luma_config(OpNode, Url, ApiKey);
        false ->
            undefined
    end.


%%--------------------------------------------------------------------
%% @private @doc For read-write storage verifies that it is accessible for all
%% op_worker service nodes.
%% @end
%%--------------------------------------------------------------------
-spec maybe_verify_storage(Helper :: helper(), Readonly :: boolean()) ->
    skipped | verified | no_return().
maybe_verify_storage(_Helper, true) ->
    skipped;
maybe_verify_storage(Helper, _) ->
    ?info("Verifying write access to storage"),
    verify_storage(Helper),
    verified.


%%--------------------------------------------------------------------
%% @private @doc Verifies that storage is accessible for all op_worker
%% service nodes.
%% @end
%%--------------------------------------------------------------------
-spec verify_storage(Helper :: any()) ->
    ok | no_return().
verify_storage(Helper) ->
    case op_worker_rpc:verify_storage_on_all_nodes(Helper) of
        ok -> ok;
        {error, Reason} ->
            ?throw_error(Reason);
        {error, Reason, Stacktrace} ->
            ?throw_stacktrace(Reason, undefined, Stacktrace);
        {badrpc, {'EXIT', {Error, Stacktrace}}} ->
            ?throw_stacktrace(Error, undefined, Stacktrace)
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
        {ok, Value} -> Value
    end.


%%--------------------------------------------------------------------
%% @private @doc Creates response for modify request
%% by gathering current storage params and performing a write test
%% as when adding new storage.
%% @end
%%--------------------------------------------------------------------
-spec make_update_result(OpNode :: node(), StorageId :: id()) -> storage_details().
make_update_result(OpNode, StorageId) ->
    Details = ?MODULE:get(StorageId),
    #{name := Name, type := Type, readonly := Readonly} = Details,
    try
        {ok, Helper} = op_worker_rpc:storage_select_helper(OpNode, StorageId, Type),
        maybe_verify_storage(Helper, Readonly)
    of
        skipped -> Details;
        verified -> Details#{verificationPassed => true}
    catch ErrType:Error ->
        ?warning("Verfication of modified storage ~p (~p) failed", [Name, StorageId]),
        % translator will log the error
        onepanel_errors:translate(ErrType, Error),
        Details#{verificationPassed => false}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates helper arguments based on storage params from user input.
%% @end
%%--------------------------------------------------------------------
-spec make_helper_args(OpNode :: node(), StorageType :: binary(),
    Params :: storage_params()) -> helper_args().
make_helper_args(OpNode, StorageType, Params) ->
    op_worker_rpc:prepare_helper_args(OpNode,
        StorageType, convert_to_binaries(Params)).


%%--------------------------------------------------------------------
%% @doc
%% Creates storage user ctx based on storage params from user input.
%% @end
%%--------------------------------------------------------------------
-spec make_user_ctx(OpNode :: node(), StorageType :: binary(),
    Params :: storage_params()) -> user_ctx().
make_user_ctx(OpNode, StorageType, Params) ->
    op_worker_rpc:prepare_user_ctx_params(OpNode,
        StorageType, convert_to_binaries(Params)).


%%--------------------------------------------------------------------
%% @doc
%% Prepares luma params based on storage params from user input.
%% @end
%%--------------------------------------------------------------------
-spec make_luma_params(Params :: storage_params()) ->
    #{url => binary(), api_key => binary(), luma_enabled => boolean()}.
make_luma_params(Params) ->
    onepanel_maps:get_store_multiple([
        {lumaUrl, url},
        {lumaApiKey, api_key},
        {lumaEnabled, luma_enabled}
    ], Params).


%% @private
-spec maybe_update_name(OpNode :: node(), Id :: id(), Params :: storage_params()) ->
    ok | no_return().
maybe_update_name(OpNode, Id, #{name := Name}) ->
    ok = op_worker_rpc:storage_update_name(OpNode, Id, Name);

maybe_update_name(_OpNode, _Id, _Params) ->
    ok.


%% @private
-spec maybe_update_admin_ctx(OpNode :: node(), Id :: id(), Type :: binary(),
    Params :: storage_params()) -> ok | no_return().
maybe_update_admin_ctx(OpNode, Id, Type, Params) ->
    Ctx = make_user_ctx(OpNode, Type, Params),
    case maps:size(Ctx) of
        0 -> ok;
        _ -> ok = op_worker_rpc:storage_update_admin_ctx(OpNode, Id, Type, Ctx)
    end.


%% @private
-spec maybe_update_args(OpNode :: node(), Id :: id(), Type :: binary(),
    Params :: storage_params()) -> ok | no_return().
maybe_update_args(OpNode, Id, Type, Params) ->
    Args = make_helper_args(OpNode, Type, Params),
    case maps:size(Args) of
        0 -> ok;
        _ -> ok = op_worker_rpc:storage_update_helper_args(OpNode, Id, Type, Args)
    end.


%% @private
-spec maybe_update_insecure(OpNode :: node(), Id :: id(), Type :: binary(),
    Params :: storage_params()) -> ok | no_return().
maybe_update_insecure(OpNode, Id, Type, #{insecure := NewInsecure}) ->
    ok = op_worker_rpc:storage_set_insecure(OpNode, Id, Type, NewInsecure);

maybe_update_insecure(_OpNode, _Id, _Type, _Params) -> ok.


%% @private
-spec maybe_update_readonly(OpNode :: node(), Id :: id(), Params :: storage_params()) ->
    ok | no_return().
maybe_update_readonly(OpNode, Id, #{readonly := NewReadonly}) ->
    ok = op_worker_rpc:storage_set_readonly(OpNode, Id, NewReadonly);

maybe_update_readonly(_OpNode, _Id, _Params) -> ok.


%% @private
-spec maybe_update_luma_config(OpNode :: node(), Id :: id(),
    Params :: #{atom() => term()}, WasEnabled :: boolean()) -> ok | no_return().
maybe_update_luma_config(OpNode, Id, Params, WasEnabled) ->
    case {make_luma_params(Params), WasEnabled} of
        {Empty, _} when map_size(Empty) == 0 ->
            ok;
        {#{luma_enabled := false}, false} ->
            ok;
        {#{luma_enabled := NewEnabled}, _} when NewEnabled /= WasEnabled ->
            LumaConfig = make_luma_config(OpNode, Params),
            ok = op_worker_rpc:storage_set_luma_config(OpNode, Id, LumaConfig),
            invalidate_luma_cache(Id);
        {Changes, true} ->
            ok = op_worker_rpc:storage_update_luma_config(OpNode, Id, Changes),
            invalidate_luma_cache(Id)
    end.


-spec maybe_update_qos_parameters(OpNode :: node(), Id :: id(),
    storage_params()) -> ok.
maybe_update_qos_parameters(OpNode, Id, #{qosParameters := Parameters}) ->
    update_qos_parameters(OpNode, Id, Parameters);
maybe_update_qos_parameters(_OpNode, _Id, _) ->
    ok.


-spec update_qos_parameters(OpNode :: node(), Id :: id(),
    qos_parameters()) -> ok.
update_qos_parameters(OpNode, Id, Parameters) ->
    ok = op_worker_rpc:storage_set_qos_parameters(OpNode, Id, Parameters).


-spec maybe_update_imported_storage(OpNode :: node(), Id :: id(),
    storage_params()) -> ok.
maybe_update_imported_storage(OpNode, Id, #{importedStorage := Value}) ->
    update_imported_storage(OpNode, Id, Value);
maybe_update_imported_storage(_OpNode, _Id, _) ->
    ok.


-spec update_imported_storage(OpNode :: node(), Id :: id(),
    boolean()) -> ok.
update_imported_storage(OpNode, Id, Value) ->
    ok = op_worker_rpc:storage_set_imported_storage(OpNode, Id, Value).

%%--------------------------------------------------------------------
%% @doc Checks if storage with given name or id exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(Node :: node(), Identifier) -> boolean()
    when Identifier :: {name, name()} | {id, id()}.
exists(Node, {name, StorageName}) ->
    case op_worker_rpc:get_storage_by_name(Node, StorageName) of
        {error, not_found} -> false;
        {ok, _} -> true
    end;

exists(Node, {id, StorageId}) ->
    op_worker_rpc:storage_exists(Node, StorageId).


%%--------------------------------------------------------------------
%% @private @doc Parses and validates auto-cleaning configuration
%% arguments.
%% @end
%%--------------------------------------------------------------------
-spec parse_auto_cleaning_configuration(map()) -> map().
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
-spec parse_auto_cleaning_rules(map()) -> map().
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
-spec parse_auto_cleaning_rule_setting(atom(), map()) -> map().
parse_auto_cleaning_rule_setting(RuleName, Args) ->
    onepanel_maps:remove_undefined(#{
        enabled => onepanel_utils:typed_get([rules, RuleName, enabled], Args, boolean, undefined),
        value => onepanel_utils:typed_get([rules, RuleName, value], Args, integer, undefined)
    }).

%%-------------------------------------------------------------------
%% @private @doc Parses and validates file-popularity configuration
%% @end
%%-------------------------------------------------------------------
-spec parse_file_popularity_configuration(map()) -> map().
parse_file_popularity_configuration(Args) ->
    onepanel_maps:remove_undefined(#{
        enabled => onepanel_utils:typed_get(enabled, Args, boolean, undefined),
        last_open_hour_weight => onepanel_utils:typed_get(last_open_hour_weight, Args, float, undefined),
        avg_open_count_per_day_weight => onepanel_utils:typed_get(avg_open_count_per_day_weight, Args, float, undefined),
        max_avg_open_count_per_day => onepanel_utils:typed_get(max_avg_open_count_per_day, Args, float, undefined)
    }).


%% @private
-spec convert_to_binaries(#{term() => term()}) -> #{binary() => binary()}.
convert_to_binaries(Map) ->
    maps:from_list([{
        onepanel_utils:convert(Key, binary),
        onepanel_utils:convert(Value, binary)
    } || {Key, Value} <- maps:to_list(Map)]).
