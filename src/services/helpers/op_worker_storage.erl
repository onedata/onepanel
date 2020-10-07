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
-export([add/1, list/0, get/1, exists/1, exists/2, update/3, remove/2]).
-export([get_supporting_storage/2, get_supporting_storages/2,
    get_file_popularity_configuration/2, get_auto_cleaning_configuration/2]).
-export([is_imported_storage/2, can_be_removed/1]).
-export([maybe_update_file_popularity/3,
    maybe_update_auto_cleaning/3]).

% @formatter:off
-type id() :: binary().
-type space_id() :: op_worker_rpc:od_space_id().
-type name() :: binary().

%% specification for updating or modifying storage
-type param() :: binary() | boolean() | integer() | float() | qos_parameters().
-type storage_params() :: #{
    Key :: atom() => Value :: param()
}.

%% Storage information retrieved from op_worker
-type storage_details() :: #{
    lumaFeed := op_worker_rpc:luma_feed(),
    qosParameters := qos_parameters(),
    atom() := binary()
}.

-type storage_type() :: op_worker_rpc:helper_name().
-type helper_args() :: op_worker_rpc:helper_args().
-type user_ctx() :: op_worker_rpc:helper_user_ctx().
-type storages_map() :: #{Name :: name() => Params :: storage_params()}.
-type qos_parameters() :: #{binary() => binary()}.

%% Opaque terms from op_worker
-type luma_config() :: op_worker_rpc:luma_config().
-type luma_feed() :: op_worker_rpc:luma_feed().
-type helper() :: op_worker_rpc:helper().
% @formatter:on

-export_type([id/0, storage_params/0, storage_details/0, storages_map/0,
    qos_parameters/0, helper_args/0, user_ctx/0]).

-define(EXEC_AND_THROW_ON_ERROR(Fun, Args),
    exec_and_throw_on_error(Fun, Args)
).

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
-spec add(Ctx :: #{name := name(), params := storage_params()}) ->
    ok | no_return().
add(#{name := Name, params := Params}) ->
    {ok, OpNode} = nodes:any(?SERVICE_OPW),
    StorageName = onepanel_utils:convert(Name, binary),
    StorageType = onepanel_utils:get_converted(type, Params, binary),

    Result = add(OpNode, StorageName, Params),

    case Result of
        ok -> ?info("Successfully added storage: \"~ts\" (~ts)",
            [StorageName, StorageType]),
            ok;
        {error, Reason} ->
            throw({error, Reason})
    end.


%%--------------------------------------------------------------------
%% @doc Updates details of a selected storage in op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec update(OpNode :: node(), Id :: id(), Params :: storage_params()) ->
    storage_details().
update(OpNode, Id, Params) ->
    Storage = op_worker_storage:get(Id),
    Id = maps:get(id, Storage),
    Type = maps:get(type, Storage),
    CurrentReadonly = maps:get(readonly, Storage),
    CurrentImported = maps:get(importedStorage, Storage),
    % remove qosParameters as they are a map and will cause errors
    % when preprocessing arg by conversion to binary
    PlainValues = maps:remove(qosParameters, Params),

    Readonly = maps:get(readonly, Params, CurrentReadonly),
    Imported = maps:get(importedStorage, Params, CurrentImported),

    verify_configuration(Id, Type, Imported, Readonly),

    % @TODO VFS-5513 Modify everything in a single datastore operation
    lists:foreach(fun({Fun, Args}) ->
        ?EXEC_AND_THROW_ON_ERROR(Fun, Args)
    end,
        [
            {fun maybe_update_qos_parameters/3, [OpNode, Id, Params]},
            {fun maybe_update_name/3, [OpNode, Id, PlainValues]},
            {fun maybe_update_admin_ctx/4, [OpNode, Id, Type, PlainValues]},
            {fun maybe_update_args/4, [OpNode, Id, Type, PlainValues]},
            {fun maybe_update_luma_config/3, [OpNode, Id, Params]},
            {fun update_readonly_and_imported/4, [OpNode, Id, Readonly, Imported]}
        ]
    ),
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
            throw(?ERROR_STORAGE_IN_USE)
    end.


%%--------------------------------------------------------------------
%% @doc Returns lists of storage ids currently configured in op_worker
%% service.
%% @end
%%--------------------------------------------------------------------
-spec list() -> [id()].
list() ->
    {ok, Ids} = op_worker_rpc:storage_list_ids(),
    Ids.


%%--------------------------------------------------------------------
%% @doc Returns details of a selected storage from op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec get(Id :: id()) -> storage_details().
get(Id) ->
    {ok, Map} = op_worker_rpc:storage_describe(Id),
    onepanel_utils:convert(
        maps_utils:undefined_to_null(Map),
        {keys, atom}).


%%--------------------------------------------------------------------
%% @doc Returns storage supporting given space on given OpNode.
%% @end
%%--------------------------------------------------------------------
-spec get_supporting_storage(OpNode :: node(), SpaceId :: id()) -> id().
get_supporting_storage(OpNode, SpaceId) ->
    {ok, StorageIds} = get_supporting_storages(OpNode, SpaceId),
    hd(StorageIds).


%%--------------------------------------------------------------------
%% @doc Returns all storages supporting given space on given OpNode.
%% @end
%%--------------------------------------------------------------------
-spec get_supporting_storages(OpNode :: node(), SpaceId :: id()) -> {ok, [id()]}.
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
        {error, _} = Error -> throw(Error);
        Result -> Result
    end.


%%-------------------------------------------------------------------
%% @doc
%% Updates autocleaning configuration.
%% @end
%%-------------------------------------------------------------------
-spec maybe_update_auto_cleaning(OpNode :: node(), space_id(), map()) -> ok.
maybe_update_auto_cleaning(_OpNode, _SpaceId, Args) when map_size(Args) =:= 0 ->
    ok;
maybe_update_auto_cleaning(OpNode, SpaceId, Args) ->
    Configuration = parse_auto_cleaning_configuration(Args),
    case op_worker_rpc:autocleaning_configure(OpNode, SpaceId, Configuration) of
        {error, _} = Error -> throw(Error);
        Result -> Result
    end.


%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching file-popularity
%% configuration from provider.
%% @end
%%-------------------------------------------------------------------
-spec get_file_popularity_configuration(OpNode :: node(),
    space_id()) -> #{atom() => term()}.
get_file_popularity_configuration(OpNode, SpaceId) ->
    case op_worker_rpc:file_popularity_api_get_configuration(OpNode, SpaceId) of
        {ok, DetailsMap} ->
            kv_utils:copy_found([
                {enabled, enabled},
                {example_query, exampleQuery},
                {last_open_hour_weight, lastOpenHourWeight},
                {avg_open_count_per_day_weight, avgOpenCountPerDayWeight},
                {max_avg_open_count_per_day, maxAvgOpenCountPerDay}
            ], DetailsMap);
        {error, _} = Error ->
            % the only possible errors here are from op-worker datastore,
            % cannot be handled gracefully by Onepanel
            error(Error)
    end.

%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for fetching autocleaning details from
%% provider.
%% @end
%%-------------------------------------------------------------------
-spec get_auto_cleaning_configuration(OpNode :: node(), space_id()) -> #{atom() => term()}.
get_auto_cleaning_configuration(OpNode, SpaceId) ->
    DetailsMap = op_worker_rpc:autocleaning_get_configuration(OpNode, SpaceId),
    DetailsMap2 = kv_utils:copy_found([
        {[rules, min_file_size], [rules, minFileSize]},
        {[rules, max_file_size], [rules, maxFileSize]},
        {[rules, min_hours_since_last_open], [rules, minHoursSinceLastOpen]},
        {[rules, max_open_count], [rules, maxOpenCount]},
        {[rules, max_hourly_moving_average], [rules, maxHourlyMovingAverage]},
        {[rules, max_daily_moving_average], [rules, maxDailyMovingAverage]},
        {[rules, max_monthly_moving_average], [rules, maxMonthlyMovingAverage]}
    ], DetailsMap, DetailsMap),
    maps_utils:undefined_to_null(DetailsMap2).

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
-spec add(OpNode :: node(), Name :: binary(), Params :: storage_params()) ->
    ok | {error, Reason :: term()}.
add(OpNode, Name, Params) ->
    StorageType = onepanel_utils:get_converted(type, Params, binary),

    ?info("Gathering storage configuration: \"~ts\" (~ts)", [Name, StorageType]),

    {QosParameters, StorageParams} = maps:take(qosParameters, Params),
    Readonly = onepanel_utils:get_converted(readonly, StorageParams, boolean, false),
    % if skipStorageDetection is not defined, set it to the same value as Readonly
    SkipStorageDetection = onepanel_utils:get_converted(skipStorageDetection, StorageParams, boolean, Readonly),
    % ensure skipStorageDetection is passed to helper
    StorageParams2 = StorageParams#{skipStorageDetection => SkipStorageDetection},
    UserCtx = make_user_ctx(OpNode, StorageType, StorageParams2),
    {ok, Helper} = make_helper(OpNode, StorageType, UserCtx, StorageParams2),
    LumaConfig = make_luma_config(OpNode, StorageParams2),
    LumaFeed = onepanel_utils:get_converted(lumaFeed, Params, atom, auto),
    ImportedStorage = onepanel_utils:get_converted(importedStorage, StorageParams2, boolean, false),

    verify_configuration(Name, StorageType, ImportedStorage, Readonly),
    maybe_verify_write_access(Helper, SkipStorageDetection, Readonly, LumaFeed),

    ?info("Adding storage: \"~ts\" (~ts)", [Name, StorageType]),
    case op_worker_rpc:storage_create(Name, Helper, LumaConfig, ImportedStorage, 
        Readonly,  normalize_numeric_qos_parameters(QosParameters)
    ) of
        {ok, _StorageId} -> ok;
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
    op_worker_rpc:new_helper(OpNode, StorageType, Args, AdminCtx).


%%--------------------------------------------------------------------
%% @private @doc Returns luma config acquired from provider.
%% If lumaFeed is set to <<"external">> it parses other luma arguments
%% from StorageParams .
%% Throws error if lumaEnabled is true and other arguments are missing.
%% @end
%%--------------------------------------------------------------------
-spec make_luma_config(OpNode :: node(), StorageParams :: storage_params()) -> luma_config().
make_luma_config(OpNode, StorageParams) ->
    case onepanel_utils:get_converted(lumaFeed, StorageParams, atom, auto) of
        auto ->
            op_worker_rpc:new_luma_config(OpNode, auto);
        local ->
            op_worker_rpc:new_luma_config(OpNode, local);
        external ->
            Url = get_required_luma_arg(lumaFeedUrl, StorageParams, binary),
            ApiKey = onepanel_utils:get_converted(lumaFeedApiKey, StorageParams, binary, undefined),
            op_worker_rpc:new_luma_config_with_external_feed(OpNode, Url, ApiKey)
    end.


-spec verify_configuration(id() | name(), storage_type(), boolean(), boolean()) -> ok.
verify_configuration(NameOrId, StorageType, Imported, Readonly) ->
    case op_worker_rpc:storage_verify_configuration(NameOrId, StorageType, Imported, Readonly) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end.


%%--------------------------------------------------------------------
%% @private @doc For read-write storage verifies that it is accessible for all
%% op_worker service nodes.
%% @end
%%--------------------------------------------------------------------
-spec maybe_verify_write_access(Helper :: helper(), SkipStorageDetection :: boolean(),
    Readonly :: boolean(), luma_feed()) -> skipped | verified | no_return().
maybe_verify_write_access(_Helper, true, _, _LumaFeed) ->
    skipped;
maybe_verify_write_access(_Helper, false, true, _LumaFeed) ->
    % TODO VFS-6594 - Implement verification of read access on RO storage
    skipped;
maybe_verify_write_access(Helper, false, false, LumaFeed) ->
    ?info("Verifying write access to storage"),
    verify_write_access(Helper, LumaFeed),
    verified.


%%--------------------------------------------------------------------
%% @private @doc Verifies that storage is accessible for all op_worker
%% service nodes.
%% @end
%%--------------------------------------------------------------------
-spec verify_write_access(helper(), luma_feed()) ->
    ok | no_return().
verify_write_access(Helper, LumaFeed) ->
    case op_worker_rpc:verify_storage_on_all_nodes(Helper, LumaFeed) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end.


%%--------------------------------------------------------------------
%% @private @doc Returns LUMA argument value associated with Key
%% in StorageParams. Throws error if key is missing
%% @end
%%--------------------------------------------------------------------
-spec get_required_luma_arg(Key :: atom(), StorageParams :: storage_params(),
    Type :: onepanel_utils:type()) -> term().
get_required_luma_arg(Key, StorageParams, Type) ->
    case onepanel_utils:find_converted(Key, StorageParams, Type) of
        error -> throw(?ERROR_MISSING_REQUIRED_VALUE(str_utils:to_binary(Key)));
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
    #{name := Name, lumaFeed := LumaFeed} = Details,
    Readonly = onepanel_utils:get_converted(readonly, Details, boolean, false),
    SkipStorageDetection = onepanel_utils:get_converted(skipStorageDetection, Details, boolean, Readonly),
    ?info("Modified storage ~tp (~tp)", [Name, StorageId]),
    try
        {ok, Helper} = op_worker_rpc:storage_get_helper(OpNode, StorageId),
        maybe_verify_write_access(Helper, SkipStorageDetection, Readonly, LumaFeed)
    of
        skipped -> Details;
        verified -> Details#{verificationPassed => true}
    catch ErrType:Error ->
        ?warning("Verfication of modified storage ~p (~p) failed: ~tp:~tp",
            [Name, StorageId, ErrType, Error]),
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
    op_worker_rpc:prepare_helper_args(OpNode, StorageType, convert_to_binaries(Params)).


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
    #{url => binary(), api_key => binary(), luma_feed => op_worker_rpc:luma_feed()}.
make_luma_params(Params) ->
    kv_utils:copy_found([
        {lumaFeedUrl, url},
        {lumaFeedApiKey, api_key},
        {lumaFeed, feed}
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
        _ -> ok = op_worker_rpc:storage_update_admin_ctx(OpNode, Id, Ctx)
    end.


%% @private
-spec maybe_update_args(OpNode :: node(), Id :: id(), Type :: binary(),
    Params :: storage_params()) -> ok | no_return().
maybe_update_args(OpNode, Id, Type, Params) ->
    Args = make_helper_args(OpNode, Type, Params),
    case maps:size(Args) of
        0 -> ok;
        _ -> ok = op_worker_rpc:storage_update_helper_args(OpNode, Id, Args)
    end.


%% @private
-spec maybe_update_luma_config(OpNode :: node(), Id :: id(),
    Params :: #{atom() => term()}) -> ok | no_return().
maybe_update_luma_config(OpNode, Id, Params) ->
    case make_luma_params(Params) of
        Empty when map_size(Empty) == 0 ->
            ok;
        Changes ->
            ok = op_worker_rpc:storage_update_luma_config(OpNode, Id, Changes)
    end.


-spec maybe_update_qos_parameters(OpNode :: node(), Id :: id(),
    storage_params()) -> ok | errors:error().
maybe_update_qos_parameters(OpNode, Id, #{qosParameters := Parameters}) ->
    update_qos_parameters(OpNode, Id, normalize_numeric_qos_parameters(Parameters));
maybe_update_qos_parameters(_OpNode, _Id, _) ->
    ok.


-spec update_qos_parameters(OpNode :: node(), Id :: id(),
    qos_parameters()) -> ok | errors:error().
update_qos_parameters(OpNode, Id, Parameters) ->
    op_worker_rpc:storage_set_qos_parameters(OpNode, Id, Parameters).


-spec update_readonly_and_imported(OpNode :: node(), Id :: id(),
    boolean(), boolean()) -> ok.
update_readonly_and_imported(OpNode, Id, Readonly, Imported) ->
    ok = op_worker_rpc:storage_update_readonly_and_imported(OpNode, Id, Readonly, Imported).


%%--------------------------------------------------------------------
%% @doc Checks if storage with given id exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> boolean().
exists(StorageId) ->
    {ok, Node} = nodes:any(?SERVICE_OPW),
    op_worker_rpc:storage_exists(Node, StorageId).


%%--------------------------------------------------------------------
%% @doc Checks if storage with given id exists,
%% using provided op_worker node for rpc.
%% @end
%%--------------------------------------------------------------------
-spec exists(Node :: node(), id()) -> boolean().
exists(Node, StorageId) ->
    op_worker_rpc:storage_exists(Node, StorageId).


%%--------------------------------------------------------------------
%% @private @doc Parses and validates auto-cleaning configuration
%% arguments.
%% @end
%%--------------------------------------------------------------------
-spec parse_auto_cleaning_configuration(map()) -> map().
parse_auto_cleaning_configuration(Args) ->
    maps_utils:remove_undefined(#{
        enabled => onepanel_utils:get_converted(enabled, Args, boolean, undefined),
        target => onepanel_utils:get_converted([target], Args, integer, undefined),
        threshold => onepanel_utils:get_converted([threshold], Args, integer, undefined),
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
        enabled => onepanel_utils:get_converted([rules, enabled], Args, boolean, undefined)
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
    maps_utils:remove_undefined(ParsedRules2).

%%--------------------------------------------------------------------
%% @private @doc Parses and validates auto-cleaning rule setting.
%% @end
%%--------------------------------------------------------------------
-spec parse_auto_cleaning_rule_setting(atom(), map()) -> map().
parse_auto_cleaning_rule_setting(RuleName, Args) ->
    maps_utils:remove_undefined(#{
        enabled => onepanel_utils:get_converted([rules, RuleName, enabled], Args, boolean, undefined),
        value => onepanel_utils:get_converted([rules, RuleName, value], Args, integer, undefined)
    }).

%%-------------------------------------------------------------------
%% @private @doc Parses and validates file-popularity configuration
%% @end
%%-------------------------------------------------------------------
-spec parse_file_popularity_configuration(map()) -> map().
parse_file_popularity_configuration(Args) ->
    maps_utils:remove_undefined(#{
        enabled => onepanel_utils:get_converted(enabled, Args, boolean, undefined),
        last_open_hour_weight => onepanel_utils:get_converted(last_open_hour_weight, Args, float, undefined),
        avg_open_count_per_day_weight => onepanel_utils:get_converted(avg_open_count_per_day_weight, Args, float, undefined),
        max_avg_open_count_per_day => onepanel_utils:get_converted(max_avg_open_count_per_day, Args, float, undefined)
    }).


%% @private
-spec convert_to_binaries(#{term() => term()}) -> #{binary() => binary()}.
convert_to_binaries(Map) ->
    onepanel_utils:convert(Map, {map, binary}).


%% @private
-spec normalize_numeric_qos_parameters(#{Term => binary()}) -> #{Term => binary() | integer()}.
normalize_numeric_qos_parameters(QosParameters) ->
    maps:map(fun(_Key, Value) ->
        case re:run(Value, <<"^[0-9]+$">>, [{capture, none}]) of
            match -> binary_to_integer(Value);
            nomatch -> Value
        end
    end, QosParameters).

%% @private
-spec exec_and_throw_on_error(function(), [term()]) -> ok | {ok, term()}.
exec_and_throw_on_error(Function, Args) ->
    try apply(Function, Args) of
        ok -> ok;
        {ok, Res} -> {ok, Res};
        {error, _} = Error -> throw(Error)
    catch
        error:{badmatch, Error2} ->
            throw(Error2)
    end.