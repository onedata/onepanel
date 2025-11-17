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
    verificationPassed => boolean(),
    atom() := binary()
}.

-type storages_map() :: #{Name :: name() => Params :: storage_params()}.
-type qos_parameters() :: #{binary() => binary()}.

%% Opaque terms from op_worker
%% Removed unused types: luma_config, luma_feed, helper
%% These were only used by the old add/4 function which has been replaced.
% @formatter:on

-export_type([id/0, name/0, storage_params/0, storage_details/0, storages_map/0, qos_parameters/0]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec add(Ctx :: #{name := name(), params := storage_params()}) ->
    {op_worker_storage:name(), {ok, op_worker_storage:id()} | {error, term()}}.
add(#{name := Name, params := Params}) ->
    {ok, OpNode} = nodes:any(?SERVICE_OPW),
    StorageName = onepanel_utils:convert(Name, binary),
    StorageType = onepanel_utils:get_converted(type, Params, binary),
    log_gathered_storage_configuration(Name, StorageType, Params),

    Result = try
        CreateSpec = storage_spec_builder:build_create_spec(StorageName, Params),
        op_worker_rpc:storage_create(OpNode, CreateSpec)
    catch
        _:{error, Reason} ->
            {error, Reason};
        Class:Reason:Stacktrace ->
            ?error_stacktrace("Unexpected error when adding storage '~ts' (~ts) - ~w:~tp", [
                StorageName, StorageType, Class, Reason
            ], Stacktrace),
            {error, storage_add_failed}
    end,
    case Result of
        {ok, AddedStorageId} ->
            ?notice("Successfully added storage '~ts' (~ts) with Id: '~ts'", [
                StorageName, StorageType, AddedStorageId
            ]);
        {error, _} = Error ->
            ?error(?autoformat_with_msg("Failed to add storage '~ts' (~ts)", [StorageName, StorageType], Error))
    end,
    {StorageName, Result}.


-spec update(OpNode :: node(), Id :: id(), Params :: storage_params()) ->
    storage_details().
update(OpNode, Id, NewParams) ->
    Storage = op_worker_storage:get(Id),
    Name = maps:get(name, Storage),
    StorageType = maps:get(type, Storage),

    try
        UpdateSpec = storage_spec_builder:build_update_spec(StorageType, NewParams),
        case op_worker_rpc:storage_update(OpNode, Id, UpdateSpec) of
            ok -> 
                Details = ?MODULE:get(Id),
                ?info("Modified storage ~tp (~tp)", [Name, Id]),
                Details#{verificationPassed => true};
            {error, _} = Error ->
                ?error(?autoformat_with_msg("Storage modification failed", [Id, Name], Error)),
                DetailsOnError = ?MODULE:get(Id),
                DetailsOnError#{verificationPassed => false}
        end
    catch Class:Reason:Stacktrace ->
        DetailsOnException = ?MODULE:get(Id),
        ?error_exception(
            ?autoformat_with_msg("Storage modification failed", [Id, Name]),
            Class, Reason, Stacktrace
        ),
        DetailsOnException#{verificationPassed => false}
    end.


%%--------------------------------------------------------------------
%% @doc Removes given storage.
%% Fails if any space is supported by this storage.
%% @end
%%--------------------------------------------------------------------
-spec remove(OpNode :: node(), id()) -> ok | no_return().
remove(OpNode, Id) ->
    case op_worker_rpc:storage_safe_remove(OpNode, Id) of
        ok ->
            ?info("Successfully removed storage with id ~tp", [Id]),
            ok;
        {error, storage_in_use} ->
            throw(?ERR_STORAGE_IN_USE(?err_ctx()))
    end.


%%--------------------------------------------------------------------
%% @doc Returns lists of storage ids currently configured in op_worker
%% service.
%% @end
%%--------------------------------------------------------------------
-spec list() -> [id()].
list() ->
    {ok, Ids} = op_worker_rpc:get_storages(),
    Ids.


%%--------------------------------------------------------------------
%% @doc Returns details of a selected storage from op_worker service.
%% @end
%%--------------------------------------------------------------------
-spec get(Id :: id()) -> storage_details().
get(Id) ->
    {ok, OpNode} = nodes:any(?SERVICE_OPW),
    {ok, Description} = op_worker_rpc:storage_describe(OpNode, Id),
    storage_spec_builder:description_to_map(Description).


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
    op_worker_rpc:space_logic_get_storages(OpNode, SpaceId).


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
-spec log_gathered_storage_configuration(Name :: binary(), StorageType :: binary(), Params :: storage_params()) ->
    ok.
log_gathered_storage_configuration(Name, StorageType, Params) ->
    ParamsWithBinaryKeys = maps_utils:map_key_value(fun(AtomKey, Value) ->
        {atom_to_binary(AtomKey, utf8), Value}
    end, Params),
    %% TODO redact confidential params
    RedactedParams = ParamsWithBinaryKeys,
%%    RedactedParams = op_worker_rpc:redact_confidential_helper_params(
%%        StorageType, maps:without([<<"type">>], ParamsWithBinaryKeys)
%%    ),
    FormattedParams = lists:map(fun
        ({Key, Value}) when is_binary(Value) ->
            str_utils:format_bin("    ~ts: ~ts", [Key, Value]);
        ({Key, Value}) ->
            str_utils:format_bin("    ~ts: ~tp", [Key, Value])
    end, maps:to_list(RedactedParams)),
    ?info("Gathered storage configuration for '~ts' (~ts) - parameters: ~n~ts", [
        Name, StorageType, str_utils:join_as_binaries(FormattedParams, str_utils:format_bin("~n", []))
    ]).
