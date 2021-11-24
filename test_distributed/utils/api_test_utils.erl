%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions used in API tests.
%%% @end
%%%-------------------------------------------------------------------
-module(api_test_utils).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("api_test_utils.hrl").
-include("api_test_storages.hrl").

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/http/headers.hrl").

% an object containing API request data, serializable to json
-type data() :: map()| undefined.

-type placeholder_substitute() :: #placeholder_substitute{}.

-export_type([placeholder_substitute/0]).

-export([ensure_defined/2]).
-export([maybe_substitute_bad_id/2]).
-export([substitute_placeholders/2]).
-export([get_storage_id_by_name/2]).
-export([to_hostnames/1]).
-export([match_location_header/2]).
-export([perform_io_test_on_storage/1]).
-export([perform_io_test_on_storage_and_revoke_test_space/1]).

%%%===================================================================
%%% API
%%%===================================================================


-spec ensure_defined
    (undefined, DefaultValue) -> DefaultValue when DefaultValue :: term();
    (Value, DefaultValue :: term()) -> Value when Value :: term().
ensure_defined(undefined, DefaultValue) -> DefaultValue;
ensure_defined(Value, _DefaultValue) -> Value.


-spec maybe_substitute_bad_id
    (ValidId :: binary(), undefined) -> {ValidId :: binary(), undefined};
    (ValidId :: binary(), Data :: map()) -> {Id :: binary(), map()}.
maybe_substitute_bad_id(ValidId, undefined) ->
    {ValidId, undefined};
maybe_substitute_bad_id(ValidId, Data) ->
    case maps:take(bad_id, Data) of
        {BadId, LeftoverData} -> {BadId, LeftoverData};
        error -> {ValidId, Data}
    end.


-spec substitute_placeholders(data(), kv_utils:nested(#{Key :: binary() => #{Placeholder :: atom() => #placeholder_substitute{}}})) -> data().
substitute_placeholders(Data, ReplacementsMap) ->
    maps:map(fun(Key, ValueOrPlaceholder) ->
        case kv_utils:find([Key, ValueOrPlaceholder], ReplacementsMap) of
            error ->
                ValueOrPlaceholder;
            {ok, #placeholder_substitute{value = Value, posthook = Posthook}} ->
                Posthook(),
                Value
        end
    end, Data).


-spec get_storage_id_by_name(oct_background:entity_selector(), binary()) -> binary().
get_storage_id_by_name(EntitySelector, StorageName) ->
    StorageIds = opw_test_rpc:get_storages(EntitySelector),
    Storages = [opw_test_rpc:storage_describe(EntitySelector, X) || X <- StorageIds],

    [StorageId | _] = [maps:get(<<"id">>, X) || X <- Storages, (maps:get(<<"name">>, X) == StorageName)],
    StorageId.


-spec to_hostnames([node()]) -> [binary()].
to_hostnames(Nodes) ->
    [list_to_binary(utils:get_host(X)) || X <- Nodes].


-spec match_location_header(map(), binary()) -> binary().
match_location_header(Headers, Path) ->
    Location = maps:get(?HDR_LOCATION, Headers),
    {match, [Item]} = ?assertMatch({match, [_]}, re:run(Location, Path ++ "(.+)", [{capture, all_but_first, binary}])),
    Item.


-spec perform_io_test_on_storage_and_revoke_test_space(binary()) -> ok | error.
perform_io_test_on_storage_and_revoke_test_space(StorageId) ->
    {Result, SpaceId} = perform_io_test_on_storage(StorageId),
    opw_test_rpc:revoke_space_support(krakow, SpaceId),
    Result.


-spec perform_io_test_on_storage(binary()) -> {ok, binary()} | {error, binary()}.
perform_io_test_on_storage(StorageId) ->
    SpaceName = str_utils:rand_hex(10),
    UserId = oct_background:get_user_id(joe),
    SpaceId = ozw_test_rpc:create_space(UserId, SpaceName),
    Token = ozw_test_rpc:create_space_support_token(UserId, SpaceId),
    {ok, SerializedToken} = tokens:serialize(Token),
    opw_test_rpc:support_space(krakow, StorageId, SerializedToken, 10000000),
    AccessToken = ozw_test_rpc:create_user_temporary_access_token(zone, UserId),
    ?assertEqual(SpaceId, opw_test_rpc:get_user_space_by_name(krakow, SpaceName, AccessToken), ?ATTEMPTS),
    ?assertEqual(true, lists:member(SpaceId, opw_test_rpc:get_spaces(krakow)), ?ATTEMPTS),
    Path = filename:join(["/", SpaceName]),
    case opw_test_rpc:perform_io_test(krakow, Path, AccessToken) of
        ok -> {ok, SpaceId};
        error -> {error, SpaceId}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
