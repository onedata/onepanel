%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides functions for making rpc calls to OpWorker Nodes.
%%% @end
%%%-------------------------------------------------------------------
-module(op_worker_test_rpc).
-author("Piotr Duleba").

-include_lib("ctool/include/test/test_utils.hrl").

-define(ATTEMPTS, 120).

%% API
-export([
    get_storage_ids/1,
    describe_storage/2,
    get_space_ids/1,
    get_space_document/2,
    get_local_storage_id/2,
    get_local_storage_ids/2,
    is_storage_imported/2,
    get_autocleaning_status/2,
    support_space/4,
    revoke_space_support/2,
    get_space_support_size/2,
    get_space_providers/2
]).

%%%===================================================================
%%% API functions
%%%===================================================================


-spec get_storage_ids(test_config:config()) -> list().
get_storage_ids(Config) ->
    {ok, StorageIds} = ?assertMatch({ok, _}, call_provider_node(Config, provider_logic, get_storage_ids, [])),
    StorageIds.


-spec describe_storage(test_config:config(), binary()) -> map().
describe_storage(Config, StorageId) ->
    {ok, StorageDetails} = ?assertMatch({ok, _}, call_provider_node(Config, storage, describe, [StorageId])),
    StorageDetails.


-spec get_space_ids(test_config:config()) -> list().
get_space_ids(Config) ->
    % TODO VFS-6780 - currently, supporting providers are calculated asynchronously
    % (effective relation) and for some time the provider may no be allowed to fetch the space details.
    {ok, SpacesIds} = ?assertMatch({ok, _}, call_provider_node(Config, provider_logic, get_spaces, []), ?ATTEMPTS),
    SpacesIds.


-spec get_space_document(test_config:config(), binary()) -> map().
get_space_document(Config, SpaceId) ->
    % TODO VFS-6780 - currently, supporting providers are calculated asynchronously
    % (effective relation) and for some time the provider may no be allowed to fetch the space details.
    {ok, SpaceDocument} = ?assertMatch({ok, _}, call_provider_node(Config, rpc_api, get_space_details, [SpaceId]), ?ATTEMPTS),
    SpaceDocument.


-spec get_local_storage_id(test_config:config(), binary()) -> binary().
get_local_storage_id(Config, SpaceId) ->
    % TODO VFS-6780 - currently, supporting providers are calculated asynchronously
    % (effective relation) and for some time the provider may no be allowed to fetch the space details.
    {ok, StorageId} = ?assertMatch({ok, _}, call_provider_node(Config, space_logic, get_local_storage_id, [SpaceId]), ?ATTEMPTS),
    StorageId.


-spec get_local_storage_ids(test_config:config(), binary()) -> list().
get_local_storage_ids(Config, SpaceId) ->
    % TODO VFS-6780 - currently, supporting providers are calculated asynchronously
    % (effective relation) and for some time the provider may no be allowed to fetch the space details.
    {ok, StoragesIds} = ?assertMatch({ok, _}, call_provider_node(Config, space_logic, get_local_storage_ids, [SpaceId]), ?ATTEMPTS),
    StoragesIds.


-spec is_storage_imported(test_config:config(), binary()) -> boolean().
is_storage_imported(Config, StorageId) ->
    IsImported = call_provider_node(Config, storage, is_imported, [StorageId]),
    ?assert(is_boolean(IsImported)),
    IsImported.


-spec get_autocleaning_status(test_config:config(), binary()) -> map().
get_autocleaning_status(Config, SpaceId) ->
    AutocleaningStatus = call_provider_node(Config, autocleaning_api, status, [SpaceId]),
    ?assert(is_map(AutocleaningStatus)),
    AutocleaningStatus.


-spec support_space(test_config:config(), binary(), binary(), binary()) -> binary().
support_space(Config, StorageId, SerializedToken, SupportSize) ->
    {ok, SupportedSpaceId} = ?assertMatch({ok, _}, call_provider_node(Config, storage_logic, support_space, [StorageId, SerializedToken, SupportSize])),
    SupportedSpaceId.


-spec revoke_space_support(test_config:config(), binary()) -> ok.
revoke_space_support(Config, SpaceId) ->
    ?assertMatch(ok, call_provider_node(Config, rpc_api, revoke_space_support, [SpaceId])).


-spec get_space_support_size(test_config:config(), binary()) -> binary().
get_space_support_size(Config, SpaceId) ->
    % TODO VFS-6780 - currently, supporting providers are calculated asynchronously
    % (effective relation) and for some time the provider may no be allowed to fetch the space details.
    {ok, SupportSize} = ?assertMatch({ok, _}, call_provider_node(Config, provider_logic, get_support_size, [SpaceId]), ?ATTEMPTS),
    SupportSize.


-spec get_space_providers(test_config:config(), binary()) -> list().
get_space_providers(Config, SpaceId) ->
    % TODO VFS-6780 - currently, supporting providers are calculated asynchronously
    % (effective relation) and for some time the provider may no be allowed to fetch the space details.
    {ok, ProvidersId} = ?assertMatch({ok, _}, call_provider_node(Config, space_logic, get_provider_ids, [SpaceId]), ?ATTEMPTS),
    ProvidersId.


%%%===================================================================
%%% Helper functions
%%%===================================================================


call_provider_node(Config, Module, Function, Args) ->
    OpWorkerNodes = test_config:get_all_op_worker_nodes(Config),
    rpc:call(lists_utils:random_element(OpWorkerNodes), Module, Function, Args).