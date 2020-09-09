%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides api for making rpc calls.
%%% @end
%%%-------------------------------------------------------------------
-module(rpc_api).
-author("Piotr Duleba").

-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([get_storages_ids/1, get_storage_details/2]).


%%%===================================================================
%%% API functions
%%%===================================================================


%% Returns list of provider`s storages ids.
get_storages_ids(ProviderNodes)->
    {ok, StorageIds} = ?assertMatch({ok, _}, rpc:call(hd(ProviderNodes), provider_logic, get_storage_ids, [])),
    StorageIds.


%% Returns map of provider`s storage details.
get_storage_details(ProviderNodes, StorageId)->
    {ok, StorageDetails} = ?assertMatch({ok, _}, rpc:call(hd(ProviderNodes), storage, describe, [StorageId])),
    StorageDetails.
