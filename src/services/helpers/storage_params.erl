%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Functions for selecting parameters relevant to various aspects
%%% of a storage, i.e. helper params, user ctx etc.
%%% Does not ensure that all required parameters are present, since
%%% it is not the case for modification requests.
%%% @end
%%%--------------------------------------------------------------------
-module(storage_params).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include_lib("hackney/include/hackney_lib.hrl").
-include_lib("ctool/include/logging.hrl").

-type storage_params() :: op_worker_storage:storage_params().
-type helper_args() :: op_worker_storage:helper_args().
-type user_ctx() :: op_worker_storage:user_ctx().

%% API
-export([make_helper_args/3, make_user_ctx/3, make_luma_params/1]).


%%%===================================================================
%%% Public API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Extracts helper arguments from storage params.
%% @end
%%--------------------------------------------------------------------
-spec make_helper_args(OpNode :: node(), StorageType :: binary(),
    Params :: storage_params()) -> helper_args().
make_helper_args(OpNode, StorageType, Params) ->
     rpc:call(OpNode, helper, prepare_helper_args,
        [StorageType, convert_to_binaries(Params)]).


%%--------------------------------------------------------------------
%% @doc
%% Extracts storage params relevant to the user ctx.
%% @end
%%--------------------------------------------------------------------
-spec make_user_ctx(OpNode :: node(), StorageType :: binary(),
    Params :: storage_params()) -> user_ctx().
make_user_ctx(OpNode, StorageType, Params) ->
    rpc:call(OpNode, helper, prepare_user_ctx_params,
        [StorageType, convert_to_binaries(Params)]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert params map to types used by helper args and ctx,
%% that is binary keys and binary or integer values.
%% @end
%%--------------------------------------------------------------------
-spec convert_to_binaries(#{term() => term()}) -> helper_args().
convert_to_binaries(Map) ->
    maps:from_list([
        {
            onepanel_utils:convert(Key, binary),
            onepanel_utils:convert(Value, binary)
        } || {Key, Value} <- maps:to_list(Map)
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Extracts storage params relevant to the luma config.
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
