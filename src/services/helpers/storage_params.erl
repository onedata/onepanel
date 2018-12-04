%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Functions for selecting parameters relevant to various aspects
%%% of a storage, ie. helper params, user ctx etc.
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
    Params :: #{atom() => term()}) -> helper_args().
make_helper_args(OpNode, StorageType, Params) ->
    Args = prepare_args(StorageType, Params),
    BinKeys = onepanel_utils:convert(Args, {keys, binary}),

    % ensure only relevant keys
    RelevantArgs = rpc:call(OpNode, helper, filter_args,
        [StorageType, BinKeys]),

    % ensure binary values
    onepanel_utils:convert(RelevantArgs, {values, binary}).


%%--------------------------------------------------------------------
%% @doc
%% Extracts storage params relevant to the user ctx.
%% @end
%%--------------------------------------------------------------------
-spec make_user_ctx(OpNode :: node(), StorageType :: binary(),
    Params :: storage_params()) -> user_ctx().
make_user_ctx(Node, StorageType, Params) ->
    Args = prepare_user_ctx_params(StorageType, Params),
    BinKeys = onepanel_utils:convert(Args, {keys, binary}),

    % ensure only relevant keys
    RelevantParams = rpc:call(Node, helper, filter_user_ctx,
        [StorageType, BinKeys]),

    % ensure binary values
    onepanel_utils:convert(RelevantParams, {values, binary}).


%%--------------------------------------------------------------------
%% @doc
%% Extracts storage params relevant to the luma config.
%% @end
%%--------------------------------------------------------------------
-spec make_luma_params(Params :: storage_params()) ->
    #{url => binary(), api_key => binary()}.
make_luma_params(Params) ->
    onepanel_maps:get_store_multiple([
        {lumaUrl, url},
        {lumaApiKey, api_key}
    ], Params).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Applies necessary transformations to storage params producing
%% helper args.
%% @end
%%--------------------------------------------------------------------
-spec prepare_args(StorageType :: binary(), Params :: storage_params()) ->
    storage_params().
prepare_args(<<"s3">>, Params) ->
    Hostname = onepanel_utils:typed_get(hostname, Params, binary),
    #hackney_url{scheme = S3Scheme, host = S3Host, port = S3Port} =
        hackney_url:parse_url(Hostname),

    Scheme = case S3Scheme of
        https -> <<"https">>;
        _ -> <<"http">>
    end,

    Params#{
        <<"hostname">> => onepanel_utils:join([S3Host, S3Port], <<":">>),
        <<"scheme">> => Scheme
    };

prepare_args(_HelperName, Params) ->
    Params.


%% @fixme maybe this needs not to be invoked on PATCH at all
%%--------------------------------------------------------------------
%% @doc
%% Applies necessary transformations to storage params producing
%% user Ctx params.
%% @end
%%--------------------------------------------------------------------
-spec prepare_user_ctx_params(StorageType :: binary(),
    Params :: storage_params()) -> #{binary() => binary()}.
prepare_user_ctx_params(StorageType, _Params) when
    StorageType == <<"glusterfs">>;
    StorageType == <<"nulldevice">>;
    StorageType == <<"posix">> ->
    #{<<"uid">> => <<"0">>, <<"gid">> => <<"0">>};

prepare_user_ctx_params(<<"webdav">>, Params) ->
    case Params of
        #{<<"credentialsType">> := <<"none">>} ->
            Params#{<<"credentials">> => <<>>};
        _ -> Params
    end;

prepare_user_ctx_params(_StorageType, Params) ->
    Params.

