%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Module responsible of creation of op_workers "helper" records.
%%% @end
%%%--------------------------------------------------------------------
-module(storage_params).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include_lib("hackney/include/hackney_lib.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([get_helper_args/3]).
-export([get_storage_user_ctx/3]).


%%--------------------------------------------------------------------
%% @doc
%% Extracts helper arguments from storage params.
%% @end
%%--------------------------------------------------------------------
get_helper_args(Node, StorageType, Params) ->
    BinKeys = onepanel_utils:convert(Params, {keys, binary}),
    Args = prepare_args(StorageType, BinKeys),

    % ensure only relevant keys
    RelevantArgs = rpc:call(Node, helper, filter_args, [StorageType, Args]),
    % ensure binary values
    onepanel_utils:convert(RelevantArgs, {values, binary}).


%%--------------------------------------------------------------------
%% @doc Returns storage user context record.
%% @end
%%--------------------------------------------------------------------
-spec get_storage_user_ctx(Node :: node(), StorageType :: binary(),
    Params :: op_worker_storage:storage_params()) -> UserCtx :: term().
get_storage_user_ctx(Node, <<"ceph">>, Params) ->
    rpc:call(Node, helper, new_ceph_user_ctx, [
        onepanel_utils:typed_get(username, Params, binary),
        onepanel_utils:typed_get(key, Params, binary)
    ]);

get_storage_user_ctx(Node, <<"cephrados">>, Params) ->
    rpc:call(Node, helper, new_cephrados_user_ctx, [
        onepanel_utils:typed_get(username, Params, binary),
        onepanel_utils:typed_get(key, Params, binary)
    ]);

get_storage_user_ctx(Node, <<"posix">>, _Params) ->
    rpc:call(Node, helper, new_posix_user_ctx, [0, 0]);

get_storage_user_ctx(Node, <<"s3">>, Params) ->
    rpc:call(Node, helper, new_s3_user_ctx, [
        onepanel_utils:typed_get(accessKey, Params, binary),
        onepanel_utils:typed_get(secretKey, Params, binary)
    ]);

get_storage_user_ctx(Node, <<"swift">>, Params) ->
    rpc:call(Node, helper, new_swift_user_ctx, [
        onepanel_utils:typed_get(username, Params, binary),
        onepanel_utils:typed_get(password, Params, binary)
    ]);

get_storage_user_ctx(Node, <<"glusterfs">>, _Params) ->
    rpc:call(Node, helper, new_glusterfs_user_ctx, [0, 0]);

get_storage_user_ctx(Node, <<"nulldevice">>, _Params) ->
    rpc:call(Node, helper, new_nulldevice_user_ctx, [0, 0]);

get_storage_user_ctx(Node, <<"webdav">>, Params) ->
    rpc:call(Node, helper, new_webdav_user_ctx, [
        <<_/binary>> = onepanel_utils:typed_get(credentialsType, Params, binary),
        <<_/binary>> = onepanel_utils:typed_get(credentials, Params, binary, <<>>)
    ]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Translates storage parameters as received in API to storage
%% helper arguments.
%% @end
%%--------------------------------------------------------------------
-spec prepare_args(StorageType :: binary(), #{binary() => term()}) ->
    #{binary() => term()}.
prepare_args(<<"s3">>, Args) ->
    maps:fold(fun
        (<<"hostname">>, Hostname, Acc) ->
            #hackney_url{scheme = S3Scheme, host = S3Host, port = S3Port} =
                hackney_url:parse_url(Hostname),
            Scheme = case S3Scheme of
                https -> <<"https">>;
                _ -> <<"http">>
            end,
            Acc#{
                <<"hostname">> => onepanel_utils:join([S3Host, S3Port], <<":">>),
                <<"scheme">> => Scheme
            };
        (Key, Value, Acc) ->
            Acc#{Key => Value}
    end, #{}, Args);

prepare_args(_HelperName, Args) ->
    Args.

