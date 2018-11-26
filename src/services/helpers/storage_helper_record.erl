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
-module(storage_helper_record).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include_lib("hackney/include/hackney_lib.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([make_storage_helper/4]).

%%--------------------------------------------------------------------
%% @private @doc Builds storage helper record. Calls given op worker
%% node when necessary.
%% @end
%%--------------------------------------------------------------------
-spec make_storage_helper(Node :: node(), StorageType :: binary(),
    UserCtx :: any(), Params :: op_worker_storage:storage_params_map()) ->
    Helper :: any().
make_storage_helper(Node, <<"ceph">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_ceph_helper, [
        onepanel_utils:typed_get(monitorHostname, Params, binary),
        onepanel_utils:typed_get(clusterName, Params, binary),
        onepanel_utils:typed_get(poolName, Params, binary),
        get_helper_opt_args([{timeout, binary}], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"flat">>)
    ]);

make_storage_helper(Node, <<"cephrados">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_cephrados_helper, [
        onepanel_utils:typed_get(monitorHostname, Params, binary),
        onepanel_utils:typed_get(clusterName, Params, binary),
        onepanel_utils:typed_get(poolName, Params, binary),
        get_helper_opt_args([
            {timeout, binary},
            {blockSize, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"flat">>)
    ]);

make_storage_helper(Node, <<"posix">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_posix_helper, [
        onepanel_utils:typed_get(mountPoint, Params, binary),
        get_helper_opt_args([{timeout, binary}], Params),
        UserCtx,
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"canonical">>)
    ]);

make_storage_helper(Node, <<"s3">>, UserCtx, Params) ->
    #hackney_url{scheme = S3Scheme, host = S3Host, port = S3Port} =
        hackney_url:parse_url(onepanel_utils:typed_get(hostname, Params, binary)),
    rpc:call(Node, helper, new_s3_helper, [
        onepanel_utils:join([S3Host, S3Port], <<":">>),
        onepanel_utils:typed_get(bucketName, Params, binary),
        S3Scheme =:= https,
        get_helper_opt_args([
            {signatureVersion, binary},
            {timeout, binary},
            {blockSize, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"flat">>)
    ]);

make_storage_helper(Node, <<"swift">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_swift_helper, [
        onepanel_utils:typed_get(authUrl, Params, binary),
        onepanel_utils:typed_get(containerName, Params, binary),
        onepanel_utils:typed_get(tenantName, Params, binary),
        get_helper_opt_args([
            {timeout, binary},
            {blockSize, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"flat">>)
    ]);

make_storage_helper(Node, <<"glusterfs">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_glusterfs_helper, [
        onepanel_utils:typed_get(volume, Params, binary),
        onepanel_utils:typed_get(hostname, Params, binary),
        get_helper_opt_args([
            {port, binary},
            {mountPoint, binary},
            {transport, binary},
            {xlatorOptions, binary},
            {timeout, binary},
            {blockSize, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"canonical">>)
    ]);

make_storage_helper(Node, <<"nulldevice">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_nulldevice_helper, [
        get_helper_opt_args([
            {latencyMin, binary},
            {latencyMax, binary},
            {timeoutProbability, binary},
            {filter, binary},
            {simulatedFilesystemParameters, binary},
            {simulatedFilesystemGrowSpeed, binary},
            {timeout, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"canonical">>)
    ]);

make_storage_helper(Node, <<"webdav">>, UserCtx, Params) ->
    rpc:call(Node, helper, new_webdav_helper, [
        onepanel_utils:typed_get(endpoint, Params, binary),
        get_helper_opt_args([
            {verifyServerCertificate, binary},
            {authorizationHeader, binary},
            {rangeWriteSupport, binary},
            {connectionPoolSize, binary},
            {maximumUploadSize, binary},
            {timeout, binary}
        ], Params),
        UserCtx,
        onepanel_utils:typed_get(insecure, Params, boolean, false),
        onepanel_utils:typed_get(storagePathType, Params, binary, <<"canonical">>)
    ]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Returns storage helper optional argument.
%% @end
%%--------------------------------------------------------------------
-spec get_helper_opt_args(KeySpec :: [{Key :: atom(), Type :: onepanel_utils:type()}],
    Params :: op_worker_storage:storage_params_map()) -> OptArgs :: #{}.
get_helper_opt_args(KeysSpec, Params) ->
    lists:foldl(fun({Key, Type}, OptArgs) ->
        case onepanel_utils:typed_get(Key, Params, Type) of
            #error{} ->
                OptArgs;
            Value ->
                maps:put(onepanel_utils:convert(Key, binary), Value, OptArgs)
        end
    end, #{}, KeysSpec).

