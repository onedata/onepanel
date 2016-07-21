%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(op_worker_storage).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

-include_lib("hackney/include/hackney_lib.hrl").

%% API
-export([add/1, get/0, get/1]).

-type name() :: binary().
-type storage_params_map() :: #{Key :: atom() | binary() => Value :: binary()}.
-type storage_params_list() :: [{Key :: atom() | binary(), Value :: binary()}].
-type storage_map() :: #{Name :: name() => Params :: storage_params_map()}.
-type storage_list() :: [{Name :: name(), Params :: storage_params_list()}].

-export_type([storage_list/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @todo write me!
%%--------------------------------------------------------------------
-spec add(Storages :: storage_map()) -> ok | no_return().
add(Storages) ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    RootUserId = rpc:call(Node, fslogic_uuid, root_user_id, []),
    maps:fold(fun(Key, Value, _) ->
        StorageName = onepanel_utils:convert(Key, binary),
        StorageType = get_helper_arg(type, Value),
        {HelperName, HelperArgs, UserModel, RootCtxArgs} =
            parse_storage_params(StorageType, Value),
        RootCtx = rpc:call(Node, UserModel, new_ctx, RootCtxArgs),
        verify_storage(HelperName, HelperArgs, RootCtx),
        StorageId = add_storage(Node, StorageName, HelperName, HelperArgs),
        rpc:call(Node, UserModel, add, [RootUserId, StorageId, RootCtx])
    end, [], Storages),
    ok.


%%--------------------------------------------------------------------
%% @doc @todo write me!
%%--------------------------------------------------------------------
-spec get() -> storage_list().
get() ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    {ok, Storages} = rpc:call(Node, storage, list, []),
    lists:foldl(fun(Storage, Acc) ->
        Acc ++ get_storage(Node, Storage)
    end, [], Storages).


%%--------------------------------------------------------------------
%% @doc @todo write me!
%%--------------------------------------------------------------------
-spec get(Name :: name()) -> storage_list().
get(Name) ->
    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(service_op_worker:name(), Host),
    {ok, Storage} = rpc:call(Node, storage, get_by_name, [Name]),
    get_storage(Node, Storage).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec parse_storage_params(Type :: binary(), Params :: storage_params_map()) ->
    {HelperName :: binary(), HelperArgs :: #{}, UserModel :: atom(),
        RootCtxArgs :: list()}.
parse_storage_params(<<"POSIX">>, Params) ->
    {
        <<"DirectIO">>,
        #{<<"root_path">> => get_helper_arg(mountPoint, Params)},
        posix_user,
        [0, 0]
    };

parse_storage_params(<<"S3">>, Params) ->
    #hackney_url{scheme = S3Scheme, host = S3Host, port = S3Port} =
        hackney_url:parse_url(get_helper_arg(s3Hostname, Params)),
    #hackney_url{scheme = IamScheme, host = IamHost, port = IamPort} =
        hackney_url:parse_url(get_helper_arg(iamHostname, Params)),
    {
        <<"AmazonS3">>,
        #{
            <<"host_name">> => onepanel_utils:join([S3Host, S3Port], <<":">>),
            <<"scheme">> => onepanel_utils:convert(S3Scheme, binary),
            <<"bucket_name">> => get_helper_arg(bucketName, Params),
            <<"iam_host">> => onepanel_utils:join([IamHost, IamPort], <<":">>),
            <<"iam_request_scheme">> => onepanel_utils:convert(IamScheme, binary)
        },
        s3_user,
        [get_helper_arg(accessKey, Params), get_helper_arg(secretKey, Params)]
    };

parse_storage_params(<<"CEPH">>, Params) ->
    {
        <<"Ceph">>,
        #{
            <<"mon_host">> => get_helper_arg(monitorHostname, Params),
            <<"cluster_name">> => get_helper_arg(clusterName, Params),
            <<"pool_name">> => get_helper_arg(poolName, Params)
        },
        ceph_user,
        [get_helper_arg(username, Params), get_helper_arg(key, Params)]
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_helper_arg(Key :: atom(), Params :: storage_params_map()) ->
    HelperArg :: binary().
get_helper_arg(Key, Params) ->
    onepanel_utils:convert(maps:get(Key, Params), binary).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec verify_storage(HelperName :: binary(), HelperArgs :: #{}, UserCtx :: any()) ->
    ok | no_return().
verify_storage(HelperName, HelperArgs, UserCtx) ->
    [Node | _] = Nodes = service_op_worker:get_nodes(),
    Args = [HelperName, HelperArgs, UserCtx],
    {FileId, FileContent} = create_test_file(Node, Args),
    NewFileId = verify_test_file(Nodes, Args, FileId, FileContent),
    remove_test_file(Node, Args, NewFileId).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec create_test_file(Node :: node(), Args :: list()) ->
    {FileId :: binary(), FileContent :: binary()} | no_return().
create_test_file(Node, Args) ->
    case rpc:call(Node, helpers_utils, create_test_file, Args) of
        {badrpc, {'EXIT', {Reason, Stacktrace}}} ->
            ?throw({?ERR_STORAGE_TEST_FILE_CREATION, Node, Reason}, Stacktrace);
        {<<_/binary>> = FileId, <<_/binary>> = FileContent} ->
            {FileId, FileContent}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec verify_test_file(Nodes :: [node()], Args :: list(), FileId :: binary(),
    FileContent :: binary()) -> FileId :: binary() | no_return().
verify_test_file([], _Args, FileId, _FileContent) ->
    FileId;

verify_test_file([Node | Nodes], Args, FileId, FileContent) ->
    ActualFileContent = rpc:call(Node, helpers_utils, read_test_file, Args ++ [FileId]),
    remove_test_file(Node, Args, FileId),

    case ActualFileContent of
        FileContent ->
            {NewFileId, NewFileContent} = create_test_file(Node, Args),
            verify_test_file(Nodes, Args, NewFileId, NewFileContent);
        <<_/binary>> ->
            ?throw({?ERR_STORAGE_TEST_FILE_VERIFICATION, Node,
                {invalid_content, FileContent, ActualFileContent}});
        {badrpc, {'EXIT', {Reason, Stacktrace}}} ->
            ?error({?ERR_STORAGE_TEST_FILE_VERIFICATION, Node, Reason}, Stacktrace)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec remove_test_file(Node :: node(), Args :: list(), FileId :: binary()) ->
    ok | no_return().
remove_test_file(Node, Args, FileId) ->
    case rpc:call(Node, helpers_utils, remove_test_file, Args ++ [FileId]) of
        {badrpc, {'EXIT', {Reason, Stacktrace}}} ->
            ?throw({?ERR_STORAGE_TEST_FILE_REMOVAL, Node, Reason}, Stacktrace);
        _ -> ok
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec add_storage(Node :: node(), StorageName :: binary(), HelperName :: binary(),
    HelperArgs :: #{}) -> StorageId :: binary() | no_return().
add_storage(Node, StorageName, HelperName, HelperArgs) ->
    Helper = rpc:call(Node, fslogic_storage, new_helper_init, [HelperName, HelperArgs]),
    Storage = rpc:call(Node, fslogic_storage, new_storage, [StorageName, [Helper]]),
    case rpc:call(Node, storage, create, [Storage]) of
        {ok, StorageId} -> StorageId;
        {error, Reason} -> ?throw({?ERR_STORAGE_ADDITION, Reason})
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_storage(Node :: node(), Storage :: any()) -> Storage :: storage_list().
get_storage(Node, Storage) ->
    Id = rpc:call(Node, storage, id, [Storage]),
    Name = rpc:call(Node, storage, name, [Storage]),
    [Helper | _] = rpc:call(Node, storage, helpers, [Storage]),
    Type = helper_name_to_type(rpc:call(Node, helpers, get_name, [Helper])),
    Args = translate_helper_args(rpc:call(Node, helpers, get_args, [Helper]), []),
    [{Name, [{id, Id}, {type, Type} | Args]}].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec helper_name_to_type(Name :: binary()) -> Type :: binary().
helper_name_to_type(<<"DirectIO">>) -> <<"POSIX">>;
helper_name_to_type(<<"AmazonS3">>) -> <<"S3">>;
helper_name_to_type(<<"Ceph">>) -> <<"CEPH">>;
helper_name_to_type(_) -> <<>>.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec translate_helper_args(Args :: storage_params_map(), Acc :: storage_params_list()) ->
    Acc :: storage_params_list().
translate_helper_args(#{<<"root_path">> := MountPoint} = Args, Acc) ->
    NewArgs = maps:remove(<<"root_path">>, Args),
    translate_helper_args(NewArgs, [{mountPoint, MountPoint} | Acc]);

translate_helper_args(#{<<"host_name">> := Hostname, <<"scheme">> := Scheme} = Args, Acc) ->
    NewArgs = maps:remove(<<"host_name">>, maps:remove(<<"scheme">>, Args)),
    S3Hostname = <<Scheme/binary, ":", Hostname/binary>>,
    translate_helper_args(NewArgs, [{s3Hostname, S3Hostname} | Acc]);

translate_helper_args(#{<<"iam_host">> := Hostname, <<"iam_request_scheme">> := Scheme} = Args, Acc) ->
    NewArgs = maps:remove(<<"iam_host">>, maps:remove(<<"iam_request_scheme">>, Args)),
    IamHostname = <<Scheme/binary, ":", Hostname/binary>>,
    translate_helper_args(NewArgs, [{iamHostname, IamHostname}, Acc]);

translate_helper_args(#{<<"bucket_name">> := Name} = Args, Acc) ->
    NewArgs = maps:remove(<<"bucket_name">>, Args),
    translate_helper_args(NewArgs, [{bucketName, Name} | Acc]);

translate_helper_args(#{<<"mon_host">> := Hostname} = Args, Acc) ->
    NewArgs = maps:remove(<<"mon_host">>, Args),
    translate_helper_args(NewArgs, [{monitorHostname, Hostname} | Acc]);

translate_helper_args(#{<<"cluster_name">> := Name} = Args, Acc) ->
    NewArgs = maps:remove(<<"cluster_name">>, Args),
    translate_helper_args(NewArgs, [{clusterName, Name} | Acc]);

translate_helper_args(#{<<"pool_name">> := Name} = Args, Acc) ->
    NewArgs = maps:remove(<<"pool_name">>, Args),
    translate_helper_args(NewArgs, [{poolName, Name} | Acc]);

translate_helper_args(_Args, Acc) ->
    Acc.