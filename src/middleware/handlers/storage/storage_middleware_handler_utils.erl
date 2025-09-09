%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Shared helpers for storage middleware handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(storage_middleware_handler_utils).
-author("Bartosz Walkowicz").

-include("middleware/middleware.hrl").

-export([
    supported_op_interfaces/0,
    common_availability/0,
    preauthorize_member/1,

    validate_storage_common_args/2,
    validate_storage_custom_args/2,

    get_storage/1,

    convert_uid_to_integer/1
]).


-define(DEFAULT_STORAGE_TIMEOUT, 5000).
-define(MIN_STORAGE_TIMEOUT, 1).

-define(DEFAULT_S3_SIGNATURE_VERSION, 4).
-define(ALLOWED_S3_SIGNATURE_VERSIONS, [4]).

-define(DEFAULT_S3_BLOCK_SIZE, 10485760).
-define(MIN_S3_BLOCK_SIZE, 0).

-define(DEFAULT_S3_MAX_CANONICAL_OBJECT_SIZE, 67108864).
-define(MIN_S3_MAX_CANONICAL_OBJECT_SIZE, 1).

-define(STORAGE_KEY(StorageName, Key), str_utils:join_as_binaries([StorageName, Key], <<".">>)).


%%%===================================================================
%%% API
%%%===================================================================


-spec supported_op_interfaces() -> {true, [rest]} | false.
supported_op_interfaces() ->
    middleware_handler_utils:if_op_then( [rest]).


-spec common_availability() -> {true, [middleware_handler:availability_level()]}.
common_availability() ->
    {true, [?SERVICE_OPW, all_healthy_ignoring_ones3]}.


-spec preauthorize_member(middleware_handler:state()) -> boolean().
preauthorize_member(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate_storage_common_args(binary(), map()) -> ok.
validate_storage_common_args(StorageName, StorageArgs) ->
    Timeout =  maps:get(timeout, StorageArgs, ?DEFAULT_STORAGE_TIMEOUT),
    case Timeout < ?MIN_STORAGE_TIMEOUT of
        true ->
            throw(?ERR_BAD_VALUE_TOO_LOW(?err_ctx(), ?STORAGE_KEY(StorageName, timeout), ?MIN_STORAGE_TIMEOUT));
        false ->
            ok
    end.


% used during create or update, so the data may include arbitrary parameters
-spec validate_storage_custom_args(binary(), map()) -> ok | no_return().
validate_storage_custom_args(StorageName, Data = #{type := <<"s3">>}) ->
    try
        case maps:find(hostname, Data) of
            error -> ok;
            {ok, Hostname} -> url_utils:infer_components(Hostname)
        end
    catch
        _:_ -> throw(?ERR_BAD_DATA(?err_ctx(), ?STORAGE_KEY(StorageName, <<"hostname">>), undefined))
    end,

    SignatureVersion = maps:get(signatureVersion, Data, ?DEFAULT_S3_SIGNATURE_VERSION),
    case lists:member(SignatureVersion, ?ALLOWED_S3_SIGNATURE_VERSIONS) of
        true -> ok;
        false -> throw(?ERR_BAD_VALUE_LIST_NOT_ALLOWED(?err_ctx(), ?STORAGE_KEY(StorageName, signatureVersion), ?ALLOWED_S3_SIGNATURE_VERSIONS))
    end,

    BlockSize = maps:get(blockSize, Data, ?DEFAULT_S3_BLOCK_SIZE),
    case BlockSize < ?MIN_S3_BLOCK_SIZE of
        true -> throw(?ERR_BAD_VALUE_TOO_LOW(?err_ctx(), ?STORAGE_KEY(StorageName, blockSize), ?MIN_S3_BLOCK_SIZE));
        false -> ok
    end,

    MaxCanonicalObjectSize = maps:get(maximumCanonicalObjectSize, Data, ?DEFAULT_S3_MAX_CANONICAL_OBJECT_SIZE),
    case MaxCanonicalObjectSize < ?MIN_S3_MAX_CANONICAL_OBJECT_SIZE of
        true -> throw(?ERR_BAD_VALUE_TOO_LOW(?err_ctx(), ?STORAGE_KEY(StorageName, maximumCanonicalObjectSize), ?MIN_S3_MAX_CANONICAL_OBJECT_SIZE));
        false -> ok
    end;

validate_storage_custom_args(_StorageName, _Data) ->
    ok.


-spec get_storage(op_worker_storage:id()) ->
    {ok, op_worker_storage:storage_details()} | errors:error().
get_storage(StorageId) ->
    middleware_handler_utils:service_call(
        ?SERVICE_OPW, get_storages, #{id => StorageId}
    ).


-spec convert_uid_to_integer(binary()) -> integer() | no_return().
convert_uid_to_integer(Value) ->
    try
        binary_to_integer(Value)
    catch error:badarg ->
        throw(?ERR_BAD_VALUE_INTEGER(?err_ctx(), uid))
    end.
