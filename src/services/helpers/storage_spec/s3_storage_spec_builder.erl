%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for building S3 storage specifications
%%% from REST API maps and converting S3 storage descriptions back to maps.
%%% It translates between the REST API format (camelCase keys, binaries)
%%% and ctool storage records (snake_case atoms, records).
%%% @end
%%%--------------------------------------------------------------------
-module(s3_storage_spec_builder).
-author("Bartosz Walkowicz").

-include_lib("ctool/include/storage/s3.hrl").

%% API
-export([
    build_credentials/1,
    build_credentials_diff/1,
    build_configuration/1,
    build_configuration_diff/1,
    credentials_to_map/1,
    configuration_to_map/1
]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec build_credentials(map()) -> onedata_storage:s3_credentials().
build_credentials(Params) ->
    #s3_credentials{
        access_key = maps:get(accessKey, Params, <<"">>),
        secret_key = maps:get(secretKey, Params, <<"">>)
    }.


-spec build_credentials_diff(map()) -> onedata_storage:s3_credentials_diff().
build_credentials_diff(Params) ->
    #s3_credentials_diff{
        access_key = maps:get(accessKey, Params, undefined),
        secret_key = maps:get(secretKey, Params, undefined)
    }.


-spec build_configuration(map()) -> onedata_storage:s3_configuration().
build_configuration(Params) ->
    {Scheme, Hostname} = parse_s3_hostname(maps:get(hostname, Params)),
    #s3_configuration{
        scheme = Scheme,
        hostname = Hostname,
        bucket_name = maps:get(bucketName, Params),
        signature_version = maps:get(signatureVersion, Params, undefined),
        verify_server_certificate = maps:get(verifyServerCertificate, Params, undefined),
        region = maps:get(region, Params, undefined),
        block_size = maps:get(blockSize, Params, undefined),
        maximum_canonical_object_size = maps:get(maximumCanonicalObjectSize, Params, undefined),
        file_mode = maps:get(fileMode, Params, undefined),
        dir_mode = maps:get(dirMode, Params, undefined),
        storage_path_type = storage_spec_builder_utils:binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"flat">>)
        )
    }.


-spec build_configuration_diff(map()) -> onedata_storage:s3_configuration_diff().
build_configuration_diff(Params) ->
    {Scheme, Hostname} = case maps:get(hostname, Params, undefined) of
        undefined -> {undefined, undefined};
        FullUrl -> parse_s3_hostname(FullUrl)
    end,
    #s3_configuration_diff{
        scheme = Scheme,
        hostname = Hostname,
        bucket_name = maps:get(bucketName, Params, undefined),
        signature_version = maps:get(signatureVersion, Params, undefined),
        verify_server_certificate = maps:get(verifyServerCertificate, Params, undefined),
        region = maps:get(region, Params, undefined),
        maximum_canonical_object_size = maps:get(maximumCanonicalObjectSize, Params, undefined),
        file_mode = maps:get(fileMode, Params, undefined),
        dir_mode = maps:get(dirMode, Params, undefined)
    }.


-spec credentials_to_map(onedata_storage:s3_credentials()) -> map().
credentials_to_map(#s3_credentials{access_key = AccessKey, secret_key = SecretKey}) ->
    #{accessKey => AccessKey, secretKey => SecretKey}.


-spec configuration_to_map(onedata_storage:s3_configuration()) -> map().
configuration_to_map(#s3_configuration{
    scheme = Scheme,
    hostname = Hostname,
    bucket_name = BucketName,
    signature_version = SignatureVersion,
    verify_server_certificate = VerifyServerCert,
    region = Region,
    block_size = BlockSize,
    maximum_canonical_object_size = MaxCanonicalObjectSize,
    file_mode = FileMode,
    dir_mode = DirMode,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        hostname => join_s3_hostname(Scheme, Hostname),
        bucketName => BucketName,
        storagePathType => storage_spec_builder_utils:storage_path_type_to_binary(StoragePathType)
    },
    Base1 = maps_utils:put_if_defined(Base, signatureVersion, SignatureVersion),
    Base2 = maps_utils:put_if_defined(Base1, verifyServerCertificate, VerifyServerCert),
    Base3 = maps_utils:put_if_defined(Base2, region, Region),
    Base4 = maps_utils:put_if_defined(Base3, blockSize, BlockSize),
    Base5 = maps_utils:put_if_defined(Base4, maximumCanonicalObjectSize, MaxCanonicalObjectSize),
    Base6 = maps_utils:put_if_defined(Base5, fileMode, FileMode),
    maps_utils:put_if_defined(Base6, dirMode, DirMode).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc Parses S3 hostname URL into scheme and hostname parts.
%% S3 REST API sends full URL like "https://s3.amazonaws.com:443"
%% but ctool records need scheme and hostname separately.
%% @end
%%--------------------------------------------------------------------
-spec parse_s3_hostname(binary()) -> {Scheme :: binary(), Hostname :: binary()}.
parse_s3_hostname(FullUrl) ->
    #{scheme := Scheme, host := Host, port := Port, path := Path} = url_utils:infer_components(FullUrl),

    Hostname = str_utils:format_bin("~ts:~B~ts", [Host, Port, Path]),
    case Scheme of
        https -> {<<"https">>, Hostname};
        _ -> {<<"http">>, Hostname}
    end.


%% @private
-spec join_s3_hostname(Scheme :: binary(), Hostname :: binary()) -> binary().
join_s3_hostname(Scheme, Hostname) ->
    <<Scheme/binary, "://", Hostname/binary>>.
