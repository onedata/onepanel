%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for building storage specifications
%%% from REST API maps and converting storage descriptions back to maps.
%%% It translates between the REST API format (camelCase keys, binaries)
%%% and ctool storage records (snake_case atoms, records).
%%% @end
%%%--------------------------------------------------------------------
-module(storage_spec_builder).
-author("Bartosz Walkowicz").

-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/storage/common.hrl").
-include_lib("ctool/include/storage/posix.hrl").
-include_lib("ctool/include/storage/s3.hrl").
-include_lib("ctool/include/storage/ceph.hrl").
-include_lib("ctool/include/storage/cephrados.hrl").
-include_lib("ctool/include/storage/swift.hrl").
-include_lib("ctool/include/storage/glusterfs.hrl").
-include_lib("ctool/include/storage/nulldevice.hrl").
-include_lib("ctool/include/storage/webdav.hrl").
-include_lib("ctool/include/storage/xrootd.hrl").
-include_lib("ctool/include/storage/nfs.hrl").
-include_lib("ctool/include/storage/http.hrl").

%% API
-export([
    build_create_spec/2,
    build_update_spec/2,
    description_to_map/1
]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec build_create_spec(binary(), map()) -> onedata_storage:create_spec().
build_create_spec(Name, Params) ->
    Type = maps:get(type, Params),

    #storage_create_spec{
        type = Type,
        name = Name,
        timeout = maps:get(timeout, Params, undefined),
        readonly = maps:get(readonly, Params, false),
        imported = maps:get(importedStorage, Params, false),
        archive = maps:get(archiveStorage, Params, false),
        luma = build_luma_spec(Params),
        qos_parameters = extract_qos_parameters(Params),
        credentials = build_credentials(Type, Params),
        configuration = build_configuration(Type, Params)
    }.


-spec build_update_spec(onedata_storage:type(), map()) -> onedata_storage:update_spec().
build_update_spec(Type, Params) ->
    #storage_update_spec{
        type = Type,
        name = maps:get(name, Params, undefined),
        timeout = maps:get(timeout, Params, undefined),
        readonly = maps:get(readonly, Params, undefined),
        imported = maps:get(importedStorage, Params, undefined),
        archive = maps:get(archiveStorage, Params, undefined),
        luma = build_luma_spec_diff(Params),
        qos_parameters = maps:get(qosParameters, Params, undefined),
        credentials = build_credentials_diff(Type, Params),
        configuration = build_configuration_diff(Type, Params)
    }.


-spec description_to_map(onedata_storage:description()) -> map().
description_to_map(#storage_description{
    id = Id,
    name = Name,
    type = Type,
    timeout = Timeout,
    readonly = Readonly,
    imported = Imported,
    archive = Archive,
    luma = Luma,
    qos_parameters = QosParameters,
    credentials = Credentials,
    configuration = Configuration
}) ->
    BaseMap = maps_utils:put_if_defined(#{
        id => Id,
        name => Name,
        type => Type,
        readonly => Readonly,
        importedStorage => Imported,
        archiveStorage => Archive,
        qosParameters => QosParameters
    }, timeout, Timeout),
    MapWithLuma = case Luma#luma_spec.feed of
        external ->
            BaseMap#{
                lumaFeed => <<"external">>,
                lumaFeedUrl => Luma#luma_spec.url,
                lumaFeedApiKey => Luma#luma_spec.api_key
            };
        _ ->
            BaseMap#{lumaFeed => luma_feed_to_binary(Luma#luma_spec.feed)}
    end,
    MapWithCredentials = maps:merge(MapWithLuma, credentials_to_map(Type, Credentials)),
    MapWithConfiguration = maps:merge(MapWithCredentials, configuration_to_map(Type, Configuration)),

    maps_utils:undefined_to_null(MapWithConfiguration).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec build_luma_spec(map()) -> onedata_storage:luma_spec() | undefined.
build_luma_spec(Params) ->
    #luma_spec{
        feed = binary_to_luma_feed(maps:get(lumaFeed, Params)),
        url = maps:get(lumaFeedUrl, Params, undefined),
        api_key = maps:get(lumaFeedApiKey, Params, undefined)
    }.


%% @private
-spec build_luma_spec_diff(map()) -> onedata_storage:luma_spec() | undefined.
build_luma_spec_diff(Params) ->
    #luma_spec{
        feed = case maps:get(lumaFeed, Params, undefined) of
            undefined -> undefined;
            Feed -> binary_to_luma_feed(Feed)
        end,
        url = maps:get(lumaFeedUrl, Params, undefined),
        api_key = maps:get(lumaFeedApiKey, Params, undefined)
    }.


%% @private
-spec binary_to_luma_feed(binary()) -> auto | local | external.
binary_to_luma_feed(<<"auto">>) -> auto;
binary_to_luma_feed(<<"local">>) -> local;
binary_to_luma_feed(<<"external">>) -> external.


%% @private
-spec luma_feed_to_binary(auto | local | external) -> binary().
luma_feed_to_binary(auto) -> <<"auto">>;
luma_feed_to_binary(local) -> <<"local">>;
luma_feed_to_binary(external) -> <<"external">>.


%% @private
-spec extract_qos_parameters(map()) -> onedata_storage:qos_parameters().
extract_qos_parameters(Params) ->
    normalize_numeric_qos_parameters(maps:get(qosParameters, Params, #{})).


%% @private
-spec normalize_numeric_qos_parameters(#{binary() => binary()}) ->
    #{binary() => binary() | number()}.
normalize_numeric_qos_parameters(QosParameters) ->
    maps:map(fun(_Key, Value) ->
        try
            %% JSON decoding parses any number (integer or float) expressed as string
            json_utils:decode(Value)
        catch
            _:invalid_json -> Value
        end
    end, QosParameters).


%% @private
-spec build_credentials(onedata_storage:type(), map()) -> onedata_storage:credentials().
build_credentials(<<"ceph">>, Params) -> build_ceph_credentials(Params);
build_credentials(<<"cephrados">>, Params) -> build_cephrados_credentials(Params);
build_credentials(<<"glusterfs">>, Params) -> build_glusterfs_credentials(Params);
build_credentials(<<"http">>, Params) -> build_http_credentials(Params);
build_credentials(<<"nfs">>, Params) -> build_nfs_credentials(Params);
build_credentials(<<"nulldevice">>, Params) -> build_nulldevice_credentials(Params);
build_credentials(<<"posix">>, Params) -> build_posix_credentials(Params);
build_credentials(<<"s3">>, Params) -> build_s3_credentials(Params);
build_credentials(<<"swift">>, Params) -> build_swift_credentials(Params);
build_credentials(<<"webdav">>, Params) -> build_webdav_credentials(Params);
build_credentials(<<"xrootd">>, Params) -> build_xrootd_credentials(Params).


%% @private
-spec build_credentials_diff(onedata_storage:type(), map()) ->
    onedata_storage:credentials_diff().
build_credentials_diff(<<"ceph">>, Params) -> build_ceph_credentials_diff(Params);
build_credentials_diff(<<"cephrados">>, Params) -> build_cephrados_credentials_diff(Params);
build_credentials_diff(<<"glusterfs">>, Params) -> build_glusterfs_credentials_diff(Params);
build_credentials_diff(<<"http">>, Params) -> build_http_credentials_diff(Params);
build_credentials_diff(<<"nfs">>, Params) -> build_nfs_credentials_diff(Params);
build_credentials_diff(<<"nulldevice">>, Params) -> build_nulldevice_credentials_diff(Params);
build_credentials_diff(<<"posix">>, Params) -> build_posix_credentials_diff(Params);
build_credentials_diff(<<"s3">>, Params) -> build_s3_credentials_diff(Params);
build_credentials_diff(<<"swift">>, Params) -> build_swift_credentials_diff(Params);
build_credentials_diff(<<"webdav">>, Params) -> build_webdav_credentials_diff(Params);
build_credentials_diff(<<"xrootd">>, Params) -> build_xrootd_credentials_diff(Params).


%% @private
-spec build_ceph_credentials(map()) -> #ceph_credentials{}.
build_ceph_credentials(Params) ->
    #ceph_credentials{
        username = maps:get(username, Params),
        key = maps:get(key, Params)
    }.


%% @private
-spec build_ceph_credentials_diff(map()) -> #ceph_credentials_diff{}.
build_ceph_credentials_diff(Params) ->
    #ceph_credentials_diff{
        username = maps:get(username, Params, undefined),
        key = maps:get(key, Params, undefined)
    }.


%% @private
-spec build_cephrados_credentials(map()) -> #cephrados_credentials{}.
build_cephrados_credentials(Params) ->
    #cephrados_credentials{
        username = maps:get(username, Params),
        key = maps:get(key, Params)
    }.


%% @private
-spec build_cephrados_credentials_diff(map()) -> #cephrados_credentials_diff{}.
build_cephrados_credentials_diff(Params) ->
    #cephrados_credentials_diff{
        username = maps:get(username, Params, undefined),
        key = maps:get(key, Params, undefined)
    }.


%% @private
-spec build_glusterfs_credentials(map()) -> #glusterfs_credentials{}.
build_glusterfs_credentials(Params) ->
    #glusterfs_credentials{
        uid = maps:get(uid, Params, 0),
        gid = maps:get(gid, Params, 0)
    }.


%% @private
-spec build_glusterfs_credentials_diff(map()) -> #glusterfs_credentials_diff{}.
build_glusterfs_credentials_diff(Params) ->
    #glusterfs_credentials_diff{
        uid = maps:get(uid, Params, undefined),
        gid = maps:get(gid, Params, undefined)
    }.


%% @private
-spec build_http_credentials(map()) -> #http_credentials{}.
build_http_credentials(Params) ->
    #http_credentials{
        credentials_type = binary_to_credentials_type(maps:get(credentialsType, Params, <<"none">>)),
        credentials = maps:get(credentials, Params, undefined),
        oauth2_idp = maps:get(oauth2IdP, Params, undefined),
        onedata_access_token = maps:get(onedataAccessToken, Params, undefined)
    }.


%% @private
-spec build_http_credentials_diff(map()) -> #http_credentials_diff{}.
build_http_credentials_diff(Params) ->
    #http_credentials_diff{
        credentials_type = case maps:get(credentialsType, Params, undefined) of
            undefined -> undefined;
            CredType -> binary_to_credentials_type(CredType)
        end,
        credentials = maps:get(credentials, Params, undefined)
    }.


%% @private
-spec build_nfs_credentials(map()) -> #nfs_credentials{}.
build_nfs_credentials(Params) ->
    #nfs_credentials{
        uid = maps:get(uid, Params, 0),
        gid = maps:get(gid, Params, 0)
    }.


%% @private
-spec build_nfs_credentials_diff(map()) -> #nfs_credentials_diff{}.
build_nfs_credentials_diff(Params) ->
    #nfs_credentials_diff{
        uid = maps:get(uid, Params, undefined),
        gid = maps:get(gid, Params, undefined)
    }.


%% @private
-spec build_nulldevice_credentials(map()) -> #nulldevice_credentials{}.
build_nulldevice_credentials(Params) ->
    #nulldevice_credentials{
        uid = maps:get(uid, Params, 0),
        gid = maps:get(gid, Params, 0)
    }.


%% @private
-spec build_nulldevice_credentials_diff(map()) -> #nulldevice_credentials_diff{}.
build_nulldevice_credentials_diff(Params) ->
    #nulldevice_credentials_diff{
        uid = maps:get(uid, Params, undefined),
        gid = maps:get(gid, Params, undefined)
    }.


%% @private
-spec build_posix_credentials(map()) -> #posix_credentials{}.
build_posix_credentials(Params) ->
    #posix_credentials{
        uid = maps:get(rootUid, Params, 0),
        gid = maps:get(rootGid, Params, 0)
    }.


%% @private
-spec build_posix_credentials_diff(map()) -> #posix_credentials_diff{}.
build_posix_credentials_diff(Params) ->
    #posix_credentials_diff{
        uid = maps:get(rootUid, Params, undefined),
        gid = maps:get(rootGid, Params, undefined)
    }.


%% @private
-spec build_s3_credentials(map()) -> #s3_credentials{}.
build_s3_credentials(Params) ->
    #s3_credentials{
        access_key = maps:get(accessKey, Params, <<"">>),
        secret_key = maps:get(secretKey, Params, <<"">>)
    }.


%% @private
-spec build_s3_credentials_diff(map()) -> #s3_credentials_diff{}.
build_s3_credentials_diff(Params) ->
    #s3_credentials_diff{
        access_key = maps:get(accessKey, Params, undefined),
        secret_key = maps:get(secretKey, Params, undefined)
    }.


%% @private
-spec build_swift_credentials(map()) -> #swift_credentials{}.
build_swift_credentials(Params) ->
    #swift_credentials{
        username = maps:get(username, Params),
        password = maps:get(password, Params),
        project_name = maps:get(projectName, Params),
        user_domain_name = maps:get(userDomainName, Params, undefined),
        project_domain_name = maps:get(projectDomainName, Params, undefined)
    }.


%% @private
-spec build_swift_credentials_diff(map()) -> #swift_credentials_diff{}.
build_swift_credentials_diff(Params) ->
    #swift_credentials_diff{
        username = maps:get(username, Params, undefined),
        password = maps:get(password, Params, undefined),
        project_name = maps:get(projectName, Params, undefined),
        user_domain_name = maps:get(userDomainName, Params, undefined),
        project_domain_name = maps:get(projectDomainName, Params, undefined)
    }.


%% @private
-spec build_webdav_credentials(map()) -> #webdav_credentials{}.
build_webdav_credentials(Params) ->
    #webdav_credentials{
        credentials_type = binary_to_credentials_type(
            maps:get(credentialsType, Params, <<"none">>)),
        credentials = maps:get(credentials, Params, undefined),
        oauth2_idp = maps:get(oauth2IdP, Params, undefined),
        onedata_access_token = maps:get(onedataAccessToken, Params, undefined)
    }.


%% @private
-spec build_webdav_credentials_diff(map()) -> #webdav_credentials_diff{}.
build_webdav_credentials_diff(Params) ->
    #webdav_credentials_diff{
        credentials_type = case maps:get(credentialsType, Params, undefined) of
            undefined -> undefined;
            CredType -> binary_to_credentials_type(CredType)
        end,
        credentials = maps:get(credentials, Params, undefined)
    }.

%% @private
-spec build_xrootd_credentials(map()) -> #xrootd_credentials{}.
build_xrootd_credentials(Params) ->
    #xrootd_credentials{
        credentials_type = binary_to_credentials_type(
            maps:get(credentialsType, Params, <<"none">>)),
        credentials = maps:get(credentials, Params, undefined)
    }.


%% @private
-spec build_xrootd_credentials_diff(map()) -> #xrootd_credentials_diff{}.
build_xrootd_credentials_diff(Params) ->
    #xrootd_credentials_diff{
        credentials_type = case maps:get(credentialsType, Params, undefined) of
            undefined -> undefined;
            CredType -> binary_to_credentials_type(CredType)
        end,
        credentials = maps:get(credentials, Params, undefined)
    }.


%% @private
-spec build_configuration(onedata_storage:type(), map()) -> onedata_storage:configuration().
build_configuration(<<"ceph">>, Params) -> build_ceph_configuration(Params);
build_configuration(<<"cephrados">>, Params) -> build_cephrados_configuration(Params);
build_configuration(<<"glusterfs">>, Params) -> build_glusterfs_configuration(Params);
build_configuration(<<"http">>, Params) -> build_http_configuration(Params);
build_configuration(<<"nfs">>, Params) -> build_nfs_configuration(Params);
build_configuration(<<"nulldevice">>, Params) -> build_nulldevice_configuration(Params);
build_configuration(<<"posix">>, Params) -> build_posix_configuration(Params);
build_configuration(<<"s3">>, Params) -> build_s3_configuration(Params);
build_configuration(<<"swift">>, Params) -> build_swift_configuration(Params);
build_configuration(<<"webdav">>, Params) -> build_webdav_configuration(Params);
build_configuration(<<"xrootd">>, Params) -> build_xrootd_configuration(Params).


%% @private
-spec build_configuration_diff(onedata_storage:type(), map()) ->
    onedata_storage:configuration_diff().
build_configuration_diff(<<"ceph">>, Params) -> build_ceph_configuration_diff(Params);
build_configuration_diff(<<"cephrados">>, Params) -> build_cephrados_configuration_diff(Params);
build_configuration_diff(<<"glusterfs">>, Params) -> build_glusterfs_configuration_diff(Params);
build_configuration_diff(<<"http">>, Params) -> build_http_configuration_diff(Params);
build_configuration_diff(<<"nfs">>, Params) -> build_nfs_configuration_diff(Params);
build_configuration_diff(<<"nulldevice">>, Params) -> build_nulldevice_configuration_diff(Params);
build_configuration_diff(<<"posix">>, Params) -> build_posix_configuration_diff(Params);
build_configuration_diff(<<"s3">>, Params) -> build_s3_configuration_diff(Params);
build_configuration_diff(<<"swift">>, Params) -> build_swift_configuration_diff(Params);
build_configuration_diff(<<"webdav">>, Params) -> build_webdav_configuration_diff(Params);
build_configuration_diff(<<"xrootd">>, Params) -> build_xrootd_configuration_diff(Params).


%% @private
-spec build_ceph_configuration(map()) -> #ceph_configuration{}.
build_ceph_configuration(Params) ->
    #ceph_configuration{
        monitor_hostname = maps:get(monitorHostname, Params),
        cluster_name = maps:get(clusterName, Params),
        pool_name = maps:get(poolName, Params),
        storage_path_type = binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"flat">>)
        )
    }.


%% @private
-spec build_ceph_configuration_diff(map()) -> #ceph_configuration_diff{}.
build_ceph_configuration_diff(Params) ->
    #ceph_configuration_diff{
        monitor_hostname = maps:get(monitorHostname, Params, undefined),
        cluster_name = maps:get(clusterName, Params, undefined),
        pool_name = maps:get(poolName, Params, undefined)
    }.


%% @private
-spec build_cephrados_configuration(map()) -> #cephrados_configuration{}.
build_cephrados_configuration(Params) ->
    #cephrados_configuration{
        monitor_hostname = maps:get(monitorHostname, Params),
        cluster_name = maps:get(clusterName, Params),
        pool_name = maps:get(poolName, Params),
        block_size = maps:get(blockSize, Params, undefined),
        storage_path_type = binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"flat">>)
        )
    }.

%% @private
-spec build_cephrados_configuration_diff(map()) -> #cephrados_configuration_diff{}.
build_cephrados_configuration_diff(Params) ->
    #cephrados_configuration_diff{
        monitor_hostname = maps:get(monitorHostname, Params, undefined),
        cluster_name = maps:get(clusterName, Params, undefined),
        pool_name = maps:get(poolName, Params, undefined)
    }.


%% @private
-spec build_glusterfs_configuration(map()) -> #glusterfs_configuration{}.
build_glusterfs_configuration(Params) ->
    #glusterfs_configuration{
        volume = maps:get(volume, Params),
        hostname = maps:get(hostname, Params),
        port = maps:get(port, Params, undefined),
        transport = case maps:get(transport, Params, undefined) of
            undefined -> undefined;
            Transport -> binary_to_transport(Transport)
        end,
        mount_point = maps:get(mountPoint, Params, undefined),
        xlator_options = maps:get(xlatorOptions, Params, undefined),
        storage_path_type = binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


%% @private
-spec build_glusterfs_configuration_diff(map()) -> #glusterfs_configuration_diff{}.
build_glusterfs_configuration_diff(Params) ->
    #glusterfs_configuration_diff{
        volume = maps:get(volume, Params, undefined),
        hostname = maps:get(hostname, Params, undefined),
        port = maps:get(port, Params, undefined),
        transport = case maps:get(transport, Params, undefined) of
            undefined -> undefined;
            Transport -> binary_to_transport(Transport)
        end,
        mount_point = maps:get(mountPoint, Params, undefined),
        xlator_options = maps:get(xlatorOptions, Params, undefined)
    }.


%% @private
-spec build_http_configuration(map()) -> #http_configuration{}.
build_http_configuration(Params) ->
    #http_configuration{
        endpoint = maps:get(endpoint, Params),
        verify_server_certificate = maps:get(verifyServerCertificate, Params, undefined),
        authorization_header = maps:get(authorizationHeader, Params, undefined),
        connection_pool_size = maps:get(connectionPoolSize, Params, undefined),
        max_requests_per_session = maps:get(maxRequestsPerSession, Params, undefined),
        file_mode = maps:get(fileMode, Params, undefined),
        storage_path_type = binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


%% @private
-spec build_http_configuration_diff(map()) -> #http_configuration_diff{}.
build_http_configuration_diff(Params) ->
    #http_configuration_diff{
        endpoint = maps:get(endpoint, Params, undefined),
        verify_server_certificate = maps:get(verifyServerCertificate, Params, undefined),
        authorization_header = maps:get(authorizationHeader, Params, undefined),
        connection_pool_size = maps:get(connectionPoolSize, Params, undefined),
        max_requests_per_session = maps:get(maxRequestsPerSession, Params, undefined),
        file_mode = maps:get(fileMode, Params, undefined)
    }.


%% @private
-spec build_nfs_configuration(map()) -> #nfs_configuration{}.
build_nfs_configuration(Params) ->
    #nfs_configuration{
        version = maps:get(version, Params, undefined),
        host = maps:get(host, Params),
        volume = maps:get(volume, Params),
        read_ahead = maps:get(readAhead, Params, undefined),
        dir_cache = maps:get(dirCache, Params, undefined),
        auto_reconnect = maps:get(autoReconnect, Params, undefined),
        connection_pool_size = maps:get(connectionPoolSize, Params, undefined),
        storage_path_type = binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


%% @private
-spec build_nfs_configuration_diff(map()) -> #nfs_configuration_diff{}.
build_nfs_configuration_diff(Params) ->
    #nfs_configuration_diff{
        version = maps:get(version, Params, undefined),
        host = maps:get(host, Params, undefined),
        volume = maps:get(volume, Params, undefined),
        read_ahead = maps:get(readAhead, Params, undefined),
        dir_cache = maps:get(dirCache, Params, undefined),
        auto_reconnect = maps:get(autoReconnect, Params, undefined),
        connection_pool_size = maps:get(connectionPoolSize, Params, undefined)
    }.


%% @private
-spec build_nulldevice_configuration(map()) -> #nulldevice_configuration{}.
build_nulldevice_configuration(Params) ->
    #nulldevice_configuration{
        latency_min = maps:get(latencyMin, Params, undefined),
        latency_max = maps:get(latencyMax, Params, undefined),
        timeout_probability = maps:get(timeoutProbability, Params, undefined),
        filter = maps:get(filter, Params, undefined),
        simulated_filesystem_parameters = maps:get(simulatedFilesystemParameters, Params, undefined),
        simulated_filesystem_grow_speed = maps:get(simulatedFilesystemGrowSpeed, Params, undefined),
        enable_data_verification = maps:get(enableDataVerification, Params, undefined),
        storage_path_type = binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


%% @private
-spec build_nulldevice_configuration_diff(map()) -> #nulldevice_configuration_diff{}.
build_nulldevice_configuration_diff(Params) ->
    #nulldevice_configuration_diff{
        latency_min = maps:get(latencyMin, Params, undefined),
        latency_max = maps:get(latencyMax, Params, undefined),
        timeout_probability = maps:get(timeoutProbability, Params, undefined),
        filter = maps:get(filter, Params, undefined),
        simulated_filesystem_parameters = maps:get(simulatedFilesystemParameters, Params, undefined),
        simulated_filesystem_grow_speed = maps:get(simulatedFilesystemGrowSpeed, Params, undefined),
        enable_data_verification = maps:get(enableDataVerification, Params, undefined)
    }.


%% @private
-spec build_posix_configuration(map()) -> #posix_configuration{}.
build_posix_configuration(Params) ->
    #posix_configuration{
        mount_point = maps:get(mountPoint, Params),
        storage_path_type = binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


%% @private
-spec build_posix_configuration_diff(map()) -> #posix_configuration_diff{}.
build_posix_configuration_diff(Params) ->
    #posix_configuration_diff{
        mount_point = maps:get(mountPoint, Params, undefined)
    }.


%% @private
-spec build_s3_configuration(map()) -> #s3_configuration{}.
build_s3_configuration(Params) ->
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
        storage_path_type = binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"flat">>)
        )
    }.


%% @private
-spec build_s3_configuration_diff(map()) -> #s3_configuration_diff{}.
build_s3_configuration_diff(Params) ->
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


%% @private
-spec build_swift_configuration(map()) -> #swift_configuration{}.
build_swift_configuration(Params) ->
    #swift_configuration{
        auth_url = maps:get(authUrl, Params),
        container_name = maps:get(containerName, Params),
        block_size = maps:get(blockSize, Params, undefined),
        storage_path_type = binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"flat">>)
        )
    }.


%% @private
-spec build_swift_configuration_diff(map()) -> #swift_configuration_diff{}.
build_swift_configuration_diff(Params) ->
    #swift_configuration_diff{
        auth_url = maps:get(authUrl, Params, undefined),
        container_name = maps:get(containerName, Params, undefined)
    }.


%% @private
-spec build_webdav_configuration(map()) -> #webdav_configuration{}.
build_webdav_configuration(Params) ->
    #webdav_configuration{
        endpoint = maps:get(endpoint, Params),
        verify_server_certificate = maps:get(verifyServerCertificate, Params, undefined),
        authorization_header = maps:get(authorizationHeader, Params, undefined),
        range_write_support = case maps:get(rangeWriteSupport, Params, undefined) of
            undefined -> undefined;
            RangeWriteSupport -> binary_to_range_write_support(RangeWriteSupport)
        end,
        connection_pool_size = maps:get(connectionPoolSize, Params, undefined),
        maximum_upload_size = maps:get(maximumUploadSize, Params, undefined),
        file_mode = maps:get(fileMode, Params, undefined),
        dir_mode = maps:get(dirMode, Params, undefined),
        storage_path_type = binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


%% @private
-spec build_webdav_configuration_diff(map()) -> #webdav_configuration_diff{}.
build_webdav_configuration_diff(Params) ->
    #webdav_configuration_diff{
        endpoint = maps:get(endpoint, Params, undefined),
        verify_server_certificate = maps:get(verifyServerCertificate, Params, undefined),
        authorization_header = maps:get(authorizationHeader, Params, undefined),
        range_write_support = case maps:get(rangeWriteSupport, Params, undefined) of
            undefined -> undefined;
            RangeWriteSupport -> binary_to_range_write_support(RangeWriteSupport)
        end,
        connection_pool_size = maps:get(connectionPoolSize, Params, undefined),
        maximum_upload_size = maps:get(maximumUploadSize, Params, undefined),
        file_mode = maps:get(fileMode, Params, undefined),
        dir_mode = maps:get(dirMode, Params, undefined)
    }.


%% @private
-spec build_xrootd_configuration(map()) -> #xrootd_configuration{}.
build_xrootd_configuration(Params) ->
    #xrootd_configuration{
        url = maps:get(url, Params),
        file_mode_mask = maps:get(fileModeMask, Params, undefined),
        dir_mode_mask = maps:get(dirModeMask, Params, undefined),
        storage_path_type = binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


%% @private
-spec build_xrootd_configuration_diff(map()) -> #xrootd_configuration_diff{}.
build_xrootd_configuration_diff(Params) ->
    #xrootd_configuration_diff{
        url = maps:get(url, Params, undefined),
        file_mode_mask = maps:get(fileModeMask, Params, undefined),
        dir_mode_mask = maps:get(dirModeMask, Params, undefined)
    }.


%% @private
-spec credentials_to_map(onedata_storage:type(), onedata_storage:credentials()) -> map().
credentials_to_map(<<"ceph">>, Creds) -> ceph_credentials_to_map(Creds);
credentials_to_map(<<"cephrados">>, Creds) -> cephrados_credentials_to_map(Creds);
credentials_to_map(<<"glusterfs">>, Creds) -> glusterfs_credentials_to_map(Creds);
credentials_to_map(<<"http">>, Creds) -> http_credentials_to_map(Creds);
credentials_to_map(<<"nfs">>, Creds) -> nfs_credentials_to_map(Creds);
credentials_to_map(<<"nulldevice">>, Creds) -> nulldevice_credentials_to_map(Creds);
credentials_to_map(<<"posix">>, Creds) -> posix_credentials_to_map(Creds);
credentials_to_map(<<"s3">>, Creds) -> s3_credentials_to_map(Creds);
credentials_to_map(<<"swift">>, Creds) -> swift_credentials_to_map(Creds);
credentials_to_map(<<"webdav">>, Creds) -> webdav_credentials_to_map(Creds);
credentials_to_map(<<"xrootd">>, Creds) -> xrootd_credentials_to_map(Creds).


%% @private
-spec ceph_credentials_to_map(#ceph_credentials{}) -> map().
ceph_credentials_to_map(#ceph_credentials{username = Username, key = Key}) ->
    #{username => Username, key => Key}.


%% @private
-spec cephrados_credentials_to_map(#cephrados_credentials{}) -> map().
cephrados_credentials_to_map(#cephrados_credentials{username = Username, key = Key}) ->
    #{username => Username, key => Key}.


%% @private
-spec glusterfs_credentials_to_map(#glusterfs_credentials{}) -> map().
glusterfs_credentials_to_map(#glusterfs_credentials{uid = Uid, gid = Gid}) ->
    Base = #{uid => Uid},
    maps_utils:put_if_defined(Base, gid, Gid).


%% @private
-spec http_credentials_to_map(#http_credentials{}) -> map().
http_credentials_to_map(#http_credentials{
    credentials_type = CredType,
    credentials = Credentials,
    oauth2_idp = Oauth2Idp,
    onedata_access_token = OnedataToken
}) ->
    Base = #{credentialsType => credentials_type_to_binary(CredType)},
    Base1 = maps_utils:put_if_defined(Base, credentials, Credentials),
    Base2 = maps_utils:put_if_defined(Base1, oauth2IdP, Oauth2Idp),
    maps_utils:put_if_defined(Base2, onedataAccessToken, OnedataToken).


%% @private
-spec nfs_credentials_to_map(#nfs_credentials{}) -> map().
nfs_credentials_to_map(#nfs_credentials{uid = Uid, gid = Gid}) ->
    Base = #{uid => Uid},
    maps_utils:put_if_defined(Base, gid, Gid).


%% @private
-spec nulldevice_credentials_to_map(#nulldevice_credentials{}) -> map().
nulldevice_credentials_to_map(#nulldevice_credentials{uid = Uid, gid = Gid}) ->
    Base = #{uid => Uid},
    maps_utils:put_if_defined(Base, gid, Gid).


%% @private
-spec posix_credentials_to_map(#posix_credentials{}) -> map().
posix_credentials_to_map(#posix_credentials{uid = Uid, gid = Gid}) ->
    Base = #{rootUid => Uid},
    maps_utils:put_if_defined(Base, rootGid, Gid).


%% @private
-spec s3_credentials_to_map(#s3_credentials{}) -> map().
s3_credentials_to_map(#s3_credentials{access_key = AccessKey, secret_key = SecretKey}) ->
    #{accessKey => AccessKey, secretKey => SecretKey}.


%% @private
-spec swift_credentials_to_map(#swift_credentials{}) -> map().
swift_credentials_to_map(#swift_credentials{
    username = Username,
    password = Password,
    project_name = ProjectName,
    user_domain_name = UserDomainName,
    project_domain_name = ProjectDomainName
}) ->
    Base = #{
        username => Username,
        password => Password,
        projectName => ProjectName
    },
    Base1 = maps_utils:put_if_defined(Base, userDomainName, UserDomainName),
    maps_utils:put_if_defined(Base1, projectDomainName, ProjectDomainName).


%% @private
-spec webdav_credentials_to_map(#webdav_credentials{}) -> map().
webdav_credentials_to_map(#webdav_credentials{
    credentials_type = CredType,
    credentials = Credentials,
    oauth2_idp = Oauth2Idp,
    onedata_access_token = OnedataToken
}) ->
    Base = #{credentialsType => credentials_type_to_binary(CredType)},
    Base1 = maps_utils:put_if_defined(Base, credentials, Credentials),
    Base2 = maps_utils:put_if_defined(Base1, oauth2IdP, Oauth2Idp),
    maps_utils:put_if_defined(Base2, onedataAccessToken, OnedataToken).


%% @private
-spec xrootd_credentials_to_map(#xrootd_credentials{}) -> map().
xrootd_credentials_to_map(#xrootd_credentials{
    credentials_type = CredType,
    credentials = Credentials
}) ->
    Base = #{credentialsType => credentials_type_to_binary(CredType)},
    case Credentials of
        undefined -> Base;
        _ -> Base#{credentials => Credentials}
    end.


%% @private
-spec configuration_to_map(onedata_storage:type(), onedata_storage:configuration()) -> map().
configuration_to_map(<<"ceph">>, Config) -> ceph_configuration_to_map(Config);
configuration_to_map(<<"cephrados">>, Config) -> cephrados_configuration_to_map(Config);
configuration_to_map(<<"glusterfs">>, Config) -> glusterfs_configuration_to_map(Config);
configuration_to_map(<<"http">>, Config) -> http_configuration_to_map(Config);
configuration_to_map(<<"nfs">>, Config) -> nfs_configuration_to_map(Config);
configuration_to_map(<<"nulldevice">>, Config) -> nulldevice_configuration_to_map(Config);
configuration_to_map(<<"posix">>, Config) -> posix_configuration_to_map(Config);
configuration_to_map(<<"s3">>, Config) -> s3_configuration_to_map(Config);
configuration_to_map(<<"swift">>, Config) -> swift_configuration_to_map(Config);
configuration_to_map(<<"webdav">>, Config) -> webdav_configuration_to_map(Config);
configuration_to_map(<<"xrootd">>, Config) -> xrootd_configuration_to_map(Config).


%% @private
-spec ceph_configuration_to_map(#ceph_configuration{}) -> map().
ceph_configuration_to_map(#ceph_configuration{
    monitor_hostname = MonitorHostname,
    cluster_name = ClusterName,
    pool_name = PoolName,
    storage_path_type = StoragePathType
}) ->
    #{
        monitorHostname => MonitorHostname,
        clusterName => ClusterName,
        poolName => PoolName,
        storagePathType => storage_path_type_to_binary(StoragePathType)
    }.


%% @private
-spec cephrados_configuration_to_map(#cephrados_configuration{}) -> map().
cephrados_configuration_to_map(#cephrados_configuration{
    monitor_hostname = MonitorHostname,
    cluster_name = ClusterName,
    pool_name = PoolName,
    block_size = BlockSize,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        monitorHostname => MonitorHostname,
        clusterName => ClusterName,
        poolName => PoolName,
        storagePathType => storage_path_type_to_binary(StoragePathType)
    },
    maps_utils:put_if_defined(Base, blockSize, BlockSize).


%% @private
-spec glusterfs_configuration_to_map(#glusterfs_configuration{}) -> map().
glusterfs_configuration_to_map(#glusterfs_configuration{
    volume = Volume,
    hostname = Hostname,
    port = Port,
    transport = Transport,
    mount_point = MountPoint,
    xlator_options = XlatorOptions,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        volume => Volume,
        hostname => Hostname,
        storagePathType => storage_path_type_to_binary(StoragePathType)
    },
    Base1 = maps_utils:put_if_defined(Base, port, Port),
    Base2 = maps_utils:put_if_defined(Base1, transport, case Transport of
        undefined -> undefined;
        _ -> transport_to_binary(Transport)
    end),
    Base3 = maps_utils:put_if_defined(Base2, mountPoint, MountPoint),
    maps_utils:put_if_defined(Base3, xlatorOptions, XlatorOptions).


%% @private
-spec http_configuration_to_map(#http_configuration{}) -> map().
http_configuration_to_map(#http_configuration{
    endpoint = Endpoint,
    verify_server_certificate = VerifyServerCert,
    authorization_header = AuthHeader,
    connection_pool_size = PoolSize,
    max_requests_per_session = MaxRequests,
    file_mode = FileMode,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        endpoint => Endpoint,
        storagePathType => storage_path_type_to_binary(StoragePathType)
    },
    Base1 = maps_utils:put_if_defined(Base, verifyServerCertificate, VerifyServerCert),
    Base2 = maps_utils:put_if_defined(Base1, authorizationHeader, AuthHeader),
    Base3 = maps_utils:put_if_defined(Base2, connectionPoolSize, PoolSize),
    Base4 = maps_utils:put_if_defined(Base3, maxRequestsPerSession, MaxRequests),
    maps_utils:put_if_defined(Base4, fileMode, FileMode).


%% @private
-spec nfs_configuration_to_map(#nfs_configuration{}) -> map().
nfs_configuration_to_map(#nfs_configuration{
    version = Version,
    host = Host,
    volume = Volume,
    read_ahead = ReadAhead,
    dir_cache = DirCache,
    auto_reconnect = AutoReconnect,
    connection_pool_size = PoolSize,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        host => Host,
        volume => Volume,
        storagePathType => storage_path_type_to_binary(StoragePathType)
    },
    Base1 = maps_utils:put_if_defined(Base, version, Version),
    Base2 = maps_utils:put_if_defined(Base1, readAhead, ReadAhead),
    Base3 = maps_utils:put_if_defined(Base2, dirCache, DirCache),
    Base4 = maps_utils:put_if_defined(Base3, autoReconnect, AutoReconnect),
    maps_utils:put_if_defined(Base4, connectionPoolSize, PoolSize).


%% @private
-spec nulldevice_configuration_to_map(#nulldevice_configuration{}) -> map().
nulldevice_configuration_to_map(#nulldevice_configuration{
    latency_min = LatencyMin,
    latency_max = LatencyMax,
    timeout_probability = TimeoutProb,
    filter = Filter,
    simulated_filesystem_parameters = SimFsParams,
    simulated_filesystem_grow_speed = SimFsGrowSpeed,
    enable_data_verification = EnableDataVerif,
    storage_path_type = StoragePathType
}) ->
    Base = #{storagePathType => storage_path_type_to_binary(StoragePathType)},
    Base1 = maps_utils:put_if_defined(Base, latencyMin, LatencyMin),
    Base2 = maps_utils:put_if_defined(Base1, latencyMax, LatencyMax),
    Base3 = maps_utils:put_if_defined(Base2, timeoutProbability, TimeoutProb),
    Base4 = maps_utils:put_if_defined(Base3, filter, Filter),
    Base5 = maps_utils:put_if_defined(Base4, simulatedFilesystemParameters, SimFsParams),
    Base6 = maps_utils:put_if_defined(Base5, simulatedFilesystemGrowSpeed, SimFsGrowSpeed),
    maps_utils:put_if_defined(Base6, enableDataVerification, EnableDataVerif).


%% @private
-spec posix_configuration_to_map(#posix_configuration{}) -> map().
posix_configuration_to_map(#posix_configuration{
    mount_point = MountPoint,
    storage_path_type = StoragePathType
}) ->
    #{
        mountPoint => MountPoint,
        storagePathType => storage_path_type_to_binary(StoragePathType)
    }.


%% @private
-spec s3_configuration_to_map(#s3_configuration{}) -> map().
s3_configuration_to_map(#s3_configuration{
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
        storagePathType => storage_path_type_to_binary(StoragePathType)
    },
    Base1 = maps_utils:put_if_defined(Base, signatureVersion, SignatureVersion),
    Base2 = maps_utils:put_if_defined(Base1, verifyServerCertificate, VerifyServerCert),
    Base3 = maps_utils:put_if_defined(Base2, region, Region),
    Base4 = maps_utils:put_if_defined(Base3, blockSize, BlockSize),
    Base5 = maps_utils:put_if_defined(Base4, maximumCanonicalObjectSize, MaxCanonicalObjectSize),
    Base6 = maps_utils:put_if_defined(Base5, fileMode, FileMode),
    maps_utils:put_if_defined(Base6, dirMode, DirMode).


%% @private
-spec swift_configuration_to_map(#swift_configuration{}) -> map().
swift_configuration_to_map(#swift_configuration{
    auth_url = AuthUrl,
    container_name = ContainerName,
    block_size = BlockSize,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        authUrl => AuthUrl,
        containerName => ContainerName,
        storagePathType => storage_path_type_to_binary(StoragePathType)
    },
    maps_utils:put_if_defined(Base, blockSize, BlockSize).


%% @private
-spec webdav_configuration_to_map(#webdav_configuration{}) -> map().
webdav_configuration_to_map(#webdav_configuration{
    endpoint = Endpoint,
    verify_server_certificate = VerifyServerCert,
    authorization_header = AuthHeader,
    range_write_support = RangeWriteSupport,
    connection_pool_size = PoolSize,
    maximum_upload_size = MaxUploadSize,
    file_mode = FileMode,
    dir_mode = DirMode,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        endpoint => Endpoint,
        storagePathType => storage_path_type_to_binary(StoragePathType)
    },
    Base1 = maps_utils:put_if_defined(Base, verifyServerCertificate, VerifyServerCert),
    Base2 = maps_utils:put_if_defined(Base1, authorizationHeader, AuthHeader),
    Base3 = maps_utils:put_if_defined(Base2, rangeWriteSupport, case RangeWriteSupport of
        undefined -> undefined;
        _ -> range_write_support_to_binary(RangeWriteSupport)
    end),
    Base4 = maps_utils:put_if_defined(Base3, connectionPoolSize, PoolSize),
    Base5 = maps_utils:put_if_defined(Base4, maximumUploadSize, MaxUploadSize),
    Base6 = maps_utils:put_if_defined(Base5, fileMode, FileMode),
    maps_utils:put_if_defined(Base6, dirMode, DirMode).


%% @private
-spec xrootd_configuration_to_map(#xrootd_configuration{}) -> map().
xrootd_configuration_to_map(#xrootd_configuration{
    url = Url,
    file_mode_mask = FileModeMask,
    dir_mode_mask = DirModeMask,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        url => Url,
        storagePathType => storage_path_type_to_binary(StoragePathType)
    },
    Base1 = maps_utils:put_if_defined(Base, fileModeMask, FileModeMask),
    maps_utils:put_if_defined(Base1, dirModeMask, DirModeMask).


%% @private
-spec binary_to_storage_path_type(binary()) -> flat | canonical.
binary_to_storage_path_type(<<"flat">>) -> flat;
binary_to_storage_path_type(<<"canonical">>) -> canonical.


%% @private
-spec storage_path_type_to_binary(flat | canonical) -> binary().
storage_path_type_to_binary(flat) -> <<"flat">>;
storage_path_type_to_binary(canonical) -> <<"canonical">>.


%% @private
-spec binary_to_credentials_type(binary()) -> none | basic | token | oauth2 | pwd.
binary_to_credentials_type(<<"none">>) -> none;
binary_to_credentials_type(<<"basic">>) -> basic;
binary_to_credentials_type(<<"token">>) -> token;
binary_to_credentials_type(<<"oauth2">>) -> oauth2;
binary_to_credentials_type(<<"pwd">>) -> pwd.


%% @private
-spec credentials_type_to_binary(none | basic | token | oauth2 | pwd) -> binary().
credentials_type_to_binary(none) -> <<"none">>;
credentials_type_to_binary(basic) -> <<"basic">>;
credentials_type_to_binary(token) -> <<"token">>;
credentials_type_to_binary(oauth2) -> <<"oauth2">>;
credentials_type_to_binary(pwd) -> <<"pwd">>.


%% @private
-spec transport_to_binary(tcp | rdma | socket) -> binary().
transport_to_binary(tcp) -> <<"tcp">>;
transport_to_binary(rdma) -> <<"rdma">>;
transport_to_binary(socket) -> <<"socket">>.

%% @private
-spec binary_to_transport(binary()) -> tcp | rdma | socket.
binary_to_transport(<<"tcp">>) -> tcp;
binary_to_transport(<<"rdma">>) -> rdma;
binary_to_transport(<<"socket">>) -> socket.


%% @private
-spec range_write_support_to_binary(none | moddav | sabredav) -> binary().
range_write_support_to_binary(none) -> <<"none">>;
range_write_support_to_binary(moddav) -> <<"moddav">>;
range_write_support_to_binary(sabredav) -> <<"sabredav">>.


%% @private
-spec binary_to_range_write_support(binary()) -> none | moddav | sabredav.
binary_to_range_write_support(<<"none">>) -> none;
binary_to_range_write_support(<<"moddav">>) -> moddav;
binary_to_range_write_support(<<"sabredav">>) -> sabredav.


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


%%--------------------------------------------------------------------
%% @private
%% @doc Joins S3 scheme and hostname into full URL for REST API.
%% @end
%%--------------------------------------------------------------------
-spec join_s3_hostname(Scheme :: binary(), Hostname :: binary()) -> binary().
join_s3_hostname(Scheme, Hostname) ->
    <<Scheme/binary, "://", Hostname/binary>>.
