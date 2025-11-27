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
build_credentials(<<"ceph">>, Params) -> ceph_storage_spec_builder:build_credentials(Params);
build_credentials(<<"cephrados">>, Params) -> cephrados_storage_spec_builder:build_credentials(Params);
build_credentials(<<"glusterfs">>, Params) -> glusterfs_storage_spec_builder:build_credentials(Params);
build_credentials(<<"http">>, Params) -> http_storage_spec_builder:build_credentials(Params);
build_credentials(<<"nfs">>, Params) -> nfs_storage_spec_builder:build_credentials(Params);
build_credentials(<<"nulldevice">>, Params) -> nulldevice_storage_spec_builder:build_credentials(Params);
build_credentials(<<"posix">>, Params) -> posix_storage_spec_builder:build_credentials(Params);
build_credentials(<<"s3">>, Params) -> s3_storage_spec_builder:build_credentials(Params);
build_credentials(<<"swift">>, Params) -> swift_storage_spec_builder:build_credentials(Params);
build_credentials(<<"webdav">>, Params) -> webdav_storage_spec_builder:build_credentials(Params);
build_credentials(<<"xrootd">>, Params) -> xrootd_storage_spec_builder:build_credentials(Params).


%% @private
-spec build_credentials_diff(onedata_storage:type(), map()) ->
    onedata_storage:credentials_diff().
build_credentials_diff(<<"ceph">>, Params) -> ceph_storage_spec_builder:build_credentials_diff(Params);
build_credentials_diff(<<"cephrados">>, Params) -> cephrados_storage_spec_builder:build_credentials_diff(Params);
build_credentials_diff(<<"glusterfs">>, Params) -> glusterfs_storage_spec_builder:build_credentials_diff(Params);
build_credentials_diff(<<"http">>, Params) -> http_storage_spec_builder:build_credentials_diff(Params);
build_credentials_diff(<<"nfs">>, Params) -> nfs_storage_spec_builder:build_credentials_diff(Params);
build_credentials_diff(<<"nulldevice">>, Params) -> nulldevice_storage_spec_builder:build_credentials_diff(Params);
build_credentials_diff(<<"posix">>, Params) -> posix_storage_spec_builder:build_credentials_diff(Params);
build_credentials_diff(<<"s3">>, Params) -> s3_storage_spec_builder:build_credentials_diff(Params);
build_credentials_diff(<<"swift">>, Params) -> swift_storage_spec_builder:build_credentials_diff(Params);
build_credentials_diff(<<"webdav">>, Params) -> webdav_storage_spec_builder:build_credentials_diff(Params);
build_credentials_diff(<<"xrootd">>, Params) -> xrootd_storage_spec_builder:build_credentials_diff(Params).


%% @private
-spec build_configuration(onedata_storage:type(), map()) -> onedata_storage:configuration().
build_configuration(<<"ceph">>, Params) -> ceph_storage_spec_builder:build_configuration(Params);
build_configuration(<<"cephrados">>, Params) -> cephrados_storage_spec_builder:build_configuration(Params);
build_configuration(<<"glusterfs">>, Params) -> glusterfs_storage_spec_builder:build_configuration(Params);
build_configuration(<<"http">>, Params) -> http_storage_spec_builder:build_configuration(Params);
build_configuration(<<"nfs">>, Params) -> nfs_storage_spec_builder:build_configuration(Params);
build_configuration(<<"nulldevice">>, Params) -> nulldevice_storage_spec_builder:build_configuration(Params);
build_configuration(<<"posix">>, Params) -> posix_storage_spec_builder:build_configuration(Params);
build_configuration(<<"s3">>, Params) -> s3_storage_spec_builder:build_configuration(Params);
build_configuration(<<"swift">>, Params) -> swift_storage_spec_builder:build_configuration(Params);
build_configuration(<<"webdav">>, Params) -> webdav_storage_spec_builder:build_configuration(Params);
build_configuration(<<"xrootd">>, Params) -> xrootd_storage_spec_builder:build_configuration(Params).


%% @private
-spec build_configuration_diff(onedata_storage:type(), map()) ->
    onedata_storage:configuration_diff().
build_configuration_diff(<<"ceph">>, Params) -> ceph_storage_spec_builder:build_configuration_diff(Params);
build_configuration_diff(<<"cephrados">>, Params) -> cephrados_storage_spec_builder:build_configuration_diff(Params);
build_configuration_diff(<<"glusterfs">>, Params) -> glusterfs_storage_spec_builder:build_configuration_diff(Params);
build_configuration_diff(<<"http">>, Params) -> http_storage_spec_builder:build_configuration_diff(Params);
build_configuration_diff(<<"nfs">>, Params) -> nfs_storage_spec_builder:build_configuration_diff(Params);
build_configuration_diff(<<"nulldevice">>, Params) -> nulldevice_storage_spec_builder:build_configuration_diff(Params);
build_configuration_diff(<<"posix">>, Params) -> posix_storage_spec_builder:build_configuration_diff(Params);
build_configuration_diff(<<"s3">>, Params) -> s3_storage_spec_builder:build_configuration_diff(Params);
build_configuration_diff(<<"swift">>, Params) -> swift_storage_spec_builder:build_configuration_diff(Params);
build_configuration_diff(<<"webdav">>, Params) -> webdav_storage_spec_builder:build_configuration_diff(Params);
build_configuration_diff(<<"xrootd">>, Params) -> xrootd_storage_spec_builder:build_configuration_diff(Params).


%% @private
-spec credentials_to_map(onedata_storage:type(), onedata_storage:credentials()) -> map().
credentials_to_map(<<"ceph">>, Creds) -> ceph_storage_spec_builder:credentials_to_map(Creds);
credentials_to_map(<<"cephrados">>, Creds) -> cephrados_storage_spec_builder:credentials_to_map(Creds);
credentials_to_map(<<"glusterfs">>, Creds) -> glusterfs_storage_spec_builder:credentials_to_map(Creds);
credentials_to_map(<<"http">>, Creds) -> http_storage_spec_builder:credentials_to_map(Creds);
credentials_to_map(<<"nfs">>, Creds) -> nfs_storage_spec_builder:credentials_to_map(Creds);
credentials_to_map(<<"nulldevice">>, Creds) -> nulldevice_storage_spec_builder:credentials_to_map(Creds);
credentials_to_map(<<"posix">>, Creds) -> posix_storage_spec_builder:credentials_to_map(Creds);
credentials_to_map(<<"s3">>, Creds) -> s3_storage_spec_builder:credentials_to_map(Creds);
credentials_to_map(<<"swift">>, Creds) -> swift_storage_spec_builder:credentials_to_map(Creds);
credentials_to_map(<<"webdav">>, Creds) -> webdav_storage_spec_builder:credentials_to_map(Creds);
credentials_to_map(<<"xrootd">>, Creds) -> xrootd_storage_spec_builder:credentials_to_map(Creds).


%% @private
-spec configuration_to_map(onedata_storage:type(), onedata_storage:configuration()) -> map().
configuration_to_map(<<"ceph">>, Config) -> ceph_storage_spec_builder:configuration_to_map(Config);
configuration_to_map(<<"cephrados">>, Config) -> cephrados_storage_spec_builder:configuration_to_map(Config);
configuration_to_map(<<"glusterfs">>, Config) -> glusterfs_storage_spec_builder:configuration_to_map(Config);
configuration_to_map(<<"http">>, Config) -> http_storage_spec_builder:configuration_to_map(Config);
configuration_to_map(<<"nfs">>, Config) -> nfs_storage_spec_builder:configuration_to_map(Config);
configuration_to_map(<<"nulldevice">>, Config) -> nulldevice_storage_spec_builder:configuration_to_map(Config);
configuration_to_map(<<"posix">>, Config) -> posix_storage_spec_builder:configuration_to_map(Config);
configuration_to_map(<<"s3">>, Config) -> s3_storage_spec_builder:configuration_to_map(Config);
configuration_to_map(<<"swift">>, Config) -> swift_storage_spec_builder:configuration_to_map(Config);
configuration_to_map(<<"webdav">>, Config) -> webdav_storage_spec_builder:configuration_to_map(Config);
configuration_to_map(<<"xrootd">>, Config) -> xrootd_storage_spec_builder:configuration_to_map(Config).
