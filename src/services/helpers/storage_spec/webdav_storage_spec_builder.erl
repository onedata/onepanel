%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for building WebDAV storage specifications
%%% from REST API maps and converting WebDAV storage descriptions back to maps.
%%% It translates between the REST API format (camelCase keys, binaries)
%%% and ctool storage records (snake_case atoms, records).
%%% @end
%%%--------------------------------------------------------------------
-module(webdav_storage_spec_builder).
-author("Bartosz Walkowicz").

-include_lib("op_panel_contracts/include/storage/webdav.hrl").

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


-spec build_credentials(map()) -> onedata_storage:webdav_credentials().
build_credentials(Params) ->
    #webdav_credentials{
        credentials_type = binary_to_credentials_type(
            maps:get(credentialsType, Params, <<"none">>)),
        credentials = maps:get(credentials, Params, undefined),
        oauth2_idp = maps:get(oauth2IdP, Params, undefined),
        onedata_access_token = maps:get(onedataAccessToken, Params, undefined)
    }.


-spec build_credentials_diff(map()) -> onedata_storage:webdav_credentials_diff().
build_credentials_diff(Params) ->
    #webdav_credentials_diff{
        credentials_type = case maps:get(credentialsType, Params, undefined) of
            undefined -> undefined;
            CredType -> binary_to_credentials_type(CredType)
        end,
        credentials = maps:get(credentials, Params, undefined),
        oauth2_idp = maps:get(oauth2IdP, Params, undefined),
        onedata_access_token = maps:get(onedataAccessToken, Params, undefined)
    }.


-spec build_configuration(map()) -> onedata_storage:webdav_configuration().
build_configuration(Params) ->
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
        storage_path_type = storage_spec_builder_utils:binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


-spec build_configuration_diff(map()) -> onedata_storage:webdav_configuration_diff().
build_configuration_diff(Params) ->
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


-spec credentials_to_map(onedata_storage:webdav_credentials()) -> map().
credentials_to_map(#webdav_credentials{
    credentials_type = CredType,
    credentials = Credentials,
    oauth2_idp = Oauth2Idp,
    onedata_access_token = OnedataToken
}) ->
    Base = #{credentialsType => credentials_type_to_binary(CredType)},
    Base1 = maps_utils:put_if_defined(Base, credentials, Credentials),
    Base2 = maps_utils:put_if_defined(Base1, oauth2IdP, Oauth2Idp),
    maps_utils:put_if_defined(Base2, onedataAccessToken, OnedataToken).


-spec configuration_to_map(onedata_storage:webdav_configuration()) -> map().
configuration_to_map(#webdav_configuration{
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
        storagePathType => storage_spec_builder_utils:storage_path_type_to_binary(StoragePathType)
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


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec binary_to_credentials_type(binary()) -> none | basic | token | oauth2.
binary_to_credentials_type(<<"none">>) -> none;
binary_to_credentials_type(<<"basic">>) -> basic;
binary_to_credentials_type(<<"token">>) -> token;
binary_to_credentials_type(<<"oauth2">>) -> oauth2.


%% @private
-spec credentials_type_to_binary(none | basic | token | oauth2) -> binary().
credentials_type_to_binary(none) -> <<"none">>;
credentials_type_to_binary(basic) -> <<"basic">>;
credentials_type_to_binary(token) -> <<"token">>;
credentials_type_to_binary(oauth2) -> <<"oauth2">>.


%% @private
-spec binary_to_range_write_support(binary()) -> none | moddav | sabredav.
binary_to_range_write_support(<<"none">>) -> none;
binary_to_range_write_support(<<"moddav">>) -> moddav;
binary_to_range_write_support(<<"sabredav">>) -> sabredav.


%% @private
-spec range_write_support_to_binary(none | moddav | sabredav) -> binary().
range_write_support_to_binary(none) -> <<"none">>;
range_write_support_to_binary(moddav) -> <<"moddav">>;
range_write_support_to_binary(sabredav) -> <<"sabredav">>.
