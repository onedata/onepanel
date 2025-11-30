%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for building HTTP storage specifications
%%% from REST API maps and converting HTTP storage descriptions back to maps.
%%% It translates between the REST API format (camelCase keys, binaries)
%%% and ctool storage records (snake_case atoms, records).
%%% @end
%%%--------------------------------------------------------------------
-module(http_storage_spec_builder).
-author("Bartosz Walkowicz").

-include_lib("op_panel_contracts/include/storage/http.hrl").

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


-spec build_credentials(map()) -> onedata_storage:http_credentials().
build_credentials(Params) ->
    #http_credentials{
        credentials_type = binary_to_credentials_type(maps:get(credentialsType, Params, <<"none">>)),
        credentials = maps:get(credentials, Params, undefined),
        oauth2_idp = maps:get(oauth2IdP, Params, undefined),
        onedata_access_token = maps:get(onedataAccessToken, Params, undefined)
    }.


-spec build_credentials_diff(map()) -> onedata_storage:http_credentials_diff().
build_credentials_diff(Params) ->
    #http_credentials_diff{
        credentials_type = case maps:get(credentialsType, Params, undefined) of
            undefined -> undefined;
            CredType -> binary_to_credentials_type(CredType)
        end,
        credentials = maps:get(credentials, Params, undefined),
        oauth2_idp = maps:get(oauth2IdP, Params, undefined),
        onedata_access_token = maps:get(onedataAccessToken, Params, undefined)
    }.


-spec build_configuration(map()) -> onedata_storage:http_configuration().
build_configuration(Params) ->
    #http_configuration{
        endpoint = maps:get(endpoint, Params),
        verify_server_certificate = maps:get(verifyServerCertificate, Params, undefined),
        authorization_header = maps:get(authorizationHeader, Params, undefined),
        connection_pool_size = maps:get(connectionPoolSize, Params, undefined),
        max_requests_per_session = maps:get(maxRequestsPerSession, Params, undefined),
        file_mode = maps:get(fileMode, Params, undefined),
        storage_path_type = storage_spec_builder_utils:binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


-spec build_configuration_diff(map()) -> onedata_storage:http_configuration_diff().
build_configuration_diff(Params) ->
    #http_configuration_diff{
        endpoint = maps:get(endpoint, Params, undefined),
        verify_server_certificate = maps:get(verifyServerCertificate, Params, undefined),
        authorization_header = maps:get(authorizationHeader, Params, undefined),
        connection_pool_size = maps:get(connectionPoolSize, Params, undefined),
        max_requests_per_session = maps:get(maxRequestsPerSession, Params, undefined),
        file_mode = maps:get(fileMode, Params, undefined)
    }.


-spec credentials_to_map(onedata_storage:http_credentials()) -> map().
credentials_to_map(#http_credentials{
    credentials_type = CredType,
    credentials = Credentials,
    oauth2_idp = Oauth2Idp,
    onedata_access_token = OnedataToken
}) ->
    Base = #{credentialsType => credentials_type_to_binary(CredType)},
    Base1 = maps_utils:put_if_defined(Base, credentials, Credentials),
    Base2 = maps_utils:put_if_defined(Base1, oauth2IdP, Oauth2Idp),
    maps_utils:put_if_defined(Base2, onedataAccessToken, OnedataToken).


-spec configuration_to_map(onedata_storage:http_configuration()) -> map().
configuration_to_map(#http_configuration{
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
        storagePathType => storage_spec_builder_utils:storage_path_type_to_binary(StoragePathType)
    },
    Base1 = maps_utils:put_if_defined(Base, verifyServerCertificate, VerifyServerCert),
    Base2 = maps_utils:put_if_defined(Base1, authorizationHeader, AuthHeader),
    Base3 = maps_utils:put_if_defined(Base2, connectionPoolSize, PoolSize),
    Base4 = maps_utils:put_if_defined(Base3, maxRequestsPerSession, MaxRequests),
    maps_utils:put_if_defined(Base4, fileMode, FileMode).


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
