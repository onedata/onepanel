%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for building XRootD storage specifications
%%% from REST API maps and converting XRootD storage descriptions back to maps.
%%% It translates between the REST API format (camelCase keys, binaries)
%%% and ctool storage records (snake_case atoms, records).
%%% @end
%%%--------------------------------------------------------------------
-module(xrootd_storage_spec_builder).
-author("Bartosz Walkowicz").

-include_lib("op_panel_contracts/include/storage/xrootd.hrl").

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


-spec build_credentials(map()) -> onedata_storage:xrootd_credentials().
build_credentials(Params) ->
    #xrootd_credentials{
        credentials_type = binary_to_credentials_type(
            maps:get(credentialsType, Params, <<"none">>)),
        credentials = maps:get(credentials, Params, undefined)
    }.


-spec build_credentials_diff(map()) -> onedata_storage:xrootd_credentials_diff().
build_credentials_diff(Params) ->
    #xrootd_credentials_diff{
        credentials_type = case maps:get(credentialsType, Params, undefined) of
            undefined -> undefined;
            CredType -> binary_to_credentials_type(CredType)
        end,
        credentials = maps:get(credentials, Params, undefined)
    }.


-spec build_configuration(map()) -> onedata_storage:xrootd_configuration().
build_configuration(Params) ->
    #xrootd_configuration{
        url = maps:get(url, Params),
        file_mode_mask = maps:get(fileModeMask, Params, undefined),
        dir_mode_mask = maps:get(dirModeMask, Params, undefined),
        storage_path_type = storage_spec_builder_utils:binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"canonical">>)
        )
    }.


-spec build_configuration_diff(map()) -> onedata_storage:xrootd_configuration_diff().
build_configuration_diff(Params) ->
    #xrootd_configuration_diff{
        url = maps:get(url, Params, undefined),
        file_mode_mask = maps:get(fileModeMask, Params, undefined),
        dir_mode_mask = maps:get(dirModeMask, Params, undefined)
    }.


-spec credentials_to_map(onedata_storage:xrootd_credentials()) -> map().
credentials_to_map(#xrootd_credentials{
    credentials_type = CredType,
    credentials = Credentials
}) ->
    Base = #{credentialsType => credentials_type_to_binary(CredType)},
    case Credentials of
        undefined -> Base;
        _ -> Base#{credentials => Credentials}
    end.


-spec configuration_to_map(onedata_storage:xrootd_configuration()) -> map().
configuration_to_map(#xrootd_configuration{
    url = Url,
    file_mode_mask = FileModeMask,
    dir_mode_mask = DirModeMask,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        url => Url,
        storagePathType => storage_spec_builder_utils:storage_path_type_to_binary(StoragePathType)
    },
    Base1 = maps_utils:put_if_defined(Base, fileModeMask, FileModeMask),
    maps_utils:put_if_defined(Base1, dirModeMask, DirModeMask).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec binary_to_credentials_type(binary()) -> none | pwd.
binary_to_credentials_type(<<"none">>) -> none;
binary_to_credentials_type(<<"pwd">>) -> pwd.


%% @private
-spec credentials_type_to_binary(none | pwd) -> binary().
credentials_type_to_binary(none) -> <<"none">>;
credentials_type_to_binary(pwd) -> <<"pwd">>.
