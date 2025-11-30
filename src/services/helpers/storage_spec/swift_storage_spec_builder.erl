%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for building Swift storage specifications
%%% from REST API maps and converting Swift storage descriptions back to maps.
%%% It translates between the REST API format (camelCase keys, binaries)
%%% and ctool storage records (snake_case atoms, records).
%%% @end
%%%--------------------------------------------------------------------
-module(swift_storage_spec_builder).
-author("Bartosz Walkowicz").

-include_lib("op_panel_contracts/include/storage/swift.hrl").

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


-spec build_credentials(map()) -> onedata_storage:swift_credentials().
build_credentials(Params) ->
    #swift_credentials{
        username = maps:get(username, Params),
        password = maps:get(password, Params),
        project_name = maps:get(projectName, Params),
        user_domain_name = maps:get(userDomainName, Params, undefined),
        project_domain_name = maps:get(projectDomainName, Params, undefined)
    }.


-spec build_credentials_diff(map()) -> onedata_storage:swift_credentials_diff().
build_credentials_diff(Params) ->
    #swift_credentials_diff{
        username = maps:get(username, Params, undefined),
        password = maps:get(password, Params, undefined),
        project_name = maps:get(projectName, Params, undefined),
        user_domain_name = maps:get(userDomainName, Params, undefined),
        project_domain_name = maps:get(projectDomainName, Params, undefined)
    }.


-spec build_configuration(map()) -> onedata_storage:swift_configuration().
build_configuration(Params) ->
    #swift_configuration{
        auth_url = maps:get(authUrl, Params),
        container_name = maps:get(containerName, Params),
        block_size = maps:get(blockSize, Params, undefined),
        storage_path_type = storage_spec_builder_utils:binary_to_storage_path_type(
            maps:get(storagePathType, Params, <<"flat">>)
        )
    }.


-spec build_configuration_diff(map()) -> onedata_storage:swift_configuration_diff().
build_configuration_diff(Params) ->
    #swift_configuration_diff{
        auth_url = maps:get(authUrl, Params, undefined),
        container_name = maps:get(containerName, Params, undefined)
    }.


-spec credentials_to_map(onedata_storage:swift_credentials()) -> map().
credentials_to_map(#swift_credentials{
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


-spec configuration_to_map(onedata_storage:swift_configuration()) -> map().
configuration_to_map(#swift_configuration{
    auth_url = AuthUrl,
    container_name = ContainerName,
    block_size = BlockSize,
    storage_path_type = StoragePathType
}) ->
    Base = #{
        authUrl => AuthUrl,
        containerName => ContainerName,
        storagePathType => storage_spec_builder_utils:storage_path_type_to_binary(StoragePathType)
    },
    maps_utils:put_if_defined(Base, blockSize, BlockSize).
