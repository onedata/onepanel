%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc 
%%% @end
%%%--------------------------------------------------------------------
-module(oneprovider_api).
-author("Krzysztof Trzepla").

-include("http/rest.hrl").

%% API
-export([routes/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), Module :: module(), State :: rest_handler:state()}].
routes() ->
    [
        %%
        %% Get cluster hosts
        %%
        {<<"/api/v3/onepanel/provider/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %%
                args_spec = #{hosts => [string]}}]
        }},

        %%
        %% Add provider manager hosts
        %%
        {<<"/api/v3/onepanel/provider/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %%
                args_spec = rest_model:manager_hosts_model()}]
        }},

        %%
        %% Add provider worker hosts
        %%
        {<<"/api/v3/onepanel/provider/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %%
                args_spec = #{hosts => [string]}}]
        }},

        %%
        %% Add storage resources
        %%
        {<<"/api/v3/onepanel/provider/storages">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = storages,
            methods = [#rmethod{type = 'PUT', args_spec = #{
                '_' => {oneof, [
                    #{
                        type => {equal, <<"POSIX">>},
                        mountPoint => string
                    },
                    #{
                        type => {equal, <<"S3">>},
                        accessKey => string,
                        secretKey => string,
                        s3Hostname => string,
                        iamHostname => string,
                        bucketName => string
                    },
                    #{
                        type => {equal, <<"CEPH">>},
                        username => string,
                        key => string,
                        monitorHostname => string,
                        clusterName => string,
                        poolName => string
                    }
                ]}
            }}]
        }},

        %%
        %% Start/stop provider manager node.
        %%
        {<<"/api/v3/onepanel/provider/managers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{type = 'PATCH',
                %% [Query parameters]
                params_spec = #{
                    %% This flag changes the intended state of the cluster manager instance. \n\nIf the state is changed, the service will be stopped or started in order \nto match the requested state. \n
                    started => {boolean, {optional, true}}}}]
        }},

        %%
        %% Get provider details
        %%
        {<<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get provider database status
        %%
        {<<"/api/v3/onepanel/provider/databases/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get provider database nodes
        %%
        {<<"/api/v3/onepanel/provider/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get provider manager status
        %%
        {<<"/api/v3/onepanel/provider/managers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get provider manager nodes
        %%
        {<<"/api/v3/onepanel/provider/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get provider spaces
        %%
        {<<"/api/v3/onepanel/provider/spaces">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = spaces,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get provider worker status
        %%
        {<<"/api/v3/onepanel/provider/workers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get provider worker nodes
        %%
        {<<"/api/v3/onepanel/provider/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get storage details
        %%
        {<<"/api/v3/onepanel/provider/storages/:name">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = storage,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get storage resources
        %%
        {<<"/api/v3/onepanel/provider/storages">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = storages,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Update configuration
        %%
        {<<"/api/v3/onepanel/provider/configuration">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oneprovider,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %% New deployment configuration.
                args_spec = #{cluster => #{databases => #{nodes => [string]},
                    domainName => string,
                    managers => #{mainNode => string, nodes => [string]},
                    nodes => #{'_' => #{hostname => string}},
                    storages => {#{'_' => {oneof, [#{mountPoint => string, type => {equal, <<"POSIX">>}},
                        #{accessKey => string,
                            bucketName => string,
                            iamHostname => string,
                            s3Hostname => string,
                            secretKey => string,
                            type => {equal, <<"S3">>}},
                        #{clusterName => string,
                            key => string,
                            monitorHostname => string,
                            poolName => string,
                            type => {equal, <<"CEPH">>},
                            username => string}]}},
                        optional},
                    workers => #{nodes => [string]}},
                    oneprovider => {#{geoLatitude => {float, optional},
                        geoLongitude => {float, optional},
                        name => string,
                        redirectionPoint => string,
                        register => boolean},
                        optional},
                    onezone => {#{domainName => string}, optional}}}]
        }},

        %%
        %% Modify provider details
        %%
        {<<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{type = 'PATCH',
                %% [Body parameter]
                %%
                args_spec = rest_model:provider_update_request_model()}]
        }},

        %%
        %% Start/stop provider database node.
        %%
        {<<"/api/v3/onepanel/provider/databases/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{type = 'PATCH',
                %% [Query parameters]
                params_spec = #{
                    %% This flag changes the intended state of the database instance. \n\nIf the state is changed, the service will be stopped or started \nin order to match the requested state. \n
                    started => {boolean, {optional, true}}}}]
        }},

        %%
        %% Start/stop provider database nodes
        %%
        {<<"/api/v3/onepanel/provider/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{type = 'PATCH',
                %% [Query parameters]
                params_spec = #{
                    %% This flag changes the intended state of the database instances. \nIf the state is changed, the service will be stopped or started in \norder to match the requested state. \n
                    started => {boolean, {optional, true}}}}]
        }},

        %%
        %% Start/stop provider manager nodes
        %%
        {<<"/api/v3/onepanel/provider/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{type = 'PATCH',
                %% [Query parameters]
                params_spec = #{
                    %% This flag changes the intended state of the cluster manager instances. \n\nIf the state is changed, the service will be stopped or started in order \nto match the requested state. \n
                    started => {boolean, {optional, true}}}}]
        }},

        %%
        %% Start/stop provider worker node
        %%
        {<<"/api/v3/onepanel/provider/workers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{type = 'PATCH',
                %% [Query parameters]
                params_spec = #{
                    %% This flag changes the intended state of the worker instance. \n\nIf the state is changed, the service will be stopped or started in \norder to match the requested state. \n
                    started => {boolean, {optional, true}}}}]
        }},

        %%
        %% Start/stop provider worker nodes
        %%
        {<<"/api/v3/onepanel/provider/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{type = 'PATCH',
                %% [Query parameters]
                params_spec = #{
                    %% This flag changes the intended state of the worker instances. \n\nIf the state is changed, the service will be stopped or started in \norder to match the requested state. \n
                    started => {boolean, {optional, true}}}}]
        }},

        %%
        %% Set provider details
        %%
        {<<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %%
                args_spec = rest_model:provider_configure_request_model()}]
        }},

        %%
        %% Support space
        %%
        {<<"/api/v3/onepanel/provider/spaces">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = spaces,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %%
                args_spec = rest_model:space_support_request_model()}]
        }},

        %%
        %% Unregister provider which invokes this request.
        %%
        {<<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{type = 'DELETE'}]
        }},

        {<<"/api/v3/onepanel/provider/spaces/:id">>, rest_handler, #rstate{
            version = 3, module = rest_oneprovider, resource = space,
            methods = [
                #rmethod{type = 'GET'}
            ]
        }},

        {<<"/api/v3/onepanel/provider/spaces/:id">>, rest_handler, #rstate{
            version = 3, module = rest_oneprovider, resource = space,
            methods = [
                #rmethod{type = 'DELETE'}
            ]
        }}

    ].




















