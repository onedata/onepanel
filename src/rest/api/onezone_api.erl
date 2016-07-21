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
-module(onezone_api).
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
        {<<"/api/v3/onepanel/zone/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %%
                args_spec = #{hosts => [string]}}]
        }},

        %%
        %% Add zone manager hosts
        %%
        {<<"/api/v3/onepanel/zone/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %%
                args_spec = rest_model:manager_hosts_model()}]
        }},

        %%
        %% Add zone worker hosts
        %%
        {<<"/api/v3/onepanel/zone/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oz_worker,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %%
                args_spec = #{hosts => [string]}}]
        }},

        %%
        %% Get zone database status
        %%
        {<<"/api/v3/onepanel/zone/databases/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get zone database nodes
        %%
        {<<"/api/v3/onepanel/zone/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get zone manager status
        %%
        {<<"/api/v3/onepanel/zone/managers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get zone manager nodes
        %%
        {<<"/api/v3/onepanel/zone/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get zone worker status
        %%
        {<<"/api/v3/onepanel/zone/workers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oz_worker,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get zone worker nodes
        %%
        {<<"/api/v3/onepanel/zone/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oz_worker,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Update zone configuration
        %%
        {<<"/api/v3/onepanel/zone/configuration">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_onezone,
            methods = [#rmethod{type = 'PUT', args_spec = #{
                cluster => #{
                    domainName => string,
                    nodes => #{
                        '_' => #{
                            hostname => string
                        }
                    },
                    managers => #{
                        mainNode => string,
                        nodes => [string]
                    },
                    workers => #{
                        nodes => [string]
                    },
                    databases => #{
                        nodes => [string]
                    }
                },
                onezone => {#{
                    name => {string, optional},
                    domainName => {string, optional}
                }, optional}
            }}]
        }},

        %%
        %% Start/stop zone database node.
        %%
        {<<"/api/v3/onepanel/zone/databases/:host">>, rest_handler, #rstate{
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
        %% Start/stop zone database nodes
        %%
        {<<"/api/v3/onepanel/zone/databases">>, rest_handler, #rstate{
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
        %% Start/stop zone manager node.
        %%
        {<<"/api/v3/onepanel/zone/managers/:host">>, rest_handler, #rstate{
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
        %% Start/stop zone manager nodes
        %%
        {<<"/api/v3/onepanel/zone/managers">>, rest_handler, #rstate{
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
        %% Start/stop zone worker node
        %%
        {<<"/api/v3/onepanel/zone/workers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oz_worker,
            methods = [#rmethod{type = 'PATCH',
                %% [Query parameters]
                params_spec = #{
                    %% This flag changes the intended state of the worker instance. \n\nIf the state is changed, the service will be stopped or started in \norder to match the requested state. \n
                    started => {boolean, {optional, true}}}}]
        }},

        %%
        %% Start/stop zone worker nodes
        %%
        {<<"/api/v3/onepanel/zone/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oz_worker,
            methods = [#rmethod{type = 'PATCH',
                %% [Query parameters]
                params_spec = #{
                    %% This flag changes the intended state of the worker instances. \n\nIf the state is changed, the service will be stopped or started in \norder to match the requested state. \n
                    started => {boolean, {optional, true}}}}]
        }}

    ].



















