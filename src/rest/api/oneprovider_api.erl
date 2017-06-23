%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for oneprovider.
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
%% @doc Returns a list of tuples consisting of a path, a handler module and
%% an initial request state.
%% @end
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), Module :: module(), State :: rest_handler:state()}].
routes() ->
    [
        %% Register provider
        {<<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{
                type = 'POST',
                %% The new provider details.
                args_spec = rest_model:provider_register_request_model()
            }]
        }},

        %% Deploy provider databases
        {<<"/api/v3/onepanel/provider/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type = 'POST',
                %% The service hosts configuration where databases should be
                %% deployed.
                args_spec = rest_model:service_databases_model()
            }]
        }},

        %% Add provider cluster managers
        {<<"/api/v3/onepanel/provider/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type = 'POST',
                %% The cluster manager service hosts configuration.
                args_spec = rest_model:manager_hosts_model()
            }]
        }},

        %% Add provider cluster workers
        {<<"/api/v3/onepanel/provider/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{
                type = 'POST',
                %% The service hosts configuration where workers should be
                %% deployed.
                args_spec = rest_model:service_hosts_model()
            }]
        }},

        %% Add storage
        {<<"/api/v3/onepanel/provider/storages">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = storages,
            methods = [#rmethod{
                type = 'POST',
                %% The configuration details of storage resources to be added to
                %% the provider deployment.
                args_spec = rest_model:storage_create_request_model()
            }]
        }},

        %% Configure provider deployment
        {<<"/api/v3/onepanel/provider/configuration">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oneprovider,
            methods = [#rmethod{
                type = 'POST',
                %% The provider configuration description.
                args_spec = rest_model:provider_configuration_model(),
                noauth = true
            }]
        }},

        %% Get provider details
        {<<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get provider cluster configuration
        {<<"/api/v3/onepanel/provider/configuration">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oneprovider,
            methods = [#rmethod{
                type = 'GET',
                noauth = true
            }]
        }},

        %% Get provider database status
        {<<"/api/v3/onepanel/provider/databases/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get provider databases status
        {<<"/api/v3/onepanel/provider/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get provider cluster manager status
        {<<"/api/v3/onepanel/provider/managers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get provider cluster managers status
        {<<"/api/v3/onepanel/provider/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get provider nagios report
        {<<"/api/v3/onepanel/provider/nagios">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = nagios,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get provider spaces
        {<<"/api/v3/onepanel/provider/spaces">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = spaces,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get provider cluster worker status
        {<<"/api/v3/onepanel/provider/workers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get provider cluster workers status
        {<<"/api/v3/onepanel/provider/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get space details
        {<<"/api/v3/onepanel/provider/spaces/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = space,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get storage details
        {<<"/api/v3/onepanel/provider/storages/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = storage,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get storages
        {<<"/api/v3/onepanel/provider/storages">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = storages,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Modify provider details
        {<<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{
                type = 'PATCH',
                %% New values for provider configuration parameters which should
                %% be changed.
                args_spec = rest_model:provider_modify_request_model()
            }]
        }},

        %% Modify space details
        {<<"/api/v3/onepanel/provider/spaces/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = space,
            methods = [#rmethod{
                type = 'PATCH',
                args_spec = rest_model:space_modify_request_model()
            }]
        }},

        %% Modify storage details
        {<<"/api/v3/onepanel/provider/storages/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = storage,
            methods = [#rmethod{
                type = 'PATCH',
                %% New values for storage configuration parameters which should
                %% be changed.
                args_spec = rest_model:storage_modify_request_model()
            }]
        }},

        %% Unregister provider
        {<<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{
                type = 'DELETE'
            }]
        }},

        %% Revoke space support for a space
        {<<"/api/v3/onepanel/provider/spaces/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = space,
            methods = [#rmethod{
                type = 'DELETE'
            }]
        }},

        %% Start/stop provider database
        {<<"/api/v3/onepanel/provider/databases/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type = 'PATCH',
                params_spec = #{
                    %% Defines the intended state of the database service. The
                    %% service will be started or stopped in order to match the
                    %% requested state.
                    started => {boolean, {optional, true}}
                }
            }]
        }},

        %% Start/stop provider databases
        {<<"/api/v3/onepanel/provider/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type = 'PATCH',
                params_spec = #{
                    %% Defines the intended state of the database service. The
                    %% service will be started or stopped in order to match the
                    %% requested state.
                    started => {boolean, {optional, true}}
                }
            }]
        }},

        %% Start/stop provider cluster manager
        {<<"/api/v3/onepanel/provider/managers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type = 'PATCH',
                params_spec = #{
                    %% Defines the intended state of the cluster manager
                    %% service. The service  will be started or stopped in order
                    %% to match the requested state.
                    started => {boolean, {optional, true}}
                }
            }]
        }},

        %% Start/stop provider cluster managers
        {<<"/api/v3/onepanel/provider/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type = 'PATCH',
                params_spec = #{
                    %% Defines the intended state of the cluster manager
                    %% service. The service will be started or stopped in order
                    %% to match the requested state.
                    started => {boolean, {optional, true}}
                }
            }]
        }},

        %% Start/stop provider cluster worker
        {<<"/api/v3/onepanel/provider/workers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{
                type = 'PATCH',
                params_spec = #{
                    %% Defines the intended state of the cluster worker service.
                    %% The service will  be started or stopped in order to match
                    %% the requested state.
                    started => {boolean, {optional, true}}
                }
            }]
        }},

        %% Start/stop provider cluster workers
        {<<"/api/v3/onepanel/provider/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{
                type = 'PATCH',
                params_spec = #{
                    %% Defines the intended state of the cluster worker service.
                    %% The service  will be started or stopped in order to match
                    %% the requested state.
                    started => {boolean, {optional, true}}
                }
            }]
        }},

        %% Create or support space
        {<<"/api/v3/onepanel/provider/spaces">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = spaces,
            methods = [#rmethod{
                type = 'POST',
                %% Specification of the space support request including name of
                %% the space, size and support token.
                args_spec = rest_model:space_support_request_model()
            }]
        }}

    ].