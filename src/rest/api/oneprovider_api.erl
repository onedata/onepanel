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
%% @doc Returns list of path with a handler module and an initial state.
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), Module :: module(), State :: rest_handler:state()}].
routes() -> 
    [
        %% Unregister provider
        { <<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{
                type='DELETE'
            }]
        }},

        %% Revoke space support
        { <<"/api/v3/onepanel/provider/spaces/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = space,
            methods = [#rmethod{
                type='DELETE'
            }]
        }},

        %% Get provider details
        { <<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{
                type='GET'
            }]
        }},

        %% Get provider databases status
        { <<"/api/v3/onepanel/provider/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type='GET'
            }]
        }},

        %% Get provider database status
        { <<"/api/v3/onepanel/provider/databases/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type='GET'
            }]
        }},

        %% Get provider managers
        { <<"/api/v3/onepanel/provider/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type='GET'
            }]
        }},

        %% Get provider manager status
        { <<"/api/v3/onepanel/provider/managers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type='GET'
            }]
        }},

        %% Get provider spaces
        { <<"/api/v3/onepanel/provider/spaces">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = spaces,
            methods = [#rmethod{
                type='GET'
            }]
        }},

        %% Get space details
        { <<"/api/v3/onepanel/provider/spaces/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = space,
            methods = [#rmethod{
                type='GET'
            }]
        }},

        %% Get storages
        { <<"/api/v3/onepanel/provider/storages">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = storages,
            methods = [#rmethod{
                type='GET'
            }]
        }},

        %% Get storage details
        { <<"/api/v3/onepanel/provider/storages/:name">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = storage,
            methods = [#rmethod{
                type='GET'
            }]
        }},

        %% Get provider workers
        { <<"/api/v3/onepanel/provider/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{
                type='GET'
            }]
        }},

        %% Get cluster worker status
        { <<"/api/v3/onepanel/provider/workers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{
                type='GET'
            }]
        }},

        %% Modify provider details
        { <<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{
                type='PATCH',
                args_spec = rest_model:provider_modify_request_model()
            }]
        }},

        %% Start/stop provider databases
        { <<"/api/v3/onepanel/provider/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type='PATCH',
                params_spec = #{
                    %% Defines the intended state of the database service. The service will be
                    %% started or stopped in order to match the requested state.
                    started => { boolean, {optional, true } }
                }
            }]
        }},

        %% Start/stop provider database
        { <<"/api/v3/onepanel/provider/databases/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type='PATCH',
                params_spec = #{
                    %% Defines the intended state of the database service. The service will be
                    %% started or stopped in order to match the requested state.
                    started => { boolean, {optional, true } }
                }
            }]
        }},

        %% Start/stop provider managers
        { <<"/api/v3/onepanel/provider/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type='PATCH',
                params_spec = #{
                    %% Defines the intended state of the cluster manager service. The service will be
                    %% started or stopped in order to match the requested state.
                    started => { boolean, {optional, true } }
                }
            }]
        }},

        %% Start/stop provider manager
        { <<"/api/v3/onepanel/provider/managers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type='PATCH',
                params_spec = #{
                    %% Defines the intended state of the cluster manager service. The service will be
                    %% started or stopped in order to match the requested state.
                    started => { boolean, {optional, true } }
                }
            }]
        }},

        %% Start/stop cluster worker service
        { <<"/api/v3/onepanel/provider/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{
                type='PATCH',
                params_spec = #{
                    %% Defines the intended state of the cluster worker service. The service will be
                    %% started or stopped in order to match the requested state.
                    started => { boolean, {optional, true } }
                }
            }]
        }},

        %% Start/stop provider workers
        { <<"/api/v3/onepanel/provider/workers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{
                type='PATCH',
                params_spec = #{
                    %% Defines the intended state of the cluster worker service. The service will be
                    %% started or stopped in order to match the requested state.
                    started => { boolean, {optional, true } }
                }
            }]
        }},

        %% Register provider
        { <<"/api/v3/onepanel/provider">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = provider,
            methods = [#rmethod{
                type='PUT',
                args_spec = rest_model:provider_register_request_model()
            }]
        }},

        %% Configure provider deployment
        { <<"/api/v3/onepanel/provider/configuration">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oneprovider,
            methods = [#rmethod{
                type='PUT',
                args_spec = rest_model:provider_configuration_model()
            }]
        }},

        %% Deploy provider databases
        { <<"/api/v3/onepanel/provider/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type='PUT',
                args_spec = rest_model:service_hosts_model()
            }]
        }},

        %% Deploy provider managers
        { <<"/api/v3/onepanel/provider/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type='PUT',
                args_spec = rest_model:manager_hosts_model()
            }]
        }},

        %% Create or support space
        { <<"/api/v3/onepanel/provider/spaces">>, rest_handler, #rstate{
            version = 3,
            module = rest_oneprovider,
            resource = spaces,
            methods = [#rmethod{
                type='PUT',
                args_spec = rest_model:space_support_request_model()
            }]
        }},

        %% Configure storage
        { <<"/api/v3/onepanel/provider/storages">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = storages,
            methods = [#rmethod{
                type='PUT',
                %% The list of configuration details of storages to be added to the provider
                %% deployment.
                args_spec = rest_model:cluster_storages_list_model()
            }]
        }},

        %% Deploy provider workers
        { <<"/api/v3/onepanel/provider/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_op_worker,
            methods = [#rmethod{
                type='PUT',
                args_spec = rest_model:service_hosts_model()
            }]
        }}

    ].