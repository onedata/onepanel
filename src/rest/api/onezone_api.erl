%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for onezone.
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
%% @doc Returns a list of tuples consisting of a path, a handler module and
%% an initial request state.
%% @end
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), Module :: module(), State :: rest_handler:state()}].
routes() ->
    [
        %% Get zone cluster configuration
        {<<"/api/v3/onepanel/zone/configuration">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_onezone,
            methods = [#rmethod{
                type = 'GET',
                noauth = true
            }]
        }},

        %% Get zone databases status
        {<<"/api/v3/onepanel/zone/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get zone database status
        {<<"/api/v3/onepanel/zone/databases/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get zone cluster managers status
        {<<"/api/v3/onepanel/zone/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get zone cluster manager status
        {<<"/api/v3/onepanel/zone/managers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get zone cluster workers status
        {<<"/api/v3/onepanel/zone/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oz_worker,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get zone cluster worker status
        {<<"/api/v3/onepanel/zone/workers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oz_worker,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Start/stop zone databases
        {<<"/api/v3/onepanel/zone/databases">>, rest_handler, #rstate{
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

        %% Start/stop zone database
        {<<"/api/v3/onepanel/zone/databases/:host">>, rest_handler, #rstate{
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

        %% Start/stop zone cluster managers
        {<<"/api/v3/onepanel/zone/managers">>, rest_handler, #rstate{
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

        %% Start/stop zone cluster manager
        {<<"/api/v3/onepanel/zone/managers/:host">>, rest_handler, #rstate{
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

        %% Start/stop zone cluster workers
        {<<"/api/v3/onepanel/zone/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oz_worker,
            methods = [#rmethod{
                type = 'PATCH',
                params_spec = #{
                    %% Defines the intended state of the cluster worker service.
                    %% The service will be started or stopped in order to match
                    %% the requested state.
                    started => {boolean, {optional, true}}
                }
            }]
        }},

        %% Start/stop zone cluster worker
        {<<"/api/v3/onepanel/zone/workers/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oz_worker,
            methods = [#rmethod{
                type = 'PATCH',
                params_spec = #{
                    %% Defines the intended state of the cluster worker service.
                    %% The service will be started or stopped in order to match
                    %% the requested state.
                    started => {boolean, {optional, true}}
                }
            }]
        }},

        %% Configure zone deployment
        {<<"/api/v3/onepanel/zone/configuration">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_onezone,
            methods = [#rmethod{
                type = 'POST',
                args_spec = rest_model:zone_configuration_model(),
                noauth = true
            }]
        }},

        %% Deploy zone databases
        {<<"/api/v3/onepanel/zone/databases">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_couchbase,
            methods = [#rmethod{
                type = 'POST',
                args_spec = rest_model:service_databases_model()
            }]
        }},

        %% Deploy zone cluster managers
        {<<"/api/v3/onepanel/zone/managers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_cluster_manager,
            methods = [#rmethod{
                type = 'POST',
                args_spec = rest_model:manager_hosts_model()
            }]
        }},

        %% Deploy zone cluster workers
        {<<"/api/v3/onepanel/zone/workers">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = service_oz_worker,
            methods = [#rmethod{
                type = 'POST',
                args_spec = rest_model:service_hosts_model()
            }]
        }}

    ].