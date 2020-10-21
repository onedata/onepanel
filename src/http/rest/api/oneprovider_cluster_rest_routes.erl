%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for oneprovider_cluster.
%%% @end
%%%--------------------------------------------------------------------
-module(oneprovider_cluster_rest_routes).
-author("Wojciech Geisler").

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
    [{Path :: binary(), #rest_req{}}].
routes() ->
    [
        %% Deploy provider databases
        {<<"/provider/databases">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = couchbase_instances,
                scope = private
            },
            %% The service hosts configuration where databases should be
            %% deployed.
            data_spec = (rest_model:service_databases_model())
        }},

        %% Add provider cluster managers
        {<<"/provider/managers">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = cluster_manager_instances,
                scope = private
            },
            %% The cluster manager service hosts configuration.
            data_spec = (rest_model:manager_hosts_model())
        }},

        %% Add provider cluster workers
        {<<"/provider/workers">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = op_worker_instances,
                scope = private
            },
            %% The service hosts configuration where workers should be deployed.
            data_spec = (rest_model:service_hosts_model())
        }},

        %% Configure provider deployment
        {<<"/provider/configuration">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = cluster,
                scope = private
            },
            %% The provider configuration description.
            data_spec = (rest_model:provider_configuration_model())
        }},

        %% Get provider cluster nodes IPs
        {<<"/provider/cluster_ips">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_host,
                id = undefined,
                aspect = external_ips,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get provider cluster configuration
        {<<"/provider/configuration">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = cluster,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get provider database status
        {<<"/provider/databases/:host">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = ?BINDING(host),
                aspect = {host_status, <<"couchbase">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get provider databases status
        {<<"/provider/databases">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = {all_hosts_status, <<"couchbase">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get provider cluster manager status
        {<<"/provider/managers/:host">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = ?BINDING(host),
                aspect = {host_status, <<"cluster_manager">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get provider cluster managers status
        {<<"/provider/managers">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = {all_hosts_status, <<"cluster_manager">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get provider nagios report
        {<<"/provider/nagios">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = {nagios, <<"op_worker">>},
                scope = private
            },
            produces = [<<"text/xml">>]
        }},

        %% Get provider cluster worker status
        {<<"/provider/workers/:host">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = ?BINDING(host),
                aspect = {host_status, <<"op_worker">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get provider cluster workers status
        {<<"/provider/workers">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = {all_hosts_status, <<"op_worker">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Set external IPs of nodes in application config
        {<<"/provider/cluster_ips">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_host,
                id = undefined,
                aspect = external_ips,
                scope = private
            },
            %% The provider configuration description.
            data_spec = (rest_model:modify_cluster_ips_model())
        }},

        %% Start/stop provider database
        {<<"/provider/databases/:host">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_service,
                id = ?BINDING(host),
                aspect = {start_stop, <<"couchbase">>},
                scope = private
            },
            data_spec = #{
                %% Defines the intended state of the database service. The
                %% service will be started or stopped in order to match the
                %% requested state.
                started => {boolean, {optional, true}}
            }
        }},

        %% Start/stop provider databases
        {<<"/provider/databases">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = {start_stop_all, <<"couchbase">>},
                scope = private
            },
            data_spec = #{
                %% Defines the intended state of the database service. The
                %% service will be started or stopped in order to match the
                %% requested state.
                started => {boolean, {optional, true}}
            }
        }},

        %% Start/stop provider cluster manager
        {<<"/provider/managers/:host">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_service,
                id = ?BINDING(host),
                aspect = {start_stop, <<"cluster_manager">>},
                scope = private
            },
            data_spec = #{
                %% Defines the intended state of the cluster manager service.
                %% The service will be started or stopped in order to match the
                %% requested state.
                started => {boolean, {optional, true}}
            }
        }},

        %% Start/stop provider cluster managers
        {<<"/provider/managers">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = {start_stop_all, <<"cluster_manager">>},
                scope = private
            },
            data_spec = #{
                %% Defines the intended state of the cluster manager service.
                %% The service will be started or stopped in order to match the
                %% requested state.
                started => {boolean, {optional, true}}
            }
        }},

        %% Start/stop provider cluster worker
        {<<"/provider/workers/:host">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_service,
                id = ?BINDING(host),
                aspect = {start_stop, <<"op_worker">>},
                scope = private
            },
            data_spec = #{
                %% Defines the intended state of the cluster worker service. The
                %% service will be started or stopped in order to match the
                %% requested state.
                started => {boolean, {optional, true}}
            }
        }},

        %% Start/stop provider cluster workers
        {<<"/provider/workers">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = {start_stop_all, <<"op_worker">>},
                scope = private
            },
            data_spec = #{
                %% Defines the intended state of the cluster worker service. The
                %% service will be started or stopped in order to match the
                %% requested state.
                started => {boolean, {optional, true}}
            }
        }}

    ].
