%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for onezone.
%%% @end
%%%--------------------------------------------------------------------
-module(onezone_rest_routes).
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
        %% Create Onezone user
        {<<"/zone/users">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_user,
                id = undefined,
                aspect = instance,
                scope = private
            },
            %% The user configuration details.
            data_spec = (rest_model:onezone_user_create_request_model())
        }},

        %% Add zone databases
        {<<"/zone/databases">>, #rest_req{
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

        %% Add zone cluster managers
        {<<"/zone/managers">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = cluster_manager_instances,
                scope = private
            },
            %% The hosts specification where cluster managers should be
            %% deployed.
            data_spec = (rest_model:manager_hosts_model())
        }},

        %% Add zone cluster workers
        {<<"/zone/workers">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = oz_worker_instances,
                scope = private
            },
            %% The hosts specification where the workers should be deployed.
            data_spec = (rest_model:service_hosts_model())
        }},

        %% Set password for Onezone user
        {<<"/zone/users/:id">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_user,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            },
            data_spec = (rest_model:password_change_request_model())
        }},

        %% Configure zone deployment
        {<<"/zone/configuration">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = cluster,
                scope = private
            },
            %% The zone configuration description.
            data_spec = (rest_model:zone_configuration_model())
        }},

        %% Get settings of a Onezone GUI message.
        {<<"/zone/gui_messages/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = {gui_message, ?BINDING(id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Onezone user details
        {<<"/zone/users/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_user,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% List Onezone users
        {<<"/zone/users">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_user,
                id = undefined,
                aspect = list,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get zone cluster nodes IPs
        {<<"/zone/cluster_ips">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_host,
                id = undefined,
                aspect = external_ips,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get zone cluster configuration
        {<<"/zone/configuration">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = cluster,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get zone database status
        {<<"/zone/databases/:host">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = ?BINDING(host),
                aspect = {host_status, <<"couchbase">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get zone databases status
        {<<"/zone/databases">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = {all_hosts_status, <<"couchbase">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get zone cluster manager status
        {<<"/zone/managers/:host">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = ?BINDING(host),
                aspect = {host_status, <<"cluster_manager">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get zone cluster managers status
        {<<"/zone/managers">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = ?BINDING(host),
                aspect = {all_hosts_status, <<"cluster_manager">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get zone nagios report
        {<<"/zone/nagios">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = {nagios, <<"oz_worker">>},
                scope = private
            },
            produces = [<<"text/xml">>]
        }},

        %% Get Onezone policies.
        {<<"/zone/policies">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = policies,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get zone cluster worker status
        {<<"/zone/workers/:host">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = ?BINDING(host),
                aspect = {host_status, <<"oz_worker">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get zone cluster workers status
        {<<"/zone/workers">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = {all_hosts_status, <<"oz_worker">>},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Modify settings of a Onezone GUI message.
        {<<"/zone/gui_messages/:id">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = {gui_message, ?BINDING(id)},
                scope = private
            },
            data_spec = (rest_model:gui_message_model())
        }},

        %% Set external IPs of nodes in application config
        {<<"/zone/cluster_ips">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_host,
                id = undefined,
                aspect = external_ips,
                scope = private
            },
            %% The zone configuration description.
            data_spec = (rest_model:modify_cluster_ips_model())
        }},

        %% Modify current Onezone policies
        {<<"/zone/policies">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = policies,
                scope = private
            },
            %% New values for Onezone policies.
            data_spec = (rest_model:zone_policies_model())
        }},

        %% Start/stop zone databases
        {<<"/zone/databases">>, #rest_req{
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

        %% Start/stop zone database
        {<<"/zone/databases/:host">>, #rest_req{
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

        %% Start/stop zone cluster manager
        {<<"/zone/managers/:host">>, #rest_req{
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

        %% Start/stop zone cluster managers
        {<<"/zone/managers">>, #rest_req{
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

        %% Start/stop zone cluster worker
        {<<"/zone/workers/:host">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_service,
                id = ?BINDING(host),
                aspect = {start_stop, <<"oz_worker">>},
                scope = private
            },
            data_spec = #{
                %% Defines the intended state of the cluster worker service. The
                %% service will be started or stopped in order to match the
                %% requested state.
                started => {boolean, {optional, true}}
            }
        }},

        %% Start/stop zone cluster workers
        {<<"/zone/workers">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_service,
                id = undefined,
                aspect = {start_stop_all, <<"oz_worker">>},
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
