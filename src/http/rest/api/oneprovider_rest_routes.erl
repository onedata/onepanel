%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for oneprovider.
%%% @end
%%%--------------------------------------------------------------------
-module(oneprovider_rest_routes).
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
        %% Add managers to ceph cluster
        {<<"/provider/ceph/managers">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = managers,
                scope = private
            },
            %% Object with a list of Ceph manager configurations.
            data_spec = (rest_model:ceph_managers_model())
        }},

        %% Add monitors to Ceph cluster
        {<<"/provider/ceph/monitors">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = monitors,
                scope = private
            },
            %% List of Ceph monitor specifications.
            data_spec = (rest_model:ceph_monitors_model())
        }},

        %% Add OSDs to Ceph cluster
        {<<"/provider/ceph/osds">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = osds,
                scope = private
            },
            %% List of OSD specifications.
            data_spec = (rest_model:ceph_osds_model())
        }},

        %% Register provider
        {<<"/provider">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = instance,
                scope = private
            },
            %% The new provider details.
            data_spec = (rest_model:provider_register_request_model())
        }},

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

        %% Add storage
        {<<"/provider/storages">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_storage,
                id = undefined,
                aspect = instances,
                scope = private
            },
            %% The configuration details of storage resources to be added to the
            %% provider deployment.
            data_spec = (rest_model:storage_create_request_model())
        }},

        %% Cancels space auto-cleaning
        {<<"/provider/spaces/:id/auto-cleaning/cancel">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = cancel_auto_cleaning,
                scope = private
            }
        }},

        %% Configure Ceph cluster
        {<<"/provider/ceph">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = cluster,
                scope = private
            },
            %% The Ceph cluster specification.
            data_spec = (rest_model:ceph_cluster_model())
        }},

        %% Configure file-popularity mechanism in the space.
        {<<"/provider/spaces/:id/file-popularity/configuration">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = file_popularity_configuration,
                scope = private
            },
            %% Configuration of the file-popularity mechanism in the space.
            data_spec = (rest_model:space_file_popularity_configuration_model())
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

        %% Configure space auto-cleaning mechanism
        {<<"/provider/spaces/:id/auto-cleaning/configuration">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_cleaning_configuration,
                scope = private
            },
            produces = [<<"application/json">>],
            %% New configuration of space auto-cleaning mechanism.
            data_spec = (rest_model:space_auto_cleaning_configuration_model())
        }},

        %% Get information about auto storage import scan.
        {<<"/provider/spaces/:id/storage-import/auto/info">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_storage_import_info,
                scope = private
            },
            produces = [<<"application/json">>],
            data_spec = #{
                %% Predefined time period for which the statistics should be
                %% fetched.
                period => {string, optional},
                %% Specify which statistic metrics should be returned - strings
                %% delimited with comma.
                metrics => {string, optional}
            }
        }},

        %% Get statistics of auto storage import mechanism.
        {<<"/provider/spaces/:id/storage-import/auto/stats">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_storage_import_stats,
                scope = private
            },
            produces = [<<"application/json">>],
            data_spec = #{
                %% Predefined time period for which the statistics should be
                %% fetched.
                period => {string, optional},
                %% Specify which statistic metrics should be returned - strings
                %% delimited with comma. Accepted values are:
                %% `queueLength`, `importCount`,
                %% `updateCount`, `deleteCount`
                metrics => {string, optional}
            }
        }},

        %% Get block devices for Ceph OSD
        {<<"/provider/ceph/preflight/block_devices">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = block_devices,
                scope = private
            },
            produces = [<<"application/json">>],
            data_spec = #{
                %% Host for which block devices should be returned.
                host => string
            }
        }},

        %% Get Ceph manager
        {<<"/provider/ceph/managers/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {manager, ?BINDING(id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% List Ceph managers
        {<<"/provider/ceph/managers">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = managers,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ceph monitor
        {<<"/provider/ceph/monitors/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {monitor, ?BINDING(id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% List Ceph monitors
        {<<"/provider/ceph/monitors">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = monitors,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ceph OSD
        {<<"/provider/ceph/osds/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {osd, ?BINDING(id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get storage space usage details for specific OSD.
        {<<"/provider/ceph/osds/:id/usage">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {osd_usage, ?BINDING(id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ceph OSDs list.
        {<<"/provider/ceph/osds">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = osds,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get global Ceph params
        {<<"/provider/ceph">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = global_params,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get details of a Ceph pool.
        {<<"/provider/ceph/pools/:name">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {pool, ?BINDING(name)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get storage space usage details for specific pool.
        {<<"/provider/ceph/pools/:name/usage">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {pool_usage, ?BINDING(name)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% List Ceph pools
        {<<"/provider/ceph/pools">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = pools,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ceph cluster health
        {<<"/provider/ceph/status">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = status,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ceph storage space usage.
        {<<"/provider/ceph/usage">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = usage,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get file-popularity configuration
        {<<"/provider/spaces/:id/file-popularity/configuration">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = file_popularity_configuration,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Onezone information
        {<<"/provider/onezone_info">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>],
            data_spec = #{
                %% Oneprovider registration token obtained from Onezone.
                %% Required if the Oneprovider is not registered.
                token => {string, optional}
            }
        }},

        %% Get provider details
        {<<"/provider">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>]
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

        %% Get the report from a space auto-cleaning run
        {<<"/provider/spaces/:id/auto-cleaning/reports/:report_id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = {auto_cleaning_report, ?BINDING(report_id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ids of of the space auto-cleaning reports
        {<<"/provider/spaces/:id/auto-cleaning/reports">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_cleaning_reports_list,
                scope = private
            },
            produces = [<<"application/json">>],
            data_spec = #{
                %% Allows to skip N first report Ids.
                offset => {integer, {optional, 0}},
                %% Allows to limit the number of returned report Ids up to N
                %% last reports. By default, all report Ids will be returned.
                limit => {integer, optional},
                %% Allows to list the report Ids starting from the specific
                %% report.
                index => {string, optional}
            }
        }},

        %% Get status of space auto-cleaning mechanism
        {<<"/provider/spaces/:id/auto-cleaning/status">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_cleaning_status,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get provider spaces
        {<<"/provider/spaces">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = list,
                scope = private
            },
            produces = [<<"application/json">>]
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

        %% Get space auto-cleaning configuration
        {<<"/provider/spaces/:id/auto-cleaning/configuration">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_cleaning_configuration,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get space details
        {<<"/provider/spaces/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get storage details
        {<<"/provider/storages/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get storages
        {<<"/provider/storages">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = undefined,
                aspect = list,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get transfers mock status
        {<<"/provider/debug/transfers_mock">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = transfers_mock,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Modify pool params
        {<<"/provider/ceph/pools/:name">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {pool, ?BINDING(name)},
                scope = private
            },
            data_spec = (rest_model:ceph_pool_model())
        }},

        %% Modify provider details
        {<<"/provider">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = instance,
                scope = private
            },
            %% New values for provider configuration parameters which should be
            %% changed.
            data_spec = (rest_model:provider_modify_request_model())
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

        %% Modify space details
        {<<"/provider/spaces/:id">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = support,
                scope = private
            },
            data_spec = (rest_model:space_modify_request_model())
        }},

        %% Modify storage config
        {<<"/provider/storages/:id">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            },
            %% New values for storage configuration parameters which should be
            %% changed. Must contain the current type of the storage.
            data_spec = (rest_model:storage_modify_request_model())
        }},

        %% Modify transfers mock
        {<<"/provider/debug/transfers_mock">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = transfers_mock,
                scope = private
            },
            %% New value for the mock setting.
            data_spec = (rest_model:transfers_mock_model())
        }},

        %% Unregister provider
        {<<"/provider">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = instance,
                scope = private
            }
        }},

        %% Remove storage
        {<<"/provider/storages/:id">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            }
        }},

        %% Revoke space support for a space
        {<<"/provider/spaces/:id">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = support,
                scope = private
            }
        }},

        %% Start auto storage import scan
        {<<"/provider/spaces/:id/storage-import/auto/start">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = start_auto_storage_import_scan,
                scope = private
            }
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
        }},

        %% Stop auto storage import scan
        {<<"/provider/spaces/:id/storage-import/auto/stop">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = stop_auto_storage_import_scan,
                scope = private
            }
        }},

        %% Support space
        {<<"/provider/spaces">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_space,
                id = undefined,
                aspect = support,
                scope = private
            },
            %% Specification of the space support request including support size
            %% and token.
            data_spec = (rest_model:space_support_request_model())
        }},

        %% Triggers space auto-cleaning
        {<<"/provider/spaces/:id/auto-cleaning/start">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = start_auto_cleaning,
                scope = private
            }
        }}

    ].
