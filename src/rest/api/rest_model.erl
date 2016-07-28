%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains REST models definitions.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_model).
-author("Krzysztof Trzepla").

-export([
    ceph_model/0,
    cluster_databases_model/0,
    cluster_managers_model/0,
    cluster_storages_model/0,
    cluster_storages_list_model/0,
    cluster_workers_model/0,
    error_model/0,
    manager_hosts_model/0,
    posix_model/0,
    provider_cluster_configuration_model/0,
    provider_configuration_model/0,
    provider_configuration_oneprovider_model/0,
    provider_configuration_onezone_model/0,
    provider_details_model/0,
    provider_modify_request_model/0,
    provider_register_request_model/0,
    s3_model/0,
    service_error_model/0,
    service_hosts_model/0,
    service_status_model/0,
    service_status_host_model/0,
    space_details_model/0,
    space_support_request_model/0,
    task_status_model/0,
    user_create_request_model/0,
    user_details_model/0,
    user_modify_request_model/0,
    zone_cluster_configuration_model/0,
    zone_cluster_configuration_nodes_model/0,
    zone_configuration_model/0,
    zone_configuration_onezone_model/0
]).


-spec ceph_model() -> #{}.
ceph_model() ->
    #{
        %% The type of a storage.
        type => {equal, <<"ceph">>},
        %% The username for authentication to Ceph cluster.
        username => string,
        %% The key to access the Ceph cluster.
        key => string,
        %% The monitor host name.
        monitorHostname => string,
        %% The Ceph cluster name.
        clusterName => string,
        %% The Ceph pool name.
        poolName => string
    }.

%%--------------------------------------------------------------------
%% @doc The cluster database service configuration.
%%--------------------------------------------------------------------
-spec cluster_databases_model() -> #{}.
cluster_databases_model() ->
    #{
        %% The list of aliases of cluster database nodes.
        nodes => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The cluster manager service configuration.
%%--------------------------------------------------------------------
-spec cluster_managers_model() -> #{}.
cluster_managers_model() ->
    #{
        %% The alias of the main cluster manager node.
        mainNode => string,
        %% The list of aliases of cluster manager nodes.
        nodes => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The cluster storage configuration.
%%--------------------------------------------------------------------
-spec cluster_storages_model() -> {oneof, Oneof :: list()}.
cluster_storages_model() ->
    {oneof, [posix_model(),s3_model(),ceph_model()]}.

%%--------------------------------------------------------------------
%% @doc The list of supported storage types.
%%--------------------------------------------------------------------
-spec cluster_storages_list_model() -> #{}.
cluster_storages_list_model() ->
    #{ '_' => cluster_storages_model() }.

%%--------------------------------------------------------------------
%% @doc The cluster worker service configuration.
%%--------------------------------------------------------------------
-spec cluster_workers_model() -> #{}.
cluster_workers_model() ->
    #{
        %% The list of aliases of cluster worker nodes.
        nodes => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The generic error model for REST requests.
%%--------------------------------------------------------------------
-spec error_model() -> #{}.
error_model() ->
    #{
        %% The name of an error type.
        error => string,
        %% The detailed error description.
        description => string
    }.

%%--------------------------------------------------------------------
%% @doc The cluster manager service hosts configuration.
%%--------------------------------------------------------------------
-spec manager_hosts_model() -> #{}.
manager_hosts_model() ->
    #{
        %% The name of a host where main cluster manager node should be configured.
        %% Main cluster manager node is responsible for monitoring cluster worker
        %% nodes. Other nodes, called optional, are suspended. In case of main cluster
        %% manager node failure one of optional nodes is resumed and it takes over
        %% main node responsibilities.
        mainHost => string,
        %% The list of hosts where service should be deployed.
        hosts => [string]
    }.

-spec posix_model() -> #{}.
posix_model() ->
    #{
        %% The type of a storage.
        type => {equal, <<"posix">>},
        %% The absolute path to the directory where the POSIX storage is mounted
        %% on the cluster nodes.
        mountPoint => string
    }.

%%--------------------------------------------------------------------
%% @doc The provider cluster configuration.
%%--------------------------------------------------------------------
-spec provider_cluster_configuration_model() -> #{}.
provider_cluster_configuration_model() ->
    #{
        %% The name of a domain common for all services in the cluster. Together with
        %% a node hostname constitutes a fully qualified domain name (FDQN) of the node.
        domainName => string,
        %% The collection of nodes aliases associated with nodes properties.
        nodes => #{ '_' => zone_cluster_configuration_nodes_model()},
        databases => cluster_databases_model(),
        managers => cluster_managers_model(),
        workers => cluster_workers_model(),
        %% The cluster storage configuration.
        storages => { #{ '_' => cluster_storages_model()}, optional }
    }.

%%--------------------------------------------------------------------
%% @doc The provider deployment configuration.
%%--------------------------------------------------------------------
-spec provider_configuration_model() -> #{}.
provider_configuration_model() ->
    #{
        cluster => provider_cluster_configuration_model(),
        oneprovider => { provider_configuration_oneprovider_model(), optional },
        onezone => { provider_configuration_onezone_model(), optional }
    }.

%%--------------------------------------------------------------------
%% @doc The provider custom configuration.
%%--------------------------------------------------------------------
-spec provider_configuration_oneprovider_model() -> #{}.
provider_configuration_oneprovider_model() ->
    #{
        %% Defines whether the provider should be registered in a zone.
        register => boolean,
        %% The address used for user redirection from a zone to the provider.
        redirectionPoint => string,
        %% The geographical longitude of the provider.
        geoLongitude => { float, optional },
        %% The name under which the provider will be registered in a zone.
        name => string,
        %% The geographical latitude of the provider.
        geoLatitude => { float, optional }
    }.

%%--------------------------------------------------------------------
%% @doc The zone custom configuration.
%%--------------------------------------------------------------------
-spec provider_configuration_onezone_model() -> #{}.
provider_configuration_onezone_model() ->
    #{
        %% The domain name of a zone where provider will be registered.
        domainName => string
    }.

%%--------------------------------------------------------------------
%% @doc The provider configuration details.
%%--------------------------------------------------------------------
-spec provider_details_model() -> #{}.
provider_details_model() ->
    #{
        %% The ID assigned by a zone.
        id => string,
        %% The name under which the provider has been registered in a zone.
        name => string,
        %% The list of IP addresses of provider cluster worker nodes.
        urls => [string],
        %% The address used for user redirection from a zone to the provider.
        redirectionPoint => string,
        %% The geographical longitude of the provider.
        geoLongitude => float,
        %% The geographical latitude of the provider.
        geoLatitude => float
    }.

%%--------------------------------------------------------------------
%% @doc The provider configuration details that can be modified.
%%--------------------------------------------------------------------
-spec provider_modify_request_model() -> #{}.
provider_modify_request_model() ->
    #{
        %% The name under which the provider has been registered in a zone.
        name => { string, optional },
        %% The address used for user redirection from a zone to the provider.
        redirectionPoint => { string, optional },
        %% The geographical longitude of the provider.
        geoLongitude => { float, optional },
        %% The geographical latitude of the provider.
        geoLatitude => { float, optional }
    }.

%%--------------------------------------------------------------------
%% @doc The provider configuration details required for registration process.
%%--------------------------------------------------------------------
-spec provider_register_request_model() -> #{}.
provider_register_request_model() ->
    #{
        %% The name under which the provider should be registered in a zone.
        name => string,
        %% The address used for user redirection from a zone to the provider.
        redirectionPoint => string,
        %% The geographical longitude of the provider.
        geoLongitude => { float, optional },
        %% The geographical latitude of the provider.
        geoLatitude => { float, optional },
        %% The domain name of a zone where provider will be registered.
        onezoneDomainName => { string, optional }
    }.

-spec s3_model() -> #{}.
s3_model() ->
    #{
        %% The type of a storage.
        type => {equal, <<"s3">>},
        %% The hostname of a machine where S3 storage is installed.
        s3Hostname => string,
        %% The IAM hostname for the S3 storage.
        iamHostname => string,
        %% The storage bucket name.
        bucketName => string,
        %% The access key to the S3 storage.
        accessKey => string,
        %% The secret key to the S3 storage.
        secretKey => string
    }.

%%--------------------------------------------------------------------
%% @doc The service error model for REST requests.
%%--------------------------------------------------------------------
-spec service_error_model() -> #{}.
service_error_model() ->
    #{
        %% The collection of hosts with associated error description.
        hosts => #{ '_' => error_model()}
    }.

%%--------------------------------------------------------------------
%% @doc The service hosts configuration.
%%--------------------------------------------------------------------
-spec service_hosts_model() -> #{}.
service_hosts_model() ->
    #{
        %% The list of hosts where service should be deployed.
        hosts => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The generic model for service status.
%%--------------------------------------------------------------------
-spec service_status_model() -> #{}.
service_status_model() ->
    #{
        %% The collection of services with associated status information.
        services => #{ '_' => #{ '_' => service_status_host_model()}}
    }.

%%--------------------------------------------------------------------
%% @doc The service status.
%%--------------------------------------------------------------------
-spec service_status_host_model() -> #{}.
service_status_host_model() ->
    #{
        %% The service status.
        status => string
    }.

%%--------------------------------------------------------------------
%% @doc The space details.
%%--------------------------------------------------------------------
-spec space_details_model() -> #{}.
space_details_model() ->
    #{
        %% The ID of the space.
        spaceId => string,
        %% The name of the space.
        name => string,
        %% The collection of provider IDs with associated supported storage space in bytes.
        supportingProviders => #{ '_' => integer}
    }.

%%--------------------------------------------------------------------
%% @doc The configuration details required to create or support a space by a provider.
%%--------------------------------------------------------------------
-spec space_support_request_model() -> #{}.
space_support_request_model() ->
    #{
        %% The space name. If this property is provided and space with given name
        %% will be created and automatically supported by a provider.
        name => { string, optional },
        %% The token for space creation or support.
        token => string,
        %% The storage space size in bytes that provider is willing to assign to the space.
        size => integer,
        %% The name of the storage resource where the space data should be stored.
        %% To be used interchangeably with `storageId`.
        storageName => { string, optional },
        %% The ID of the storage resource where the space data should be stored.
        %% To be used interchangeably with `storageName`.
        storageId => { string, optional }
    }.

%%--------------------------------------------------------------------
%% @doc The result of a scheduled operation, e.g. database service configuration.
%%--------------------------------------------------------------------
-spec task_status_model() -> #{}.
task_status_model() ->
    #{
        %% The operation status.
        status => string,
        %% The list of operation steps that has been so far executed.
        steps => [string],
        %% The collection of hosts with associated error description. This property
        %% is set only when an error occurred during operation execution, i.e. the
        %% value of property 'status' is set to 'error'.
        hosts => { #{ '_' => error_model()}, optional }
    }.

%%--------------------------------------------------------------------
%% @doc The user configuration details required for creation process.
%%--------------------------------------------------------------------
-spec user_create_request_model() -> #{}.
user_create_request_model() ->
    #{
        %% The user name. It must be at least 4 characters long and contain only
        %% alphanumeric characters [a-zA-Z0-9].
        username => string,
        %% The user password. It must be at least 8 characters long and contain
        %% a minimum of 1 lower case letter [a-z] and a minimum of 1 upper case
        %% letter [A-Z] and a minimum of 1 numeric character [0-9]. The Password must
        %% not contain a colon character [:].
        password => string,
        %% The user role, one of 'admin' or 'regular'.
        userRole => atom
    }.

%%--------------------------------------------------------------------
%% @doc The user configuration details.
%%--------------------------------------------------------------------
-spec user_details_model() -> #{}.
user_details_model() ->
    #{
        %% The user ID.
        userId => string,
        %% The user role, one of 'admin' or 'regular'.
        userRole => atom
    }.

%%--------------------------------------------------------------------
%% @doc The user configuration details that can be modified.
%%--------------------------------------------------------------------
-spec user_modify_request_model() -> #{}.
user_modify_request_model() ->
    #{
        %% The user password.
        password => string
    }.

%%--------------------------------------------------------------------
%% @doc The zone cluster configuration.
%%--------------------------------------------------------------------
-spec zone_cluster_configuration_model() -> #{}.
zone_cluster_configuration_model() ->
    #{
        %% The name of a domain common for all services in the cluster. Together with
        %% a node hostname constitute a node fully qualified domain name.
        domainName => string,
        %% The collection of nodes aliases associated with nodes properties.
        nodes => #{ '_' => zone_cluster_configuration_nodes_model()},
        databases => cluster_databases_model(),
        managers => cluster_managers_model(),
        workers => cluster_workers_model()
    }.

-spec zone_cluster_configuration_nodes_model() -> #{}.
zone_cluster_configuration_nodes_model() ->
    #{
        %% The name of a host.
        hostname => string
    }.

%%--------------------------------------------------------------------
%% @doc The zone deployment configuration.
%%--------------------------------------------------------------------
-spec zone_configuration_model() -> #{}.
zone_configuration_model() ->
    #{
        cluster => zone_cluster_configuration_model(),
        onezone => { zone_configuration_onezone_model(), optional }
    }.

%%--------------------------------------------------------------------
%% @doc The zone custom configuration.
%%--------------------------------------------------------------------
-spec zone_configuration_onezone_model() -> #{}.
zone_configuration_onezone_model() ->
    #{
        %% The name of a zone.
        name => { string, optional },
        %% The name of a HTTP domain.
        domainName => { string, optional }
    }.

