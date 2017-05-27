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
    cookie_model/0,
    error_model/0,
    manager_hosts_model/0,
    panel_configuration_model/0,
    panel_configuration_users_model/0,
    posix_model/0,
    provider_cluster_configuration_model/0,
    provider_configuration_model/0,
    provider_configuration_oneprovider_model/0,
    provider_configuration_onezone_model/0,
    provider_details_model/0,
    provider_modify_request_model/0,
    provider_register_request_model/0,
    provider_spaces_model/0,
    s3_model/0,
    service_databases_model/0,
    service_error_model/0,
    service_hosts_model/0,
    service_status_model/0,
    service_status_host_model/0,
    space_details_model/0,
    space_support_request_model/0,
    storage_modify_request_model/0,
    swift_model/0,
    task_status_model/0,
    user_create_request_model/0,
    user_details_model/0,
    user_modify_request_model/0,
    zone_cluster_configuration_model/0,
    zone_cluster_configuration_nodes_model/0,
    zone_configuration_model/0,
    zone_configuration_onezone_model/0
]).


%%--------------------------------------------------------------------
%% @doc The Ceph storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec ceph_model() -> maps:map().
ceph_model() ->
    #{
        %% The type of storage.
        type => {equal, <<"ceph">>},
        %% The username of the Ceph cluster administrator.
        username => string,
        %% The admin key to access the Ceph cluster.
        key => string,
        %% The monitor host name.
        monitorHostname => string,
        %% The Ceph cluster name.
        clusterName => string,
        %% The Ceph pool name.
        poolName => string,
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The cluster database service configuration.
%% @end
%%--------------------------------------------------------------------
-spec cluster_databases_model() -> maps:map().
cluster_databases_model() ->
    #{
        %% The list of aliases of cluster database nodes.
        nodes => [string],
        %% The server quota is the RAM memory in bytes that is allocated to the
        %% server when Couchbase Server is first installed. This sets the limit
        %% of RAM allocated by Couchbase for caching data for all buckets and is
        %% configured on a per-node basis.
        serverQuota => {integer, optional},
        %% The bucket quota is the amount of RAM memory in bytes allocated to an
        %% individual bucket for caching data.
        bucketQuota => {integer, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The cluster manager service configuration.
%% @end
%%--------------------------------------------------------------------
-spec cluster_managers_model() -> maps:map().
cluster_managers_model() ->
    #{
        %% The alias of the main cluster manager node.
        mainNode => string,
        %% The list of aliases of cluster manager nodes.
        nodes => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The cluster storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec cluster_storages_model() -> {oneof, Oneof :: list()}.
cluster_storages_model() ->
    {oneof, [posix_model(), s3_model(), ceph_model(), swift_model()]}.

%%--------------------------------------------------------------------
%% @doc The list of supported storage types.
%% @end
%%--------------------------------------------------------------------
-spec cluster_storages_list_model() -> maps:map().
cluster_storages_list_model() ->
    #{'_' => cluster_storages_model()}.

%%--------------------------------------------------------------------
%% @doc The cluster worker service configuration.
%% @end
%%--------------------------------------------------------------------
-spec cluster_workers_model() -> maps:map().
cluster_workers_model() ->
    #{
        %% The list of aliases of cluster worker nodes.
        nodes => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The cookie is a character sequence that is common for all the cluster
%% nodes. If this parameter is not provided, in case of a cluster initialization
%% request, it will be generated, and in case of a cluster extension request the
%% current cookie value will be used. However, if the cluster cookie and the
%% cookie of the host that is about to join the cluster doesn't match there
%% will be a connection error.
%% @end
%%--------------------------------------------------------------------
-spec cookie_model() -> maps:map().
cookie_model() ->
    #{
        %% The cluster cookie.
        cookie => {atom, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The generic error model for REST requests.
%% @end
%%--------------------------------------------------------------------
-spec error_model() -> maps:map().
error_model() ->
    #{
        %% The name of an error type.
        error => string,
        %% The detailed error description.
        description => string
    }.

%%--------------------------------------------------------------------
%% @doc The cluster manager service hosts configuration.
%% @end
%%--------------------------------------------------------------------
-spec manager_hosts_model() -> maps:map().
manager_hosts_model() ->
    #{
        %% The name of a host where main cluster manager node should be
        %% deployed. Main cluster manager node is responsible for monitoring
        %% cluster worker nodes. Other nodes, called optional, are suspended. In
        %% case of main cluster manager node failure one of optional nodes is
        %% resumed and takes over main node responsibilities.
        mainHost => string,
        %% The list of hosts where service should be deployed.
        hosts => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The panel configuration.
%% @end
%%--------------------------------------------------------------------
-spec panel_configuration_model() -> maps:map().
panel_configuration_model() ->
    #{
        %% The collection of user names associated with users properties.
        users => #{'_' => panel_configuration_users_model()}
    }.

-spec panel_configuration_users_model() -> maps:map().
panel_configuration_users_model() ->
    #{
        %% The user role, one of 'admin' or 'regular'.
        userRole => atom,
        %% The user password.
        password => string
    }.

%%--------------------------------------------------------------------
%% @doc The POSIX storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec posix_model() -> maps:map().
posix_model() ->
    #{
        %% The type of storage.
        type => {equal, <<"posix">>},
        %% The absolute path to the directory where the POSIX storage is mounted
        %% on the cluster nodes.
        mountPoint => string,
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The provider cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec provider_cluster_configuration_model() -> maps:map().
provider_cluster_configuration_model() ->
    #{
        %% Defines whether administrative cluster should be created from the
        %% list of provided cluster nodes.
        autoDeploy => {boolean, optional},
        %% The name of a domain common for all services in the cluster. Together
        %% with a node hostname constitutes a fully qualified domain name (FDQN)
        %% of the node.
        domainName => string,
        %% The collection of nodes aliases associated with nodes properties.
        nodes => #{'_' => zone_cluster_configuration_nodes_model()},
        databases => cluster_databases_model(),
        managers => cluster_managers_model(),
        workers => cluster_workers_model(),
        %% The cluster storage configuration.
        storages => {#{'_' => cluster_storages_model()}, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The provider deployment configuration.
%% @end
%%--------------------------------------------------------------------
-spec provider_configuration_model() -> maps:map().
provider_configuration_model() ->
    #{
        cluster => provider_cluster_configuration_model(),
        oneprovider => {provider_configuration_oneprovider_model(), optional},
        onezone => {provider_configuration_onezone_model(), optional},
        onepanel => {panel_configuration_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc The provider custom configuration.
%% @end
%%--------------------------------------------------------------------
-spec provider_configuration_oneprovider_model() -> maps:map().
provider_configuration_oneprovider_model() ->
    #{
        %% Defines whether the provider should be registered in a zone.
        register => boolean,
        %% The address used for user redirection from a zone to the provider.
        redirectionPoint => string,
        %% The geographical longitude of the provider.
        geoLongitude => {float, optional},
        %% The name under which the provider will be registered in a zone.
        name => string,
        %% The geographical latitude of the provider.
        geoLatitude => {float, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The zone custom configuration.
%% @end
%%--------------------------------------------------------------------
-spec provider_configuration_onezone_model() -> maps:map().
provider_configuration_onezone_model() ->
    #{
        %% The domain name of a zone where provider will be registered.
        domainName => string
    }.

%%--------------------------------------------------------------------
%% @doc The provider configuration details.
%% @end
%%--------------------------------------------------------------------
-spec provider_details_model() -> maps:map().
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
%% @end
%%--------------------------------------------------------------------
-spec provider_modify_request_model() -> maps:map().
provider_modify_request_model() ->
    #{
        %% The name under which the provider has been registered in a zone.
        name => {string, optional},
        %% The address used for user redirection from a zone to the provider.
        redirectionPoint => {string, optional},
        %% The geographical longitude of the provider.
        geoLongitude => {float, optional},
        %% The geographical latitude of the provider.
        geoLatitude => {float, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The storage provider configuration details required for registration
%% process.
%% @end
%%--------------------------------------------------------------------
-spec provider_register_request_model() -> maps:map().
provider_register_request_model() ->
    #{
        %% The name under which the provider should be registered in a zone.
        name => string,
        %% The address used for user redirection from a zone to the storage
        %% provider.
        redirectionPoint => string,
        %% The geographical longitude of the storage provider.
        geoLongitude => {float, optional},
        %% The geographical latitude of the storage provider.
        geoLatitude => {float, optional},
        %% The domain name of a zone where this storage provider will be
        %% registered.
        onezoneDomainName => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The provider spaces details.
%% @end
%%--------------------------------------------------------------------
-spec provider_spaces_model() -> maps:map().
provider_spaces_model() ->
    #{
        %% The list of IDs of spaces supported by a provider.
        ids => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The Simple Storage Service configuration.
%% @end
%%--------------------------------------------------------------------
-spec s3_model() -> maps:map().
s3_model() ->
    #{
        %% The type of storage.
        type => {equal, <<"s3">>},
        %% The hostname of a machine where S3 storage is installed.
        hostname => string,
        %% The storage bucket name.
        bucketName => string,
        %% The access key to the S3 storage.
        accessKey => string,
        %% The secret key to the S3 storage.
        secretKey => string,
        %% The version of signature used to sign requests. One of: 2, 4.
        %% Default: 4.
        signatureVersion => {integer, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Storage block size in bytes.
        blockSize => {integer, optional},
        %% Defines whether storage administrator credentials (accessKey and
        %% secretKey) may be used by users without storage accounts to access
        %% storage in direct IO mode.
        insecure => {boolean, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The service hosts configuration.
%% @end
%%--------------------------------------------------------------------
-spec service_databases_model() -> maps:map().
service_databases_model() ->
    #{
        %% The list of hosts where service should be deployed.
        hosts => [string],
        %% The server quota is the RAM memory in bytes that is allocated to the
        %% server when Couchbase Server is first installed. This sets the limit
        %% of RAM allocated by Couchbase for caching data for all buckets and is
        %% configured on a per-node basis.
        serverQuota => {integer, optional},
        %% The bucket quota is the amount of RAM memory in bytes allocated to an
        %% individual bucket for caching data.
        bucketQuota => {integer, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The service error model for REST requests.
%% @end
%%--------------------------------------------------------------------
-spec service_error_model() -> maps:map().
service_error_model() ->
    #{
        %% The name of an error type.
        error => string,
        %% The detailed error description.
        description => string,
        %% The name of a module containing function that returned error.
        module => {string, optional},
        %% The name of a function that returned error.
        function => {string, optional},
        %% The collection of hosts with associated error description.
        hosts => {#{'_' => error_model()}, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The service hosts configuration.
%% @end
%%--------------------------------------------------------------------
-spec service_hosts_model() -> maps:map().
service_hosts_model() ->
    #{
        %% The list of hosts where service should be deployed.
        hosts => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The generic model for service status.
%% @end
%%--------------------------------------------------------------------
-spec service_status_model() -> maps:map().
service_status_model() ->
    #{
        %% The collection of hosts with associated service status, for each
        %% hostwhere given service has been deployed.
        hosts => {#{'_' => service_status_host_model()}, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The service status.
%% @end
%%--------------------------------------------------------------------
-spec service_status_host_model() -> maps:map().
service_status_host_model() ->
    #{
        %% The service status.
        status => string
    }.

%%--------------------------------------------------------------------
%% @doc The space details.
%% @end
%%--------------------------------------------------------------------
-spec space_details_model() -> maps:map().
space_details_model() ->
    #{
        %% The ID of the space.
        id => {string, optional},
        %% The name of the space.
        name => string,
        %% The collection of provider IDs with associated supported storage
        %% space in bytes.
        supportingProviders => #{'_' => integer}
    }.

%%--------------------------------------------------------------------
%% @doc The configuration details required to create or support a space by a
%% provider.
%% @end
%%--------------------------------------------------------------------
-spec space_support_request_model() -> maps:map().
space_support_request_model() ->
    #{
        %% The space name. If this property is provided and space with given
        %% name will be created and automatically supported by a provider.
        name => {string, optional},
        %% The token for space creation or support.
        token => string,
        %% The storage space size in bytes that provider is willing to assign to
        %% the space.
        size => integer,
        %% The user defined name of the storage resource, where the space data
        %% should be stored. To be used interchangeably with
        %% `storageId`.
        storageName => {string, optional},
        %% The ID of the storage resource where the space data should be stored.
        %% To be used interchangeably with `storageName`.
        storageId => {string, optional},
        %% Defines whether space will be mounted in / or /{SpaceId}/ path.
        mountInRoot => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The storage configuration details that can be modified.
%% @end
%%--------------------------------------------------------------------
-spec storage_modify_request_model() -> maps:map().
storage_modify_request_model() ->
    #{
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The OpenStack Swift configuration.
%% @end
%%--------------------------------------------------------------------
-spec swift_model() -> maps:map().
swift_model() ->
    #{
        %% The type of storage.
        type => {equal, <<"swift">>},
        %% The URL to OpenStack Keystone identity service.
        authUrl => string,
        %% The name of the tenant to which the user belongs.
        tenantName => string,
        %% The name of the Swift storage container.
        containerName => string,
        %% The Keystone authentication username.
        username => string,
        %% The Keystone authentication password.
        password => string,
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Storage block size in bytes.
        blockSize => {integer, optional},
        %% Defines whether storage administrator credentials (username and
        %% password) may be used by users without storage accounts to access
        %% storage in direct IO mode.
        insecure => {boolean, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The result of a scheduled operation, e.g. database service
%% configuration.
%% @end
%%--------------------------------------------------------------------
-spec task_status_model() -> maps:map().
task_status_model() ->
    #{
        %% The operation status.
        status => string,
        %% The list of operation steps that have been executed successfully.
        steps => [string],
        %% The name of an error type.
        error => {string, optional},
        %% The detailed error description.
        description => {string, optional},
        %% The name of a module containing function that returned error.
        module => {string, optional},
        %% The name of a function that returned error.
        function => {string, optional},
        %% The collection of hosts with associated error description.
        hosts => {#{'_' => error_model()}, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The new user account details.
%% @end
%%--------------------------------------------------------------------
-spec user_create_request_model() -> maps:map().
user_create_request_model() ->
    #{
        %% The user name. It must be at least 4 characters long and contain only
        %% alphanumeric characters [a-zA-Z0-9].
        username => string,
        %% The user password. It must be at least 8 characters long and contain
        %% a minimum of 1 lower case letter [a-z] and a minimum of 1 upper case
        %% letter [A-Z] and a minimum of 1 numeric character [0-9]. The Password
        %% must not contain a colon character [:].
        password => string,
        %% The user role, one of 'admin' or 'regular'.
        userRole => atom
    }.

%%--------------------------------------------------------------------
%% @doc The user configuration details.
%% @end
%%--------------------------------------------------------------------
-spec user_details_model() -> maps:map().
user_details_model() ->
    #{
        %% The user ID.
        userId => string,
        %% The user role, one of `admin` or `regular`.
        userRole => atom
    }.

%%--------------------------------------------------------------------
%% @doc The user configuration details that can be modified.
%% @end
%%--------------------------------------------------------------------
-spec user_modify_request_model() -> maps:map().
user_modify_request_model() ->
    #{
        %% The user password.
        password => string
    }.

%%--------------------------------------------------------------------
%% @doc The zone cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec zone_cluster_configuration_model() -> maps:map().
zone_cluster_configuration_model() ->
    #{
        %% Defines whether administrative cluster should be created from the
        %% list of provided cluster nodes.
        autoDeploy => {boolean, optional},
        %% The name of a domain common for all services in the cluster. Together
        %% with a node hostname constitute a node fully qualified domain name.
        domainName => string,
        %% The collection of nodes aliases associated with nodes properties.
        nodes => #{'_' => zone_cluster_configuration_nodes_model()},
        databases => cluster_databases_model(),
        managers => cluster_managers_model(),
        workers => cluster_workers_model()
    }.

-spec zone_cluster_configuration_nodes_model() -> maps:map().
zone_cluster_configuration_nodes_model() ->
    #{
        %% The name of a host.
        hostname => string
    }.

%%--------------------------------------------------------------------
%% @doc The zone deployment configuration.
%% @end
%%--------------------------------------------------------------------
-spec zone_configuration_model() -> maps:map().
zone_configuration_model() ->
    #{
        cluster => zone_cluster_configuration_model(),
        onezone => {zone_configuration_onezone_model(), optional},
        onepanel => {panel_configuration_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc The zone custom configuration.
%% @end
%%--------------------------------------------------------------------
-spec zone_configuration_onezone_model() -> maps:map().
zone_configuration_onezone_model() ->
    #{
        %% The name of a zone.
        name => {string, optional},
        %% The name of a HTTP domain.
        domainName => {string, optional}
    }.

