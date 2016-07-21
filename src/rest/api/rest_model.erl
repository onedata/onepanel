%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(rest_model).
-author("Krzysztof Trzepla").

-export([
    ceph_model/0,
    cluster_model/0,
    cluster_managers_model/0,
    cluster_nodes_model/0,
    cluster_workers_model/0,
    configuration_model/0,
    database_model/0,
    error_model/0,
    hosts_model/0,
    manager_hosts_model/0,
    oneprovider_cluster_model/0,
    oneprovider_configuration_model/0,
    onezone_cluster_model/0,
    onezone_configuration_model/0,
    posix_model/0,
    provider_model/0,
    provider_configure_request_model/0,
    provider_update_request_model/0,
    s3_model/0,
    service_status_model/0,
    services_status_model/0,
    services_status_results_model/0,
    space_details_model/0,
    space_support_request_model/0,
    storage_model/0,
    task_status_model/0,
    user_details_model/0,
    user_details_response_model/0,
    user_details_update_model/0
]).


%%
%% 
%%
ceph_model() ->
    #{
        type => string, %% Type of storage resource
        username => {string, optional}, %% The username for authentication to Ceph.
        key => {string, optional}, %% The key to access the Ceph resource.
        monitorHostname => {string, optional}, %% Monitor host name.
        clusterName => {string, optional}, %% Cluster name.
        poolName => {string, optional}  %% Name of a Ceph pool.
    }.


%%
%% Generic type for Onedata service cluster definition.
%%
cluster_model() ->
    #{
        type => string, %% Type of Onedata service
        domainName => string, %% Domain name of the service.
        nodes => #{'_' => cluster_nodes_model()}, %%
        managers => {cluster_managers_model(), optional}, %%
        workers => {cluster_workers_model(), optional}, %%
        databases => {cluster_workers_model(), optional}  %%
    }.


%%
%% 
%%
cluster_managers_model() ->
    #{
        nodes => [string], %%
        mainNode => string  %% The Id of the main cluster manager node.
    }.


%%
%% 
%%
cluster_nodes_model() ->
    #{
        hostname => string  %% Node hostname
    }.


%%
%% 
%%
cluster_workers_model() ->
    #{
        nodes => [string]  %%
    }.


%%
%% Describes a the properties of a Database instance.
%%
configuration_model() ->
    #{
        cluster => {cluster_model(), optional}  %%
    }.


%%
%% Describes a the properties of a Database instance.
%%
database_model() ->
    #{
        memoryQuota => {integer, optional}  %% The maximum size of RAM memory to be used by the database instance in MB.
    }.


%%
%% Generic error model for REST requests.
%%
error_model() ->
    #{
        error => string, %% Identifier representing internal error code.
        error_description => string  %% Detailed error message.
    }.


%%
%% The list of hosts which should be added to the current deployment.
%%
hosts_model() ->
    #{hosts => {[string], {optional, []}}}.


%%
%% The nodes for cluster manager components, including the main node.
%%
manager_hosts_model() ->
    #{
        mainHost => string, %% The name of the main cluster manager host.
        hosts => [string]  %% The list of hosts which should be added to the current deployment.
    }.


%%
%% 
%%
oneprovider_cluster_model() ->
    #{
        type => string, %% Type of Onedata service
        domainName => string, %% Domain name of the service.
        nodes => #{'_' => cluster_nodes_model()}, %%
        managers => {cluster_managers_model(), optional}, %%
        workers => {cluster_workers_model(), optional}, %%
        databases => {cluster_workers_model(), optional}, %%
        storages => {#{'_' => storage_model()}, optional}  %%
    }.


%%
%% 
%%
oneprovider_configuration_model() ->
    #{
        cluster => {cluster_model(), optional}, %%
        oneprovider => {provider_model(), optional}, %%
        onezone => {object, optional}  %%
    }.


%%
%% 
%%
onezone_cluster_model() ->
    #{
        type => string, %% Type of Onedata service
        domainName => string, %% Domain name of the service.
        nodes => #{'_' => cluster_nodes_model()}, %%
        managers => {cluster_managers_model(), optional}, %%
        workers => {cluster_workers_model(), optional}, %%
        databases => {cluster_workers_model(), optional}  %%
    }.


%%
%% 
%%
onezone_configuration_model() ->
    #{
        cluster => {cluster_model(), optional}, %%
        onezone => {object, optional}  %%
    }.


%%
%% 
%%
posix_model() ->
    #{
        type => string, %% Type of storage resource
        mountPoint => {string, optional}  %% The absolute path of the mountpoint where the storage is mounted on the Oneprovider nodes.
    }.


%%
%% Describes the properties of a Worker process instance.
%%
provider_model() ->
    #{
        register => {boolean, optional}, %% Whether the provider should be automatically registered.
        name => {string, optional}, %% The name under which the provider should be registered in the zone.
        redirectionPoint => {string, optional}, %% Redirection point.
        geoLongitude => {number, optional}, %% Geographical longitude of the storage provider.
        geoLatitude => {number, optional}  %% Geographical latitude of the storage provider.
    }.


%%
%% Describes the properties of a Worker process instance.
%%
provider_configure_request_model() ->
    #{
        name => string, %% The name under which the provider should be registered in the zone.
        redirectionPoint => string, %% Redirection point.
        geoLongitude => {float, optional}, %% Geographical longitude of the storage provider.
        geoLatitude => {float, optional}, %% Geographical latitude of the storage provider.
        onezoneDomainName => {string, optional}  %% The name of Onezone domain to which this provider belongs.
    }.


%%
%% Describes the properties of a Worker process instance.
%%
provider_update_request_model() ->
    #{
        name => {string, optional}, %% The name under which the provider should be registered in the zone.
        redirectionPoint => {string, optional}, %% Redirection point.
        geoLongitude => {float, optional}, %% Geographical longitude of the storage provider.
        geoLatitude => {float, optional}  %% Geographical latitude of the storage provider.
    }.


%%
%% 
%%
s3_model() ->
    #{
        type => string, %% Type of storage resource
        s3Hostname => {string, optional}, %% The hostname where the S3 interface is available on.
        iamHostname => {string, optional}, %% The IAM hostname for the S3 storage.
        bucketName => {string, optional}, %% The name of the bucket with the storage.
        accessKey => {string, optional}, %% The access key to the S3 storage.
        secretKey => {string, optional}  %% The secret key to the S3 storage.
    }.


%%
%% Provides status information on requested service.
%%
service_status_model() ->
    #{
        service => {string, optional}, %% The type of service.\n* oz_worker - an instance of Onezone worker process\n* op_worker - an instance of Oneprovider worker process\n* cluster_manager - an instance of ClusterManager process\n* couchbase - an instance of Couchbase metadata database\n
        result => {string, optional}  %% The service status.
    }.


%%
%% Provides status information about multiple services.
%%
services_status_model() ->
    #{
        service => {string, optional}, %% The type of service.\n* oz_worker - an instance of Onezone worker process\n* op_worker - an instance of Oneprovider worker process\n* cluster_manager - an instance of ClusterManager process\n* couchbase - an instance of Couchbase metadata database\n
        results => {[ref], optional}  %%
    }.


%%
%% 
%%
services_status_results_model() ->
    #{
        result => {string, optional}, %% The status of the service instance on this host.
        host => {string, optional}  %% The host where the service instance is deployed.
    }.


%%
%% Space provider details.
%%
space_details_model() ->
    #{
        spaceId => {string, optional}, %% The ID of the space
        name => {string, optional}, %% The name of the space
        supportingProviders => {#{'_' => integer}, optional}  %% The list of providers supporting a space.
    }.


%%
%% Details needed by provider to support a space.
%%
space_support_request_model() ->
    #{
        name => {string, optional}, %% Space name.
        token => string, %% Space support token.
        size => integer, %% Requested storage space in bytes.
        storageName => {string, optional}, %% The name of the storage resource where the space data should be stored.\nTo be used interchangeably with &#x60;storageId&#x60;.\n
        storageId => {string, optional}  %% The ID of the storage resource where the space data should be stored.\nTo be used interchangeably with &#x60;storageName&#x60;.\n
    }.


%%
%% Describes the properties of a storage resource.
%%
storage_model() ->
    #{
        type => string  %% Type of storage resource
    }.


%%
%% Status of specific Onepanel task.
%%
task_status_model() ->
    #{
        status => string, %% Status code.
        steps => [string], %% The list of configuration steps already performed.
        hosts => {[object], optional}  %% The list of hosts impacted by this task.
    }.


%%
%% Contains user details necessary when creating a new account.
%%
user_details_model() ->
    #{
        username => string, %% User name.
        password => string, %% User password.
        userRole => string  %% User role - currently only 'admin' and 'regular' are supported.
    }.


%%
%% Contains existing user details.
%%
user_details_response_model() ->
    #{
        username => string, %% User name.
        userRole => string  %% User role - currently only 'admin' and 'regular' are supported.
    }.


%%
%% Contains details necessary when modifying user account.
%%
user_details_update_model() ->
    #{
        password => string  %% User password.
    }.

























