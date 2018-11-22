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
    cluster_configuration_details_model/0,
    cluster_databases_model/0,
    cluster_ips_model/0,
    cluster_managers_model/0,
    cluster_workers_model/0,
    cookie_model/0,
    database_hosts_model/0,
    dns_check_model/0,
    dns_check_configuration_model/0,
    dns_check_result_model/0,
    error_model/0,
    manager_hosts_model/0,
    modify_cluster_ips_model/0,
    panel_configuration_model/0,
    panel_configuration_users_model/0,
    provider_cluster_configuration_model/0,
    provider_configuration_model/0,
    provider_configuration_details_model/0,
    provider_configuration_details_oneprovider_model/0,
    provider_configuration_oneprovider_model/0,
    provider_configuration_onezone_model/0,
    provider_details_model/0,
    provider_modify_request_model/0,
    provider_register_request_model/0,
    provider_spaces_model/0,
    provider_storages_model/0,
    service_databases_model/0,
    service_error_model/0,
    service_hosts_model/0,
    service_status_model/0,
    service_status_host_model/0,
    session_details_model/0,
    space_auto_cleaning_configuration_model/0,
    space_auto_cleaning_report_model/0,
    space_auto_cleaning_report_collection_model/0,
    space_auto_cleaning_rule_setting_model/0,
    space_auto_cleaning_rules_model/0,
    space_auto_cleaning_status_model/0,
    space_details_model/0,
    space_files_popularity_configuration_model/0,
    space_id_model/0,
    space_modify_request_model/0,
    space_support_request_model/0,
    space_sync_stats_model/0,
    storage_create_request_model/0,
    storage_details_model/0,
    storage_import_details_model/0,
    storage_modify_request_model/0,
    storage_update_details_model/0,
    task_status_model/0,
    time_stats_model/0,
    time_stats_collection_model/0,
    user_create_request_model/0,
    user_details_model/0,
    user_modify_request_model/0,
    web_cert_model/0,
    web_cert_modify_request_model/0,
    web_cert_paths_model/0,
    worker_hosts_model/0,
    zone_cluster_configuration_model/0,
    zone_cluster_configuration_nodes_model/0,
    zone_configuration_model/0,
    zone_configuration_details_model/0,
    zone_configuration_details_onezone_model/0,
    zone_configuration_onezone_model/0,
    zone_policies_model/0,
    ceph_model/0,
    cephrados_model/0,
    glusterfs_model/0,
    nulldevice_model/0,
    posix_model/0,
    s3_model/0,
    swift_model/0,
    webdav_model/0
]).


%%--------------------------------------------------------------------
%% @doc The cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec cluster_configuration_details_model() -> maps:map().
cluster_configuration_details_model() ->
    #{
        %% Host responsible for deploying cluster and coordinating cluster
        %% restarts.
        master => string,
        databases => database_hosts_model(),
        managers => manager_hosts_model(),
        workers => worker_hosts_model()
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
%% @doc External IPs used by cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec cluster_ips_model() -> maps:map().
cluster_ips_model() ->
    #{
        %% If true, user has already sent a request updating IPs thus marking
        %% them as accepted.
        isConfigured => boolean,
        %% The collection of cluster nodes associated with their IPs.
        hosts => #{'_' => string}
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
%% @doc The cluster database service hosts configuration.
%% @end
%%--------------------------------------------------------------------
-spec database_hosts_model() -> maps:map().
database_hosts_model() ->
    #{
        %% The list of service hosts.
        hosts => [string]
    }.

%%--------------------------------------------------------------------
%% @doc Gathers results of DNS checks for various aspects of the cluster domain.
%% Both Oneprovider and Onezone return field 'domain' for checking if
%% cluster's domain can be resolved. In Onezone there is additional field
%% 'dnsZone' for checking whether DNS zone management for the
%% Onezone's domain has been delegated to Onezone server (SOA and NS
%% records) allowing for subdomain delegation. If the cluster is configured with
%% an IP neither 'domain' nor 'dnsZone' is returned.
%% @end
%%--------------------------------------------------------------------
-spec dns_check_model() -> maps:map().
dns_check_model() ->
    #{
        domain => {dns_check_result_model(), optional},
        dnsZone => {dns_check_result_model(), optional},
        %% Time at which the DNS check was perfmormed. Formatted according to
        %% ISO 8601.
        timestamp => string
    }.

%%--------------------------------------------------------------------
%% @doc Configuration of the 'dns_check' method calls.
%% @end
%%--------------------------------------------------------------------
-spec dns_check_configuration_model() -> maps:map().
dns_check_configuration_model() ->
    #{
        %% A collection of IP addresses for DNS servers used in checking DNS.
        dnsServers => {[string], optional},
        %% If true, DNS check will verify that control of DNS zone of
        %% Onezone's domain was delegated to the DNS server built into
        %% Onezone service. This option is available only in Onezone service.
        builtInDnsServer => {boolean, optional},
        %% Flag indicating that user completed the DNS check step during
        %% interactive deployment.
        dnsCheckAcknowledged => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Describes results obtained from a DNS check. DNS check involves querying
%% DNS servers to ensure publicly visible records match expected values.
%% @end
%%--------------------------------------------------------------------
-spec dns_check_result_model() -> maps:map().
dns_check_result_model() ->
    #{
        %% An interpreation of results obtained from DNS check. Possible values
        %% are: 'unresolvable' - query returned empty results;
        %% 'missing_records' - only some of the expected results were
        %% returned; 'bad_records' - none of the expected results were
        %% returned; 'ok' - all of expected values were present in
        %% obtained results.
        summary => string,
        %% List of expected query results.
        expected => [string],
        %% List of obtained query results.
        got => [string],
        %% List of suggested DNS records to set at your DNS provider to fulfill
        %% this check. Each record is provided in the format of BIND server.
        recommended => [string]
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
        %% The main cluster manager host. Main cluster manager node is
        %% responsible for monitoring cluster worker nodes. Other nodes, which
        %% are redundant, are suspended. In case of main cluster manager node
        %% failure one of redundant nodes is resumed and takes over main node
        %% responsibilities.
        mainHost => string,
        %% The list of service hosts.
        hosts => [string]
    }.

%%--------------------------------------------------------------------
%% @doc External IPs used by cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec modify_cluster_ips_model() -> maps:map().
modify_cluster_ips_model() ->
    #{
        %% The collection of cluster nodes associated with their IPs.
        hosts => #{'_' => string}
    }.

%%--------------------------------------------------------------------
%% @doc The panel configuration.
%% @end
%%--------------------------------------------------------------------
-spec panel_configuration_model() -> maps:map().
panel_configuration_model() ->
    #{
        %% Indicates that interactive deployment is performed. If false, users
        %% entering GUI will not be asked to complete the configuration. In that
        %% case default values will be used, available for change later via
        %% appropriate onepanel GUI pages or REST.
        interactiveDeployment => {boolean, optional},
        %% The collection of user names associated with users properties.
        users => {#{'_' => panel_configuration_users_model()}, {optional, #{}}}
    }.

-spec panel_configuration_users_model() -> maps:map().
panel_configuration_users_model() ->
    #{
        %% The user password.
        password => string,
        %% The user role, one of 'admin' or 'regular'.
        userRole => atom
    }.

%%--------------------------------------------------------------------
%% @doc The provider cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec provider_cluster_configuration_model() -> maps:map().
provider_cluster_configuration_model() ->
    #{
        %% Hostname suffix common for all services in the cluster. Together with
        %% a node hostname constitutes a fully qualified domain name (FQDN) of
        %% the node. May be skipped to allow unrelated hostnames for each node.
        domainName => {string, optional},
        %% The collection of nodes aliases associated with nodes properties.
        nodes => #{'_' => zone_cluster_configuration_nodes_model()},
        databases => cluster_databases_model(),
        managers => cluster_managers_model(),
        workers => cluster_workers_model(),
        storages => {storage_create_request_model(), optional}
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
%% @doc The provider deployment configuration.
%% @end
%%--------------------------------------------------------------------
-spec provider_configuration_details_model() -> maps:map().
provider_configuration_details_model() ->
    #{
        cluster => cluster_configuration_details_model(),
        oneprovider => {provider_configuration_details_oneprovider_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc The provider custom configuration.
%% @end
%%--------------------------------------------------------------------
-spec provider_configuration_details_oneprovider_model() -> maps:map().
provider_configuration_details_oneprovider_model() ->
    #{
        %% The name of a provider. Null if not registered.
        name => string,
        %% True if all steps of cluster deployment and configuration have been
        %% performed.
        configured => boolean
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
        %% The name under which the provider will be registered in a zone.
        name => string,
        %% If enabled, the storage provider will be assigned a subdomain in
        %% onezone's domain and 'subdomain' property must be
        %% provided. If disabled, 'domain' property should be provided.
        subdomainDelegation => {boolean, optional},
        %% Unique subdomain in onezone's domain for the provider. Required
        %% if subdomain delegation is enabled.
        subdomain => {string, optional},
        %% If enabled the provider will use Let's Encrypt service to obtain
        %% SSL certificates. Otherwise certificates must be manually provided.
        %% This option cannot be enabled if subdomainDelegation is false. By
        %% enabling this option you agree to the Let's Encrypt Subscriber
        %% Agreement.
        letsEncryptEnabled => {boolean, optional},
        %% The fully qualified domain name of the provider or its IP address
        %% (only for single-node deployments or clusters with a reverse proxy).
        %% Required if subdomain delegation is disabled.
        domain => {string, optional},
        %% The geographical longitude of the provider.
        geoLongitude => {float, optional},
        %% The geographical latitude of the provider.
        geoLatitude => {float, optional},
        %% Email address of the oneprovider administrator.
        adminEmail => string
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
        %% The Id assigned by a zone.
        id => string,
        %% The name under which the provider has been registered in a zone.
        name => string,
        %% If enabled, the storage provider has a subdomain in onezone's
        %% domain and 'subdomain' property must be provided.
        subdomainDelegation => boolean,
        %% Unique subdomain in onezone's domain for the provider. Required
        %% if subdomain delegation is enabled.
        subdomain => {string, optional},
        %% The fully qualified domain name of the provider or its IP address
        %% (only for single-node deployments or clusters with a reverse proxy).
        domain => string,
        %% Email address of the oneprovider administrator.
        adminEmail => string,
        %% The geographical longitude of the provider.
        geoLongitude => float,
        %% The geographical latitude of the provider.
        geoLatitude => float,
        %% The domain name of a zone where this storage provider is registered.
        onezoneDomainName => string
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
        %% If enabled, the storage provider will be assigned a subdomain in
        %% onezone's domain and 'subdomain' property must be
        %% provided. If disabled, 'domain' property should be provided.
        subdomainDelegation => {boolean, optional},
        %% Unique subdomain in onezone's domain for the provider. This
        %% property is required only if subdomain delegation is enabled.
        %% Otherwise it is ignored.
        subdomain => {string, optional},
        %% The fully qualified domain name of the provider or its IP address
        %% (only for single-node deployments or clusters with a reverse proxy).
        %% This property is required only if subdomain delegation is disabled.
        %% Otherwise it is ignored.
        domain => {string, optional},
        %% The geographical longitude of the provider.
        geoLongitude => {float, optional},
        %% The geographical latitude of the provider.
        geoLatitude => {float, optional},
        %% Email address of the oneprovider administrator.
        adminEmail => {string, optional}
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
        %% If enabled, the storage provider will be assigned a subdomain in
        %% onezone's domain and 'subdomain' property must be
        %% provided. If disabled, 'domain' property should be provided.
        subdomainDelegation => boolean,
        %% Unique subdomain in onezone's domain for the storage provider.
        %% Required if subdomain delegation is enabled.
        subdomain => {string, optional},
        %% The fully qualified domain name of the storage provider or its IP
        %% address (only for single-node deployments or clusters with a reverse
        %% proxy). Required if subdomain delegation is disabled.
        domain => {string, optional},
        %% The geographical longitude of the storage provider.
        geoLongitude => {float, optional},
        %% The geographical latitude of the storage provider.
        geoLatitude => {float, optional},
        %% The domain name of a zone where this storage provider will be
        %% registered.
        onezoneDomainName => string,
        %% Email address of the oneprovider administrator.
        adminEmail => string
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
%% @doc The cluster storage resources.
%% @end
%%--------------------------------------------------------------------
-spec provider_storages_model() -> maps:map().
provider_storages_model() ->
    #{
        %% The list of IDs of cluster storage resources.
        ids => [string]
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
        %% The collection of hosts with associated service status, for each host
        %% where given service has been deployed.
        hosts => #{'_' => service_status_host_model()}
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
%% @doc The user session details.
%% @end
%%--------------------------------------------------------------------
-spec session_details_model() -> maps:map().
session_details_model() ->
    #{
        %% The session Id.
        sessionId => string,
        %% The name of a user associated with the session.
        username => string
    }.

%%--------------------------------------------------------------------
%% @doc Settings for space auto-cleaning mechanism. Setting enabled to
%% `false` disables given parameter. It will be ignored by auto-
%% cleaning mechanism. All presented parameters' ranges are inclusive.
%% @end
%%--------------------------------------------------------------------
-spec space_auto_cleaning_configuration_model() -> maps:map().
space_auto_cleaning_configuration_model() ->
    #{
        %% If true, auto-cleaning mechanism is enabled in the space.
        enabled => {boolean, optional},
        %% Amount of data [b], which should trigger the auto-cleaning in the
        %% space. Only replicas maintained by this storage provider will be
        %% removed.  This parameter is required to enable auto-cleaning.
        threshold => {integer, optional},
        %% Amount of data [b], at which the auto-cleaning process should stop.
        %% This parameter is required to enable auto-cleaning.
        target => {integer, optional},
        %% Rules used to select certain list of file replicas that can be
        %% evicted by auto-cleaning mechanism.
        rules => {space_auto_cleaning_rules_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc Auto-cleaning report
%% @end
%%--------------------------------------------------------------------
-spec space_auto_cleaning_report_model() -> maps:map().
space_auto_cleaning_report_model() ->
    #{
        %% Start time of auto-cleaning procedure in ISO 8601 format
        startedAt => string,
        %% Finish time of auto-cleaning procedure in ISO 8601 format
        stoppedAt => string,
        %% Number of bytes deleted during auto-cleaning procedure.
        releasedBytes => integer,
        %% Number of bytes that should be deleted.
        bytesToRelease => integer,
        %% Number of deleted files.
        filesNumber => integer
    }.

%%--------------------------------------------------------------------
%% @doc List of auto-cleaning report entries
%% @end
%%--------------------------------------------------------------------
-spec space_auto_cleaning_report_collection_model() -> maps:map().
space_auto_cleaning_report_collection_model() ->
    #{
        reportEntries => {[space_auto_cleaning_report_model()], optional}
    }.

%%--------------------------------------------------------------------
%% @doc Rule setting for a space auto-cleaning mechanism. Setting field
%% `enabled` to `false` disables the rule.
%% @end
%%--------------------------------------------------------------------
-spec space_auto_cleaning_rule_setting_model() -> maps:map().
space_auto_cleaning_rule_setting_model() ->
    #{
        %% Informs whether given setting is enabled.
        enabled => {boolean, optional},
        %% Integer value of a given setting.
        value => {integer, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Rules used to select certain list of file replicas that can be evicted
%% by auto-cleaning mechanism. A rule is enabled by setting its
%% `enabled` field to `true`. By default all rules are
%% disabled (ignored). A rule can be enabled without specifying its value. In
%% that case previous value is used. If the rule is enabled for the first time a
%% default value will be used. All rules' values are inclusive.
%% @end
%%--------------------------------------------------------------------
-spec space_auto_cleaning_rules_model() -> maps:map().
space_auto_cleaning_rules_model() ->
    #{
        %% Informs whether selective rules should be used by auto-cleaning
        %% mechanism.
        enabled => {boolean, optional},
        %% Files that have been opened less than `maxOpenCount` times
        %% may be cleaned. The default value is `9007199254740991
        %% (2^53-1)`.
        maxOpenCount => {space_auto_cleaning_rule_setting_model(), optional},
        %% Files that haven't been opened for longer than or equal to given
        %% period [h] may be cleaned. The default value is `0`.
        minHoursSinceLastOpen => {space_auto_cleaning_rule_setting_model(), optional},
        %% Only files which size [b] is greater than given value may be cleaned.
        %% The default value is `1`.
        lowerFileSizeLimit => {space_auto_cleaning_rule_setting_model(), optional},
        %% Only files which size [b] is less than given value may be cleaned.
        %% The default value is `1125899906842624 (1 PiB)`.
        upperFileSizeLimit => {space_auto_cleaning_rule_setting_model(), optional},
        %% Files that have moving average of open operations count per hour less
        %% than given value may be cleaned. The average is calculated in 24
        %% hours window. The default value is `9007199254740991
        %% (2^53-1)`.
        maxHourlyMovingAverage => {space_auto_cleaning_rule_setting_model(), optional},
        %% Files that have moving average of open operations count per day less
        %% than given value may be cleaned. The average is calculated in 31 days
        %% window. The default value is `9007199254740991 (2^53-1)`.
        maxDailyMovingAverage => {space_auto_cleaning_rule_setting_model(), optional},
        %% Files that have moving average of open operations count per month
        %% less than given value may be cleaned. The average is calculated in 12
        %% months window. The default value is `9007199254740991
        %% (2^53-1)`.
        maxMonthlyMovingAverage => {space_auto_cleaning_rule_setting_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc Status of current auto-cleaning process for given space
%% @end
%%--------------------------------------------------------------------
-spec space_auto_cleaning_status_model() -> maps:map().
space_auto_cleaning_status_model() ->
    #{
        %% Flag which indicates whether auto-cleaning process is currently in
        %% progress
        inProgress => boolean,
        %% Amount of storage [b] used by data from given space on that storage.
        spaceOccupancy => integer
    }.

%%--------------------------------------------------------------------
%% @doc The space details.
%% @end
%%--------------------------------------------------------------------
-spec space_details_model() -> maps:map().
space_details_model() ->
    #{
        %% The Id of the space.
        id => string,
        %% The name of the space.
        name => string,
        %% Id of StorageDetails that supports this space on provider that is
        %% associated with this panel.
        storageId => string,
        %% The list of IDs of cluster storage resources.
        localStorages => [string],
        %% The collection of provider IDs with associated supported storage
        %% space in bytes.
        supportingProviders => #{'_' => integer},
        %% Defines whether space will be mounted in / or /{SpaceId}/ path.
        mountInRoot => {boolean, optional},
        storageImport => {storage_import_details_model(), optional},
        storageUpdate => {storage_update_details_model(), optional},
        %% Amount of storage [b] used by data from given space on that storage.
        spaceOccupancy => integer
    }.

%%--------------------------------------------------------------------
%% @doc Settings of files-popularity feature of space
%% @end
%%--------------------------------------------------------------------
-spec space_files_popularity_configuration_model() -> maps:map().
space_files_popularity_configuration_model() ->
    #{
        %% If true, collecting files-popularity mechanism in the space is
        %% enabled
        enabled => boolean,
        %% REST endpoint to view files-popularity statistics
        restUrl => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Provides Id of a space.
%% @end
%%--------------------------------------------------------------------
-spec space_id_model() -> maps:map().
space_id_model() ->
    #{
        %% The Id of the space.
        id => string
    }.

%%--------------------------------------------------------------------
%% @doc The space configuration details that can be modified.
%% @end
%%--------------------------------------------------------------------
-spec space_modify_request_model() -> maps:map().
space_modify_request_model() ->
    #{
        %% The storage space size in bytes that provider is willing to assign to
        %% the space.
        size => {integer, optional},
        storageImport => {storage_import_details_model(), optional},
        storageUpdate => {storage_update_details_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc The configuration details required to create or support a space by a
%% provider.
%% @end
%%--------------------------------------------------------------------
-spec space_support_request_model() -> maps:map().
space_support_request_model() ->
    #{
        %% The token for space creation or support.
        token => string,
        %% The storage space size in bytes that provider is willing to assign to
        %% the space.
        size => integer,
        %% The Id of the storage resource where the space data should be stored.
        storageId => string,
        %% Defines whether space will be mounted in / or /{SpaceId}/ path.
        mountInRoot => {boolean, optional},
        storageImport => {storage_import_details_model(), optional},
        storageUpdate => {storage_update_details_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc Status and statistics of storage/space synchronization.
%% @end
%%--------------------------------------------------------------------
-spec space_sync_stats_model() -> maps:map().
space_sync_stats_model() ->
    #{
        %% Describes import algorithm run status.
        importStatus => string,
        %% Describes update algorithm run status.
        updateStatus => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The configuration details required to add storage resources.
%% @end
%%--------------------------------------------------------------------
-spec storage_create_request_model() -> maps:map().
storage_create_request_model() ->
    #{'_' => storage_details_model()}.

%%--------------------------------------------------------------------
%% @doc The cluster storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec storage_details_model() -> {oneof, Oneof :: list()}.
storage_details_model() ->
    {oneof, [posix_model(), s3_model(), ceph_model(), cephrados_model(),
             swift_model(), glusterfs_model(), nulldevice_model(),
             webdav_model()]}.

%%--------------------------------------------------------------------
%% @doc The storage import configuration. Storage import allows to import data
%% from storage to space without need for copying the data.
%% @end
%%--------------------------------------------------------------------
-spec storage_import_details_model() -> maps:map().
storage_import_details_model() ->
    #{
        %% The import strategy. One of no_import, simple_scan.
        strategy => string,
        %% Maximum depth of filesystem tree that will be traversed during
        %% storage synchronization.
        maxDepth => {integer, optional},
        %% Flag that enables synchronization of NFSv4 ACLs.
        syncAcl => {boolean, optional}
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
%% @doc The storage update configuration. Storage update ensures that all
%% changes on storage will be reflected in space.
%% @end
%%--------------------------------------------------------------------
-spec storage_update_details_model() -> maps:map().
storage_update_details_model() ->
    #{
        %% The update strategy. One of no_update, simple_scan.
        strategy => string,
        %% Maximum depth of filesystem tree that will be traversed during
        %% storage synchronization.
        maxDepth => {integer, optional},
        %% Period between subsequent scans in seconds (counted from end of one
        %% scan till beginning of the following).
        scanInterval => {integer, optional},
        %% Flag determining that synchronized storage will be treated as
        %% immutable (only creations and deletions of files on storage will be
        %% detected).
        writeOnce => {boolean, optional},
        %% Flag determining that deletions of files will be detected.
        deleteEnable => {boolean, optional},
        %% Flag that enables synchronization of NFSv4 ACLs.
        syncAcl => {boolean, optional}
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
%% @doc Statistics for single metric over specified time.
%% @end
%%--------------------------------------------------------------------
-spec time_stats_model() -> maps:map().
time_stats_model() ->
    #{
        %% Name of metric for which this object holds statistics.
        name => string,
        %% Date of last measurement value in this object in ISO 8601 format
        lastValueDate => string,
        %% Predefined time period for which the statistics were fetched
        period => {string, optional},
        %% List of sample values for given metric. The used period is divided
        %% into array-length number of parts. E.g. if the used period is an
        %% hour, and if there are 12 values in this array, every value is a
        %% value for 1/12 of day, which gives value for every hour of the day.
        %% If the value is null, there is no sample for given time part.
        values => [number]
    }.

%%--------------------------------------------------------------------
%% @doc Statistics for single metric over specified time.
%% @end
%%--------------------------------------------------------------------
-spec time_stats_collection_model() -> maps:map().
time_stats_collection_model() ->
    #{
        %% Statistics of storage sync jobs queue length.
        queueLength => {time_stats_model(), optional},
        %% Statistics of storage sync imported files.
        insertCount => {time_stats_model(), optional},
        %% Statistics of storage sync updated files.
        updateCount => {time_stats_model(), optional},
        %% Statistics of storage sync deleted files.
        deleteCount => {time_stats_model(), optional}
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
        %% The user Id.
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
        %% The current user password that should be changed or password of an
        %% administrator that is issuing this request on behalf of a user.
        currentPassword => string,
        %% The new user password.
        newPassword => string
    }.

%%--------------------------------------------------------------------
%% @doc The SSL certificate details.
%% @end
%%--------------------------------------------------------------------
-spec web_cert_model() -> maps:map().
web_cert_model() ->
    #{
        %% If true, the certificate is obtained from Let's Encrypt service
        %% and renewed automatically. Otherwise, the certificate management is
        %% up to the administrator.
        letsEncrypt => boolean,
        %% Installed certificate's expiration time in ISO 8601 format.
        expirationTime => string,
        %% Installed certificate's creation time in ISO 8601 format.
        creationTime => string,
        %% Describes certificate validity status.
        status => string,
        paths => {web_cert_paths_model(), optional},
        %% The domain (Common Name) for which current certificate was issued.
        domain => string,
        %% Issuer value of the current certificate.
        issuer => string,
        %% Date and time in ISO 8601 format. Represents last successful
        %% Let's Encrypt certification. If there are no successful attempts
        %% its value is null. This property is omitted if letsEncrypt is off.
        lastRenewalSuccess => {string, optional},
        %% Date and time in ISO 8601 format. Represents last unsuccessful
        %% Let's Encrypt certification. If there are no successful attempts
        %% its value is null. This property is omitted if letsEncrypt is off.
        lastRenewalFailure => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The SSL certificate configuration details that can be modified.
%% @end
%%--------------------------------------------------------------------
-spec web_cert_modify_request_model() -> maps:map().
web_cert_modify_request_model() ->
    #{
        %% If enabled Let's Encrypt service will be used to obtain SSL
        %% certificates and renew them before expiration. Otherwise certificates
        %% must be manually provided.
        letsEncrypt => boolean
    }.

%%--------------------------------------------------------------------
%% @doc Paths to certificate-related files.
%% @end
%%--------------------------------------------------------------------
-spec web_cert_paths_model() -> maps:map().
web_cert_paths_model() ->
    #{
        %% Path to the certificate PEM file.
        cert => string,
        %% Path to the corresponding private key PEM file.
        key => string,
        %% Path to the file containing certificate chain.
        chain => string
    }.

%%--------------------------------------------------------------------
%% @doc The cluster worker service hosts configuration.
%% @end
%%--------------------------------------------------------------------
-spec worker_hosts_model() -> maps:map().
worker_hosts_model() ->
    #{
        %% The list of service hosts.
        hosts => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The zone cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec zone_cluster_configuration_model() -> maps:map().
zone_cluster_configuration_model() ->
    #{
        %% Hostname suffix common for all services in the cluster. Together with
        %% a node hostname constitutes a fully qualified domain name (FQDN) of
        %% the node. May be skipped to allow unrelated hostnames for each node.
        domainName => {string, optional},
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
        hostname => string,
        %% External IP of the node.
        externalIp => {string, optional}
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
%% @doc The zone cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec zone_configuration_details_model() -> maps:map().
zone_configuration_details_model() ->
    #{
        cluster => cluster_configuration_details_model(),
        onezone => {zone_configuration_details_onezone_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc The zone custom configuration.
%% @end
%%--------------------------------------------------------------------
-spec zone_configuration_details_onezone_model() -> maps:map().
zone_configuration_details_onezone_model() ->
    #{
        %% Onezone's domain.
        domainName => string,
        %% The name of a zone.
        name => string,
        %% True if all steps of cluster deployment and configuration have been
        %% performed.
        configured => boolean
    }.

%%--------------------------------------------------------------------
%% @doc The zone custom configuration.
%% @end
%%--------------------------------------------------------------------
-spec zone_configuration_onezone_model() -> maps:map().
zone_configuration_onezone_model() ->
    #{
        %% The name of a HTTP domain.
        domainName => {string, optional},
        %% The name of a zone.
        name => {string, optional},
        %% If enabled the zone will use Let's Encrypt service to obtain SSL
        %% certificates. Otherwise certificates must be manually provided. By
        %% enabling this option you agree to the Let's Encrypt Subscriber
        %% Agreement.
        letsEncryptEnabled => {boolean, optional},
        policies => {zone_policies_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc State of Onezone operation policies.
%% @end
%%--------------------------------------------------------------------
-spec zone_policies_model() -> maps:map().
zone_policies_model() ->
    #{
        %% If true, Oneproviders are allowed to request subdomains of the
        %% Onezone domain for use as their domains.
        subdomainDelegation => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The Ceph storage configuration (uses libradosstriper).
%% @end
%%--------------------------------------------------------------------
-spec ceph_model() -> maps:map().
ceph_model() ->
    #{
        %% The Id of storage.
        id => {string, optional},
        %% The name of storage.
        name => {string, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% The type of storage.
        type => {equal, <<"ceph">>},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
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
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The Ceph storage configuration (uses librados).
%% @end
%%--------------------------------------------------------------------
-spec cephrados_model() -> maps:map().
cephrados_model() ->
    #{
        %% The Id of storage.
        id => {string, optional},
        %% The name of storage.
        name => {string, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% The type of storage.
        type => {equal, <<"cephrados">>},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
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
        %% Storage block size in bytes.
        blockSize => {integer, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The GlusterFS storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec glusterfs_model() -> maps:map().
glusterfs_model() ->
    #{
        %% The Id of storage.
        id => {string, optional},
        %% The name of storage.
        name => {string, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% The type of storage.
        type => {equal, <<"glusterfs">>},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% The name of the volume to use as a storage backend.
        volume => string,
        %% The hostname (IP address or FQDN) of GlusterFS volume server.
        hostname => string,
        %% The GlusterFS port on volume server.
        port => {integer, optional},
        %% The transport protocol to use to connect to the volume server.
        transport => {string, optional},
        %% Relative mountpoint within the volume which should be used by
        %% Oneprovider.
        mountPoint => {string, optional},
        %% Volume specific GlusterFS translator options, in the format:
        %% TRANSLATOR1.OPTION1=VALUE1;TRANSLATOR2.OPTION2=VALUE2;...
        xlatorOptions => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The Null Device storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec nulldevice_model() -> maps:map().
nulldevice_model() ->
    #{
        %% The Id of storage.
        id => {string, optional},
        %% The name of storage.
        name => {string, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% The type of storage.
        type => {equal, <<"nulldevice">>},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% Minimum latency in milliseconds, which should be simulated for
        %% selected operations.
        latencyMin => {integer, optional},
        %% Maximum latency in milliseconds, which should be simulated for
        %% selected operations.
        latencyMax => {integer, optional},
        %% Probability (0.0, 1.0), with which an operation should return a
        %% timeout error.
        timeoutProbability => {float, optional},
        %% Comma-separated list of filesystem operations, for which latency and
        %% timeout should be simulated. Empty or '*' mean all operations
        %% will be affected.
        filter => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, optional},
        %% Specifies the parameters for a simulated null device filesystem. For
        %% example `2-2:2-2:0-1` will generate a filesystem tree which
        %% has 2 directories (`0` and `1`) and 2 files
        %% (`2` and `3`) in the root of the filesystem, each
        %% of these directories will have 2 subdirectories (`0` and
        %% `1`) and 2 files (`2` and `3`) and each
        %% of these subdirectories has only a single file (`0`).
        %% Default empty string disables the simulated filesystem feature.
        simulatedFilesystemParameters => {string, optional},
        %% Determines the simulated filesystem grow rate. Default 0.0 value will
        %% cause all the files and directories defined by the
        %% `simulatedFilesystemParameters` specification to be visible
        %% immediately. For example value of 0.01 will increase the number of
        %% the visible filesystem entries by 1 file per 100 seconds, while 100.0
        %% will increase it by 100 files per second.
        simulatedFilesystemGrowSpeed => {float, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The POSIX storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec posix_model() -> maps:map().
posix_model() ->
    #{
        %% The Id of storage.
        id => {string, optional},
        %% The name of storage.
        name => {string, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% The type of storage.
        type => {equal, <<"posix">>},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% The absolute path to the directory where the POSIX storage is mounted
        %% on the cluster nodes.
        mountPoint => string,
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The Simple Storage Service configuration.
%% @end
%%--------------------------------------------------------------------
-spec s3_model() -> maps:map().
s3_model() ->
    #{
        %% The Id of storage.
        id => {string, optional},
        %% The name of storage.
        name => {string, optional},
        %% Defines whether storage administrator credentials (accessKey and
        %% secretKey) may be used by users without storage accounts to access
        %% storage in direct IO mode.
        insecure => {boolean, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% The type of storage.
        type => {equal, <<"s3">>},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
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
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The OpenStack Swift configuration.
%% @end
%%--------------------------------------------------------------------
-spec swift_model() -> maps:map().
swift_model() ->
    #{
        %% The Id of storage.
        id => {string, optional},
        %% The name of storage.
        name => {string, optional},
        %% Defines whether storage administrator credentials (username and
        %% password) may be used by users without storage accounts to access
        %% storage in direct IO mode.
        insecure => {boolean, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% The type of storage.
        type => {equal, <<"swift">>},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
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
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The WebDAV storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec webdav_model() -> maps:map().
webdav_model() ->
    #{
        %% The Id of storage.
        id => {string, optional},
        %% The name of storage.
        name => {string, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% The type of storage.
        type => {equal, <<"webdav">>},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% Full URL of the WebDAV server, including scheme (http or https) and
        %% path.
        endpoint => string,
        %% Determines whether Oneprovider should verify the certificate of the
        %% WebDAV server.
        verifyServerCertificate => {boolean, {optional, true}},
        %% Determines the types of credentials provided in the credentials
        %% field.
        credentialsType => {string, {optional, none}},
        %% The credentials to authenticate with the WebDAV server.
        %% `basic` credentials should be provided in the form
        %% `username:password`, for `token` just the token.
        %% For `none` this field is ignored.
        credentials => {string, optional},
        %% The authorization header to be used for passing the access token.
        %% This field can contain any prefix that should be added to the header
        %% value. Default is `Authorization: Bearer {}`. The token
        %% will placed where `{}` is provided.
        authorizationHeader => {string, optional},
        %% The type of partial write support enabled in the WebDAV server.
        %% Currently 2 types are supported `sabredav` which assumes
        %% the server supports the SabreDAV PartialUpdate extension via
        %% `PATCH` method, and `moddav` which assumes server
        %% supports partial `PUT` requests with `Content-
        %% Range` header. If `none` is selected no write support
        %% is available for this WebDAV storage.
        rangeWriteSupport => {string, {optional, none}},
        %% Defines the maximum number of parallel connections for a single
        %% WebDAV storage.
        connectionPoolSize => {integer, {optional, 10}},
        %% Defines the maximum upload size for a single `PUT` or
        %% `PATCH` request. If set to 0, assumes that the WebDAV
        %% server has no upload limit.
        maximumUploadSize => {integer, {optional, 0}},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, optional}
    }.

