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
    block_devices_model/0,
    block_devices_block_devices_model/0,
    ceph_global_params_model/0,
    ceph_manager_model/0,
    ceph_managers_model/0,
    ceph_monitor_model/0,
    ceph_monitors_model/0,
    ceph_osd_model/0,
    ceph_osds_model/0,
    ceph_pool_model/0,
    ceph_pool_usage_model/0,
    ceph_pools_model/0,
    ceph_status_model/0,
    ceph_usage_model/0,
    cluster_configuration_details_model/0,
    cluster_databases_model/0,
    cluster_details_model/0,
    cluster_ips_model/0,
    cluster_managers_model/0,
    cluster_members_summary_model/0,
    cluster_workers_model/0,
    configuration_model/0,
    current_user_model/0,
    data_usage_model/0,
    database_hosts_model/0,
    dns_check_model/0,
    dns_check_configuration_model/0,
    dns_check_result_model/0,
    emergency_passphrase_change_request_model/0,
    emergency_passphrase_status_model/0,
    error_model/0,
    error_error_model/0,
    gui_message_model/0,
    host_model/0,
    host_add_request_model/0,
    ids_model/0,
    join_cluster_request_model/0,
    manager_hosts_model/0,
    modify_cluster_ips_model/0,
    node_model/0,
    onezone_info_model/0,
    onezone_user_model/0,
    onezone_user_create_request_model/0,
    panel_configuration_model/0,
    password_change_request_model/0,
    progress_model/0,
    progress_modify_model/0,
    provider_cluster_configuration_model/0,
    provider_configuration_model/0,
    provider_configuration_details_model/0,
    provider_configuration_details_oneprovider_model/0,
    provider_configuration_oneprovider_model/0,
    provider_details_model/0,
    provider_modify_request_model/0,
    provider_register_request_model/0,
    provider_spaces_model/0,
    provider_storages_model/0,
    remote_provider_details_model/0,
    service_databases_model/0,
    service_hosts_model/0,
    service_status_model/0,
    service_status_host_model/0,
    space_auto_cleaning_configuration_model/0,
    space_auto_cleaning_report_model/0,
    space_auto_cleaning_reports_model/0,
    space_auto_cleaning_rule_setting_model/0,
    space_auto_cleaning_rules_model/0,
    space_auto_cleaning_status_model/0,
    space_details_model/0,
    space_file_popularity_configuration_model/0,
    space_id_model/0,
    space_modify_request_model/0,
    space_support_request_model/0,
    space_sync_stats_model/0,
    storage_create_details_model/0,
    storage_create_request_model/0,
    storage_get_details_model/0,
    storage_import_details_model/0,
    storage_modify_details_model/0,
    storage_modify_request_model/0,
    storage_update_details_model/0,
    task_status_model/0,
    time_stats_model/0,
    time_stats_collection_model/0,
    token_model/0,
    transfers_mock_model/0,
    version_info_model/0,
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
    blockdevice_model/0,
    ceph_model/0,
    ceph_cluster_model/0,
    ceph_modify_model/0,
    cephrados_model/0,
    cephrados_modify_model/0,
    glusterfs_model/0,
    glusterfs_modify_model/0,
    localceph_model/0,
    localceph_modify_model/0,
    loopdevice_model/0,
    nulldevice_model/0,
    nulldevice_modify_model/0,
    op_configuration_model/0,
    oz_configuration_model/0,
    posix_model/0,
    posix_modify_model/0,
    s3_model/0,
    s3_modify_model/0,
    swift_model/0,
    swift_modify_model/0,
    webdav_model/0,
    webdav_modify_model/0
]).


%%--------------------------------------------------------------------
%% @doc List of block device descriptions.
%% @end
%%--------------------------------------------------------------------
-spec block_devices_model() -> onepanel_parser:object_spec().
block_devices_model() ->
    #{
        %% List of available block devices.
        blockDevices => {[block_devices_block_devices_model()], optional}
    }.

-spec block_devices_block_devices_model() -> onepanel_parser:object_spec().
block_devices_block_devices_model() ->
    #{
        %% Device type, as returned by lsblk.
        type => {{enum, string, [<<"disk">>, <<"part">>]}, optional},
        %% Host on which the device is available.
        host => string,
        path => string,
        %% Device size in bytes.
        size => integer,
        %% Result of heuristic attempt to detect whether a device or any of its
        %% children is mounted.
        mounted => boolean
    }.

%%--------------------------------------------------------------------
%% @doc Describes global Ceph cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec ceph_global_params_model() -> onepanel_parser:object_spec().
ceph_global_params_model() ->
    #{
        %% Name of the cluster.
        name => {string, optional},
        %% Unique UUID of the cluster. Autogenerated when cluster is deployed if
        %% not specified.
        fsid => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Ceph manager specification.
%% @end
%%--------------------------------------------------------------------
-spec ceph_manager_model() -> onepanel_parser:object_spec().
ceph_manager_model() ->
    #{
        %% Host on which given manager should be deployed.
        host => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Object containing a list of Ceph manager daemons.
%% @end
%%--------------------------------------------------------------------
-spec ceph_managers_model() -> onepanel_parser:object_spec().
ceph_managers_model() ->
    #{
        %% List of Ceph manager configurations.
        managers => {[ceph_manager_model()], optional}
    }.

%%--------------------------------------------------------------------
%% @doc Ceph monitor specification.
%% @end
%%--------------------------------------------------------------------
-spec ceph_monitor_model() -> onepanel_parser:object_spec().
ceph_monitor_model() ->
    #{
        %% Host on which given monitor should be deployed. There may be only one
        %% monitor per host. Attempts at creating additional monitors at the
        %% same host will be ignored.
        host => string,
        %% Local IP to be used for communication between Ceph nodes. If not
        %% specified it will be autodetected.
        ip => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Object containing a list of Ceph monitor daemons.
%% @end
%%--------------------------------------------------------------------
-spec ceph_monitors_model() -> onepanel_parser:object_spec().
ceph_monitors_model() ->
    #{
        %% List of Ceph monitor specifications.
        monitors => {[ceph_monitor_model()], optional}
    }.

%%--------------------------------------------------------------------
%% @doc The cluster storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec ceph_osd_model() -> onepanel_parser:multi_spec().
ceph_osd_model() ->
    {subclasses, onepanel_parser:prepare_subclasses([blockdevice_model(), loopdevice_model()])}.

%%--------------------------------------------------------------------
%% @doc Object containing a list of Ceph OSD specifications.
%% @end
%%--------------------------------------------------------------------
-spec ceph_osds_model() -> onepanel_parser:object_spec().
ceph_osds_model() ->
    #{
        %% List of Ceph OSD specifications.
        osds => {[ceph_osd_model()], optional}
    }.

%%--------------------------------------------------------------------
%% @doc Describes a Ceph pool.
%% @end
%%--------------------------------------------------------------------
-spec ceph_pool_model() -> onepanel_parser:object_spec().
ceph_pool_model() ->
    #{
        %% Desired number of object replicas in the pool. When below this number
        %% the pool still may be used in 'degraded' mode. Defaults to
        %% `2` if there are at least 2 OSDs, `1` otherwise.
        copiesNumber => {integer, optional},
        %% Minimum number of object replicas in the pool. Below this threshold
        %% any I/O for the pool is disabled. Must be lower or equal to
        %% 'copiesNumber'. Defaults to `min(2, copiesNumber)`
        %% if there are at least 2 OSDs, `1` otherwise.
        minCopiesNumber => {integer, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Space usage of a single Ceph pool.
%% @end
%%--------------------------------------------------------------------
-spec ceph_pool_usage_model() -> onepanel_parser:object_spec().
ceph_pool_usage_model() ->
    #{
        %% Total size of objects in the pool in bytes.
        used => integer,
        %% Projected size in bytes of data which may be written to the pool. See
        %% \&quot;Checking a Cluster’s Usage Stats\&quot; in the Ceph
        %% documentation.
        maxAvailable => {integer, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Object containing a list of Ceph pools.
%% @end
%%--------------------------------------------------------------------
-spec ceph_pools_model() -> onepanel_parser:object_spec().
ceph_pools_model() ->
    #{
        %% List of Ceph pools.
        pools => {[ceph_pool_model()], optional}
    }.

%%--------------------------------------------------------------------
%% @doc Status report of the Ceph cluster.
%% @end
%%--------------------------------------------------------------------
-spec ceph_status_model() -> onepanel_parser:object_spec().
ceph_status_model() ->
    #{
        %% General health status.
        level => {enum, string, [<<"ok">>, <<"warning">>, <<"error">>]},
        %% List of Ceph status messages.
        messages => [string]
    }.

%%--------------------------------------------------------------------
%% @doc Summary of storage space usage in the ceph cluster.
%% @end
%%--------------------------------------------------------------------
-spec ceph_usage_model() -> onepanel_parser:object_spec().
ceph_usage_model() ->
    #{
        total => data_usage_model(),
        %% Dictionary of OSDs with associated usage data.
        osds => #{'_' => data_usage_model()},
        %% Dictionary of pools with associated usage data.
        pools => #{'_' => ceph_pool_usage_model()}
    }.

%%--------------------------------------------------------------------
%% @doc The cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec cluster_configuration_details_model() -> onepanel_parser:object_spec().
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
-spec cluster_databases_model() -> onepanel_parser:object_spec().
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
%% @doc Details of a cluster.
%% @end
%%--------------------------------------------------------------------
-spec cluster_details_model() -> onepanel_parser:object_spec().
cluster_details_model() ->
    #{
        %% Id of the cluster record.
        id => string,
        %% Type of the cluster.
        type => {enum, string, [<<"oneprovider">>, <<"onezone">>]},
        %% The Id of the service hosted on this cluster - depending on the type
        %% equal to the Oneprovider Id or \&quot;onezone\&quot; in case of
        %% Onezone cluster
        serviceId => string,
        workerVersion => version_info_model(),
        onepanelVersion => version_info_model(),
        %% Is Onepanel proxy enabled - if so, onepanel GUI is served on
        %% cluster's domain at port 443 (rather than 9443).
        onepanelProxy => boolean
    }.

%%--------------------------------------------------------------------
%% @doc External IPs used by cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec cluster_ips_model() -> onepanel_parser:object_spec().
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
-spec cluster_managers_model() -> onepanel_parser:object_spec().
cluster_managers_model() ->
    #{
        %% The alias of the main cluster manager node.
        mainNode => string,
        %% The list of aliases of cluster manager nodes.
        nodes => [string]
    }.

%%--------------------------------------------------------------------
%% @doc Summary of cluster members, listing number of direct and effective users
%% and groups.
%% @end
%%--------------------------------------------------------------------
-spec cluster_members_summary_model() -> onepanel_parser:object_spec().
cluster_members_summary_model() ->
    #{
        %% Number of users belonging directly to the cluster.
        usersCount => {integer, optional},
        %% Number of users belonging directly and indirectly to the cluster.
        effectiveUsersCount => {integer, optional},
        %% Number of groups belonging directly to the cluster.
        groupsCount => {integer, optional},
        %% Number of groups belonging directly and indirectly to the cluster.
        effectiveGroupsCount => {integer, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The cluster worker service configuration.
%% @end
%%--------------------------------------------------------------------
-spec cluster_workers_model() -> onepanel_parser:object_spec().
cluster_workers_model() ->
    #{
        %% The list of aliases of cluster worker nodes.
        nodes => [string]
    }.

%%--------------------------------------------------------------------
%% @doc Public service configuration details.
%% @end
%%--------------------------------------------------------------------
-spec configuration_model() -> onepanel_parser:multi_spec().
configuration_model() ->
    {subclasses, onepanel_parser:prepare_subclasses(
        [op_configuration_model(), oz_configuration_model()])}.

%%--------------------------------------------------------------------
%% @doc Information about the authenticated user.
%% @end
%%--------------------------------------------------------------------
-spec current_user_model() -> onepanel_parser:object_spec().
current_user_model() ->
    #{
        %% The user Id.
        userId => string,
        %% User's full name (given names + surname).
        username => string,
        %% List of cluster privileges held by the user in the current cluster.
        clusterPrivileges => {[{enum, [string], [<<"cluster_view">>, <<"cluster_update">>, <<"cluster_delete">>, <<"cluster_view_privileges">>, <<"cluster_set_privileges">>, <<"cluster_add_user">>, <<"cluster_remove_user">>, <<"cluster_add_group">>, <<"cluster_remove_group">>]}], optional}
    }.

%%--------------------------------------------------------------------
%% @doc Describes storage space usage level.
%% @end
%%--------------------------------------------------------------------
-spec data_usage_model() -> onepanel_parser:object_spec().
data_usage_model() ->
    #{
        %% Total space (used and available) in bytes.
        total => integer,
        %% Total used space in bytes.
        used => integer,
        %% Total available space in bytes.
        available => integer
    }.

%%--------------------------------------------------------------------
%% @doc The cluster database service hosts configuration.
%% @end
%%--------------------------------------------------------------------
-spec database_hosts_model() -> onepanel_parser:object_spec().
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
-spec dns_check_model() -> onepanel_parser:object_spec().
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
-spec dns_check_configuration_model() -> onepanel_parser:object_spec().
dns_check_configuration_model() ->
    #{
        %% A collection of IP addresses for DNS servers used in checking DNS. If
        %% empty, local system configuration will be used.
        dnsServers => {[string], optional},
        %% If true, DNS check will verify that control of DNS zone for
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
-spec dns_check_result_model() -> onepanel_parser:object_spec().
dns_check_result_model() ->
    #{
        %% An interpretation of results obtained from DNS check. Possible values
        %% are: 'unresolvable' - query returned empty results;
        %% 'missing_records' - only some of the expected results were
        %% returned; 'bad_records' - none of the expected results were
        %% returned; 'ok' - all of expected values were present in
        %% obtained results.
        summary => {enum, string, [<<"unresolvable">>, <<"missing_records">>, <<"bad_records">>, <<"ok">>]},
        %% List of expected query results.
        expected => [string],
        %% List of obtained query results.
        got => [string],
        %% List of suggested DNS records to set at your DNS provider to fulfill
        %% this check. Each record is provided in the format of BIND server.
        recommended => [string]
    }.

%%--------------------------------------------------------------------
%% @doc Emergency passphrase to set and old passphrase to authorize the change.
%% @end
%%--------------------------------------------------------------------
-spec emergency_passphrase_change_request_model() -> onepanel_parser:object_spec().
emergency_passphrase_change_request_model() ->
    #{
        %% New passphrase to be set.
        newPassphrase => string,
        %% Currently set passphrase. Not required when setting the passphrase
        %% for the first time.
        currentPassphrase => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Emergency passphrase status.
%% @end
%%--------------------------------------------------------------------
-spec emergency_passphrase_status_model() -> onepanel_parser:object_spec().
emergency_passphrase_status_model() ->
    #{
        %% True if the passphrase is set.
        isSet => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The generic error model for REST requests.
%% @end
%%--------------------------------------------------------------------
-spec error_model() -> onepanel_parser:object_spec().
error_model() ->
    #{
        error => {error_error_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc Object describing an error.
%% @end
%%--------------------------------------------------------------------
-spec error_error_model() -> onepanel_parser:object_spec().
error_error_model() ->
    #{
        %% String identifying the error type. Does not change between error
        %% instances.
        id => string,
        %% Human readable error description. May contain information specific to
        %% given error instance.
        description => string,
        %% Details about the error instance. The object schema is specific to
        %% each error type.
        details => {#{}, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Settings of a message displayed in Onezone GUI.
%% @end
%%--------------------------------------------------------------------
-spec gui_message_model() -> onepanel_parser:object_spec().
gui_message_model() ->
    #{
        %% True if the message should be displayed.
        enabled => {boolean, optional},
        %% HTML content of the message to display.
        body => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Details of a cluster host.
%% @end
%%--------------------------------------------------------------------
-spec host_model() -> onepanel_parser:object_spec().
host_model() ->
    #{
        %% Host's hostname.
        hostname => string
    }.

%%--------------------------------------------------------------------
%% @doc Details of host added to cluster.
%% @end
%%--------------------------------------------------------------------
-spec host_add_request_model() -> onepanel_parser:object_spec().
host_add_request_model() ->
    #{
        %% Address at which the host is available, IP or hostname.
        address => string
    }.

-spec ids_model() -> onepanel_parser:object_spec().
ids_model() ->
    #{
        %% List of ids.
        ids => [string]
    }.

%%--------------------------------------------------------------------
%% @doc Information allowing new host to join the cluster.
%% @end
%%--------------------------------------------------------------------
-spec join_cluster_request_model() -> onepanel_parser:object_spec().
join_cluster_request_model() ->
    #{
        %% Hostname of an existing cluster node.
        clusterHost => string,
        %% The cookie is a character sequence that is common for all the cluster
        %% nodes. If this parameter is not provided, in case of a cluster
        %% initialization request, it will be generated, and in case of a
        %% cluster extension request the current cookie value will be used.
        %% However, if the cluster cookie and the cookie of the host that is
        %% about to join the cluster doesn't match there will be a
        %% connection error.
        cookie => {atom, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The cluster manager service hosts configuration.
%% @end
%%--------------------------------------------------------------------
-spec manager_hosts_model() -> onepanel_parser:object_spec().
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
-spec modify_cluster_ips_model() -> onepanel_parser:object_spec().
modify_cluster_ips_model() ->
    #{
        %% The collection of cluster nodes associated with their IPs.
        hosts => #{'_' => string}
    }.

%%--------------------------------------------------------------------
%% @doc Details of a onepanel node.
%% @end
%%--------------------------------------------------------------------
-spec node_model() -> onepanel_parser:object_spec().
node_model() ->
    #{
        %% Hostname of the node.
        hostname => string,
        %% Type of Onedata cluster managed by this onepanel.
        clusterType => {enum, string, [<<"oneprovider">>, <<"onezone">>]}
    }.

%%--------------------------------------------------------------------
%% @doc Information which can be obtained about remote Onezone.
%% @end
%%--------------------------------------------------------------------
-spec onezone_info_model() -> onepanel_parser:object_spec().
onezone_info_model() ->
    #{
        %% True if connection to the Onezone was achieved. If false, fields
        %% other than 'domain' will not be sent.
        online => boolean,
        %% Onezone cluster version.
        version => {string, optional},
        %% Domain of the Onezone.
        domain => string,
        %% Name of the Onezone cluster.
        name => {string, optional},
        %% True if versions of this Oneprovider and the Onezone are compatible.
        compatible => {boolean, optional},
        %% Whether given Onezone allows subdomain delegation.
        subdomainDelegationSupported => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Describes a user account.
%% @end
%%--------------------------------------------------------------------
-spec onezone_user_model() -> onepanel_parser:object_spec().
onezone_user_model() ->
    #{
        %% Unique user Id.
        userId => string,
        %% User's full name (given names + surname).
        fullName => string,
        %% User's human-readable identifier, unique across the system. Makes
        %% it easier to identify the user and can be used for signing in with
        %% password.
        username => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The new Onezone user account details.
%% @end
%%--------------------------------------------------------------------
-spec onezone_user_create_request_model() -> onepanel_parser:object_spec().
onezone_user_create_request_model() ->
    #{
        username => string,
        password => string,
        %% Ids of Onezone groups to which the user should be added. The groups
        %% must already exist.
        groups => {[string], {optional, []}}
    }.

%%--------------------------------------------------------------------
%% @doc The panel configuration.
%% @end
%%--------------------------------------------------------------------
-spec panel_configuration_model() -> onepanel_parser:object_spec().
panel_configuration_model() ->
    #{
        %% Indicates that interactive deployment is being performed. If false,
        %% users entering GUI will not be asked to complete the configuration.
        %% In that case default values will be used, available for change later
        %% via appropriate Onepanel GUI pages or REST.
        interactiveDeployment => {boolean, optional},
        %% When true, all GUIs hosted in this cluster will print debug logs to
        %% browser console.
        guiDebugMode => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Request to change user's password.
%% @end
%%--------------------------------------------------------------------
-spec password_change_request_model() -> onepanel_parser:object_spec().
password_change_request_model() ->
    #{
        %% The new user password.
        newPassword => string
    }.

%%--------------------------------------------------------------------
%% @doc Progress markers indicating which steps of interactive deployment were
%% reached by the admin performing cluster setup.
%% @end
%%--------------------------------------------------------------------
-spec progress_model() -> onepanel_parser:object_spec().
progress_model() ->
    #{
        %% True after user provided public IPs of cluster nodes or confirmed
        %% autodetected defaults. Also true if interactiveDeployment was
        %% disabled.
        clusterIps => {boolean, optional},
        %% True after user decided whether to use Let's Encrypt certificates
        %% or if interactiveDeployment was disabled.
        webCertificate => {boolean, optional},
        %% True after user reviewed results of DNS check or if
        %% interactiveDeployment was disabled.
        dnsCheck => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Progress markers which can be set or unset by the GUI.
%% @end
%%--------------------------------------------------------------------
-spec progress_modify_model() -> onepanel_parser:object_spec().
progress_modify_model() ->
    #{
        %% True after user confirmed detected external IPs or if
        %% interactiveDeployment was disabled.
        clusterIps => {boolean, optional},
        %% True after user decided whether to use Let's Encrypt certificates
        %% or if interactiveDeployment was disabled.
        webCertificate => {boolean, optional},
        %% True after user reviewed results of DNS check or if
        %% interactiveDeployment was disabled.
        dnsCheck => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The provider cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec provider_cluster_configuration_model() -> onepanel_parser:object_spec().
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
-spec provider_configuration_model() -> onepanel_parser:object_spec().
provider_configuration_model() ->
    #{
        cluster => provider_cluster_configuration_model(),
        ceph => {ceph_cluster_model(), optional},
        oneprovider => {provider_configuration_oneprovider_model(), optional},
        onepanel => {panel_configuration_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc The provider deployment configuration.
%% @end
%%--------------------------------------------------------------------
-spec provider_configuration_details_model() -> onepanel_parser:object_spec().
provider_configuration_details_model() ->
    #{
        cluster => cluster_configuration_details_model(),
        oneprovider => {provider_configuration_details_oneprovider_model(), optional},
        ceph => {ceph_cluster_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc The provider custom configuration.
%% @end
%%--------------------------------------------------------------------
-spec provider_configuration_details_oneprovider_model() -> onepanel_parser:object_spec().
provider_configuration_details_oneprovider_model() ->
    #{
        %% The name of a provider. `null` if not registered.
        name => string,
        %% True if all steps of cluster deployment and configuration have been
        %% performed.
        configured => boolean
    }.

%%--------------------------------------------------------------------
%% @doc The provider custom configuration.
%% @end
%%--------------------------------------------------------------------
-spec provider_configuration_oneprovider_model() -> onepanel_parser:object_spec().
provider_configuration_oneprovider_model() ->
    #{
        %% Defines whether the provider should be registered in a zone.
        register => boolean,
        %% Registration token obtained from Onezone. This token identifies
        %% Onezone to be used and authorizes the registration request.
        token => {string, optional},
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
%% @doc The Oneprovider configuration details.
%% @end
%%--------------------------------------------------------------------
-spec provider_details_model() -> onepanel_parser:object_spec().
provider_details_model() ->
    #{
        %% The Id assigned by a zone.
        id => string,
        %% The name under which the Oneprovider has been registered in a zone.
        name => string,
        %% If enabled, the storage Oneprovider has a subdomain in onezone's
        %% domain and 'subdomain' property must be provided.
        subdomainDelegation => boolean,
        %% Unique subdomain in onezone's domain for the Oneprovider.
        %% Required if subdomain delegation is enabled.
        subdomain => {string, optional},
        %% The fully qualified domain name of the Oneprovider or its IP address
        %% (only for single-node deployments or clusters with a reverse proxy).
        domain => string,
        %% Email address of the Oneprovider administrator. Omitted if it could
        %% not be retrievied.
        adminEmail => {string, optional},
        %% The geographical longitude of the Oneprovider.
        geoLongitude => float,
        %% The geographical latitude of the Oneprovider.
        geoLatitude => float,
        %% The domain name of a zone where this storage Oneprovider is
        %% registered.
        onezoneDomainName => string
    }.

%%--------------------------------------------------------------------
%% @doc The provider configuration details that can be modified.
%% @end
%%--------------------------------------------------------------------
-spec provider_modify_request_model() -> onepanel_parser:object_spec().
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
-spec provider_register_request_model() -> onepanel_parser:object_spec().
provider_register_request_model() ->
    #{
        %% The name under which the provider should be registered in a zone.
        name => string,
        %% Registration token obtained from Onezone. This token identifies
        %% Onezone to be used and authorizes the registration request.
        token => string,
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
        %% Email address of the Oneprovider administrator.
        adminEmail => string
    }.

%%--------------------------------------------------------------------
%% @doc The provider spaces details.
%% @end
%%--------------------------------------------------------------------
-spec provider_spaces_model() -> onepanel_parser:object_spec().
provider_spaces_model() ->
    #{
        %% The list of IDs of spaces supported by a provider.
        ids => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The cluster storage resources.
%% @end
%%--------------------------------------------------------------------
-spec provider_storages_model() -> onepanel_parser:object_spec().
provider_storages_model() ->
    #{
        %% The list of Ids of cluster storage resources.
        ids => [string]
    }.

%%--------------------------------------------------------------------
%% @doc Information about another Oneprovider.
%% @end
%%--------------------------------------------------------------------
-spec remote_provider_details_model() -> onepanel_parser:object_spec().
remote_provider_details_model() ->
    #{
        %% The Oneprovider Id assigned by Onezone.
        id => string,
        %% The name under which the Oneprovider has been registered in Onezone.
        name => string,
        %% The fully qualified domain name of the Oneprovider.
        domain => string,
        %% The geographical longitude of the provider.
        geoLongitude => float,
        %% The geographical latitude of the provider.
        geoLatitude => float,
        %% The Id of the corresponding cluster record.
        cluster => string,
        %% Indicates if the Oneprovider is currently online.
        online => boolean
    }.

%%--------------------------------------------------------------------
%% @doc The service hosts configuration.
%% @end
%%--------------------------------------------------------------------
-spec service_databases_model() -> onepanel_parser:object_spec().
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
%% @doc The service hosts configuration.
%% @end
%%--------------------------------------------------------------------
-spec service_hosts_model() -> onepanel_parser:object_spec().
service_hosts_model() ->
    #{
        %% The list of hosts where service should be deployed.
        hosts => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The generic model for service status.
%% @end
%%--------------------------------------------------------------------
-spec service_status_model() -> onepanel_parser:object_spec().
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
-spec service_status_host_model() -> onepanel_parser:object_spec().
service_status_host_model() ->
    #{
        %% The service status.
        status => {enum, string, [<<"healthy">>, <<"unhealthy">>, <<"stopped">>, <<"missing">>]}
    }.

%%--------------------------------------------------------------------
%% @doc Settings for space auto-cleaning mechanism. Setting enabled to
%% `false` disables given parameter. It will be ignored by auto-
%% cleaning mechanism. All presented parameters' ranges are inclusive.
%% @end
%%--------------------------------------------------------------------
-spec space_auto_cleaning_configuration_model() -> onepanel_parser:object_spec().
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
%% @doc Report from an auto-cleaning run.
%% @end
%%--------------------------------------------------------------------
-spec space_auto_cleaning_report_model() -> onepanel_parser:object_spec().
space_auto_cleaning_report_model() ->
    #{
        %% Id of an auto-cleaning report.
        id => string,
        %% Index of an auto-cleaning report. It can be used to list report Ids
        %% starting from given report.
        index => string,
        %% Start time of an auto-cleaning run in ISO 8601 format.
        startedAt => string,
        %% Finish time of an auto-cleaning run in ISO 8601 format.
        stoppedAt => string,
        %% Number of bytes deleted during an auto-cleaning run.
        releasedBytes => integer,
        %% Number of bytes that should be deleted.
        bytesToRelease => integer,
        %% Number of deleted files.
        filesNumber => integer
    }.

%%--------------------------------------------------------------------
%% @doc The space auto-cleaning reports.
%% @end
%%--------------------------------------------------------------------
-spec space_auto_cleaning_reports_model() -> onepanel_parser:object_spec().
space_auto_cleaning_reports_model() ->
    #{
        %% The list of Ids of space auto-cleaning reports.
        ids => [string]
    }.

%%--------------------------------------------------------------------
%% @doc Rule setting for a space auto-cleaning mechanism. Setting field
%% `enabled` to `false` disables the rule.
%% @end
%%--------------------------------------------------------------------
-spec space_auto_cleaning_rule_setting_model() -> onepanel_parser:object_spec().
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
-spec space_auto_cleaning_rules_model() -> onepanel_parser:object_spec().
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
        minFileSize => {space_auto_cleaning_rule_setting_model(), optional},
        %% Only files which size [b] is less than given value may be cleaned.
        %% The default value is `1125899906842624 (1 PiB)`.
        maxFileSize => {space_auto_cleaning_rule_setting_model(), optional},
        %% Files that have moving average of open operations count per hour less
        %% than given value may be cleaned. The average is calculated in 24
        %% hours window. The default value is `9007199254740991
        %% (2^53-1)`.
        maxHourlyMovingAverage => {space_auto_cleaning_rule_setting_model(), optional},
        %% Files that have moving average of open operations count per day less
        %% than given value may be cleaned. The average is calculated in 30 days
        %% window. The default value is `9007199254740991 (2^53-1)`.
        maxDailyMovingAverage => {space_auto_cleaning_rule_setting_model(), optional},
        %% Files that have moving average of open operations count per month
        %% less than given value may be cleaned. The average is calculated in 12
        %% months window. The default value is `9007199254740991
        %% (2^53-1)`.
        maxMonthlyMovingAverage => {space_auto_cleaning_rule_setting_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc Status of current auto-cleaning process for given space.
%% @end
%%--------------------------------------------------------------------
-spec space_auto_cleaning_status_model() -> onepanel_parser:object_spec().
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
-spec space_details_model() -> onepanel_parser:object_spec().
space_details_model() ->
    #{
        %% The Id of the space.
        id => string,
        %% The name of the space.
        name => string,
        %% Id of storage that supports this space on provider that is associated
        %% with this panel.
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
%% @doc Configuration of the file-popularity mechanism in the space.
%% @end
%%--------------------------------------------------------------------
-spec space_file_popularity_configuration_model() -> onepanel_parser:object_spec().
space_file_popularity_configuration_model() ->
    #{
        %% If true, collecting file-popularity mechanism in the space is
        %% enabled.
        enabled => {boolean, optional},
        %% Example `curl` command that can be executed to query the
        %% file-popularity view in the space.
        exampleQuery => {string, optional},
        %% Weight of `lastOpenHour` parameter.
        lastOpenHourWeight => {float, optional},
        %% Weight of `avgOpenCountPerDayWeight` parameter.
        avgOpenCountPerDayWeight => {float, optional},
        %% Maximal value of average open count per day taken to calculate the
        %% value of popularity function.
        maxAvgOpenCountPerDay => {float, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Provides Id of a space.
%% @end
%%--------------------------------------------------------------------
-spec space_id_model() -> onepanel_parser:object_spec().
space_id_model() ->
    #{
        %% The Id of the space.
        id => string
    }.

%%--------------------------------------------------------------------
%% @doc The space configuration details that can be modified.
%% @end
%%--------------------------------------------------------------------
-spec space_modify_request_model() -> onepanel_parser:object_spec().
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
-spec space_support_request_model() -> onepanel_parser:object_spec().
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
-spec space_sync_stats_model() -> onepanel_parser:object_spec().
space_sync_stats_model() ->
    #{
        %% Describes import algorithm run status.
        importStatus => {enum, string, [<<"inProgress">>, <<"done">>]},
        %% Describes update algorithm run status.
        updateStatus => {{enum, string, [<<"waiting">>, <<"inProgress">>]}, optional},
        %% Collection of statistics for requested metrics.
        stats => {time_stats_collection_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc The cluster storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec storage_create_details_model() -> onepanel_parser:multi_spec().
storage_create_details_model() ->
    {subclasses, onepanel_parser:prepare_subclasses(
        [posix_model(), s3_model(), cephrados_model(), localceph_model(), swift_model(), glusterfs_model(), nulldevice_model(), webdav_model()])}.

%%--------------------------------------------------------------------
%% @doc The configuration details required to add storage resources.
%% @end
%%--------------------------------------------------------------------
-spec storage_create_request_model() -> onepanel_parser:object_spec().
storage_create_request_model() ->
    #{'_' => storage_create_details_model()}.

%%--------------------------------------------------------------------
%% @doc The cluster storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec storage_get_details_model() -> onepanel_parser:multi_spec().
storage_get_details_model() ->
    {subclasses, onepanel_parser:prepare_subclasses(
        [posix_model(), s3_model(), ceph_model(), cephrados_model(), localceph_model(), swift_model(), glusterfs_model(), nulldevice_model(), webdav_model()])}.

%%--------------------------------------------------------------------
%% @doc The storage import configuration. Storage import allows to import data
%% from storage to space without need for copying the data.
%% @end
%%--------------------------------------------------------------------
-spec storage_import_details_model() -> onepanel_parser:object_spec().
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
%% @doc The part of storage configuration which can be modified after storage
%% creation.
%% @end
%%--------------------------------------------------------------------
-spec storage_modify_details_model() -> onepanel_parser:multi_spec().
storage_modify_details_model() ->
    {subclasses, onepanel_parser:prepare_subclasses(
        [posix_modify_model(), s3_modify_model(), ceph_modify_model(), cephrados_modify_model(), localceph_modify_model(), swift_modify_model(), glusterfs_modify_model(), nulldevice_modify_model(), webdav_modify_model()])}.

%%--------------------------------------------------------------------
%% @doc The storage parameters to be changed. Should be a single-valued
%% dictionary with storage name as the key and parameters to be changed as the
%% value. If changing the storage name, use old name as dictionary key and
%% provide new name among the changed params.
%% @end
%%--------------------------------------------------------------------
-spec storage_modify_request_model() -> onepanel_parser:object_spec().
storage_modify_request_model() ->
    #{'_' => storage_modify_details_model()}.

%%--------------------------------------------------------------------
%% @doc The storage update configuration. Storage update ensures that all
%% changes on storage will be reflected in space.
%% @end
%%--------------------------------------------------------------------
-spec storage_update_details_model() -> onepanel_parser:object_spec().
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
-spec task_status_model() -> onepanel_parser:object_spec().
task_status_model() ->
    #{
        %% The operation status.
        status => {enum, string, [<<"ok">>, <<"error">>, <<"running">>]},
        %% The list of operation steps that have been executed successfully.
        steps => [string],
        %% Total number of steps to be executed.
        totalSteps => integer,
        error => {error_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc Statistics for single metric over specified time.
%% @end
%%--------------------------------------------------------------------
-spec time_stats_model() -> onepanel_parser:object_spec().
time_stats_model() ->
    #{
        %% Name of metric for which this object holds statistics.
        name => {enum, string, [<<"queueLength">>, <<"insertCount">>, <<"updateCount">>, <<"deleteCount">>]},
        %% Date of last measurement value in this object in ISO 8601 format.
        lastValueDate => string,
        %% Predefined time period for which the statistics were fetched.
        period => {{enum, string, [<<"minute">>, <<"hour">>, <<"day">>]}, optional},
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
-spec time_stats_collection_model() -> onepanel_parser:object_spec().
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
%% @doc A token.
%% @end
%%--------------------------------------------------------------------
-spec token_model() -> onepanel_parser:object_spec().
token_model() ->
    #{
        token => string
    }.

%%--------------------------------------------------------------------
%% @doc State of transfers mock.
%% @end
%%--------------------------------------------------------------------
-spec transfers_mock_model() -> onepanel_parser:object_spec().
transfers_mock_model() ->
    #{
        %% If true, transfers are marked as successful without actually
        %% transmiting any data.
        transfersMock => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Service version info.
%% @end
%%--------------------------------------------------------------------
-spec version_info_model() -> onepanel_parser:object_spec().
version_info_model() ->
    #{
        %% Release version.
        release => string,
        %% Build number.
        build => string,
        %% GUI version indicated by a SHA256 hash.
        gui => string
    }.

%%--------------------------------------------------------------------
%% @doc The SSL certificate details.
%% @end
%%--------------------------------------------------------------------
-spec web_cert_model() -> onepanel_parser:object_spec().
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
        status => {enum, string, [<<"valid">>, <<"near_expiration">>, <<"expired">>, <<"domain_mismatch">>, <<"regenerating">>, <<"unknown">>]},
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
-spec web_cert_modify_request_model() -> onepanel_parser:object_spec().
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
-spec web_cert_paths_model() -> onepanel_parser:object_spec().
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
-spec worker_hosts_model() -> onepanel_parser:object_spec().
worker_hosts_model() ->
    #{
        %% The list of service hosts.
        hosts => [string]
    }.

%%--------------------------------------------------------------------
%% @doc The zone cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec zone_cluster_configuration_model() -> onepanel_parser:object_spec().
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

-spec zone_cluster_configuration_nodes_model() -> onepanel_parser:object_spec().
zone_cluster_configuration_nodes_model() ->
    #{
        %% The name of a host.
        hostname => string,
        %% External IP of the node.
        externalIp => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The Onezone deployment configuration.
%% @end
%%--------------------------------------------------------------------
-spec zone_configuration_model() -> onepanel_parser:object_spec().
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
-spec zone_configuration_details_model() -> onepanel_parser:object_spec().
zone_configuration_details_model() ->
    #{
        cluster => cluster_configuration_details_model(),
        onezone => {zone_configuration_details_onezone_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc The zone custom configuration.
%% @end
%%--------------------------------------------------------------------
-spec zone_configuration_details_onezone_model() -> onepanel_parser:object_spec().
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
%% @doc The Onezone custom configuration.
%% @end
%%--------------------------------------------------------------------
-spec zone_configuration_onezone_model() -> onepanel_parser:object_spec().
zone_configuration_onezone_model() ->
    #{
        %% The domain of Onezone cluster.
        domainName => {string, optional},
        %% The Onezone cluster name.
        name => {string, optional},
        %% If enabled the zone will use Let's Encrypt service to obtain SSL
        %% certificates. Otherwise certificates must be manually provided. By
        %% enabling this option you agree to the Let's Encrypt Subscriber
        %% Agreement.
        letsEncryptEnabled => {boolean, optional},
        %% If true, DNS check will verify that control of DNS zone for
        %% Onezone's domain was delegated to the DNS server built into
        %% Onezone service.
        builtInDnsServer => {boolean, optional},
        policies => {zone_policies_model(), optional},
        %% List of Onezone user specifications.
        users => {[onezone_user_create_request_model()], {optional, []}}
    }.

%%--------------------------------------------------------------------
%% @doc State of Onezone operation policies.
%% @end
%%--------------------------------------------------------------------
-spec zone_policies_model() -> onepanel_parser:object_spec().
zone_policies_model() ->
    #{
        %% Indicates policy enforced during provider registration. Possible
        %% options are: open - anyone can acquire a registration token and
        %% register a new Oneprovider restricted - requires an administrative
        %% privilege 'oz_providers_invite'              to generate a
        %% Oneprovider registration token. The token              can be issued
        %% for someone else.
        oneproviderRegistration => {{enum, string, [<<"open">>, <<"restricted">>]}, optional},
        %% If true, Oneproviders are allowed to request subdomains of the
        %% Onezone domain for use as their domains.
        subdomainDelegation => {boolean, optional},
        %% When this value is true, GUI packages uploaded by services operating
        %% under Onezone or by harvester admins are checked against known
        %% SHA-256 check-sums using the compatibility registry. Setting this
        %% value to false disables the verification. WARNING: disabling GUI
        %% package verification poses a severe security threat, allowing
        %% Oneprovider owners to upload arbitrary GUI to Onezone (which is then
        %% hosted in Onezone's domain).
        guiPackageVerification => {boolean, optional},
        %% This policy can be used to disable GUI package verification for
        %% harvester plugins only. See \&quot;guiPackageVerification\&quot; for
        %% detailed description. This setting has no effect if
        %% \&quot;guiPackageVerification\&quot; is set to false.
        harvesterGuiPackageVerification => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Blockdevice OSD specification. Blockdevice means OSD is backed by a
%% whole block device formatted to its needs.
%% @end
%%--------------------------------------------------------------------
-spec blockdevice_model() -> onepanel_parser:object_spec().
blockdevice_model() ->
    #{
        %% Host on which given OSD should be deployed. It must be the full host
        %% name and not an \&quot;alias\&quot; as used in Oneprovider cluster
        %% deployment.
        host => string,
        %% UUID of the OSD daemon. If provided, will be used to skip deployment
        %% of existing OSDs (identified by the UUID). Must be a 32-character hex
        %% string. By default will be generated automatically.
        uuid => {string, optional},
        %% Type of the OSD. Available types are: - blockdevice - formats a raw
        %% block device to store the data - loopdevice - stores data in a file
        %% mounted as loop device
        type => {discriminator, <<"blockdevice">>},
        %% Specifies block device to be ERASED and FORMATTED for use as the main
        %% data store of this OSD.
        device => string
    }.

%%--------------------------------------------------------------------
%% @doc The Ceph storage configuration (uses libradosstriper).
%% @end
%%--------------------------------------------------------------------
-spec ceph_model() -> onepanel_parser:object_spec().
ceph_model() ->
    #{
        %% The type of storage.
        type => {discriminator, <<"ceph">>},
        %% The username of the Ceph cluster administrator.
        username => string,
        %% The admin key to access the Ceph cluster.
        key => string,
        %% The monitor hostname.
        monitorHostname => string,
        %% The Ceph cluster name.
        clusterName => string,
        %% The Ceph pool name.
        poolName => string,
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"flat">>}}
    }.

%%--------------------------------------------------------------------
%% @doc Describes whole of Ceph configuration.
%% @end
%%--------------------------------------------------------------------
-spec ceph_cluster_model() -> onepanel_parser:object_spec().
ceph_cluster_model() ->
    #{
        %% Name of the cluster.
        name => {string, optional},
        %% Unique UUID of the cluster. Autogenerated when cluster is deployed if
        %% not specified.
        fsid => {string, optional},
        %% List of Ceph OSD specifications.
        osds => {[ceph_osd_model()], optional},
        %% List of Ceph monitor specifications.
        monitors => {[ceph_monitor_model()], optional},
        %% List of Ceph manager configurations.
        managers => {[ceph_manager_model()], optional}
    }.

%%--------------------------------------------------------------------
%% @doc The Ceph storage configuration (uses libradosstriper).
%% @end
%%--------------------------------------------------------------------
-spec ceph_modify_model() -> onepanel_parser:object_spec().
ceph_modify_model() ->
    #{
        %% The name of storage.
        name => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% Type of the modified storage. Must match the type of existing
        %% storage, needed only for OpenAPI polymorphism disambiguation.
        type => {discriminator, <<"ceph">>},
        %% The username of the Ceph cluster administrator.
        username => {string, optional},
        %% The admin key to access the Ceph cluster.
        key => {string, optional},
        %% The monitor hostname.
        monitorHostname => {string, optional},
        %% The Ceph cluster name.
        clusterName => {string, optional},
        %% The Ceph pool name.
        poolName => {string, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The Ceph storage configuration (uses librados).
%% @end
%%--------------------------------------------------------------------
-spec cephrados_model() -> onepanel_parser:object_spec().
cephrados_model() ->
    #{
        %% The type of storage.
        type => {discriminator, <<"cephrados">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% The username of the Ceph cluster administrator.
        username => string,
        %% The admin key to access the Ceph cluster.
        key => string,
        %% The monitor hostname.
        monitorHostname => string,
        %% The Ceph cluster name.
        clusterName => string,
        %% The Ceph pool name.
        poolName => string,
        %% Storage block size in bytes.
        blockSize => {integer, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"flat">>}}
    }.

%%--------------------------------------------------------------------
%% @doc The Ceph storage configuration (uses librados).
%% @end
%%--------------------------------------------------------------------
-spec cephrados_modify_model() -> onepanel_parser:object_spec().
cephrados_modify_model() ->
    #{
        %% The name of storage.
        name => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% Type of the modified storage. Must match the type of existing
        %% storage, needed only for OpenAPI polymorphism disambiguation.
        type => {discriminator, <<"cephrados">>},
        %% The username of the Ceph cluster administrator.
        username => {string, optional},
        %% The admin key to access the Ceph cluster.
        key => {string, optional},
        %% The monitor hostname.
        monitorHostname => {string, optional},
        %% The Ceph cluster name.
        clusterName => {string, optional},
        %% The Ceph pool name.
        poolName => {string, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The GlusterFS storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec glusterfs_model() -> onepanel_parser:object_spec().
glusterfs_model() ->
    #{
        %% The type of storage.
        type => {discriminator, <<"glusterfs">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
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
        transport => {{enum, string, [<<"tcp">>, <<"rdma">>, <<"socket">>]}, optional},
        %% Relative mountpoint within the volume which should be used by
        %% Oneprovider.
        mountPoint => {string, optional},
        %% Volume specific GlusterFS translator options, in the format:
        %% TRANSLATOR1.OPTION1=VALUE1;TRANSLATOR2.OPTION2=VALUE2;...
        xlatorOptions => {string, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"canonical">>}}
    }.

%%--------------------------------------------------------------------
%% @doc The GlusterFS storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec glusterfs_modify_model() -> onepanel_parser:object_spec().
glusterfs_modify_model() ->
    #{
        %% The name of storage.
        name => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% Type of the modified storage. Must match the type of existing
        %% storage, needed only for OpenAPI polymorphism disambiguation.
        type => {discriminator, <<"glusterfs">>},
        %% The name of the volume to use as a storage backend.
        volume => {string, optional},
        %% The hostname (IP address or FQDN) of GlusterFS volume server.
        hostname => {string, optional},
        %% The GlusterFS port on volume server.
        port => {integer, optional},
        %% The transport protocol to use to connect to the volume server.
        transport => {{enum, string, [<<"tcp">>, <<"rdma">>, <<"socket">>]}, optional},
        %% Relative mountpoint within the volume which should be used by
        %% Oneprovider.
        mountPoint => {string, optional},
        %% Volume specific GlusterFS translator options, in the format:
        %% TRANSLATOR1.OPTION1=VALUE1;TRANSLATOR2.OPTION2=VALUE2;...
        xlatorOptions => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Configuration of a Ceph (librados) storage backed by a Ceph pool created
%% in the local Ceph cluster.
%% @end
%%--------------------------------------------------------------------
-spec localceph_model() -> onepanel_parser:object_spec().
localceph_model() ->
    #{
        %% The type of storage.
        type => {discriminator, <<"localceph">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% Desired number of object replicas in the pool. When below this number
        %% the pool still may be used in 'degraded' mode. Defaults to
        %% `2` if there are at least 2 OSDs, `1` otherwise.
        copiesNumber => {integer, optional},
        %% Minimum number of object replicas in the pool. Below this threshold
        %% any I/O for the pool is disabled. Must be lower or equal to
        %% 'copiesNumber'. Defaults to `min(2, copiesNumber)`
        %% if there are at least 2 OSDs, `1` otherwise.
        minCopiesNumber => {integer, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, {optional, true}},
        %% Storage block size in bytes.
        blockSize => {integer, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"flat">>}}
    }.

%%--------------------------------------------------------------------
%% @doc Modifiable fields of a Ceph storage backed by a local pool.
%% @end
%%--------------------------------------------------------------------
-spec localceph_modify_model() -> onepanel_parser:object_spec().
localceph_modify_model() ->
    #{
        %% Desired number of object replicas in the pool. When below this number
        %% the pool still may be used in 'degraded' mode. Defaults to
        %% `2` if there are at least 2 OSDs, `1` otherwise.
        copiesNumber => {integer, optional},
        %% Minimum number of object replicas in the pool. Below this threshold
        %% any I/O for the pool is disabled. Must be lower or equal to
        %% 'copiesNumber'. Defaults to `min(2, copiesNumber)`
        %% if there are at least 2 OSDs, `1` otherwise.
        minCopiesNumber => {integer, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% The type of storage.
        type => {discriminator, <<"localceph">>}
    }.

%%--------------------------------------------------------------------
%% @doc Loopdevice OSD specification. This is a Blockdevice OSD backed by a
%% loopdevice backed by a file in given directory.
%% @end
%%--------------------------------------------------------------------
-spec loopdevice_model() -> onepanel_parser:object_spec().
loopdevice_model() ->
    #{
        %% Host on which given OSD should be deployed. It must be the full host
        %% name and not an \&quot;alias\&quot; as used in Oneprovider cluster
        %% deployment.
        host => string,
        %% UUID of the OSD daemon. If provided, will be used to skip deployment
        %% of existing OSDs (identified by the UUID). Must be a 32-character hex
        %% string. By default will be generated automatically.
        uuid => {string, optional},
        %% Type of the OSD. Available types are: - blockdevice - formats a raw
        %% block device to store the data - loopdevice - stores data in a file
        %% mounted as loop device
        type => {discriminator, <<"loopdevice">>},
        %% Path of the loopdevice file to be created. If omitted, default path
        %% will be generated according to following template:
        %% /volumes/persistence/ceph-loopdevices/osd-{uuid}.loop
        path => {string, optional},
        %% Size in bytes of the loopdevice file.
        size => integer
    }.

%%--------------------------------------------------------------------
%% @doc The Null Device storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec nulldevice_model() -> onepanel_parser:object_spec().
nulldevice_model() ->
    #{
        %% The type of storage.
        type => {discriminator, <<"nulldevice">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
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
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"canonical">>}},
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
%% @doc The Null Device storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec nulldevice_modify_model() -> onepanel_parser:object_spec().
nulldevice_modify_model() ->
    #{
        %% The name of storage.
        name => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% Type of the modified storage. Must match the type of existing
        %% storage, needed only for OpenAPI polymorphism disambiguation.
        type => {discriminator, <<"nulldevice">>},
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
%% @doc Public Oneprovider configuration details.
%% @end
%%--------------------------------------------------------------------
-spec op_configuration_model() -> onepanel_parser:object_spec().
op_configuration_model() ->
    #{
        %% The Id of cluster record for this cluster. `null` if the
        %% cluster is not registered.
        clusterId => string,
        %% Version of this Onepanel.
        version => string,
        %% Build number of this Onepanel.
        build => string,
        %% True when cluster deployment is finished.
        deployed => boolean,
        %% Indicates that this is Oneprovider's panel.
        serviceType => {discriminator, <<"oneprovider">>},
        %% This cluster's Oneprovider Id. `null` if the
        %% Oneprovider is not registered or Oneprovider worker is down.
        providerId => string,
        %% The domain of the Onezone where this Oneprovider is registered.
        %% `null` if the Oneprovider is not registered.
        zoneDomain => string,
        %% True if the Oneprovider has been registered at a Onezone.
        isRegistered => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Public Onezone configuration details.
%% @end
%%--------------------------------------------------------------------
-spec oz_configuration_model() -> onepanel_parser:object_spec().
oz_configuration_model() ->
    #{
        %% The Id of cluster record for this cluster. `null` if the
        %% cluster is not registered.
        clusterId => string,
        %% Version of this Onepanel.
        version => string,
        %% Build number of this Onepanel.
        build => string,
        %% True when cluster deployment is finished.
        deployed => boolean,
        %% Indicates that this is Onezone's panel.
        serviceType => {discriminator, <<"onezone">>},
        %% The domain of this Onezone cluster. `null` before cluster
        %% is configured.
        zoneDomain => string,
        %% The name of this Onezone cluster. `null` before cluster is
        %% configured.
        zoneName => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The POSIX storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec posix_model() -> onepanel_parser:object_spec().
posix_model() ->
    #{
        %% The type of storage.
        type => {discriminator, <<"posix">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% The absolute path to the directory where the POSIX storage is mounted
        %% on the cluster nodes.
        mountPoint => string,
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"canonical">>}}
    }.

%%--------------------------------------------------------------------
%% @doc The POSIX storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec posix_modify_model() -> onepanel_parser:object_spec().
posix_modify_model() ->
    #{
        %% The name of storage.
        name => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% Type of the modified storage. Must match the type of existing
        %% storage, needed only for OpenAPI polymorphism disambiguation.
        type => {discriminator, <<"posix">>},
        %% The absolute path to the directory where the POSIX storage is mounted
        %% on the cluster nodes.
        mountPoint => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The Simple Storage Service configuration.
%% @end
%%--------------------------------------------------------------------
-spec s3_model() -> onepanel_parser:object_spec().
s3_model() ->
    #{
        %% The type of storage.
        type => {discriminator, <<"s3">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
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
        %% Storage block size in bytes. In case the block size is `0`
        %% and `canonical` path type is selected, each file is stored
        %% in a single S3 object. This value must be set to `0` to
        %% enable data import from an existing S3 bucket.
        blockSize => {integer, optional},
        %% Defines the maximum size for objects, which can be modified on the S3
        %% storage in `canonical` path mode. In this mode, entire file
        %% needs to be downloaded to memory, modified and uploaded back, which
        %% is impractical for large files (default 64 MiB).
        maximumCanonicalObjectSize => {integer, {optional, 67108864}},
        %% Defines the file permissions, which files imported from S3 storage
        %% will have in Onedata. Values should be provided in octal format e.g.
        %% `0644`.
        fileMode => {string, optional},
        %% Defines the directory mode which directories imported from S3 storage
        %% will have in Onedata. Values should be provided in octal format e.g.
        %% `0775`.
        dirMode => {string, optional},
        %% Defines whether storage administrator credentials (accessKey and
        %% secretKey) may be used by users without storage accounts to access
        %% storage in direct IO mode.
        insecure => {boolean, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"flat">>}}
    }.

%%--------------------------------------------------------------------
%% @doc The Simple Storage Service configuration.
%% @end
%%--------------------------------------------------------------------
-spec s3_modify_model() -> onepanel_parser:object_spec().
s3_modify_model() ->
    #{
        %% The name of storage.
        name => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% Type of the modified storage. Must match the type of existing
        %% storage, needed only for OpenAPI polymorphism disambiguation.
        type => {discriminator, <<"s3">>},
        %% The hostname of a machine where S3 storage is installed.
        hostname => {string, optional},
        %% The storage bucket name.
        bucketName => {string, optional},
        %% The access key to the S3 storage.
        accessKey => {string, optional},
        %% The secret key to the S3 storage.
        secretKey => {string, optional},
        %% The version of signature used to sign requests. One of: 2, 4.
        %% Default: 4.
        signatureVersion => {integer, optional},
        %% Defines the maximum size for objects, which can be modified on the S3
        %% storage in `canonical` path mode. In this mode, entire file
        %% needs to be downloaded to memory, modified and uploaded back, which
        %% is impractical for large files (default 64 MiB).
        maximumCanonicalObjectSize => {integer, optional},
        %% Defines the file permissions, which files imported from S3 storage
        %% will have in Onedata. Values should be provided in octal format e.g.
        %% `0644`.
        fileMode => {string, optional},
        %% Defines the directory mode which directories imported from S3 storage
        %% will have in Onedata. Values should be provided in octal format e.g.
        %% `0775`.
        dirMode => {string, optional},
        %% Defines whether storage administrator credentials (accessKey and
        %% secretKey) may be used by users without storage accounts to access
        %% storage in direct IO mode.
        insecure => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The OpenStack Swift configuration.
%% @end
%%--------------------------------------------------------------------
-spec swift_model() -> onepanel_parser:object_spec().
swift_model() ->
    #{
        %% The type of storage.
        type => {discriminator, <<"swift">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
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
        %% Storage block size in bytes.
        blockSize => {integer, optional},
        %% Defines whether storage administrator credentials (username and
        %% password) may be used by users without storage accounts to access
        %% storage in direct IO mode.
        insecure => {boolean, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"flat">>}}
    }.

%%--------------------------------------------------------------------
%% @doc The OpenStack Swift configuration.
%% @end
%%--------------------------------------------------------------------
-spec swift_modify_model() -> onepanel_parser:object_spec().
swift_modify_model() ->
    #{
        %% The name of storage.
        name => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% Type of the modified storage. Must match the type of existing
        %% storage, needed only for OpenAPI polymorphism disambiguation.
        type => {discriminator, <<"swift">>},
        %% The URL to OpenStack Keystone identity service.
        authUrl => {string, optional},
        %% The name of the tenant to which the user belongs.
        tenantName => {string, optional},
        %% The name of the Swift storage container.
        containerName => {string, optional},
        %% The Keystone authentication username.
        username => {string, optional},
        %% The Keystone authentication password.
        password => {string, optional},
        %% Defines whether storage administrator credentials (username and
        %% password) may be used by users without storage accounts to access
        %% storage in direct IO mode.
        insecure => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The WebDAV storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec webdav_model() -> onepanel_parser:object_spec().
webdav_model() ->
    #{
        %% The type of storage.
        type => {discriminator, <<"webdav">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
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
        credentialsType => {{enum, string, [<<"none">>, <<"basic">>, <<"token">>, <<"oauth2">>]}, {optional, none}},
        %% The credentials to authenticate with the WebDAV server.
        %% `basic` credentials should be provided in the form
        %% `username:password`, for `token` just the token.
        %% In case of `oauth2`, this field should contain the username
        %% for the WebDAV, while the token will be obtained and refreshed
        %% automatically in the background. For `none` this field is
        %% ignored.
        credentials => {string, optional},
        %% In case `oauth2` credential type is selected and Onezone is
        %% configured with support for multiple external IdP's, this field
        %% must contain the name of the IdP which authenticates requests to the
        %% WebDAV endpoint. If Onezone has only one external IdP, it will be
        %% selected automatically.
        oauth2IdP => {string, optional},
        %% When registering storage in `insecure` mode with
        %% `oauth2` external IdP, this field must contain a valid
        %% Onedata access token of the user on whose behalf the WebDAV storage
        %% will be accessed by all users with access to any space supported by
        %% this storage.
        onedataAccessToken => {string, optional},
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
        rangeWriteSupport => {{enum, string, [<<"none">>, <<"moddav">>, <<"sabredav">>]}, {optional, none}},
        %% Defines the maximum number of parallel connections for a single
        %% WebDAV storage.
        connectionPoolSize => {integer, {optional, 25}},
        %% Defines the maximum upload size for a single `PUT` or
        %% `PATCH` request. If set to 0, assumes that the WebDAV
        %% server has no upload limit.
        maximumUploadSize => {integer, {optional, 0}},
        %% Defines the file permissions, which files imported from WebDAV
        %% storage will have in Onedata. Values should be provided in octal
        %% format e.g. `0644`.
        fileMode => {string, optional},
        %% Defines the directory mode which directories imported from WebDAV
        %% storage will have in Onedata. Values should be provided in octal
        %% format e.g. `0775`.
        dirMode => {string, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"canonical">>}}
    }.

%%--------------------------------------------------------------------
%% @doc The WebDAV storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec webdav_modify_model() -> onepanel_parser:object_spec().
webdav_modify_model() ->
    #{
        %% The name of storage.
        name => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% Defines whether storage is readonly.
        readonly => {boolean, optional},
        %% If true LUMA and reverse LUMA services will be enabled.
        lumaEnabled => {boolean, optional},
        %% URL of external LUMA service.
        lumaUrl => {string, optional},
        %% LUMA API Key, must be identical with API Key in external LUMA
        %% service.
        lumaApiKey => {string, optional},
        %% Type of the modified storage. Must match the type of existing
        %% storage, needed only for OpenAPI polymorphism disambiguation.
        type => {discriminator, <<"webdav">>},
        %% Full URL of the WebDAV server, including scheme (http or https) and
        %% path.
        endpoint => {string, optional},
        %% Determines whether Oneprovider should verify the certificate of the
        %% WebDAV server.
        verifyServerCertificate => {boolean, optional},
        %% Determines the types of credentials provided in the credentials
        %% field.
        credentialsType => {{enum, string, [<<"none">>, <<"basic">>, <<"token">>]}, optional},
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
        rangeWriteSupport => {{enum, string, [<<"none">>, <<"moddav">>, <<"sabredav">>]}, optional},
        %% Defines the maximum number of parallel connections for a single
        %% WebDAV storage.
        connectionPoolSize => {integer, optional},
        %% Defines the maximum upload size for a single `PUT` or
        %% `PATCH` request. If set to 0, assumes that the WebDAV
        %% server has no upload limit.
        maximumUploadSize => {integer, optional},
        %% Defines whether storage administrator credentials (username and key)
        %% may be used by users without storage accounts to access storage in
        %% direct IO mode.
        insecure => {boolean, optional}
    }.

