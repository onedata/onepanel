%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @author Krzysztof Trzepla
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
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
    auto_storage_import_config_model/0,
    auto_storage_import_info_model/0,
    auto_storage_import_stats_model/0,
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
    id_model/0,
    ids_model/0,
    inline_response_202_model/0,
    invite_token_model/0,
    luma_config_model/0,
    luma_onedata_group_model/0,
    luma_onedata_user_model/0,
    luma_storage_credentials_model/0,
    luma_storage_user_model/0,
    luma_user_mapping_model/0,
    manager_hosts_model/0,
    manual_storage_import_example_model/0,
    modify_cluster_ips_model/0,
    node_model/0,
    onezone_info_model/0,
    onezone_user_model/0,
    onezone_user_create_request_model/0,
    panel_configuration_model/0,
    password_change_request_model/0,
    posix_compatible_credentials_model/0,
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
    space_modify_request_model/0,
    space_support_request_model/0,
    storage_create_details_model/0,
    storage_create_request_model/0,
    storage_get_details_model/0,
    storage_import_model/0,
    storage_modify_details_model/0,
    storage_modify_request_model/0,
    task_id_model/0,
    task_status_model/0,
    time_stats_model/0,
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
    ceph_credentials_model/0,
    ceph_modify_model/0,
    cephrados_model/0,
    cephrados_credentials_model/0,
    cephrados_modify_model/0,
    glusterfs_model/0,
    glusterfs_credentials_model/0,
    glusterfs_modify_model/0,
    http_model/0,
    http_credentials_model/0,
    http_modify_model/0,
    localceph_model/0,
    localceph_modify_model/0,
    loopdevice_model/0,
    luma_idp_entitlement_scheme_model/0,
    luma_idp_user_scheme_model/0,
    luma_onedata_group_scheme_model/0,
    luma_onedata_user_scheme_model/0,
    nulldevice_model/0,
    nulldevice_credentials_model/0,
    nulldevice_modify_model/0,
    op_configuration_model/0,
    oz_configuration_model/0,
    posix_model/0,
    posix_credentials_model/0,
    posix_modify_model/0,
    s3_model/0,
    s3_credentials_model/0,
    s3_modify_model/0,
    swift_model/0,
    swift_credentials_model/0,
    swift_modify_model/0,
    webdav_model/0,
    webdav_credentials_model/0,
    webdav_modify_model/0,
    xrootd_model/0,
    xrootd_credentials_model/0,
    xrootd_modify_model/0
]).


%%--------------------------------------------------------------------
%% @doc Configuration of auto storage import mechanism. The auto import is based
%% on scans - gradual traversing of the file system and registration of files
%% and directories.
%% @end
%%--------------------------------------------------------------------
-spec auto_storage_import_config_model() -> onepanel_parser:object_spec().
auto_storage_import_config_model() ->
    #{
        %% Maximum depth of filesystem tree that will be traversed during the
        %% scan.
        maxDepth => {integer, optional},
        %% Flag that enables synchronization of NFSv4 ACLs.
        syncAcl => {boolean, optional},
        %% With this option enabled the storage will be scanned periodically and
        %% direct changes on the storage will be reflected in the assigned
        %% Onedata space (upon the consecutive scan).
        continuousScan => {boolean, optional},
        %% Period between subsequent scans in seconds (counted from end of one
        %% scan till beginning of the following). This parameter is relevant
        %% only for continuous scans.
        scanInterval => {integer, optional},
        %% Flag determining that modifications of files on the synchronized
        %% storage will be detected. If disabled, the storage will be treated as
        %% immutable (only creations and deletions of files on storage will be
        %% detected). This parameter is relevant only for continuous scans.
        detectModifications => {boolean, optional},
        %% Flag determining that deletions of files from the synchronized
        %% storage will be detected. This parameter is relevant only for
        %% continuous scans.
        detectDeletions => {boolean, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Information about current (or last finished) auto storage import scan in
%% given space.
%% @end
%%--------------------------------------------------------------------
-spec auto_storage_import_info_model() -> onepanel_parser:object_spec().
auto_storage_import_info_model() ->
    #{
        %% Describes status of current (or last finished) auto storage import
        %% scan in given space.
        status => {enum, string, [<<"enqueued">>, <<"running">>, <<"aborting">>, <<"completed">>, <<"failed">>, <<"aborted">>]},
        %% Time at which current (or last finished) scan has been started.
        start => integer,
        %% Time at which current (or last finished) scan has been stopped.
        stop => integer,
        %% Counter of created files (both directories and regular files) that
        %% has been detected during current (or last finished) scan.
        createdFiles => integer,
        %% Counter of modified files (both directories and regular files) that
        %% has been detected during current (or last finished) scan.
        modifiedFiles => integer,
        %% Counter of deleted files (both directories and regular files) that
        %% has been detected during current (or last finished) scan.
        deletedFiles => integer,
        %% Counter of unmodified files (both directories and regular files) that
        %% has been detected during current (or last finished) scan.
        unmodifiedFiles => integer,
        %% Counter of files (both directories and regular files) for which the
        %% processing has failed during current (or last finished) scan.
        failedFiles => integer,
        %% Estimated time at which next scan will be enqueued.
        nextScan => {integer, optional},
        %% Total number of performed scans.
        totalScans => integer
    }.

%%--------------------------------------------------------------------
%% @doc Statistics of auto storage import mechanism in given space over
%% specified time.
%% @end
%%--------------------------------------------------------------------
-spec auto_storage_import_stats_model() -> onepanel_parser:object_spec().
auto_storage_import_stats_model() ->
    #{
        %% Statistics of auto storage import jobs queue length.
        queueLength => {time_stats_model(), optional},
        %% Statistics of count of created files (both directories and regular
        %% files) detected by auto storage import.
        createdFiles => {time_stats_model(), optional},
        %% Statistics of count of modified files (both directories and regular
        %% files) detected by auto storage import.
        modifiedFiles => {time_stats_model(), optional},
        %% Statistics of count of deleted files (both directories and regular
        %% files) detected by auto storage import.
        deletedFiles => {time_stats_model(), optional}
    }.

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
        %% List of hosts belonging to the Onepanel cluster.
        hosts => [string],
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
    {subclasses, onepanel_parser:prepare_subclasses([op_configuration_model(), oz_configuration_model()])}.

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
        clusterPrivileges => [{enum, [string], [<<"cluster_view">>, <<"cluster_update">>, <<"cluster_delete">>, <<"cluster_view_privileges">>, <<"cluster_set_privileges">>, <<"cluster_add_user">>, <<"cluster_remove_user">>, <<"cluster_add_group">>, <<"cluster_remove_group">>]}]
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
        dnsServers => {[ip4], optional},
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

-spec id_model() -> onepanel_parser:object_spec().
id_model() ->
    #{
        %% Resource Id.
        id => string
    }.

-spec ids_model() -> onepanel_parser:object_spec().
ids_model() ->
    #{
        %% List of ids.
        ids => [string]
    }.

-spec inline_response_202_model() -> onepanel_parser:object_spec().
inline_response_202_model() ->
    #{
        reportId => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc An invite token.
%% @end
%%--------------------------------------------------------------------
-spec invite_token_model() -> onepanel_parser:object_spec().
invite_token_model() ->
    #{
        inviteToken => string
    }.

%%--------------------------------------------------------------------
%% @doc Configuration of Local User Mapping database (LUMA DB).
%% @end
%%--------------------------------------------------------------------
-spec luma_config_model() -> onepanel_parser:object_spec().
luma_config_model() ->
    #{
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {enum, string, [<<"auto">>, <<"local">>, <<"external">>]},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Representation of Onedata group.
%% @end
%%--------------------------------------------------------------------
-spec luma_onedata_group_model() -> onepanel_parser:multi_spec().
luma_onedata_group_model() ->
    {subclasses, onepanel_parser:prepare_subclasses([luma_onedata_group_scheme_model(), luma_idp_entitlement_scheme_model()])}.

%%--------------------------------------------------------------------
%% @doc Representation of Onedata user.
%% @end
%%--------------------------------------------------------------------
-spec luma_onedata_user_model() -> onepanel_parser:multi_spec().
luma_onedata_user_model() ->
    {subclasses, onepanel_parser:prepare_subclasses([luma_onedata_user_scheme_model(), luma_idp_user_scheme_model()])}.

%%--------------------------------------------------------------------
%% @doc Credentials that will be used to perform actions on the local storage
%% resources in the context of the Onedata user.
%% @end
%%--------------------------------------------------------------------
-spec luma_storage_credentials_model() -> onepanel_parser:multi_spec().
luma_storage_credentials_model() ->
    {subclasses, onepanel_parser:prepare_subclasses([posix_credentials_model(), s3_credentials_model(), ceph_credentials_model(), cephrados_credentials_model(), swift_credentials_model(), glusterfs_credentials_model(), nulldevice_credentials_model(), webdav_credentials_model(), xrootd_credentials_model(), http_credentials_model()])}.

%%--------------------------------------------------------------------
%% @doc Credentials identifying user on the local storage resources.
%% @end
%%--------------------------------------------------------------------
-spec luma_storage_user_model() -> onepanel_parser:object_spec().
luma_storage_user_model() ->
    #{
        storageCredentials => luma_storage_credentials_model(),
        %% This value will be used in the Oneclient to display it's owner
        %% UID if the corresponding user is owner of the file.
        displayUid => {integer, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Mapping that will be stored in LUMA DB.
%% @end
%%--------------------------------------------------------------------
-spec luma_user_mapping_model() -> onepanel_parser:object_spec().
luma_user_mapping_model() ->
    #{
        onedataUser => luma_onedata_user_model(),
        storageUser => luma_storage_user_model()
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
%% @doc Example `curl` command that can be executed to manually import
%% (register) file from storage. For more info please read:
%% https://onedata.org/#/home/api/stable/oneprovider?anchor=tag/File-
%% registration
%% @end
%%--------------------------------------------------------------------
-spec manual_storage_import_example_model() -> onepanel_parser:object_spec().
manual_storage_import_example_model() ->
    #{
        curl => string
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
        fullName => {string, optional},
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
%% @doc Credentials on POSIX compatible storages.
%% @end
%%--------------------------------------------------------------------
-spec posix_compatible_credentials_model() -> onepanel_parser:object_spec().
posix_compatible_credentials_model() ->
    #{
        %% User identifier.
        uid => {integer, optional},
        %% Group identifier.
        gid => {integer, optional}
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
%% @doc The collection of hosts with associated service status, for each host
%% where given service has been deployed.
%% @end
%%--------------------------------------------------------------------
-spec service_status_model() -> onepanel_parser:object_spec().
service_status_model() ->
    #{'_' => service_status_host_model()}.

%%--------------------------------------------------------------------
%% @doc The service status.
%% @end
%%--------------------------------------------------------------------
-spec service_status_host_model() -> onepanel_parser:object_spec().
service_status_host_model() ->
    #{
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
        filesNumber => integer,
        %% Status of an auto-cleaning run.
        status => {enum, string, [<<"active">>, <<"cancelling">>, <<"completed">>, <<"failed">>, <<"cancelled">>]}
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
        storageImport => {storage_import_model(), optional},
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
%% @doc The space configuration details that can be modified.
%% @end
%%--------------------------------------------------------------------
-spec space_modify_request_model() -> onepanel_parser:object_spec().
space_modify_request_model() ->
    #{
        %% The storage space size in bytes that provider is willing to assign to
        %% the space.
        size => {integer, optional},
        autoStorageImportConfig => {auto_storage_import_config_model(), optional}
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
        storageImport => {storage_import_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc The storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec storage_create_details_model() -> onepanel_parser:multi_spec().
storage_create_details_model() ->
    {subclasses, onepanel_parser:prepare_subclasses([posix_model(), s3_model(), cephrados_model(), localceph_model(), swift_model(), glusterfs_model(), nulldevice_model(), webdav_model(), xrootd_model(), http_model()])}.

%%--------------------------------------------------------------------
%% @doc The configuration details required to add storage resources.
%% @end
%%--------------------------------------------------------------------
-spec storage_create_request_model() -> onepanel_parser:object_spec().
storage_create_request_model() ->
    #{'_' => storage_create_details_model()}.

%%--------------------------------------------------------------------
%% @doc The storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec storage_get_details_model() -> onepanel_parser:multi_spec().
storage_get_details_model() ->
    {subclasses, onepanel_parser:prepare_subclasses([posix_model(), s3_model(), ceph_model(), cephrados_model(), localceph_model(), swift_model(), glusterfs_model(), nulldevice_model(), webdav_model(), xrootd_model()])}.

%%--------------------------------------------------------------------
%% @doc Configuration of the storage import within the space.
%% @end
%%--------------------------------------------------------------------
-spec storage_import_model() -> onepanel_parser:object_spec().
storage_import_model() ->
    #{
        %% Mode of the storage import within the space.  In case of
        %% `auto` mode, the storage will be automatically scanned and
        %% data will be imported from storage into the assigned Onedata space
        %% without need for copying the data.  Configuration of the auto storage
        %% import can be passed in the `autoStorageImportConfig`
        %% parameter. It is possible to enable periodical scans for automatic
        %% detection of changes on the storage (refer to the option
        %% `continuousScan` in the config).  In case of
        %% `manual` mode, the files must be registered manually by the
        %% space users with REST API. Registration of directories is not
        %% supported. For more info please read:
        %% https://onedata.org/#/home/api/stable/oneprovider?anchor=tag
        %% /File-registration
        mode => {{enum, string, [<<"auto">>, <<"manual">>]}, {optional, <<"auto">>}},
        autoStorageImportConfig => {auto_storage_import_config_model(), optional}
    }.

%%--------------------------------------------------------------------
%% @doc An object with new values for storage parameters to be updated.
%% @end
%%--------------------------------------------------------------------
-spec storage_modify_details_model() -> onepanel_parser:multi_spec().
storage_modify_details_model() ->
    {subclasses, onepanel_parser:prepare_subclasses([posix_modify_model(), s3_modify_model(), ceph_modify_model(), cephrados_modify_model(), localceph_modify_model(), swift_modify_model(), glusterfs_modify_model(), nulldevice_modify_model(), webdav_modify_model(), xrootd_modify_model(), http_modify_model()])}.

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
%% @doc Object providing task Id of the started task.
%% @end
%%--------------------------------------------------------------------
-spec task_id_model() -> onepanel_parser:object_spec().
task_id_model() ->
    #{
        taskId => {string, optional}
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
%% @doc Statistics of single metric over specified time.
%% @end
%%--------------------------------------------------------------------
-spec time_stats_model() -> onepanel_parser:object_spec().
time_stats_model() ->
    #{
        %% Date of last measurement value in this object in ISO 8601 format.
        lastValueDate => string,
        %% List of sample values for given metric. The used period is divided
        %% into array-length number of parts. E.g. if the used period is an
        %% hour, and if there are 12 values in this array, every value is a
        %% value for 1/12 of day, which gives value for every hour of the day.
        %% If the value is null, there is no sample for given time part.
        values => [number]
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
        domainName => string,
        %% The Onezone cluster name.
        name => string,
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
        %% SHA-256 checksums using the compatibility registry. Setting this
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
        %% The type of storage.  `type = \&quot;ceph\&quot;`
        %% (**DEPRECATED** - use Ceph RADOS instead) storage backend compatible
        %% with [Ceph](http://ceph.com/ceph-storage/) object storage, using the
        %% deprecated `libradosstriper` library.
        type => {discriminator, <<"ceph">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, {optional, <<"auto">>}},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        qosParameters => {#{'_' => string}, {optional, #{}}},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The username of the Ceph cluster user. In case of configuring
        %% storage, this field must be equal to name of the Ceph cluster admin.
        username => string,
        %% The key to access the Ceph cluster. In case of configuring storage,
        %% the key must be the key of admin user passed in `username`.
        key => string,
        %% The monitor hostname.
        monitorHostname => string,
        %% The Ceph cluster name.
        clusterName => string,
        %% The Ceph pool name.
        poolName => string,
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
%% @doc Credentials on the CEPH storage.
%% @end
%%--------------------------------------------------------------------
-spec ceph_credentials_model() -> onepanel_parser:object_spec().
ceph_credentials_model() ->
    #{
        %% Type of the storage. Must be given explicitly and must match the
        %% actual type of subject storage - this redundancy is needed due to
        %% limitations of OpenAPI polymorphism.
        type => {discriminator, <<"ceph">>},
        %% The username of the Ceph cluster user. In case of configuring
        %% storage, this field must be equal to name of the Ceph cluster admin.
        username => string,
        %% The key to access the Ceph cluster. In case of configuring storage,
        %% the key must be the key of admin user passed in `username`.
        key => string
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
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, optional},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        %% Overrides all previously set parameters.
        qosParameters => {#{'_' => string}, optional},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% Type of the modified storage. Must be given explicitly and must match
        %% the actual type of subject storage - this redundancy is needed due to
        %% limitations of OpenAPI polymorphism.  `type =
        %% \&quot;ceph\&quot;`  (**DEPRECATED** - use Ceph RADOS instead)
        %% storage backend compatible with [Ceph](http://ceph.com/ceph-storage/)
        %% object storage, using the deprecated `libradosstriper`
        %% library.
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
        poolName => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The Ceph storage configuration (uses librados).
%% @end
%%--------------------------------------------------------------------
-spec cephrados_model() -> onepanel_parser:object_spec().
cephrados_model() ->
    #{
        %% The type of storage.  `type = \&quot;cephrados\&quot;`
        %% Storage backend compatible with [Ceph](http://ceph.com/ceph-storage/)
        %% object storage.
        type => {discriminator, <<"cephrados">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, {optional, <<"auto">>}},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        qosParameters => {#{'_' => string}, {optional, #{}}},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
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
        blockSize => {integer, {optional, 4194304}},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"flat">>}}
    }.

%%--------------------------------------------------------------------
%% @doc Credentials on the Ceph storage (using librados).
%% @end
%%--------------------------------------------------------------------
-spec cephrados_credentials_model() -> onepanel_parser:object_spec().
cephrados_credentials_model() ->
    #{
        %% Type of the storage. Must be given explicitly and must match the
        %% actual type of subject storage - this redundancy is needed due to
        %% limitations of OpenAPI polymorphism.
        type => {discriminator, <<"cephrados">>},
        %% The username of the Ceph cluster administrator.
        username => string,
        %% The admin key to access the Ceph cluster.
        key => string
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
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, optional},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        %% Overrides all previously set parameters.
        qosParameters => {#{'_' => string}, optional},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The type of storage.  `type = \&quot;cephrados\&quot;`
        %% Storage backend compatible with [Ceph](http://ceph.com/ceph-storage/)
        %% object storage.
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
        poolName => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The GlusterFS storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec glusterfs_model() -> onepanel_parser:object_spec().
glusterfs_model() ->
    #{
        %% The type of storage.  `type = \&quot;glusterfs\&quot;`
        %% [GlusterFS](https://www.gluster.org/) volume directly attached to the
        %% Oneprovider.
        type => {discriminator, <<"glusterfs">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, {optional, <<"auto">>}},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        qosParameters => {#{'_' => string}, {optional, #{}}},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The name of the volume to use as a storage backend.
        volume => string,
        %% The hostname (IP address or FQDN) of GlusterFS volume server.
        hostname => string,
        %% The GlusterFS port on volume server.
        port => {integer, {optional, 24007}},
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
%% @doc Credentials on the GlusterFS storage.
%% @end
%%--------------------------------------------------------------------
-spec glusterfs_credentials_model() -> onepanel_parser:object_spec().
glusterfs_credentials_model() ->
    #{
        %% User identifier.
        uid => {integer, optional},
        %% Group identifier.
        gid => {integer, optional},
        %% Type of the storage. Must be given explicitly and must match the
        %% actual type of subject storage - this redundancy is needed due to
        %% limitations of OpenAPI polymorphism.
        type => {discriminator, <<"glusterfs">>}
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
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, optional},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        %% Overrides all previously set parameters.
        qosParameters => {#{'_' => string}, optional},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The type of storage.  `type = \&quot;glusterfs\&quot;`
        %% [GlusterFS](https://www.gluster.org/) volume directly attached to the
        %% Oneprovider.
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
%% @doc The HTTP storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec http_model() -> onepanel_parser:object_spec().
http_model() ->
    #{
        %% The type of storage.  `type = \&quot;http\&quot;`  Any
        %% [HTTP](https://tools.ietf.org/html/rfc7231) or HTTPS compatible
        %% server. Supported only with Readonly option enabled and in manual
        %% import mode.
        type => {discriminator, <<"http">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, {optional, <<"auto">>}},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        qosParameters => {#{'_' => string}, {optional, #{}}},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% Determines the types of credentials provided in the credentials
        %% field.
        credentialsType => {{enum, string, [<<"none">>, <<"basic">>, <<"token">>, <<"oauth2">>]}, {optional, <<"none">>}},
        %% The credentials to authenticate with the HTTP server.
        %% `basic` credentials should be provided in the form
        %% `username:password`, for `token` just the token.
        %% In case of `oauth2`, this field should contain the username
        %% for the HTTP, while the token will be obtained and refreshed
        %% automatically in the background. For `none` this field is
        %% ignored.
        credentials => {string, optional},
        %% In case `oauth2` credential type is selected and Onezone is
        %% configured with support for multiple external IdP's, this field
        %% must contain the name of the IdP which authenticates requests to the
        %% HTTP endpoint. If Onezone has only one external IdP, it will be
        %% selected automatically.
        oauth2IdP => {string, optional},
        %% When registering storage with feed of LUMA DB set to`auto`
        %% and with `oauth2` external IdP, this field must contain a
        %% valid Onedata access token of the user on whose behalf the HTTP
        %% storage will be accessed by all users with access to any space
        %% supported by this storage.
        onedataAccessToken => {string, optional},
        %% Full URL of the HTTP server, including scheme (http or https) and
        %% path.
        endpoint => string,
        %% Determines whether Oneprovider should verify the certificate of the
        %% HTTP server.
        verifyServerCertificate => {boolean, {optional, true}},
        %% The authorization header to be used for passing the access token.
        %% This field can contain any prefix that should be added to the header
        %% value. Default is `Authorization: Bearer {}`. The token
        %% will placed where `{}` is provided.
        authorizationHeader => {string, {optional, <<"Authorization: Bearer {}">>}},
        %% Defines the maximum number of parallel connections for a single HTTP
        %% storage.
        connectionPoolSize => {integer, {optional, 150}},
        %% Defines the file permissions, which files imported from HTTP storage
        %% will have in Onedata. Values should be provided in octal format e.g.
        %% `0664`.
        fileMode => {string, optional},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"canonical">>}}
    }.

%%--------------------------------------------------------------------
%% @doc Credentials on the HTTP storage.
%% @end
%%--------------------------------------------------------------------
-spec http_credentials_model() -> onepanel_parser:object_spec().
http_credentials_model() ->
    #{
        %% Type of the storage. Must be given explicitly and must match the
        %% actual type of subject storage - this redundancy is needed due to
        %% limitations of OpenAPI polymorphism.
        type => {discriminator, <<"http">>},
        %% Determines the types of credentials provided in the credentials
        %% field.
        credentialsType => {{enum, string, [<<"none">>, <<"basic">>, <<"token">>, <<"oauth2">>]}, {optional, <<"none">>}},
        %% The credentials to authenticate with the HTTP server.
        %% `basic` credentials should be provided in the form
        %% `username:password`, for `token` just the token.
        %% In case of `oauth2`, this field should contain the username
        %% for the HTTP, while the token will be obtained and refreshed
        %% automatically in the background. For `none` this field is
        %% ignored.
        credentials => {string, optional},
        %% In case `oauth2` credential type is selected and Onezone is
        %% configured with support for multiple external IdP's, this field
        %% must contain the name of the IdP which authenticates requests to the
        %% HTTP endpoint. If Onezone has only one external IdP, it will be
        %% selected automatically.
        oauth2IdP => {string, optional},
        %% When registering storage with feed of LUMA DB set to`auto`
        %% and with `oauth2` external IdP, this field must contain a
        %% valid Onedata access token of the user on whose behalf the HTTP
        %% storage will be accessed by all users with access to any space
        %% supported by this storage.
        onedataAccessToken => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The HTTP storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec http_modify_model() -> onepanel_parser:object_spec().
http_modify_model() ->
    #{
        %% The name of storage.
        name => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, optional},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        %% Overrides all previously set parameters.
        qosParameters => {#{'_' => string}, optional},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The type of storage.  `type = \&quot;http\&quot;`  Any
        %% [HTTP](https://tools.ietf.org/html/rfc7231) or HTTPS compatible
        %% server. Supported only with Readonly option enabled and in manual
        %% import mode.
        type => {discriminator, <<"http">>},
        %% Full URL of the HTTP server, including scheme (http or https) and
        %% path.
        endpoint => {string, optional},
        %% Determines whether Oneprovider should verify the certificate of the
        %% HTTP server.
        verifyServerCertificate => {boolean, optional},
        %% Determines the types of credentials provided in the credentials
        %% field.
        credentialsType => {{enum, string, [<<"none">>, <<"basic">>, <<"token">>]}, optional},
        %% The credentials to authenticate with the HTTP server.
        %% `basic` credentials should be provided in the form
        %% `username:password`, for `token` just the token.
        %% For `none` this field is ignored.
        credentials => {string, optional},
        %% The authorization header to be used for passing the access token.
        %% This field can contain any prefix that should be added to the header
        %% value. Default is `Authorization: Bearer {}`. The token
        %% will placed where `{}` is provided.
        authorizationHeader => {string, optional},
        %% Defines the maximum number of parallel connections for a single HTTP
        %% storage.
        connectionPoolSize => {integer, optional},
        %% Defines the file permissions, which files imported from HTTP storage
        %% will have in Onedata. Values should be provided in octal format e.g.
        %% `0664`.
        fileMode => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Configuration of a Ceph (librados) storage backed by a Ceph pool created
%% in the local Ceph cluster.
%% @end
%%--------------------------------------------------------------------
-spec localceph_model() -> onepanel_parser:object_spec().
localceph_model() ->
    #{
        %% The type of storage.  `type = \&quot;localceph\&quot;`
        %% Local Ceph cluster that has been deployed during deployment of
        %% Oneprovider. For more information on local Ceph deployment please see
        %% [here](https://onedata.org/#/home/documentation/stable/doc/administering_onedata/ceph_cluster_deployment.html).
        type => {discriminator, <<"localceph">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, {optional, <<"auto">>}},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        qosParameters => {#{'_' => string}, {optional, #{}}},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% Desired number of object replicas in the pool. When below this number
        %% the pool still may be used in 'degraded' mode. Defaults to
        %% `2` if there are at least 2 OSDs, `1` otherwise.
        copiesNumber => {integer, optional},
        %% Minimum number of object replicas in the pool. Below this threshold
        %% any I/O for the pool is disabled. Must be lower or equal to
        %% 'copiesNumber'. Defaults to `min(2, copiesNumber)`
        %% if there are at least 2 OSDs, `1` otherwise.
        minCopiesNumber => {integer, optional},
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
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, optional},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        %% Overrides all previously set parameters.
        qosParameters => {#{'_' => string}, optional},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The type of storage.  `type = \&quot;localceph\&quot;`
        %% Local Ceph cluster that has been deployed during deployment of
        %% Oneprovider. For more information on local Ceph deployment please see
        %% [here](https://onedata.org/#/home/documentation/stable/doc/administering_onedata/ceph_cluster_deployment.html).
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
%% @doc Representation of Onedata group in an external identity provider.
%% @end
%%--------------------------------------------------------------------
-spec luma_idp_entitlement_scheme_model() -> onepanel_parser:object_spec().
luma_idp_entitlement_scheme_model() ->
    #{
        %% Name of scheme used to represent group identity.
        mappingScheme => {discriminator, <<"idpEntitlement">>},
        %% The id of an external identity provider.
        idp => string,
        %% The id of the group understood by the external identity provider, in
        %% the same format as received during OIDC/SAML login flow.
        idpEntitlement => string,
        %% The id of group in the Onedata system.
        onedataGroupId => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Representation of Onedata user in an external identity provider.
%% @end
%%--------------------------------------------------------------------
-spec luma_idp_user_scheme_model() -> onepanel_parser:object_spec().
luma_idp_user_scheme_model() ->
    #{
        %% Name of scheme used to represent user identity.
        mappingScheme => {discriminator, <<"idpUser">>},
        %% The id of an external identity provider.
        idp => string,
        %% The id of the user understood by the external identity provider.
        subjectId => string,
        %% The id of user in Onedata system.
        onedataUserId => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc Representation of Onedata group in the Onedata system.
%% @end
%%--------------------------------------------------------------------
-spec luma_onedata_group_scheme_model() -> onepanel_parser:object_spec().
luma_onedata_group_scheme_model() ->
    #{
        %% Name of scheme used to represent group identity.
        mappingScheme => {discriminator, <<"onedataGroup">>},
        %% The id of group in the Onedata system.
        onedataGroupId => string
    }.

%%--------------------------------------------------------------------
%% @doc Representation of Onedata user in the Onedata system.
%% @end
%%--------------------------------------------------------------------
-spec luma_onedata_user_scheme_model() -> onepanel_parser:object_spec().
luma_onedata_user_scheme_model() ->
    #{
        %% Name of scheme used to represent user identity.
        mappingScheme => {discriminator, <<"onedataUser">>},
        %% The id of user in Onedata system.
        onedataUserId => string
    }.

%%--------------------------------------------------------------------
%% @doc The Null Device storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec nulldevice_model() -> onepanel_parser:object_spec().
nulldevice_model() ->
    #{
        %% The type of storage.  `type =
        %% \&quot;nulldevice\&quot;`  POSIX compatible storage which
        %% emulates behavior of `/dev/null` on local filesystem.
        %% Allows running various performance tests, which are not impacted by
        %% actual storage latency. Skip storage detection option is obligatory
        %% for this type of storage.
        type => {discriminator, <<"nulldevice">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, {optional, <<"auto">>}},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        qosParameters => {#{'_' => string}, {optional, #{}}},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
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
%% @doc Credentials on the Null Device storage.
%% @end
%%--------------------------------------------------------------------
-spec nulldevice_credentials_model() -> onepanel_parser:object_spec().
nulldevice_credentials_model() ->
    #{
        %% User identifier.
        uid => {integer, optional},
        %% Group identifier.
        gid => {integer, optional},
        %% Type of the storage. Must be given explicitly and must match the
        %% actual type of subject storage - this redundancy is needed due to
        %% limitations of OpenAPI polymorphism.
        type => {discriminator, <<"nulldevice">>}
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
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, optional},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        %% Overrides all previously set parameters.
        qosParameters => {#{'_' => string}, optional},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The type of storage.  `type =
        %% \&quot;nulldevice\&quot;`  POSIX compatible storage which
        %% emulates behavior of `/dev/null` on local filesystem.
        %% Allows running various performance tests, which are not impacted by
        %% actual storage latency. Skip storage detection option is obligatory
        %% for this type of storage.
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
        %% The type of storage.  `type = \&quot;posix\&quot;`
        %% Any POSIX compatible storage, typically attached over high-throughput
        %% local network, such as NFS.
        type => {discriminator, <<"posix">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, {optional, <<"auto">>}},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        qosParameters => {#{'_' => string}, {optional, #{}}},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
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
%% @doc Credentials on POSIX storage.
%% @end
%%--------------------------------------------------------------------
-spec posix_credentials_model() -> onepanel_parser:object_spec().
posix_credentials_model() ->
    #{
        %% User identifier.
        uid => {integer, optional},
        %% Group identifier.
        gid => {integer, optional},
        %% Type of the storage. Must be given explicitly and must match the
        %% actual type of subject storage - this redundancy is needed due to
        %% limitations of OpenAPI polymorphism.
        type => {discriminator, <<"posix">>}
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
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, optional},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        %% Overrides all previously set parameters.
        qosParameters => {#{'_' => string}, optional},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The type of storage.  `type = \&quot;posix\&quot;`
        %% Any POSIX compatible storage, typically attached over high-throughput
        %% local network, such as NFS.
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
        %% The type of storage.  `type = \&quot;s3\&quot;`
        %% [Amazon
        %% S3](http://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html)
        %% compatible storage.
        type => {discriminator, <<"s3">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, {optional, <<"auto">>}},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        qosParameters => {#{'_' => string}, {optional, #{}}},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The access key to the S3 storage.
        accessKey => {string, {optional, <<"">>}},
        %% The secret key to the S3 storage.
        secretKey => {string, {optional, <<"">>}},
        %% The hostname of a machine where S3 storage is installed.
        hostname => string,
        %% The storage bucket name.
        bucketName => string,
        %% The version of signature used to sign requests. One of: 2, 4.
        %% Default: 4.
        signatureVersion => {integer, {optional, 4}},
        %% Storage block size in bytes. In case the block size is `0`
        %% and `canonical` path type is selected, each file is stored
        %% in a single S3 object. This value must be set to `0` to
        %% enable data import from an existing S3 bucket.
        blockSize => {integer, {optional, 10485760}},
        %% Defines the maximum size for objects, which can be modified on the S3
        %% storage in `canonical` path mode. In this mode, entire file
        %% needs to be downloaded to memory, modified and uploaded back, which
        %% is impractical for large files (default 64 MiB).
        maximumCanonicalObjectSize => {integer, {optional, 67108864}},
        %% Defines the file permissions, which files imported from S3 storage
        %% will have in Onedata. Values should be provided in octal format e.g.
        %% `0644`.
        fileMode => {string, {optional, <<"0664">>}},
        %% Defines the directory mode which directories imported from S3 storage
        %% will have in Onedata. Values should be provided in octal format e.g.
        %% `0775`.
        dirMode => {string, {optional, <<"0775">>}},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"flat">>}}
    }.

%%--------------------------------------------------------------------
%% @doc Credentials on the Simple Storage Service (S3).
%% @end
%%--------------------------------------------------------------------
-spec s3_credentials_model() -> onepanel_parser:object_spec().
s3_credentials_model() ->
    #{
        %% Type of the storage. Must be given explicitly and must match the
        %% actual type of subject storage - this redundancy is needed due to
        %% limitations of OpenAPI polymorphism.
        type => {discriminator, <<"s3">>},
        %% The access key to the S3 storage.
        accessKey => {string, {optional, <<"">>}},
        %% The secret key to the S3 storage.
        secretKey => {string, {optional, <<"">>}}
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
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, optional},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        %% Overrides all previously set parameters.
        qosParameters => {#{'_' => string}, optional},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The type of storage.  `type = \&quot;s3\&quot;`
        %% [Amazon
        %% S3](http://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html)
        %% compatible storage.
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
        dirMode => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The OpenStack Swift configuration.
%% @end
%%--------------------------------------------------------------------
-spec swift_model() -> onepanel_parser:object_spec().
swift_model() ->
    #{
        %% The type of storage.  `type = \&quot;swift\&quot;`
        %% Storage backend compatible with
        %% [OpenStack](http://docs.openstack.org/developer/swift/) SWIFT
        %% protocol.
        type => {discriminator, <<"swift">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, {optional, <<"auto">>}},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        qosParameters => {#{'_' => string}, {optional, #{}}},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The Keystone authentication username.
        username => string,
        %% The Keystone authentication password.
        password => string,
        %% The URL to OpenStack Keystone identity service.
        authUrl => string,
        %% The name of the tenant to which the user belongs.
        tenantName => string,
        %% The name of the Swift storage container.
        containerName => string,
        %% Storage block size in bytes.
        blockSize => {integer, {optional, 10485760}},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"flat">>}}
    }.

%%--------------------------------------------------------------------
%% @doc Credentials on the SWIFT storage.
%% @end
%%--------------------------------------------------------------------
-spec swift_credentials_model() -> onepanel_parser:object_spec().
swift_credentials_model() ->
    #{
        %% Type of the storage. Must be given explicitly and must match the
        %% actual type of subject storage - this redundancy is needed due to
        %% limitations of OpenAPI polymorphism.
        type => {discriminator, <<"swift">>},
        %% The Keystone authentication username.
        username => string,
        %% The Keystone authentication password.
        password => string
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
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, optional},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        %% Overrides all previously set parameters.
        qosParameters => {#{'_' => string}, optional},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The type of storage.  `type = \&quot;swift\&quot;`
        %% Storage backend compatible with
        %% [OpenStack](http://docs.openstack.org/developer/swift/) SWIFT
        %% protocol.
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
        password => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The WebDAV storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec webdav_model() -> onepanel_parser:object_spec().
webdav_model() ->
    #{
        %% The type of storage.  `type = \&quot;webdav\&quot;`
        %% Storage backend compatible with
        %% [WebDAV](https://tools.ietf.org/html/rfc4918) protocol.
        type => {discriminator, <<"webdav">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, {optional, <<"auto">>}},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        qosParameters => {#{'_' => string}, {optional, #{}}},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% Determines the types of credentials provided in the credentials
        %% field.
        credentialsType => {{enum, string, [<<"none">>, <<"basic">>, <<"token">>, <<"oauth2">>]}, {optional, <<"none">>}},
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
        %% When registering storage with feed of LUMA DB set to`auto`
        %% and with `oauth2` external IdP, this field must contain a
        %% valid Onedata access token of the user on whose behalf the WebDAV
        %% storage will be accessed by all users with access to any space
        %% supported by this storage.
        onedataAccessToken => {string, optional},
        %% Full URL of the WebDAV server, including scheme (http or https) and
        %% path.
        endpoint => string,
        %% Determines whether Oneprovider should verify the certificate of the
        %% WebDAV server.
        verifyServerCertificate => {boolean, {optional, true}},
        %% The authorization header to be used for passing the access token.
        %% This field can contain any prefix that should be added to the header
        %% value. Default is `Authorization: Bearer {}`. The token
        %% will placed where `{}` is provided.
        authorizationHeader => {string, {optional, <<"Authorization: Bearer {}">>}},
        %% The type of partial write support enabled in the WebDAV server.
        %% Currently 2 types are supported `sabredav` which assumes
        %% the server supports the SabreDAV PartialUpdate extension via
        %% `PATCH` method, and `moddav` which assumes server
        %% supports partial `PUT` requests with `Content-
        %% Range` header. If `none` is selected no write support
        %% is available for this WebDAV storage.
        rangeWriteSupport => {{enum, string, [<<"none">>, <<"moddav">>, <<"sabredav">>]}, {optional, <<"none">>}},
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
        fileMode => {string, {optional, <<"0664">>}},
        %% Defines the directory mode which directories imported from WebDAV
        %% storage will have in Onedata. Values should be provided in octal
        %% format e.g. `0775`.
        dirMode => {string, {optional, <<"0775">>}},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"canonical">>}}
    }.

%%--------------------------------------------------------------------
%% @doc Credentials on the WebDAV storage.
%% @end
%%--------------------------------------------------------------------
-spec webdav_credentials_model() -> onepanel_parser:object_spec().
webdav_credentials_model() ->
    #{
        %% Type of the storage. Must be given explicitly and must match the
        %% actual type of subject storage - this redundancy is needed due to
        %% limitations of OpenAPI polymorphism.
        type => {discriminator, <<"webdav">>},
        %% Determines the types of credentials provided in the credentials
        %% field.
        credentialsType => {{enum, string, [<<"none">>, <<"basic">>, <<"token">>, <<"oauth2">>]}, {optional, <<"none">>}},
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
        %% When registering storage with feed of LUMA DB set to`auto`
        %% and with `oauth2` external IdP, this field must contain a
        %% valid Onedata access token of the user on whose behalf the WebDAV
        %% storage will be accessed by all users with access to any space
        %% supported by this storage.
        onedataAccessToken => {string, optional}
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
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, optional},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        %% Overrides all previously set parameters.
        qosParameters => {#{'_' => string}, optional},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The type of storage.  `type = \&quot;webdav\&quot;`
        %% Storage backend compatible with
        %% [WebDAV](https://tools.ietf.org/html/rfc4918) protocol.
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
        %% Defines the file permissions, which files imported from WebDAV
        %% storage will have in Onedata. Values should be provided in octal
        %% format e.g. `0644`.
        fileMode => {string, optional},
        %% Defines the directory mode which directories imported from WebDAV
        %% storage will have in Onedata. Values should be provided in octal
        %% format e.g. `0775`.
        dirMode => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The XRootD storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec xrootd_model() -> onepanel_parser:object_spec().
xrootd_model() ->
    #{
        %% The type of storage.  `type = \&quot;xrootd\&quot;`
        %% Storage backend compatible with [XRootD](http://www.xrootd.org/)
        %% protocol.
        type => {discriminator, <<"xrootd">>},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, {optional, <<"auto">>}},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        qosParameters => {#{'_' => string}, {optional, #{}}},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% Determines the types of credentials provided in the credentials
        %% field.
        credentialsType => {{enum, string, [<<"none">>, <<"pwd">>]}, {optional, <<"none">>}},
        %% The credentials to authenticate with the XRootD server. For
        %% `pwd` credentials type, this field should contain simply
        %% user and password, e.g. `admin:password`. For
        %% `none` this field is ignored.
        credentials => {string, optional},
        %% Full URL of the XRootD server, including scheme (root or http) and
        %% path, e.g. `root://192.168.0.1//data`. Please note, that
        %% XRootD URL format requires double slash after host to indicate
        %% absolute path.
        url => string,
        %% Defines the file permissions mask, which is used to map XRootD file
        %% mode to POSIX mode. For instance a fileModeMask `0664` for
        %% readable file on XRootD would result in a file which is readable for
        %% all users, but file which is writeable in XRootD will be only
        %% writeable by user and group.
        fileModeMask => {string, {optional, <<"0664">>}},
        %% Defines the directory permissions mask, which is used to map XRootD
        %% dir mode to POSIX mode. For instance a dirModeMask `0770`
        %% for readable directory on XRootD would result in a directory which is
        %% readable for owner and group but not for others.
        dirModeMask => {string, {optional, <<"0775">>}},
        %% Determines how the logical file paths will be mapped on the storage.
        %% 'canonical' paths reflect the logical file names and
        %% directory structure, however each rename operation will require
        %% renaming the files on the storage. 'flat' paths are based on
        %% unique file UUID's and do not require on-storage rename when
        %% logical file name is changed.
        storagePathType => {string, {optional, <<"canonical">>}}
    }.

%%--------------------------------------------------------------------
%% @doc Credentials on the XRootD storage.
%% @end
%%--------------------------------------------------------------------
-spec xrootd_credentials_model() -> onepanel_parser:object_spec().
xrootd_credentials_model() ->
    #{
        %% Type of the storage. Must be given explicitly and must match the
        %% actual type of subject storage - this redundancy is needed due to
        %% limitations of OpenAPI polymorphism.
        type => {discriminator, <<"xrootd">>},
        %% Determines the types of credentials provided in the credentials
        %% field.
        credentialsType => {{enum, string, [<<"none">>, <<"pwd">>]}, {optional, <<"none">>}},
        %% The credentials to authenticate with the XRootD server. For
        %% `pwd` credentials type, this field should contain simply
        %% user and password, e.g. `admin:password`. For
        %% `none` this field is ignored.
        credentials => {string, optional}
    }.

%%--------------------------------------------------------------------
%% @doc The XRootD storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec xrootd_modify_model() -> onepanel_parser:object_spec().
xrootd_modify_model() ->
    #{
        %% The name of storage.
        name => {string, optional},
        %% Storage operation timeout in milliseconds.
        timeout => {integer, optional},
        %% If true, detecting whether storage is directly accessible by the
        %% Oneclient will not be performed. This option should be set to true on
        %% readonly storages.
        skipStorageDetection => {boolean, optional},
        %% Type of feed for LUMA DB. Feed is a source of user/group mappings
        %% used to populate the LUMA DB. For more info please read:
        %% https://onedata.org/#/home/documentation/doc/administering_onedata/luma.html
        lumaFeed => {{enum, string, [<<"auto">>, <<"local">>, <<"external">>]}, optional},
        %% URL of external feed for LUMA DB. Relevant only if lumaFeed equals
        %% `external`.
        lumaFeedUrl => {string, optional},
        %% API key checked by external service used as feed for LUMA DB.
        %% Relevant only if lumaFeed equals `external`.
        lumaFeedApiKey => {string, optional},
        %% Map with key-value pairs used for describing storage QoS parameters.
        %% Overrides all previously set parameters.
        qosParameters => {#{'_' => string}, optional},
        %% Defines whether storage contains existing data to be imported.
        importedStorage => {boolean, optional},
        %% Defines whether the storage is readonly. If enabled, Oneprovider will
        %% block any operation that writes, modifies or deletes data on the
        %% storage. Such storage can only be used to import data into the space.
        %% Mandatory to ensure proper behaviour if the backend storage is
        %% actually configured as readonly. This option is available only for
        %% imported storages.
        readonly => {boolean, optional},
        %% The type of storage.  `type = \&quot;xrootd\&quot;`
        %% Storage backend compatible with [XRootD](http://www.xrootd.org/)
        %% protocol.
        type => {discriminator, <<"xrootd">>},
        %% Full URL of the XRootD server, including scheme (root or http) and
        %% path, e.g. `root://192.168.0.1//data`. Please note, that
        %% XRootD URL format requires double slash after host to indicate
        %% absolute path.
        url => {string, optional},
        %% Determines the types of credentials provided in the credentials
        %% field.
        credentialsType => {{enum, string, [<<"none">>, <<"pwd">>]}, optional},
        %% The credentials to authenticate with the XRootD server. For
        %% `pwd` credentials type, this field should contain simply
        %% user and password, e.g. `admin:password`. For
        %% `none` this field is ignored.
        credentials => {string, optional},
        %% Defines the file permissions mask, which is used to map XRootD file
        %% mode to POSIX mode. For instance a fileModeMask `0664` for
        %% readable file on XRootD would result in a file which is readable for
        %% all users, but file which is writeable in XRootD will be only
        %% writeable by user and group.
        fileModeMask => {string, optional},
        %% Defines the directory permissions mask, which is used to map XRootD
        %% dir mode to POSIX mode. For instance a dirModeMask `0770`
        %% for readable directory on XRootD would result in a directory which is
        %% readable for owner and group but not for others.
        dirModeMask => {string, optional}
    }.

