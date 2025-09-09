%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module resolves middleware_handler modules for requests.
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_router).
-author("Bartosz Walkowicz").

-include_lib("ctool/include/graph_sync/gri.hrl").


-export([resolve_handler/2]).


%%%===================================================================
%%% API
%%%===================================================================


-spec resolve_handler(middleware_handler:operation(), gri:gri()) ->
    {true, middleware_handler:t()} | false.
resolve_handler(Operation, #gri{type = onp_cluster, aspect = Aspect}) ->
    resolve_cluster_handler(Aspect, Operation);

resolve_handler(Operation, #gri{type = onp_host, aspect = Aspect}) ->
    resolve_host_handler(Aspect, Operation);

resolve_handler(Operation, #gri{type = onp_panel, aspect = Aspect}) ->
    resolve_panel_handler(Aspect, Operation);

resolve_handler(Operation, #gri{type = onp_provider, aspect = Aspect}) ->
    resolve_provider_handler(Aspect, Operation);

resolve_handler(Operation, #gri{type = onp_service, aspect = Aspect}) ->
    resolve_service_handler(Aspect, Operation);
    
resolve_handler(Operation, #gri{type = onp_space, aspect = Aspect}) ->
    resolve_space_handler(Aspect, Operation);

resolve_handler(Operation, #gri{type = onp_storage, aspect = Aspect}) ->
    resolve_storage_handler(Aspect, Operation);

resolve_handler(Operation, #gri{type = onp_user, aspect = Aspect}) ->
    resolve_user_handler(Aspect, Operation);

resolve_handler(Operation, #gri{type = onp_zone, aspect = Aspect}) ->
    resolve_zone_handler(Aspect, Operation);

resolve_handler(_, _) ->
    false.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec resolve_cluster_handler(gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_cluster_handler(current_cluster, get) ->
    {true, cluster_current_cluster_get_middleware_handler:module_info(module)};

resolve_cluster_handler(current_cluster_members_summary, get) ->
    {true, cluster_current_cluster_members_summary_get_middleware_handler:module_info(module)};

resolve_cluster_handler(instance, get) ->
    {true, cluster_instance_get_middleware_handler:module_info(module)};

resolve_cluster_handler(invite_user_token, create) ->
    {true, cluster_invite_user_token_create_middleware_handler:module_info(module)};

resolve_cluster_handler(_, _) ->
    false.


%% @private
-spec resolve_host_handler(gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_host_handler(external_ips, get) ->
    {true, host_external_ips_get_middleware_handler:module_info(module)};
resolve_host_handler(external_ips, update) ->
    {true, host_external_ips_update_middleware_handler:module_info(module)};

resolve_host_handler(instance, create) ->
    {true, host_instance_create_middleware_handler:module_info(module)};
resolve_host_handler(instance, get) ->
    {true, host_instance_get_middleware_handler:module_info(module)};
resolve_host_handler(instance, delete) ->
    {true, host_instance_delete_middleware_handler:module_info(module)};

resolve_host_handler(join_cluster, create) ->
    {true, host_join_cluster_middleware_handler:module_info(module)};

resolve_host_handler(list, get) ->
    {true, host_list_get_middleware_handler:module_info(module)};

resolve_host_handler(_, _) ->
    false.


%% @private
-spec resolve_panel_handler(gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_panel_handler(configuration, get) ->
    {true, panel_configuration_get_middleware_handler:module_info(module)};

resolve_panel_handler(cookie, get) ->
    {true, panel_cookie_get_middleware_handler:module_info(module)};

resolve_panel_handler(dns_check, get) ->
    {true, panel_dns_check_middleware_handler:module_info(module)};
resolve_panel_handler(dns_check_configuration, get) ->
    {true, panel_dns_check_configuration_get_middleware_handler:module_info(module)};
resolve_panel_handler(dns_check_configuration, update) ->
    {true, panel_dns_check_configuration_update_middleware_handler:module_info(module)};

resolve_panel_handler(emergency_passphrase, create) ->
    {true, panel_emergency_passphrase_create_middleware_handler:module_info(module)};
resolve_panel_handler(emergency_passphrase, get) ->
    {true, panel_emergency_passphrase_get_middleware_handler:module_info(module)};

resolve_panel_handler(health, get) ->
    {true, panel_health_get_middleware_handler:module_info(module)};

resolve_panel_handler(invite_token, create) ->
    {true, panel_invite_token_create_middleware_handler:module_info(module)};

resolve_panel_handler(progress, get) ->
    {true, panel_progress_get_middleware_handler:module_info(module)};
resolve_panel_handler(progress, update) ->
    {true, panel_progress_update_middleware_handler:module_info(module)};

resolve_panel_handler({task, _}, get) ->
    {true, panel_task_get_middleware_handler:module_info(module)};

resolve_panel_handler(test_image, get) ->
    {true, panel_test_image_get_middleware_handler:module_info(module)};

resolve_panel_handler(web_cert, get) ->
    {true, panel_web_cert_get_middleware_handler:module_info(module)};
resolve_panel_handler(web_cert, update) ->
    {true, panel_web_cert_update_middleware_handler:module_info(module)};

resolve_panel_handler(_, _) ->
    false.


%% @private
-spec resolve_provider_handler(gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_provider_handler(cluster, create) ->
    {true, provider_cluster_create_middleware_handler:module_info(module)};
resolve_provider_handler(cluster, get) ->
    {true, provider_cluster_get_middleware_handler:module_info(module)};

resolve_provider_handler(instance, create) ->
    {true, provider_instance_create_middleware_handler:module_info(module)};
resolve_provider_handler(instance, get) ->
    {true, provider_instance_get_middleware_handler:module_info(module)};
resolve_provider_handler(instance, update) ->
    {true, provider_instance_update_middleware_handler:module_info(module)};
resolve_provider_handler(instance, delete) ->
    {true, provider_instance_delete_middleware_handler:module_info(module)};

resolve_provider_handler(remote_instance, get) ->
    {true, provider_remote_instance_get_middleware_handler:module_info(module)};

resolve_provider_handler(transfers_mock, get) ->
    {true, provider_transfers_mock_get_middleware_handler:module_info(module)};
resolve_provider_handler(transfers_mock, update) ->
    {true, provider_transfers_mock_update_middleware_handler:module_info(module)};

resolve_provider_handler(_, _) ->
    false.


%% @private
-spec resolve_service_handler(gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_service_handler(cluster_manager_instances, create) ->
    {true, service_cluster_manager_instances_create_middleware_handler:module_info(module)};
resolve_service_handler({all_hosts_status, <<"cluster_manager">>}, get) ->
    {true, service_cluster_manager_all_hosts_status_get_middleware_handler:module_info(module)};
resolve_service_handler({host_status, <<"cluster_manager">>}, get) ->
    {true, service_cluster_manager_host_status_get_middleware_handler:module_info(module)};
resolve_service_handler({start_stop_all, <<"cluster_manager">>}, update) ->
    {true, service_cluster_manager_start_stop_all_update_middleware_handler:module_info(module)};
resolve_service_handler({start_stop, <<"cluster_manager">>}, update) ->
    {true, service_cluster_manager_start_stop_update_middleware_handler:module_info(module)};

resolve_service_handler(couchbase_instances, create) ->
    {true, service_couchbase_instances_create_middleware_handler:module_info(module)};
resolve_service_handler({all_hosts_status, <<"couchbase">>}, get) ->
    {true, service_couchbase_all_hosts_status_get_middleware_handler:module_info(module)};
resolve_service_handler({host_status, <<"couchbase">>}, get) ->
    {true, service_couchbase_host_status_get_middleware_handler:module_info(module)};
resolve_service_handler({start_stop_all, <<"couchbase">>}, update) ->
    {true, service_couchbase_start_stop_all_update_middleware_handler:module_info(module)};
resolve_service_handler({start_stop, <<"couchbase">>}, update) ->
    {true, service_couchbase_start_stop_update_middleware_handler:module_info(module)};

resolve_service_handler(ones3_instances, create) ->
    {true, service_ones3_instances_create_middleware_handler:module_info(module)};
resolve_service_handler({all_hosts_status, <<"ones3">>}, get) ->
    {true, service_ones3_all_hosts_status_get_middleware_handler:module_info(module)};
resolve_service_handler({host_status, <<"ones3">>}, get) ->
    {true, service_ones3_host_status_get_middleware_handler:module_info(module)};
resolve_service_handler({start_stop_all, <<"ones3">>}, update) ->
    {true, service_ones3_start_stop_all_update_middleware_handler:module_info(module)};
resolve_service_handler({start_stop, <<"ones3">>}, update) ->
    {true, service_ones3_start_stop_update_middleware_handler:module_info(module)};

resolve_service_handler(op_worker_instances, create) ->
    {true, service_op_worker_instances_create_middleware_handler:module_info(module)};
resolve_service_handler({nagios, <<"op_worker">>}, get) ->
    {true, service_op_worker_nagios_get_middleware_handler:module_info(module)};
resolve_service_handler({all_hosts_status, <<"op_worker">>}, get) ->
    {true, service_op_worker_all_hosts_status_get_middleware_handler:module_info(module)};
resolve_service_handler({host_status, <<"op_worker">>}, get) ->
    {true, service_op_worker_host_status_get_middleware_handler:module_info(module)};
resolve_service_handler({start_stop_all, <<"op_worker">>}, update) ->
    {true, service_op_worker_start_stop_all_update_middleware_handler:module_info(module)};
resolve_service_handler({start_stop, <<"op_worker">>}, update) ->
    {true, service_op_worker_start_stop_update_middleware_handler:module_info(module)};

resolve_service_handler(oz_worker_instances, create) ->
    {true, service_oz_worker_instances_create_middleware_handler:module_info(module)};
resolve_service_handler({nagios, <<"oz_worker">>}, get) ->
    {true, service_oz_worker_nagios_get_middleware_handler:module_info(module)};
resolve_service_handler({all_hosts_status, <<"oz_worker">>}, get) ->
    {true, service_oz_worker_all_hosts_status_get_middleware_handler:module_info(module)};
resolve_service_handler({host_status, <<"oz_worker">>}, get) ->
    {true, service_oz_worker_host_status_get_middleware_handler:module_info(module)};
resolve_service_handler({start_stop_all, <<"oz_worker">>}, update) ->
    {true, service_oz_worker_start_stop_all_update_middleware_handler:module_info(module)};
resolve_service_handler({start_stop, <<"oz_worker">>}, update) ->
    {true, service_oz_worker_start_stop_update_middleware_handler:module_info(module)};

resolve_service_handler(_, _) ->
    false.


%% @private
-spec resolve_space_handler(gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_space_handler(auto_cleaning_configuration, get) ->
    {true, space_auto_cleaning_configuration_get_middleware_handler:module_info(module)};
resolve_space_handler(auto_cleaning_configuration, update) ->
    {true, space_auto_cleaning_configuration_update_middleware_handler:module_info(module)};
resolve_space_handler(auto_cleaning_status, get) ->
    {true, space_auto_cleaning_status_get_middleware_handler:module_info(module)};
resolve_space_handler(auto_cleaning_reports_list, get) ->
    {true, space_auto_cleaning_reports_list_get_middleware_handler:module_info(module)};
resolve_space_handler({auto_cleaning_report, _}, get) ->
    {true, space_auto_cleaning_report_get_middleware_handler:module_info(module)};
resolve_space_handler(start_auto_cleaning, create) ->
    {true, space_start_auto_cleaning_create_middleware_handler:module_info(module)};
resolve_space_handler(cancel_auto_cleaning, create) ->
    {true, space_cancel_auto_cleaning_create_middleware_handler:module_info(module)};

resolve_space_handler(file_popularity_configuration, get) ->
    {true, space_file_popularity_configuration_get_middleware_handler:module_info(module)};
resolve_space_handler(file_popularity_configuration, update) ->
    {true, space_file_popularity_configuration_update_middleware_handler:module_info(module)};

resolve_space_handler(instance, get) ->
    {true, space_instance_get_middleware_handler:module_info(module)};

resolve_space_handler(list, get) ->
    {true, space_list_get_middleware_handler:module_info(module)};

resolve_space_handler(auto_storage_import_info, get) ->
    {true, space_auto_storage_import_info_get_middleware_handler:module_info(module)};
resolve_space_handler(auto_storage_import_stats, get) ->
    {true, space_auto_storage_import_stats_get_middleware_handler:module_info(module)};
resolve_space_handler(force_start_auto_storage_import_scan, create) ->
    {true, space_force_start_auto_storage_import_scan_create_middleware_handler:module_info(module)};
resolve_space_handler(force_stop_auto_storage_import_scan, create) ->
    {true, space_force_stop_auto_storage_import_scan_create_middleware_handler:module_info(module)};
resolve_space_handler(manual_storage_import_example, get) ->
    {true, space_manual_storage_import_example_get_middleware_handler:module_info(module)};

resolve_space_handler(support, create) ->
    {true, space_support_create_middleware_handler:module_info(module)};
resolve_space_handler(support, update) ->
    {true, space_support_update_middleware_handler:module_info(module)};
resolve_space_handler(support, delete) ->
    {true, space_support_delete_middleware_handler:module_info(module)};

resolve_space_handler(_, _) ->
    false.


%% @private
-spec resolve_storage_handler(gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_storage_handler(instance, get) ->
    {true, storage_instance_get_middleware_handler:module_info(module)};
resolve_storage_handler(instance, update) ->
    {true, storage_instance_update_middleware_handler:module_info(module)};
resolve_storage_handler(instance, delete) ->
    {true, storage_instance_delete_middleware_handler:module_info(module)};

resolve_storage_handler(instances, create) ->
    {true, storage_instances_create_middleware_handler:module_info(module)};

resolve_storage_handler(list, get) ->
    {true, storage_list_get_middleware_handler:module_info(module)};

resolve_storage_handler(luma_configuration, get) ->
    {true, storage_luma_configuration_get_middleware_handler:module_info(module)};

resolve_storage_handler({local_feed_luma_acl_group_to_onedata_group_mapping, _}, create) ->
    {true, storage_luma_acl_group_to_onedata_group_mapping_create_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_acl_group_to_onedata_group_mapping, _}, get) ->
    {true, storage_luma_acl_group_to_onedata_group_mapping_get_middleware_handler:module_info(module)};
resolve_storage_handler({luma_acl_group_to_onedata_group_mapping, _}, get) ->
    {true, storage_luma_acl_group_to_onedata_group_mapping_get_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_acl_group_to_onedata_group_mapping, _}, delete) ->
    {true, storage_luma_acl_group_to_onedata_group_mapping_delete_middleware_handler:module_info(module)};
resolve_storage_handler({luma_acl_group_to_onedata_group_mapping, _}, delete) ->
    {true, storage_luma_acl_group_to_onedata_group_mapping_delete_middleware_handler:module_info(module)};

resolve_storage_handler({local_feed_luma_acl_user_to_onedata_user_mapping, _}, create) ->
    {true, storage_luma_acl_user_to_onedata_user_mapping_create_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_acl_user_to_onedata_user_mapping, _}, get) ->
    {true, storage_luma_acl_user_to_onedata_user_mapping_get_middleware_handler:module_info(module)};
resolve_storage_handler({luma_acl_user_to_onedata_user_mapping, _}, get) ->
    {true, storage_luma_acl_user_to_onedata_user_mapping_get_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_acl_user_to_onedata_user_mapping, _}, delete) ->
    {true, storage_luma_acl_user_to_onedata_user_mapping_delete_middleware_handler:module_info(module)};
resolve_storage_handler({luma_acl_user_to_onedata_user_mapping, _}, delete) ->
    {true, storage_luma_acl_user_to_onedata_user_mapping_delete_middleware_handler:module_info(module)};

resolve_storage_handler({local_feed_luma_default_posix_credentials, _}, create) ->
    {true, storage_luma_default_posix_credentials_create_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_default_posix_credentials, _}, get) ->
    {true, storage_luma_default_posix_credentials_get_middleware_handler:module_info(module)};
resolve_storage_handler({luma_default_posix_credentials, _}, get) ->
    {true, storage_luma_default_posix_credentials_get_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_default_posix_credentials, _}, delete) ->
    {true, storage_luma_default_posix_credentials_delete_middleware_handler:module_info(module)};
resolve_storage_handler({luma_default_posix_credentials, _}, delete) ->
    {true, storage_luma_default_posix_credentials_delete_middleware_handler:module_info(module)};

resolve_storage_handler({local_feed_luma_display_credentials, _}, create) ->
    {true, storage_luma_display_credentials_create_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_display_credentials, _}, get) ->
    {true, storage_luma_display_credentials_get_middleware_handler:module_info(module)};
resolve_storage_handler({luma_display_credentials, _}, get) ->
    {true, storage_luma_display_credentials_get_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_display_credentials, _}, delete) ->
    {true, storage_luma_display_credentials_delete_middleware_handler:module_info(module)};
resolve_storage_handler({luma_display_credentials, _}, delete) ->
    {true, storage_luma_display_credentials_delete_middleware_handler:module_info(module)};

resolve_storage_handler(local_feed_luma_onedata_user_to_credentials_mapping, create) ->
    {true, storage_luma_onedata_user_to_credentials_mapping_create_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_onedata_user_to_credentials_mapping, _}, get) ->
    {true, storage_luma_onedata_user_to_credentials_mapping_get_middleware_handler:module_info(module)};
resolve_storage_handler({luma_onedata_user_to_credentials_mapping, _}, get) ->
    {true, storage_luma_onedata_user_to_credentials_mapping_get_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_onedata_user_to_credentials_mapping, _}, update) ->
    {true, storage_luma_onedata_user_to_credentials_mapping_update_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_onedata_user_to_credentials_mapping, _}, delete) ->
    {true, storage_luma_onedata_user_to_credentials_mapping_delete_middleware_handler:module_info(module)};
resolve_storage_handler({luma_onedata_user_to_credentials_mapping, _}, delete) ->
    {true, storage_luma_onedata_user_to_credentials_mapping_delete_middleware_handler:module_info(module)};

resolve_storage_handler({local_feed_luma_uid_to_onedata_user_mapping, _}, create) ->
    {true, storage_luma_uid_to_onedata_user_mapping_create_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_uid_to_onedata_user_mapping, _}, get) ->
    {true, storage_luma_uid_to_onedata_user_mapping_get_middleware_handler:module_info(module)};
resolve_storage_handler({luma_uid_to_onedata_user_mapping, _}, get) ->
    {true, storage_luma_uid_to_onedata_user_mapping_get_middleware_handler:module_info(module)};
resolve_storage_handler({local_feed_luma_uid_to_onedata_user_mapping, _}, delete) ->
    {true, storage_luma_uid_to_onedata_user_mapping_delete_middleware_handler:module_info(module)};
resolve_storage_handler({luma_uid_to_onedata_user_mapping, _}, delete) ->
    {true, storage_luma_uid_to_onedata_user_mapping_delete_middleware_handler:module_info(module)};

resolve_storage_handler(luma_db, delete) ->
    {true, storage_luma_db_delete_middleware_handler:module_info(module)};

resolve_storage_handler(_, _) ->
    false.


%% @private
-spec resolve_user_handler(gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_user_handler(current_user, get) ->
    {true, user_current_user_get_middleware_handler:module_info(module)};
resolve_user_handler(current_user_clusters, get) ->
    {true, user_current_user_clusters_get_middleware_handler:module_info(module)};

resolve_user_handler(instance, create) ->
    {true, user_instance_create_middleware_handler:module_info(module)};
resolve_user_handler(instance, get) ->
    {true, user_instance_get_middleware_handler:module_info(module)};
resolve_user_handler(instance, update) ->
    {true, user_instance_update_middleware_handler:module_info(module)};

resolve_user_handler(list, get) ->
    {true, user_list_get_middleware_handler:module_info(module)};

resolve_user_handler(_, _) ->
    false.


%% @private
-spec resolve_zone_handler(gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_zone_handler(cluster, create) ->
    {true, zone_cluster_create_middleware_handler:module_info(module)};
resolve_zone_handler(cluster, get) ->
    {true, zone_cluster_get_middleware_handler:module_info(module)};

resolve_zone_handler({gui_message, _}, get) ->
    {true, zone_gui_message_get_middleware_handler:module_info(module)};
resolve_zone_handler({gui_message, _}, update) ->
    {true, zone_gui_message_update_middleware_handler:module_info(module)};

resolve_zone_handler(instance, get) ->
    {true, zone_instance_get_middleware_handler:module_info(module)};

resolve_zone_handler(policies, get) ->
    {true, zone_policies_get_middleware_handler:module_info(module)};
resolve_zone_handler(policies, update) ->
    {true, zone_policies_update_middleware_handler:module_info(module)};
    
resolve_zone_handler(_, _) ->
    false.
