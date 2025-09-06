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

-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").


-export([resolve_handler/3]).


%%%===================================================================
%%% API
%%%===================================================================


-spec resolve_handler(middleware_handler:interface(), middleware_handler:operation(), gri:gri()) ->
    {true, middleware_handler:t()} | false.
resolve_handler(Interface, Operation, #gri{type = onp_cluster, aspect = Aspect}) ->
    resolve_cluster_handler(Interface, Aspect, Operation);

resolve_handler(Interface, Operation, #gri{type = onp_host, aspect = Aspect}) ->
    resolve_host_handler(Interface, Aspect, Operation);

resolve_handler(Interface, Operation, #gri{type = onp_panel, aspect = Aspect}) ->
    resolve_panel_handler(Interface, Aspect, Operation);

resolve_handler(Interface, Operation, #gri{type = onp_provider, aspect = Aspect}) ->
    resolve_provider_handler(Interface, Aspect, Operation);

resolve_handler(Interface, Operation, #gri{type = onp_space, aspect = Aspect}) ->
    resolve_space_handler(Interface, Aspect, Operation);

resolve_handler(Interface, Operation, #gri{type = onp_user, aspect = Aspect}) ->
    resolve_user_handler(Interface, Aspect, Operation);
    
resolve_handler(Interface, Operation, #gri{type = onp_zone, aspect = Aspect}) ->
    resolve_zone_handler(Interface, Aspect, Operation);

resolve_handler(_, _, _) ->
    false.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec resolve_cluster_handler(middleware_handler:interface(), gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_cluster_handler(_, current_cluster, get) ->
    {true, cluster_current_cluster_get_middleware_handler:module_info(module)};

resolve_cluster_handler(_, current_cluster_members_summary, get) ->
    {true, cluster_current_cluster_members_summary_get_middleware_handler:module_info(module)};

resolve_cluster_handler(_, instance, get) ->
    {true, cluster_instance_get_middleware_handler:module_info(module)};

resolve_cluster_handler(_, invite_user_token, create) ->
    {true, cluster_invite_user_token_create_middleware_handler:module_info(module)};

resolve_cluster_handler(_, _, _) ->
    false.


%% @private
-spec resolve_host_handler(middleware_handler:interface(), gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_host_handler(_, external_ips, get) ->
    {true, host_external_ips_get_middleware_handler:module_info(module)};
resolve_host_handler(_, external_ips, update) ->
    {true, host_external_ips_update_middleware_handler:module_info(module)};

resolve_host_handler(_, instance, create) ->
    {true, host_instance_create_middleware_handler:module_info(module)};
resolve_host_handler(_, instance, get) ->
    {true, host_instance_get_middleware_handler:module_info(module)};
resolve_host_handler(_, instance, delete) ->
    {true, host_instance_delete_middleware_handler:module_info(module)};

resolve_host_handler(_, join_cluster, create) ->
    {true, host_join_cluster_middleware_handler:module_info(module)};

resolve_host_handler(_, list, get) ->
    {true, host_list_get_middleware_handler:module_info(module)};

resolve_host_handler(_, _, _) ->
    false.


%% @private
-spec resolve_panel_handler(middleware_handler:interface(), gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_panel_handler(_, configuration, get) ->
    {true, panel_configuration_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, cookie, get) ->
    {true, panel_cookie_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, dns_check, get) ->
    {true, panel_dns_check_middleware_handler:module_info(module)};
resolve_panel_handler(_, dns_check_configuration, get) ->
    {true, panel_dns_check_configuration_get_middleware_handler:module_info(module)};
resolve_panel_handler(_, dns_check_configuration, update) ->
    {true, panel_dns_check_configuration_update_middleware_handler:module_info(module)};

resolve_panel_handler(_, emergency_passphrase, create) ->
    {true, panel_emergency_passphrase_create_middleware_handler:module_info(module)};
resolve_panel_handler(_, emergency_passphrase, get) ->
    {true, panel_emergency_passphrase_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, health, get) ->
    {true, panel_health_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, invite_token, create) ->
    {true, panel_invite_token_create_middleware_handler:module_info(module)};

resolve_panel_handler(_, progress, get) ->
    {true, panel_progress_get_middleware_handler:module_info(module)};
resolve_panel_handler(_, progress, update) ->
    {true, panel_progress_update_middleware_handler:module_info(module)};

resolve_panel_handler(_, {task, _}, get) ->
    {true, panel_task_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, test_image, get) ->
    {true, panel_test_image_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, web_cert, get) ->
    {true, panel_web_cert_get_middleware_handler:module_info(module)};
resolve_panel_handler(_, web_cert, update) ->
    {true, panel_web_cert_update_middleware_handler:module_info(module)};

resolve_panel_handler(_, _, _) ->
    false.


%% @private
-spec resolve_provider_handler(middleware_handler:interface(), gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_provider_handler(_, cluster, create) ->
    {true, provider_cluster_create_middleware_handler:module_info(module)};
resolve_provider_handler(_, cluster, get) ->
    {true, provider_cluster_get_middleware_handler:module_info(module)};

resolve_provider_handler(_, instance, create) ->
    {true, provider_instance_create_middleware_handler:module_info(module)};
resolve_provider_handler(_, instance, get) ->
    {true, provider_instance_get_middleware_handler:module_info(module)};
resolve_provider_handler(_, instance, update) ->
    {true, provider_instance_update_middleware_handler:module_info(module)};
resolve_provider_handler(_, instance, delete) ->
    {true, provider_instance_delete_middleware_handler:module_info(module)};

resolve_provider_handler(_, remote_instance, get) ->
    {true, provider_remote_instance_get_middleware_handler:module_info(module)};

resolve_provider_handler(_, transfers_mock, get) ->
    {true, provider_transfers_mock_get_middleware_handler:module_info(module)};
resolve_provider_handler(_, transfers_mock, update) ->
    {true, provider_transfers_mock_update_middleware_handler:module_info(module)};

resolve_provider_handler(_, _, _) ->
    false.


%% @private
-spec resolve_space_handler(middleware_handler:interface(), gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_space_handler(_, auto_cleaning_configuration, get) ->
    {true, space_auto_cleaning_configuration_get_middleware_handler:module_info(module)};
resolve_space_handler(_, auto_cleaning_configuration, update) ->
    {true, space_auto_cleaning_configuration_update_middleware_handler:module_info(module)};
resolve_space_handler(_, auto_cleaning_status, get) ->
    {true, space_auto_cleaning_status_get_middleware_handler:module_info(module)};
resolve_space_handler(_, auto_cleaning_reports_list, get) ->
    {true, space_auto_cleaning_reports_list_get_middleware_handler:module_info(module)};
resolve_space_handler(_, {auto_cleaning_report, _}, get) ->
    {true, space_auto_cleaning_report_get_middleware_handler:module_info(module)};
resolve_space_handler(_, start_auto_cleaning, create) ->
    {true, space_start_auto_cleaning_create_middleware_handler:module_info(module)};
resolve_space_handler(_, cancel_auto_cleaning, create) ->
    {true, space_cancel_auto_cleaning_create_middleware_handler:module_info(module)};

resolve_space_handler(_, file_popularity_configuration, get) ->
    {true, space_file_popularity_configuration_get_middleware_handler:module_info(module)};
resolve_space_handler(_, file_popularity_configuration, update) ->
    {true, space_file_popularity_configuration_update_middleware_handler:module_info(module)};

resolve_space_handler(_, instance, get) ->
    {true, space_instance_get_middleware_handler:module_info(module)};

resolve_space_handler(_, list, get) ->
    {true, space_list_get_middleware_handler:module_info(module)};

resolve_space_handler(_, auto_storage_import_info, get) ->
    {true, space_auto_storage_import_info_get_middleware_handler:module_info(module)};
resolve_space_handler(_, auto_storage_import_stats, get) ->
    {true, space_auto_storage_import_stats_get_middleware_handler:module_info(module)};
resolve_space_handler(_, force_start_auto_storage_import_scan, create) ->
    {true, space_force_start_auto_storage_import_scan_create_middleware_handler:module_info(module)};
resolve_space_handler(_, force_stop_auto_storage_import_scan, create) ->
    {true, space_force_stop_auto_storage_import_scan_create_middleware_handler:module_info(module)};
resolve_space_handler(_, manual_storage_import_example, get) ->
    {true, space_manual_storage_import_example_get_middleware_handler:module_info(module)};

resolve_space_handler(_, support, create) ->
    {true, space_support_create_middleware_handler:module_info(module)};
resolve_space_handler(_, support, update) ->
    {true, space_support_update_middleware_handler:module_info(module)};
resolve_space_handler(_, support, delete) ->
    {true, space_support_delete_middleware_handler:module_info(module)};

resolve_space_handler(_, _, _) ->
    false.


%% @private
-spec resolve_user_handler(middleware_handler:interface(), gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_user_handler(_, current_user, get) ->
    {true, user_current_user_get_middleware_handler:module_info(module)};
resolve_user_handler(_, current_user_clusters, get) ->
    {true, user_current_user_clusters_get_middleware_handler:module_info(module)};

resolve_user_handler(_, instance, create) ->
    {true, user_instance_create_middleware_handler:module_info(module)};
resolve_user_handler(_, instance, get) ->
    {true, user_instance_get_middleware_handler:module_info(module)};
resolve_user_handler(_, instance, update) ->
    {true, user_instance_update_middleware_handler:module_info(module)};

resolve_user_handler(_, list, get) ->
    {true, user_list_get_middleware_handler:module_info(module)};

resolve_user_handler(_, _, _) ->
    false.


%% @private
-spec resolve_zone_handler(middleware_handler:interface(), gri:aspect(), middleware_handler:operation()) ->
    {true, middleware_handler:t()} | false.
resolve_zone_handler(_, cluster, create) ->
    {true, zone_cluster_create_middleware_handler:module_info(module)};
resolve_zone_handler(_, cluster, get) ->
    {true, zone_cluster_get_middleware_handler:module_info(module)};

resolve_zone_handler(_, {gui_message, _}, get) ->
    {true, zone_gui_message_get_middleware_handler:module_info(module)};
resolve_zone_handler(_, {gui_message, _}, update) ->
    {true, zone_gui_message_update_middleware_handler:module_info(module)};

resolve_zone_handler(_, instance, get) ->
    {true, zone_instance_get_middleware_handler:module_info(module)};

resolve_zone_handler(_, policies, get) ->
    {true, zone_policies_get_middleware_handler:module_info(module)};
resolve_zone_handler(_, policies, update) ->
    {true, zone_policies_update_middleware_handler:module_info(module)};
    
resolve_zone_handler(_, _, _) ->
    false.
