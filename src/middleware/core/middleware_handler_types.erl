%%%-------------------------------------------------------------------
%%% This file has been automatically generated - DO NOT EDIT!!!
%%%
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements aggregated types for behaviour: middleware_handler.
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_handler_types).

-type t() ::
    cluster_current_cluster_get_middleware_handler:t() |
    cluster_current_cluster_members_summary_get_middleware_handler:t() |
    cluster_instance_get_middleware_handler:t() |
    cluster_invite_user_token_create_middleware_handler:t() |
    host_external_ips_get_middleware_handler:t() |
    host_external_ips_update_middleware_handler:t() |
    host_instance_create_middleware_handler:t() |
    host_instance_delete_middleware_handler:t() |
    host_instance_get_middleware_handler:t() |
    host_join_cluster_middleware_handler:t() |
    host_list_get_middleware_handler:t() |
    panel_configuration_get_middleware_handler:t() |
    panel_cookie_get_middleware_handler:t() |
    panel_dns_check_configuration_get_middleware_handler:t() |
    panel_dns_check_configuration_update_middleware_handler:t() |
    panel_dns_check_middleware_handler:t() |
    panel_emergency_passphrase_create_middleware_handler:t() |
    panel_emergency_passphrase_get_middleware_handler:t() |
    panel_health_get_middleware_handler:t() |
    panel_invite_token_create_middleware_handler:t() |
    panel_progress_get_middleware_handler:t() |
    panel_progress_update_middleware_handler:t() |
    panel_task_get_middleware_handler:t() |
    panel_test_image_get_middleware_handler:t() |
    panel_web_cert_get_middleware_handler:t() |
    panel_web_cert_update_middleware_handler:t().

-type input() ::
    cluster_current_cluster_get_middleware_handler:input() |
    cluster_current_cluster_members_summary_get_middleware_handler:input() |
    cluster_instance_get_middleware_handler:input() |
    cluster_invite_user_token_create_middleware_handler:input() |
    host_external_ips_get_middleware_handler:input() |
    host_external_ips_update_middleware_handler:input() |
    host_instance_create_middleware_handler:input() |
    host_instance_delete_middleware_handler:input() |
    host_instance_get_middleware_handler:input() |
    host_join_cluster_middleware_handler:input() |
    host_list_get_middleware_handler:input() |
    panel_configuration_get_middleware_handler:input() |
    panel_cookie_get_middleware_handler:input() |
    panel_dns_check_configuration_get_middleware_handler:input() |
    panel_dns_check_configuration_update_middleware_handler:input() |
    panel_dns_check_middleware_handler:input() |
    panel_emergency_passphrase_create_middleware_handler:input() |
    panel_emergency_passphrase_get_middleware_handler:input() |
    panel_health_get_middleware_handler:input() |
    panel_invite_token_create_middleware_handler:input() |
    panel_progress_get_middleware_handler:input() |
    panel_progress_update_middleware_handler:input() |
    panel_task_get_middleware_handler:input() |
    panel_test_image_get_middleware_handler:input() |
    panel_web_cert_get_middleware_handler:input() |
    panel_web_cert_update_middleware_handler:input().

-type state() ::
    cluster_current_cluster_get_middleware_handler:state() |
    cluster_current_cluster_members_summary_get_middleware_handler:state() |
    cluster_instance_get_middleware_handler:state() |
    cluster_invite_user_token_create_middleware_handler:state() |
    host_external_ips_get_middleware_handler:state() |
    host_external_ips_update_middleware_handler:state() |
    host_instance_create_middleware_handler:state() |
    host_instance_delete_middleware_handler:state() |
    host_instance_get_middleware_handler:state() |
    host_join_cluster_middleware_handler:state() |
    host_list_get_middleware_handler:state() |
    panel_configuration_get_middleware_handler:state() |
    panel_cookie_get_middleware_handler:state() |
    panel_dns_check_configuration_get_middleware_handler:state() |
    panel_dns_check_configuration_update_middleware_handler:state() |
    panel_dns_check_middleware_handler:state() |
    panel_emergency_passphrase_create_middleware_handler:state() |
    panel_emergency_passphrase_get_middleware_handler:state() |
    panel_health_get_middleware_handler:state() |
    panel_invite_token_create_middleware_handler:state() |
    panel_progress_get_middleware_handler:state() |
    panel_progress_update_middleware_handler:state() |
    panel_task_get_middleware_handler:state() |
    panel_test_image_get_middleware_handler:state() |
    panel_web_cert_get_middleware_handler:state() |
    panel_web_cert_update_middleware_handler:state().

-type output() ::
    cluster_current_cluster_get_middleware_handler:output() |
    cluster_current_cluster_members_summary_get_middleware_handler:output() |
    cluster_instance_get_middleware_handler:output() |
    cluster_invite_user_token_create_middleware_handler:output() |
    host_external_ips_get_middleware_handler:output() |
    host_external_ips_update_middleware_handler:output() |
    host_instance_create_middleware_handler:output() |
    host_instance_delete_middleware_handler:output() |
    host_instance_get_middleware_handler:output() |
    host_join_cluster_middleware_handler:output() |
    host_list_get_middleware_handler:output() |
    panel_configuration_get_middleware_handler:output() |
    panel_cookie_get_middleware_handler:output() |
    panel_dns_check_configuration_get_middleware_handler:output() |
    panel_dns_check_configuration_update_middleware_handler:output() |
    panel_dns_check_middleware_handler:output() |
    panel_emergency_passphrase_create_middleware_handler:output() |
    panel_emergency_passphrase_get_middleware_handler:output() |
    panel_health_get_middleware_handler:output() |
    panel_invite_token_create_middleware_handler:output() |
    panel_progress_get_middleware_handler:output() |
    panel_progress_update_middleware_handler:output() |
    panel_task_get_middleware_handler:output() |
    panel_test_image_get_middleware_handler:output() |
    panel_web_cert_get_middleware_handler:output() |
    panel_web_cert_update_middleware_handler:output().

-export_type([t/0, input/0, state/0, output/0]).
