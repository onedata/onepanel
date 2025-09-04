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
    cluster_create_invite_user_token_middleware_handler:t() |
    cluster_get_current_cluster_members_summary_middleware_handler:t() |
    cluster_get_current_cluster_middleware_handler:t() |
    cluster_get_instance_middleware_handler:t() |
    host_create_instance_middleware_handler:t() |
    host_create_join_cluster_middleware_handler:t() |
    host_delete_instance_middleware_handler:t() |
    host_get_external_ips_middleware_handler:t() |
    host_get_instance_middleware_handler:t() |
    host_get_list_middleware_handler:t() |
    host_update_external_ips_middleware_handler:t().

-type input() ::
    cluster_create_invite_user_token_middleware_handler:input() |
    cluster_get_current_cluster_members_summary_middleware_handler:input() |
    cluster_get_current_cluster_middleware_handler:input() |
    cluster_get_instance_middleware_handler:input() |
    host_create_instance_middleware_handler:input() |
    host_create_join_cluster_middleware_handler:input() |
    host_delete_instance_middleware_handler:input() |
    host_get_external_ips_middleware_handler:input() |
    host_get_instance_middleware_handler:input() |
    host_get_list_middleware_handler:input() |
    host_update_external_ips_middleware_handler:input().

-type state() ::
    cluster_create_invite_user_token_middleware_handler:state() |
    cluster_get_current_cluster_members_summary_middleware_handler:state() |
    cluster_get_current_cluster_middleware_handler:state() |
    cluster_get_instance_middleware_handler:state() |
    host_create_instance_middleware_handler:state() |
    host_create_join_cluster_middleware_handler:state() |
    host_delete_instance_middleware_handler:state() |
    host_get_external_ips_middleware_handler:state() |
    host_get_instance_middleware_handler:state() |
    host_get_list_middleware_handler:state() |
    host_update_external_ips_middleware_handler:state().

-type output() ::
    cluster_create_invite_user_token_middleware_handler:output() |
    cluster_get_current_cluster_members_summary_middleware_handler:output() |
    cluster_get_current_cluster_middleware_handler:output() |
    cluster_get_instance_middleware_handler:output() |
    host_create_instance_middleware_handler:output() |
    host_create_join_cluster_middleware_handler:output() |
    host_delete_instance_middleware_handler:output() |
    host_get_external_ips_middleware_handler:output() |
    host_get_instance_middleware_handler:output() |
    host_get_list_middleware_handler:output() |
    host_update_external_ips_middleware_handler:output().

-export_type([t/0, input/0, state/0, output/0]).
