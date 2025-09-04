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


-spec resolve_handler(middleware_handler:interface(), middleware:operation(), gri:gri()) ->
    {ok, middleware_handler:t()} | no_return().
resolve_handler(Interface, Operation, #gri{type = onp_cluster, aspect = Aspect}) ->
    resolve_cluster_handler(Interface, Aspect, Operation);

resolve_handler(Interface, Operation, #gri{type = onp_host, aspect = Aspect}) ->
    resolve_host_handler(Interface, Aspect, Operation);

resolve_handler(Interface, Operation, #gri{type = onp_panel, aspect = Aspect}) ->
    resolve_panel_handler(Interface, Aspect, Operation);

resolve_handler(Interface, Operation, #gri{type = onp_zone, aspect = Aspect}) ->
    resolve_zone_handler(Interface, Aspect, Operation);
    
resolve_handler(_, _, _) ->
    ?ERROR_NOT_SUPPORTED.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec resolve_cluster_handler(middleware_handler:interface(), gri:aspect(), middleware:operation()) ->
    {ok, middleware_handler:t()} | no_return().
resolve_cluster_handler(_, current_cluster, get) ->
    {ok, cluster_current_cluster_get_middleware_handler:module_info(module)};

resolve_cluster_handler(_, current_cluster_members_summary, get) ->
    {ok, cluster_current_cluster_members_summary_get_middleware_handler:module_info(module)};

resolve_cluster_handler(_, instance, get) ->
    {ok, cluster_instance_get_middleware_handler:module_info(module)};

resolve_cluster_handler(_, invite_user_token, create) ->
    {ok, cluster_invite_user_token_create_middleware_handler:module_info(module)};

resolve_cluster_handler(_, _, _) ->
    ?ERROR_NOT_SUPPORTED.


%% @private
-spec resolve_host_handler(middleware_handler:interface(), gri:aspect(), middleware:operation()) ->
    {ok, middleware_handler:t()} | no_return().
resolve_host_handler(_, external_ips, get) ->
    {ok, host_external_ips_get_middleware_handler:module_info(module)};
resolve_host_handler(_, external_ips, update) ->
    {ok, host_external_ips_update_middleware_handler:module_info(module)};

resolve_host_handler(_, instance, create) ->
    {ok, host_instance_create_middleware_handler:module_info(module)};
resolve_host_handler(_, instance, get) ->
    {ok, host_instance_get_middleware_handler:module_info(module)};
resolve_host_handler(_, instance, delete) ->
    {ok, host_instance_delete_middleware_handler:module_info(module)};

resolve_host_handler(_, join_cluster, create) ->
    {ok, host_join_cluster_middleware_handler:module_info(module)};

resolve_host_handler(_, list, get) ->
    {ok, host_list_get_middleware_handler:module_info(module)};

resolve_host_handler(_, _, _) ->
    ?ERROR_NOT_SUPPORTED.


%% @private
-spec resolve_panel_handler(middleware_handler:interface(), gri:aspect(), middleware:operation()) ->
    {ok, middleware_handler:t()} | no_return().
resolve_panel_handler(_, configuration, get) ->
    {ok, panel_configuration_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, cookie, get) ->
    {ok, panel_cookie_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, dns_check, get) ->
    {ok, panel_dns_check_middleware_handler:module_info(module)};
resolve_panel_handler(_, dns_check_configuration, get) ->
    {ok, panel_dns_check_configuration_get_middleware_handler:module_info(module)};
resolve_panel_handler(_, dns_check_configuration, update) ->
    {ok, panel_dns_check_configuration_update_middleware_handler:module_info(module)};

resolve_panel_handler(_, emergency_passphrase, create) ->
    {ok, panel_emergency_passphrase_create_middleware_handler:module_info(module)};
resolve_panel_handler(_, emergency_passphrase, get) ->
    {ok, panel_emergency_passphrase_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, health, get) ->
    {ok, panel_health_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, invite_token, create) ->
    {ok, panel_invite_token_create_middleware_handler:module_info(module)};

resolve_panel_handler(_, progress, get) ->
    {ok, panel_progress_get_middleware_handler:module_info(module)};
resolve_panel_handler(_, progress, update) ->
    {ok, panel_progress_update_middleware_handler:module_info(module)};

resolve_panel_handler(_, test_image, get) ->
    {ok, panel_test_image_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, web_cert, get) ->
    {ok, panel_web_cert_get_middleware_handler:module_info(module)};
resolve_panel_handler(_, web_cert, update) ->
    {ok, panel_web_cert_update_middleware_handler:module_info(module)};

resolve_panel_handler(_, {task, _}, get) ->
    {ok, panel_task_get_middleware_handler:module_info(module)};

resolve_panel_handler(_, _, _) ->
    ?ERROR_NOT_SUPPORTED.


%% @private
-spec resolve_zone_handler(middleware_handler:interface(), gri:aspect(), middleware:operation()) ->
    {ok, middleware_handler:t()} | no_return().
resolve_zone_handler(_, cluster, create) ->
    {ok, zone_cluster_create_middleware_handler:module_info(module)};
resolve_zone_handler(_, cluster, get) ->
    {ok, zone_cluster_get_middleware_handler:module_info(module)};

resolve_zone_handler(_, {gui_message, _}, get) ->
    {ok, zone_gui_message_get_middleware_handler:module_info(module)};
resolve_zone_handler(_, {gui_message, _}, update) ->
    {ok, zone_gui_message_update_middleware_handler:module_info(module)};

% get information about a remote onezone - either the issuer of given
% registration token, or the one to which this Oneprovider is registered.
resolve_zone_handler(_, instance, get) ->
    {ok, zone_instance_get_middleware_handler:module_info(module)};

resolve_zone_handler(_, policies, get) ->
    {ok, zone_policies_get_middleware_handler:module_info(module)};
resolve_zone_handler(_, policies, update) ->
    {ok, zone_policies_update_middleware_handler:module_info(module)};
    
resolve_zone_handler(_, _, _) ->
    ?ERROR_NOT_SUPPORTED.
