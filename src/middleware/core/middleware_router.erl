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
    resolve_cluster_handler(Interface, Operation, Aspect);

resolve_handler(Interface, Operation, #gri{type = onp_host, aspect = Aspect}) ->
    resolve_host_handler(Interface, Operation, Aspect);

resolve_handler(_, _, _) ->
    ?ERROR_NOT_SUPPORTED.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec resolve_cluster_handler(middleware_handler:interface(), middleware:operation(), gri:aspect()) ->
    {ok, middleware_handler:t()} | no_return().
resolve_cluster_handler(_, create, invite_user_token) ->
    {ok, cluster_create_invite_user_token_middleware_handler:module_info(module)};
resolve_cluster_handler(_, get, instance) ->
    {ok, cluster_get_instance_middleware_handler:module_info(module)};
resolve_cluster_handler(_, get, current_cluster) ->
    {ok, cluster_get_current_cluster_middleware_handler:module_info(module)};
resolve_cluster_handler(_, get, current_cluster_members_summary) ->
    {ok, cluster_get_current_cluster_members_summary_middleware_handler:module_info(module)};
resolve_cluster_handler(_, _, _) ->
    ?ERROR_NOT_SUPPORTED.


%% @private
-spec resolve_host_handler(middleware_handler:interface(), middleware:operation(), gri:aspect()) ->
    {ok, middleware_handler:t()} | no_return().
resolve_host_handler(_, create, instance) ->
    {ok, host_create_instance_middleware_handler:module_info(module)};
resolve_host_handler(_, create, join_cluster) ->
    {ok, host_create_join_cluster_middleware_handler:module_info(module)};
resolve_host_handler(_, get, external_ips) ->
    {ok, host_get_external_ips_middleware_handler:module_info(module)};
resolve_host_handler(_, get, instance) ->
    {ok, host_get_instance_middleware_handler:module_info(module)};
resolve_host_handler(_, get, list) ->
    {ok, host_get_list_middleware_handler:module_info(module)};
resolve_host_handler(_, update, external_ips) ->
    {ok, host_update_external_ips_middleware_handler:module_info(module)};
resolve_host_handler(_, delete, instance) ->
    {ok, host_delete_instance_middleware_handler:module_info(module)};
resolve_host_handler(_, _, _) ->
    ?ERROR_NOT_SUPPORTED.
