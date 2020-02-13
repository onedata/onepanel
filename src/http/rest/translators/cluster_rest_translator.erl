%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles translation of middleware results concerning
%%% the onp_cluster resource type into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_rest_translator).
-author("Wojciech Geisler").

-include("middleware/middleware.hrl").
-include("http/rest.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").

-export([create_response/3, get_response/2]).


%%%===================================================================
%%% API
%%%===================================================================

-spec create_response(gri:gri(), gs_protocol:data_format(), Result) -> #rest_resp{}
    when Result :: term() | {gri:gri(), term()}.
create_response(#gri{aspect = invite_user_token}, value, Token) ->
    ?OK_REPLY(#{<<"token">> => Token}).


-spec get_response(gri:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{aspect = current_cluster_members_summary}, Data) when is_map(Data) ->
    ?OK_REPLY(kv_utils:copy_all([
        {users_count, <<"usersCount">>},
        {groups_count, <<"groupsCount">>},
        {effective_users_count, <<"effectiveUsersCount">>},
        {effective_groups_count, <<"effectiveGroupsCount">>}
    ], Data, #{}));

get_response(#gri{}, Data) when is_map(Data) ->
    ?OK_REPLY(Data).
