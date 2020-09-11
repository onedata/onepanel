%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles translation of middleware results concerning
%%% the onp_user resource type into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(user_rest_translator).
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
create_response(#gri{aspect = instance}, resource, {#gri{id = UserId}, _}) ->
    ?CREATED_REPLY(
        [<<"zone">>, <<"users">>, UserId],
        #{<<"id">> => UserId}
    ).


-spec get_response(gri:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{aspect = current_user_clusters}, CurrentIds) ->
    ?OK_REPLY(#{<<"ids">> => CurrentIds});

get_response(#gri{aspect = list}, Ids) ->
    ?OK_REPLY(#{<<"ids">> => Ids});

get_response(#gri{}, Data) when is_map(Data) ->
    ?OK_REPLY(Data).
