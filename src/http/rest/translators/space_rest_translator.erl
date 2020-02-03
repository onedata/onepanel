%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles translation of middleware results concerning
%%% the onp_space resource type into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(space_rest_translator).
-author("Wojciech Geisler").

-include("middleware/middleware.hrl").
-include("http/rest.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").

-export([create_response/3, get_response/2, update_response/2]).


%%%===================================================================
%%% API
%%%===================================================================

-spec create_response(gri:gri(), gs_protocol:data_format(),
    Result) -> #rest_resp{}
    when Result :: term() | {gri:gri(), term()}.
create_response(#gri{aspect = support, id = SpaceId}, value, _) ->
    ?CREATED_REPLY(
        [<<"provider">>, <<"spaces">>, SpaceId],
        #{<<"id">> => SpaceId}
    );

create_response(#gri{aspect = start_auto_cleaning, id = SpaceId}, value, ReportId) ->
    #rest_resp{
        code = ?HTTP_202_ACCEPTED,
        headers = rest_translator:make_location_header([
            <<"provider/spaces">>, SpaceId,
            <<"auto-cleaning/reports">>, ReportId
        ]),
        body = #{<<"reportId">> => ReportId}
    }.


-spec get_response(gri:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{aspect = list}, SpaceIds) ->
    ?OK_REPLY(#{<<"ids">> => SpaceIds});

get_response(#gri{aspect = auto_cleaning_reports_list}, ReportIds) ->
    ?OK_REPLY(#{<<"ids">> => ReportIds});

get_response(#gri{}, Data) when is_map(Data) ->
    ?OK_REPLY(Data).


-spec update_response(gri:gri(), Result :: term()) -> #rest_resp{}.
update_response(#gri{aspect = file_popularity_configuration}, AsyncTaskId) ->
    ?ASYNC_TASK_REPLY(AsyncTaskId).
