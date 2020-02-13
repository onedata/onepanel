%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles translation of middleware results concerning
%%% the onp_service resource type into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(service_rest_translator).
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
create_response(#gri{aspect = Aspect}, value, AsyncTaskId) when
    Aspect == couchbase_instances;
    Aspect == cluster_manager_instances;
    Aspect == op_worker_instances;
    Aspect == oz_worker_instances
->
    ?ASYNC_TASK_REPLY(AsyncTaskId).


-spec get_response(gri:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{aspect = {host_status, _}}, Status)  ->
    ?OK_REPLY(atom_to_binary(Status, utf8));

get_response(#gri{aspect = {nagios, _}}, Response)  ->
    #{code := Code, headers := Headers, body := Body} = Response,
    #rest_resp{code = Code, headers = Headers, body = {binary, Body}};

get_response(#gri{}, Data) when is_map(Data) ->
    ?OK_REPLY(Data).
