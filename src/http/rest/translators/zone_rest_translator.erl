%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles translation of middleware results concerning
%%% the onp_zone resource type into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(zone_rest_translator).
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
create_response(#gri{aspect = cluster}, value, AsyncTaskId) ->
    ?ASYNC_TASK_REPLY(AsyncTaskId).


get_response(#gri{aspect = policies}, Policies)  ->
    ?OK_REPLY(kv_utils:copy_found([
        {oneprovider_registration, <<"oneproviderRegistration">>},
        {subdomain_delegation, <<"subdomainDelegation">>},
        {gui_package_verification, <<"guiPackageVerification">>},
        {harvester_gui_package_verification, <<"harvesterGuiPackageVerification">>}
    ], Policies));

get_response(#gri{}, Data) when is_map(Data) ->
    ?OK_REPLY(Data).
