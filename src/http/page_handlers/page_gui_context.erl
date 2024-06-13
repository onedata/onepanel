%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when gui-context page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_gui_context).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("modules/models.hrl").
-include("authentication.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/onedata.hrl").

-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"GET">>, Req) ->
    ApiOrigin = str_utils:format_bin("~ts:~B", [
        cowboy_req:host(Req), https_listener:port()
    ]),
    % get_id/0 throws in an unregistered Oneprovider
    ClusterId = try clusters:get_id() catch _:_ -> null end,
    cowboy_req:reply(
        ?HTTP_200_OK,
        #{?HDR_CONTENT_TYPE => <<"application/json">>},
        json_utils:encode(#{
            <<"clusterType">> => onepanel_env:get_cluster_type(),
            <<"clusterId">> => ClusterId,
            <<"serviceType">> => ?ONEPANEL,
            <<"apiOrigin">> => ApiOrigin,
            <<"guiMode">> => ?EMERGENCY,
            <<"browserDebugLogs">> => onepanel_env:get(gui_debug_mode)
        }),
        Req
    ).
