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
handle(<<"POST">>, Req) ->
    Domain = case onepanel_env:get_cluster_type() of
        onezone -> service_oz_worker:get_domain();
        oneprovider -> service_op_worker:get_domain()
    end,
    Origin = str_utils:format_bin("https://~s:~B", [Domain, https_listener:port()]),
    cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"application/json">>},
        json_utils:encode(#{
            <<"clusterType">> => onepanel_env:get_cluster_type(),
            <<"clusterId">> => clusters:get_id(),
            <<"serviceType">> => ?ONEPANEL,
            <<"origin">> => Origin,
            <<"guiMode">> => ?EMERGENCY
        }),
        Req
    ).
