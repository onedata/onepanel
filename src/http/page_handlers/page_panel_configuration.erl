%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when configuration page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_panel_configuration).
-author("Michal Stanisz").

-behaviour(dynamic_page_behaviour).

-include("deployment_progress.hrl").
-include("names.hrl").
-include_lib("ctool/include/http/codes.hrl").

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
    cowboy_req:reply(?HTTP_200_OK,
        #{<<"content-type">> => <<"application/json">>},
        json_utils:encode(rest_replier:format_onepanel_configuration()),
        Req
    ).
