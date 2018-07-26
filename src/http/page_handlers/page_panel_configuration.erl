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

-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(new_gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"GET">>, Req) ->
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        json_utils:encode(get_config()),
        Req
    ).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_config() -> maps:map().
get_config() ->
    BuildVersion = list_to_binary(application:get_env(?APP_NAME, build_version, "unknown")),
    {_AppId, _AppName, AppVersion} = lists:keyfind(
        ?APP_NAME, 1, application:loaded_applications()
    ),
    Version = list_to_binary(AppVersion),
    
    #{
        <<"version">> => Version,
        <<"build">> => BuildVersion,
        <<"deployed">> => onepanel_deployment:is_completed(?PROGRESS_READY)
    }.

