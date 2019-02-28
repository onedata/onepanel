%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when rest credentials page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_gui_token).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("modules/models.hrl").

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
    case gui_session:validate(Req) of
        {ok, _Username, Cookie, NewReq} ->
            SessionId = gui_session:get_session_id(Cookie),
            {ok, Session} = onepanel_session:get(SessionId),
            #onepanel_session{
                rest_tokens = [{Token, Expires} | _]
            } = onepanel_session:maybe_update_token(Session),

            BodyJson = json_utils:encode(#{
                token => Token,
                ttl => Expires - time_utils:system_time_seconds()
            }),
            Headers = #{<<"content-type">> => <<"application/json">>},
            cowboy_req:reply(200, Headers, BodyJson, NewReq);
        {error, no_session_cookie} ->
            cowboy_req:reply(401, Req);
        {error, invalid} ->
            cowboy_req:reply(401, Req)
    end.
