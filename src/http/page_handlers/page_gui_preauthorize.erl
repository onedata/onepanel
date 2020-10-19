%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called when
%%% gui-preauthorize page is visited (used to acquire gui tokens for authorizing
%%% operations).
%%% @end
%%%-------------------------------------------------------------------
-module(page_gui_preauthorize).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("modules/models.hrl").
-include("authentication.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").

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
        {ok, _Username, SessionId, NewReq} ->
            {ok, Session} = onepanel_session:get(SessionId),
            #onepanel_session{username = ?LOCAL_SESSION_USERNAME} = Session,
            #onepanel_session{
                auth_tokens = [{Token, Expires} | _]
            } = onepanel_session:ensure_fresh_token(Session),

            BodyJson = json_utils:encode(#{
                token => Token,
                ttl => Expires - time_utils:timestamp_seconds()
            }),
            Headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
            cowboy_req:reply(?HTTP_200_OK, Headers, BodyJson, NewReq);
        {error, no_session_cookie} ->
            cowboy_req:reply(?HTTP_401_UNAUTHORIZED, Req);
        {error, invalid} ->
            cowboy_req:reply(?HTTP_401_UNAUTHORIZED, Req)
    end.
