%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when user attempts to log in with basic auth credentials.
%%% @end
%%%-------------------------------------------------------------------
-module(page_basic_auth_login).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/logging.hrl").

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
    try
        <<"Basic ", B64/binary>> = cowboy_req:header(<<"authorization">>, Req),
        [Username, Password] = binary:split(base64:decode(B64), <<":">>),
        case onepanel_user:authenticate_by_basic_auth(Username, Password) of
            {ok, #onepanel_user{}} ->
                Req2 = gui_session:log_in(Username, Req),
                ?info("User ~s logged in", [Username]),
                cowboy_req:reply(204, Req2);
            #error{} = Error->
                BodyJson = rest_replier:format_error(undefined, Error),
                cowboy_req:reply(401, #{}, json_utils:encode(BodyJson), Req)
        end
    catch Type:Reason ->
        ?error_stacktrace("Login by credentials failed - ~p:~p", [Type, Reason]),
        cowboy_req:reply(500, Req)
    end.
