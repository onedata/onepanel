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

-include("authentication.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/http/codes.hrl").
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
        case rest_auth:authenticate_by_basic_auth(Req) of
            {#client{role = root}, Req2} ->
                Req3 = gui_session:log_in(?LOCAL_SESSION_USERNAME, Req2),
                cowboy_req:reply(?HTTP_204_NO_CONTENT, Req3);
            {{error, _} = Error, Req2} ->
                rest_handler:send_response(
                    rest_translator:error_response(Error), Req2
                )
        end
    catch Type:Reason ->
        ?error_stacktrace("Login by credentials failed - ~p:~p", [Type, Reason]),
        cowboy_req:reply(?HTTP_500_INTERNAL_SERVER_ERROR, Req)
    end.
