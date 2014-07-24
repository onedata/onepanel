%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page handles users' logging in.
%% @end
%% ===================================================================

-module(page_login).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%% main/0
%% ====================================================================
%% @doc Template points to the template file, which will be filled with content.
-spec main() -> Result when
    Result :: #dtl{}.
%% ====================================================================
main() ->
    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.


%% title/0
%% ====================================================================
%% @doc Page title.
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Log in">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{} | no_return().
%% ====================================================================
body() ->
    case gui_ctx:user_logged_in() of
        true -> gui_jq:redirect(?PAGE_ROOT);
        false ->
            SourcePage = gui_ctx:url_param(<<"x">>),
            ErrorId = gui_ctx:url_param(<<"id">>),
            Header = error_message(SourcePage, ErrorId),
            Main = #panel{
                class = <<"alert alert-success">>,
                style = <<"width: 30em; margin: 0 auto; text-align: center; margin-top: 10em;">>,
                body = [
                    #h3{
                        body = <<"Welcome to OnePanel">>
                    },
                    #form{
                        id = <<"login_form">>,
                        method = <<"POST">>,
                        action = case SourcePage of
                                     undefined -> ?PAGE_LOGIN_VALIDATION;
                                     _ -> <<(?PAGE_LOGIN_VALIDATION)/binary, "?x=", SourcePage/binary>>
                                 end,
                        style = <<"width: 15em; margin: 0 auto; padding-top: 1em; float: center">>,
                        body = [
                            #textbox{
                                id = <<"username">>,
                                name = <<"username">>,
                                class = <<"span">>,
                                placeholder = <<"Username">>
                            },
                            #password{
                                id = <<"password">>,
                                name = <<"password">>,
                                class = <<"span">>,
                                placeholder = <<"Password">>
                            },
                            #button{
                                id = <<"login_button">>,
                                type = <<"submit">>,
                                class = <<"btn btn-primary btn-block">>,
                                style = <<"margin: 0 auto;">>,
                                body = <<"Log in">>
                            }
                        ]
                    }
                ]
            },
            onepanel_gui_utils:body(Header, Main)
    end.


%% error_message/2
%% ====================================================================
%% @doc Handles page events.
-spec error_message(SourcePage :: binary() | undefined, ErrorId :: binary() | undefined) -> Result when
    Result :: #panel{}.
%% ====================================================================
error_message(undefined, undefined) ->
    #panel{
        class = <<"hidden dialog dialog-danger">>
    };
error_message(undefined, ErrorId) ->
    ErrorMessage = onepanel_gui_utils:get_error_message(binary_to_atom(gui_str:to_binary(ErrorId), latin1)),
    #panel{
        class = <<"dialog dialog-danger">>,
        body = ErrorMessage
    };
error_message(_, _) ->
    #panel{
        class = <<"dialog dialog-danger">>,
        body = <<"No session or session expired.">>
    }.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:focus(<<"username">>),
    gui_jq:bind_enter_to_change_focus(<<"username">>, <<"password">>),
    gui_jq:bind_enter_to_submit_button(<<"password">>, <<"login_button">>),
    ok;

event(terminate) ->
    ok.