%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% The page handles users' logging in.
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
    <<"Login">>.


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

            {ErrorPanelStyle, ErrorMessage} =
                case SourcePage of
                    undefined ->
                        case gui_ctx:url_param(<<"id">>) of
                            undefined -> {<<"position: fixed; top: 0; width: 100%; display: none;">>, <<"">>};
                            ErrorId -> {<<"position: fixed; top: 0; width: 100%;">>,
                                onepanel_gui_utils:get_error_message(binary_to_atom(gui_str:to_binary(ErrorId), latin1))}
                        end;
                    _ ->
                        {<<"position: fixed; top: 0; width: 100%;">>, <<"No session or session expired. Please log in.">>}
                end,

            #panel{
                style = <<"position: relative;">>,
                body = [
                    #panel{
                        id = <<"error_message">>,
                        style = ErrorPanelStyle,
                        class = <<"dialog dialog-danger">>,
                        body = #p{
                            body = ErrorMessage
                        }
                    },
                    #panel{
                        class = <<"alert alert-success login-page">>,
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
                                style = <<"width: 50%; margin: 0 auto; padding-top: 20px; float: center">>,
                                body = [
                                    #textbox{
                                        id = <<"username">>,
                                        name = <<"username">>,
                                        placeholder = <<"Username">>,
                                        class = <<"span2">>
                                    },
                                    #password{
                                        id = <<"password">>,
                                        name = <<"password">>,
                                        placeholder = <<"Password">>,
                                        class = <<"span2">>
                                    },
                                    #button{
                                        id = <<"login_button">>,
                                        type = <<"submit">>,
                                        class = <<"btn btn-primary btn-block">>,
                                        style = <<"width: 50%; margin: 0 auto;">>,
                                        body = <<"Login">>
                                    }
                                ]
                            }
                        ]
                    }
                ] ++ onepanel_gui_utils:logotype_footer(120)
            }
    end.


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