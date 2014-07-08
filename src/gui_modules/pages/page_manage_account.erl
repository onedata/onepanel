%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% The page allows user to change password.
%% @end
%% ===================================================================
-module(page_manage_account).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").

-define(MIN_PASSWORD_LENGTH, 8).

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
    case gui_ctx:user_logged_in() of
        true ->
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]};
        _ ->
            gui_jq:redirect_to_login(true),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


%% title/0
%% ====================================================================
%% @doc Page title.
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Manage account">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    #panel{
        style = <<"position: relative;">>,
        body = [
            onepanel_gui_utils:top_menu(manage_account_tab),

            #panel{
                id = <<"ok_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-success">>
            },
            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #panel{
                style = <<"position: relative;">>,
                body = [
                    #panel{
                        class = <<"alert alert-success login-page">>,
                        body = [
                            #panel{
                                style = <<"width: 50%; margin: 0 auto; float: center">>,
                                body = [
                                    #password{
                                        id = <<"old_password">>,
                                        placeholder = <<"Old password">>,
                                        class = <<"span2">>
                                    },
                                    #password{
                                        id = <<"new_password">>,
                                        placeholder = <<"New password">>,
                                        class = <<"span2">>
                                    },
                                    #password{
                                        id = <<"confirm_password">>,
                                        placeholder = <<"Confirm password">>,
                                        class = <<"span2">>
                                    }
                                ]
                            },
                            #button{
                                id = <<"change_password_button">>,
                                actions = gui_jq:form_submit_action(<<"change_password_button">>,
                                    change_password, [<<"old_password">>, <<"new_password">>, <<"confirm_password">>]),
                                class = <<"btn btn-primary btn-block">>,
                                style = <<"width: 50%; margin: 0 auto;">>,
                                body = <<"Change password">>
                            }
                        ]
                    }
                ]
            }
        ] ++ onepanel_gui_utils:logotype_footer(120)
    }.


%% verify_new_password/2
%% ====================================================================
%% @doc Checks whether password can be changed, that is new password and
%% confirmed password match and new password is at least ?MIN_PASSWORD_LENGTH
%% characters long.
-spec verify_new_password(NewPassword :: binary(), ConfirmedPassword :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
verify_new_password(Password, Password) ->
    case size(Password) >= ?MIN_PASSWORD_LENGTH of
        true -> ok;
        _ -> {error, "Password should be at least " ++ integer_to_list(?MIN_PASSWORD_LENGTH) ++ " characters long."}
    end;

verify_new_password(_, _) ->
    {error, "Passwords do not match."}.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:bind_enter_to_change_focus(<<"old_password">>, <<"new_password">>),
    gui_jq:bind_enter_to_change_focus(<<"new_password">>, <<"confirm_password">>),
    gui_jq:bind_enter_to_submit_button(<<"confirm_password">>, <<"change_password_button">>),
    ok;

event(change_password) ->
    Username = gui_ctx:get_user_id(),
    OldPassword = gui_ctx:postback_param(<<"old_password">>),
    NewPassword = gui_ctx:postback_param(<<"new_password">>),
    ConfirmPassword = gui_ctx:postback_param(<<"confirm_password">>),
    case verify_new_password(NewPassword, ConfirmPassword) of
        ok ->
            case user_logic:change_password(Username, OldPassword, NewPassword) of
                ok ->
                    gui_jq:fade_out(<<"error_message">>, 300),
                    onepanel_gui_utils:message(<<"ok_message">>, "Password changed."),
                    lists:foreach(fun(PasswordBoxId) ->
                        gui_jq:set_value(PasswordBoxId, <<"''">>)
                    end, [<<"old_password">>, <<"new_password">>, <<"confirm_password">>]);
                {error, ErrorId} ->
                    onepanel_gui_utils:message(<<"error_message">>,
                        onepanel_gui_utils:get_error_message(binary_to_atom(gui_str:to_binary(ErrorId), latin1)))
            end;
        {error, Reason} ->
            onepanel_gui_utils:message(<<"error_message">>, Reason)
    end;

event(terminate) -> ok.