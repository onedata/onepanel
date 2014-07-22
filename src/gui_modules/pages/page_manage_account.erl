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
-include_lib("ctool/include/logging.hrl").

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
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, custom()}]};
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


%% custom/0
%% ====================================================================
%% @doc This will be placed instead of {{custom}} tag in template.
-spec custom() -> binary().
%% ====================================================================
custom() ->
    <<"<script src='/js/bootbox.min.js' type='text/javascript' charset='utf-8'></script>",
    "<script type='text/javascript' charset='utf-8'> $(document).ready(function() {
        $('body').on('show', '.modal', function () {
            $(this).css({
                'top': '50%',
                'margin-top': function () {
                    return -($(this).height() / 2);
                }
            });
        });
    });</script>">>.


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
                style = <<"margin-top: 150px; text-align: center;">>,
                body = [
                    #panel{
                        style = <<"width: 300px; margin: 0 auto;">>,
                        body = [
                            #h6{
                                style = <<"font-size: 18px;">>,
                                body = <<"Change Onepanel password">>
                            },
                            #password{
                                style = <<"width: 200px; margin: 0 auto; margin-top: 10px;">>,
                                id = <<"onepanel_old_password">>,
                                placeholder = <<"Old password">>,
                                class = <<"span2">>
                            },
                            #password{
                                style = <<"width: 200px; margin: 0 auto; margin-top: 10px;">>,
                                id = <<"onepanel_new_password">>,
                                placeholder = <<"New password">>,
                                class = <<"span2">>
                            },
                            #password{
                                style = <<"width: 200px; margin: 0 auto; margin-top: 10px;">>,
                                id = <<"onepanel_confirm_password">>,
                                placeholder = <<"Confirm password">>,
                                class = <<"span2">>
                            },
                            #button{
                                id = <<"onepanel_change_password_button">>,
                                actions = gui_jq:form_submit_action(
                                    <<"onepanel_change_password_button">>,
                                    onepanel_change_password,
                                    [<<"onepanel_old_password">>, <<"onepanel_new_password">>, <<"onepanel_confirm_password">>]
                                ),
                                style = <<"margin-top: 10px;">>,
                                class = <<"btn btn-info">>,
                                body = <<"Change password">>
                            }
                        ]
                    },
                    #panel{
                        style = <<"width: 300px; margin: 0 auto; margin-top: 30px;">>,
                        body = [
                            #h6{
                                style = <<"font-size: 18px;">>,
                                body = <<"Change database password">>
                            },
                            #password{
                                style = <<"width: 200px; margin: 0 auto; margin-top: 10px;">>,
                                id = <<"db_old_password">>,
                                placeholder = <<"Old password">>,
                                class = <<"span2">>
                            },
                            #password{
                                style = <<"width: 200px; margin: 0 auto; margin-top: 10px;">>,
                                id = <<"db_new_password">>,
                                placeholder = <<"New password">>,
                                class = <<"span2">>
                            },
                            #password{
                                style = <<"width: 200px; margin: 0 auto; margin-top: 10px;">>,
                                id = <<"db_confirm_password">>,
                                placeholder = <<"Confirm password">>,
                                class = <<"span2">>
                            },
                            #button{
                                id = <<"db_change_password_button">>,
                                actions = gui_jq:form_submit_action(
                                    <<"db_change_password_button">>,
                                    db_change_password,
                                    [<<"db_old_password">>, <<"db_new_password">>, <<"db_confirm_password">>]
                                ),
                                style = <<"margin-top: 10px;">>,
                                class = <<"btn btn-info">>,
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
        _ ->
            {error, <<"Password should be at least ", (integer_to_binary(?MIN_PASSWORD_LENGTH))/binary, " characters long.">>}
    end;

verify_new_password(_, _) ->
    {error, <<"Passwords do not match.">>}.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:focus(<<"onepanel_old_password">>),

    gui_jq:bind_enter_to_change_focus(<<"onepanel_old_password">>, <<"onepanel_new_password">>),
    gui_jq:bind_enter_to_change_focus(<<"onepanel_new_password">>, <<"onepanel_confirm_password">>),
    gui_jq:bind_enter_to_submit_button(<<"onepanel_confirm_password">>, <<"onepanel_change_password_button">>),

    gui_jq:bind_enter_to_change_focus(<<"db_old_password">>, <<"db_new_password">>),
    gui_jq:bind_enter_to_change_focus(<<"db_new_password">>, <<"db_confirm_password">>),
    gui_jq:bind_enter_to_submit_button(<<"db_confirm_password">>, <<"db_change_password_button">>),
    ok;

event(onepanel_change_password) ->
    Username = gui_ctx:get_user_id(),
    OldPassword = gui_ctx:postback_param(<<"onepanel_old_password">>),
    NewPassword = gui_ctx:postback_param(<<"onepanel_new_password">>),
    ConfirmPassword = gui_ctx:postback_param(<<"onepanel_confirm_password">>),
    case verify_new_password(NewPassword, ConfirmPassword) of
        ok ->
            case user_logic:change_password(Username, OldPassword, NewPassword) of
                ok ->
                    gui_jq:fade_out(<<"error_message">>, 300),
                    onepanel_gui_utils:message(<<"ok_message">>, "Password changed."),
                    lists:foreach(fun(PasswordBoxId) ->
                        gui_jq:set_value(PasswordBoxId, <<"''">>)
                    end, [<<"onepanel_old_password">>, <<"onepanel_new_password">>, <<"onepanel_confirm_password">>]),
                    gui_jq:focus(<<"onepanel_old_password">>);
                {error, ErrorId} ->
                    onepanel_gui_utils:message(<<"error_message">>,
                        onepanel_gui_utils:get_error_message(binary_to_atom(gui_str:to_binary(ErrorId), latin1)))
            end;
        {error, Reason} ->
            onepanel_gui_utils:message(<<"error_message">>, Reason)
    end;

event(db_change_password) ->
    Username = gui_ctx:get_user_id(),
    OldPassword = gui_ctx:postback_param(<<"db_old_password">>),
    NewPassword = gui_ctx:postback_param(<<"db_new_password">>),
    ConfirmPassword = gui_ctx:postback_param(<<"db_confirm_password">>),
    case verify_new_password(NewPassword, ConfirmPassword) of
        ok ->
            case db_logic:change_password(Username, OldPassword, NewPassword) of
                ok ->
                    gui_jq:fade_out(<<"error_message">>, 300),
                    onepanel_gui_utils:message(<<"ok_message">>, "Password changed."),
                    lists:foreach(fun(PasswordBoxId) ->
                        gui_jq:set_value(PasswordBoxId, <<"''">>)
                    end, [<<"db_old_password">>, <<"db_new_password">>, <<"db_confirm_password">>]),
                    gui_jq:focus(<<"db_old_password">>);
                {error, Error} when is_list(Error) ->
                    onepanel_gui_utils:message(<<"error_message">>, gui_str:to_binary(Error));
                {error, {host, Host}} ->
                    onepanel_gui_utils:message(<<"error_message">>,
                        <<"Cannot change database password on host: ", (gui_str:to_binary(Host))/binary>>);
                _ ->
                    onepanel_gui_utils:message(<<"error_message">>, <<"Cannot change database password">>)
            end;
        {error, Reason} ->
            onepanel_gui_utils:message(<<"error_message">>, Reason)
    end;

event(terminate) -> ok.