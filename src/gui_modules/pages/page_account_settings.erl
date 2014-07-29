%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page allows user to change username and password.
%% @end
%% ===================================================================
-module(page_account_settings).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").
-include("onepanel_modules/user_logic.hrl").
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
    <<"Account settings">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(account_settings_tab),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
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
            #h6{
                style = <<"font-size: x-large; margin-bottom: 3em;">>,
                body = <<"Account settings">>
            },
            settings_table()
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% settings_table/0
%% ====================================================================
%% @doc Renders the body of settings table.
-spec settings_table() -> Result when
    Result :: #table{}.
%% ====================================================================
settings_table() ->
    RowStyle = <<"line-height: 4em;">>,
    DescriptionStyle = <<"border-width: 0; text-align: right; padding: 1em 1em; width: 50%;">>,
    MainStyle = <<"border-width: 0;  text-align: left; padding: 1em 1em;">>,
    #table{
        style = <<"border-width: 0; width: 100%;">>, body = [
            #tr{
                style = RowStyle,
                cells = [
                    #td{
                        style = DescriptionStyle,
                        body = #label{
                            class = <<"label label-large label-inverse">>,
                            body = <<"Username">>
                        }
                    },
                    #td{
                        id = <<"username">>,
                        style = MainStyle,
                        body = username(gui_ctx:get_user_id())
                    }
                ]
            },
            #tr{
                style = RowStyle,
                cells = [
                    #td{
                        style = DescriptionStyle,
                        body = #label{
                            class = <<"label label-large label-inverse">>,
                            body = <<"Password">>
                        }
                    },
                    #td{
                        id = <<"password">>,
                        style = MainStyle,
                        body = password()
                    }
                ]
            }
        ]
    }.


%% username/1
%% ====================================================================
%% @doc Renders user's current name.
-spec username(Username :: binary()) -> Result when
    Result :: #span{}.
%% ====================================================================
username(Username) ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            Username,
            #link{
                title = <<"Edit">>,
                style = <<"margin-left: 1em;">>,
                class = <<"glyph-link">>,
                postback = change_username,
                body = #span{
                    class = <<"fui-new">>
                }
            }
        ]
    }.


%% change_username/0
%% ====================================================================
%% @doc Renders change username input field.
-spec change_username() -> Result when
    Result :: list().
%% ====================================================================
change_username() ->
    [
        #textbox{
            id = <<"new_username_textbox">>,
            class = <<"span">>,
            placeholder = <<"New username">>
        },
        #link{
            id = <<"new_username_submit">>,
            class = <<"glyph-link">>,
            style = <<"margin-left: 1em;">>,
            title = <<"Submit">>,
            actions = gui_jq:form_submit_action(<<"new_username_submit">>, submit_new_username, <<"new_username_textbox">>),
            body = #span{
                class = <<"fui-check-inverted">>,
                style = <<"font-size: large;">>
            }
        },
        #link{
            class = <<"glyph-link">>,
            style = <<"margin-left: 10px;">>,
            title = <<"Cancel">>,
            postback = cancel_new_username_submit,
            body = #span{
                class = <<"fui-cross-inverted">>,
                style = <<"font-size: large;">>
            }
        }
    ].


%% password/0
%% ====================================================================
%% @doc Renders hypothetic user password as sequence od dots.
-spec password() -> Result when
    Result :: #span{}.
%% ====================================================================
password() ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            <<"&#9679&#9679&#9679&#9679&#9679&#9679&#9679&#9679">>,
            #link{
                title = <<"Edit">>,
                style = <<"margin-left: 1em;">>,
                class = <<"glyph-link">>,
                postback = change_password,
                body = #span{
                    class = <<"fui-new">>
                }
            }
        ]
    }.


%% change_password/0
%% ====================================================================
%% @doc Renders change password input fields.
-spec change_password() -> Result when
    Result :: #panel{}.
%% ====================================================================
change_password() ->
    #panel{
        style = <<"width: 19em;">>,
        body = [
            #password{
                id = <<"current_password_textbox">>,
                class = <<"span">>,
                placeholder = <<"Current password">>
            },
            #link{
                id = <<"new_password_submit">>,
                class = <<"glyph-link">>,
                style = <<"margin-left: 1em;">>,
                title = <<"Submit">>,
                actions = gui_jq:form_submit_action(<<"new_password_submit">>, submit_new_password,
                    [<<"current_password_textbox">>, <<"new_password_textbox">>, <<"confirm_password_textbox">>]),
                body = #span{
                    class = <<"fui-check-inverted">>,
                    style = <<"font-size: large;">>
                }
            },
            #link{
                class = <<"glyph-link">>,
                style = <<"margin-left: 10px;">>,
                title = <<"Cancel">>,
                postback = cancel_new_password_submit,
                body = #span{
                    class = <<"fui-cross-inverted">>,
                    style = <<"font-size: large;">>
                }
            },
            #password{
                id = <<"new_password_textbox">>,
                class = <<"span">>,
                placeholder = <<"New password">>
            },
            #password{
                id = <<"confirm_password_textbox">>,
                class = <<"span">>,
                placeholder = <<"Confirm password">>
            }
        ]
    }.


%% verify_new_username/2
%% ====================================================================
%% @doc Checks whether username can be changed, that is new username is
%% not empty and is different from current username.
-spec verify_new_username(Username :: binary(), NewUsername :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
verify_new_username(Username, Username) ->
    {error, <<"New username is the same as current username.">>};

verify_new_username(_, NewUsername) ->
    case size(NewUsername) > 0 of
        true -> ok;
        _ -> {error, <<"Username cannot be empty.">>}
    end.


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
    ok;

event(change_username) ->
    gui_jq:update(<<"username">>, change_username()),
    gui_jq:bind_enter_to_submit_button(<<"new_username_textbox">>, <<"new_username_submit">>),
    gui_jq:focus(<<"new_username_textbox">>);

event(cancel_new_username_submit) ->
    Username = gui_ctx:get_user_id(),
    gui_jq:update(<<"username">>, username(Username));

event(submit_new_username) ->
    Username = gui_ctx:get_user_id(),
    NewUsername = gui_ctx:postback_param(<<"new_username_textbox">>),
    case verify_new_username(Username, NewUsername) of
        ok ->
            case user_logic:change_username(Username, NewUsername) of
                ok ->
                    gui_ctx:set_user_id(NewUsername),
                    gui_jq:update(<<"page-header">>, onepanel_gui_utils:top_menu(account_settings_tab)),
                    onepanel_gui_utils:message(<<"ok_message">>, "Username changed."),
                    gui_jq:update(<<"username">>, username(NewUsername));
                {error, ErrorId} ->
                    onepanel_gui_utils:message(<<"error_message">>,
                        onepanel_gui_utils:get_error_message(binary_to_atom(gui_str:to_binary(ErrorId), latin1))),
                    gui_jq:update(<<"username">>, username(Username))
            end;
        {error, Reason} ->
            onepanel_gui_utils:message(<<"error_message">>, Reason),
            gui_jq:update(<<"username">>, username(Username))
    end;

event(change_password) ->
    gui_jq:update(<<"password">>, change_password()),
    gui_jq:bind_enter_to_change_focus(<<"current_password_textbox">>, <<"new_password_textbox">>),
    gui_jq:bind_enter_to_change_focus(<<"new_password_textbox">>, <<"confirm_password_textbox">>),
    gui_jq:bind_enter_to_submit_button(<<"confirm_password_textbox">>, <<"new_password_submit">>),
    gui_jq:focus(<<"current_password_textbox">>);

event(cancel_new_password_submit) ->
    gui_jq:update(<<"password">>, password());

event(submit_new_password) ->
    Username = gui_ctx:get_user_id(),
    CurrentPassword = gui_ctx:postback_param(<<"current_password_textbox">>),
    NewPassword = gui_ctx:postback_param(<<"new_password_textbox">>),
    ConfirmPassword = gui_ctx:postback_param(<<"confirm_password_textbox">>),
    case verify_new_password(NewPassword, ConfirmPassword) of
        ok ->
            case user_logic:change_password(Username, CurrentPassword, NewPassword) of
                ok ->
                    gui_jq:fade_out(<<"error_message">>, 300),
                    onepanel_gui_utils:message(<<"ok_message">>, "Password changed."),
                    gui_jq:update(<<"password">>, password());
                {error, ErrorId} ->
                    onepanel_gui_utils:message(<<"error_message">>,
                        onepanel_gui_utils:get_error_message(binary_to_atom(gui_str:to_binary(ErrorId), latin1)))
            end;
        {error, Reason} ->
            onepanel_gui_utils:message(<<"error_message">>, Reason)
    end;

event(terminate) -> ok.
