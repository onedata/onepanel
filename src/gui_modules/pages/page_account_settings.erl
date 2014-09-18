%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows user to change username and password.
%% @end
%% ===================================================================
-module(page_account_settings).

-include("gui_modules/common.hrl").
-include("onepanel_modules/logic/user_logic.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([main/0, event/1, comet_loop/1]).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {}).

%% ====================================================================
%% API functions
%% ====================================================================

%% main/0
%% ====================================================================
%% @doc Template points to the template file, which will be filled with content.
%% @end
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
%% @doc This will be placed instead of {{title}} tag in template.
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Account settings">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(account_settings_tab),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Account settings">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"Any change to username or password is also done for account in administration database.">>
            },
            settings_table()
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% settings_table/0
%% ====================================================================
%% @doc Renders the body of settings table.
%% @end
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
%% @end
-spec username(Username :: binary()) -> Result when
    Result :: #span{}.
%% ====================================================================
username(Username) ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            gui_str:html_encode(Username),
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
%% @end
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
%% @end
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
%% @end
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
                    [<<"current_password_textbox">>, <<"new_password_textbox">>, <<"confirmed_password_textbox">>]),
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
                id = <<"confirmed_password_textbox">>,
                class = <<"span">>,
                placeholder = <<"Confirm password">>
            }
        ]
    }.


%% ====================================================================
%% Events handling
%% ====================================================================

%% comet_loop/1
%% ====================================================================
%% @doc Handles user's account management actions.
%% @end
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{} = State) ->
    NewState = try
        receive
            {submit_new_username, Username, NewUsername} ->
                case user_logic:change_username(Username, NewUsername) of
                    ok ->
                        gui_ctx:set_user_id(NewUsername),
                        gui_jq:update(<<"account_settings_tab">>, onepanel_gui_utils:account_settings_tab(NewUsername)),
                        onepanel_gui_utils:message(<<"ok_message">>, <<"Username changed.">>),
                        gui_jq:update(<<"username">>, username(NewUsername));
                    {error, Reason} ->
                        onepanel_gui_utils:message(<<"error_message">>, Reason)
                end,
                gui_jq:hide(<<"main_spinner">>),
                State;

            {submit_new_password, Username, CurrentPassword, NewPassword, ConfirmedPassword} ->
                case user_logic:change_password(Username, CurrentPassword, NewPassword, ConfirmedPassword) of
                    ok ->
                        onepanel_gui_utils:message(<<"ok_message">>, "Password changed."),
                        gui_jq:update(<<"password">>, password());
                    {error, Reason} ->
                        onepanel_gui_utils:message(<<"error_message">>, Reason)
                end,
                gui_jq:hide(<<"main_spinner">>),
                State

        after ?COMET_PROCESS_RELOAD_DELAY ->
            State
        end
               catch Type:Message ->
                   ?error("Comet process exception: ~p:~p", [Type, Message]),
                   onepanel_gui_utils:message(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Message}
               end,
    gui_comet:flush(),
    ?MODULE:comet_loop(NewState).


%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#?STATE{}) end),
    put(?COMET_PID, Pid),
    ok;

event(change_username) ->
    gui_jq:update(<<"username">>, change_username()),
    gui_jq:focus(<<"new_username_textbox">>),
    gui_jq:bind_enter_to_submit_button(<<"new_username_textbox">>, <<"new_username_submit">>);

event(cancel_new_username_submit) ->
    Username = gui_ctx:get_user_id(),
    gui_jq:update(<<"username">>, username(Username));

event(submit_new_username) ->
    Username = gui_ctx:get_user_id(),
    NewUsername = gui_ctx:postback_param(<<"new_username_textbox">>),
    get(?COMET_PID) ! {submit_new_username, Username, NewUsername},
    gui_jq:show(<<"main_spinner">>);

event(change_password) ->
    gui_jq:update(<<"password">>, change_password()),
    gui_jq:focus(<<"current_password_textbox">>),
    gui_jq:bind_enter_to_change_focus(<<"current_password_textbox">>, <<"new_password_textbox">>),
    gui_jq:bind_enter_to_change_focus(<<"new_password_textbox">>, <<"confirmed_password_textbox">>),
    gui_jq:bind_enter_to_submit_button(<<"confirmed_password_textbox">>, <<"new_password_submit">>);

event(cancel_new_password_submit) ->
    gui_jq:update(<<"password">>, password());

event(submit_new_password) ->
    Username = gui_ctx:get_user_id(),
    CurrentPassword = gui_ctx:postback_param(<<"current_password_textbox">>),
    NewPassword = gui_ctx:postback_param(<<"new_password_textbox">>),
    ConfirmedPassword = gui_ctx:postback_param(<<"confirmed_password_textbox">>),
    get(?COMET_PID) ! {submit_new_password, Username, CurrentPassword, NewPassword, ConfirmedPassword},
    gui_jq:show(<<"main_spinner">>);

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.
