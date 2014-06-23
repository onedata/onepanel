%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code.
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
main() -> case gui_utils:user_logged_in() of
              true ->
                  #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}]};
              false ->
                  gui_utils:redirect_to_login(true),
                  #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}]}
          end.


%% title/0
%% ====================================================================
%% @doc Page title.
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() -> <<"Manage account">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    #panel{style = <<"position: relative;">>, body = [
        gui_utils:top_menu(manage_account_tab),
        #panel{id = <<"ok_message">>, style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
            class = <<"dialog dialog-success">>},
        #panel{id = <<"error_message">>, style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
            class = <<"dialog dialog-danger">>},
        #panel{style = <<"position: relative;">>, body = [
            #panel{class = <<"alert alert-success login-page">>, body = [
                #panel{style = <<"width: 50%; margin: 0 auto; float: center">>, body = [
                    #password{id = old_password, placeholder = <<"Old password">>, class = <<"flat">>},
                    #password{id = new_password, placeholder = <<"New password">>, class = <<"flat">>},
                    #password{id = confirm_password, placeholder = <<"Confirm password">>, class = <<"flat">>}
                ]},
                #button{id = "change_button", postback = change_password, source = [old_password, new_password, confirm_password], class = <<"btn btn-primary btn-block">>,
                    style = <<"width: 50%; margin: 0 auto;">>, body = <<"Change password">>}
            ]}
        ]}
    ]}.


%% verify_new_password/2
%% ====================================================================
%% @doc Checks whether password can be changed, that is new password and
%% confirmed password match and new password is at least ?MIN_PASSWORD_LENGTH
%% characters long.
-spec verify_new_password(NewPassword :: string(), ConfirmedPassword :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
verify_new_password(Password, Password) ->
    case length(Password) >= ?MIN_PASSWORD_LENGTH of
        true -> ok;
        _ -> {error, "Password should have at least " ++ integer_to_list(?MIN_PASSWORD_LENGTH) ++ " characters long."}
    end;
verify_new_password(_, _) ->
    {error, "Passwords does not match."}.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    wf:wire(gui_utils:script_for_enter_submission("confirm_password", "change_button")),
    ok;

event(change_password) ->
    Username = wf:user(),
    OldPassword = wf:q(old_password),
    NewPassword = wf:q(new_password),
    ConfirmedPassword = wf:q(confirm_password),
    case verify_new_password(NewPassword, ConfirmedPassword) of
        ok ->
            case user_logic:change_password(Username, OldPassword, NewPassword) of
                ok ->
                    gui_utils:update("ok_message", "Password changed."),
                    wf:wire(#jquery{target = "error_message", method = ["fadeOut"], args = [300]}),
                    wf:wire(#jquery{target = "ok_message", method = ["fadeIn"], args = [300]}),
                    lists:foreach(fun(PasswordBox) ->
                        wf:wire(#jquery{target = PasswordBox, method = ["val"], args = ["\"\""]})
                    end, ["old_password", "new_password", "confirm_password"]);
                {error, Reason} when is_list(Reason) ->
                    lager:error("Cannot change user password: ~p", [Reason]),
                    gui_utils:update("error_message", Reason),
                    wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]});
                Other ->
                    lager:error("Cannot change user password: ~p", [Other]),
                    gui_utils:update("error_message", "Internal server error."),
                    wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]})
            end;
        {error, Reason} ->
            gui_utils:update("error_message", Reason),
            wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]})
    end.
