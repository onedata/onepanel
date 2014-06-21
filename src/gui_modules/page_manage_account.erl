%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code
%% @end
%% ===================================================================

-module(page_manage_account).
-compile(export_all).
-include("gui_modules/common.hrl").
-include("onepanel_modules/errors.hrl").

%% Template points to the template file, which will be filled with content
main() -> case gui_utils:user_logged_in() of
            true ->
              #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}]};
            false ->
              gui_utils:redirect_to_login(true),
              #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}]}
          end.

%% Page title
title() -> <<"Manage account">>.

%% This will be placed in the template instead of [[[page:body()]]] tag
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

% Checks whether password can be changed
verify_new_password(Password, Password) ->
  case length(Password) >= ?MIN_PASSWORD_LENGTH of
    true -> ok;
    _ -> {error, ?PASSWORD_TO_SHORT}
  end;
verify_new_password(_, _) ->
  {error, ?PASSWORDS_DONT_MATCH}.

event(init) ->
  wf:wire(gui_utils:script_for_enter_submission("confirm_password", "change_button")),
  ok;

event(change_password) ->
  Username = wf:user(),
  OldPassword = wf:q(old_password),
  NewPassword = wf:q(new_password),
  ConfirmPassword = wf:q(confirm_password),
  case verify_new_password(NewPassword, ConfirmPassword) of
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
          gui_utils:update("error_message", Reason),
          wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]});
        _ ->
          gui_utils:update("error_message", ?INTERNAL_ERROR),
          wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]})
      end;
    {error, Reason} ->
      gui_utils:update("error_message", Reason),
      wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]})
  end.
