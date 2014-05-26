%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code
%% @end
%% ===================================================================

-module(page_login).
-compile(export_all).
-include("gui_modules/common.hrl").
-include("spanel_modules/errors.hrl").

%% Template points to the template file, which will be filled with content
main() -> #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}]}.

%% Page title
title() -> <<"Login page">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
  case gui_utils:user_logged_in() of
    true -> wf:redirect(<<"/">>);
    false ->
      ErrorPanelStyle = case wf:q(<<"x">>) of
                          undefined -> <<"position: fixed; top: 0; width: 100%; display: none;">>;
                          _ -> <<"position: fixed; top: 0; width: 100%;">>
                        end,
      #panel{style = <<"position: relative;">>, body = [
        #panel{id = <<"error_message">>, style = ErrorPanelStyle, class = <<"dialog dialog-danger">>,
          body = <<"Session error or session expired. Please log in again.">>},
        #panel{class = <<"alert alert-success login-page">>, body = [
          #h3{body = <<"Welcome to SPanel">>},
          #panel{style = <<"width: 50%; margin: 0 auto; padding-top: 20px; float: center">>, body = [
            #textbox{id = username, placeholder = <<"Username">>, class = <<"flat">>},
            #password{id = password, placeholder = <<"Password">>, class = <<"flat">>}
          ]},
          #button{id = "login_button", postback = login, source = [username, password], class = <<"btn btn-primary btn-block">>,
            style = <<"width: 50%; margin: 0 auto;">>, body = <<"Login">>}
        ]}
      ] ++ gui_utils:logotype_footer(120)}
  end.

event(init) ->
  wf:wire(gui_utils:script_for_enter_submission("password", "login_button")),
  ok;

event(login) ->
  Username = wf:q(username),
  Password = wf:q(password),
  case gen_server:call(?SPANEL_NAME, {authenticate, Username, Password}, ?GEN_SERVER_TIMEOUT) of
    ok ->
      wf:user(Username),
      gui_utils:redirect_from_login();
    {error, Reason} when is_list(Reason) ->
      gui_utils:update("error_message", Reason),
      wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]});
    _ ->
      gui_utils:update("error_message", ?INTERNAL_ERROR),
      wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]})
  end.