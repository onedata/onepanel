%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code.
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
main() -> #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}]}.


%% title/0
%% ====================================================================
%% @doc Page title.
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() -> <<"Login page">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{} | no_return().
%% ====================================================================
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
                    #h3{body = <<"Welcome tonepanelel">>},
                    #panel{style = <<"width: 50%; margin: 0 auto; padding-top: 20px; float: center">>, body = [
                        #textbox{id = username, placeholder = <<"Username">>, class = <<"flat">>},
                        #password{id = password, placeholder = <<"Password">>, class = <<"flat">>}
                    ]},
                    #button{id = "login_button", postback = login, source = [username, password], class = <<"btn btn-primary btn-block">>,
                        style = <<"width: 50%; margin: 0 auto;">>, body = <<"Login">>}
                ]}
            ] ++ gui_utils:logotype_footer(120)}
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
    wf:wire(gui_utils:script_for_enter_submission("password", "login_button")),
    ok;

event(login) ->
    Username = wf:q(username),
    Password = wf:q(password),
    case user_logic:authenticate(Username, Password) of
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