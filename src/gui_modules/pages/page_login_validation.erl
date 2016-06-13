%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains n2o website code.
%% This page handles user validation via OpenID.
%% @end
%% ===================================================================
-module(page_login_validation).

-include("onepanel_modules/logic/db_logic.hrl").
-include("gui_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1]).

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
    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.


%% title/0
%% ====================================================================
%% @doc This will be placed instead of {{title}} tag in template.
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() -> <<"Login validation">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: no_return().
%% ====================================================================
body() ->
    case gui_ctx:user_logged_in() of
        true -> gui_jq:redirect(?PAGE_ROOT);
        false ->
            Params = gui_ctx:form_params(),
            Username = proplists:get_value(<<"username">>, Params),
            Password = proplists:get_value(<<"password">>, Params),
            case user_logic:authenticate(Username, Password) of
                {ok, #?USER_RECORD{role = admin}} ->
                    case gen_server:call(?ONEPANEL_SERVER, {set_password, Username, Password}) of
                        ok ->
                            ?info("Successful login of user: ~p", [Username]),
                            gui_ctx:create_session(),
                            gui_ctx:set_user_id(Username),
                            gui_jq:redirect_from_login();
                        Other ->
                            ?error("Cannot set password for user ~p: ~p", [Username, Other]),
                            page_error:redirect_with_error(?INTERNAL_SERVER_ERROR)
                    end;
                {ok, #?USER_RECORD{}} ->
                    page_error:redirect_with_error(?AUTHORIZATION_ERROR);
                {error, ErrorId} ->
                    ?error("Invalid login attempt, user ~p: ~p", [Username, ErrorId]),
                    page_error:redirect_with_error(ErrorId)
            end
    end.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    ok;

event(terminate) ->
    ok.