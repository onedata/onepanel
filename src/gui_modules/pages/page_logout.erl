%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page handles users' logging out.
%% @end
%% ===================================================================
-module(page_logout).

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
title() ->
    <<"Log out">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    ?info("Successful logout of user: ~p", [gui_ctx:get_user_id()]),
    gen_server:cast(?ONEPANEL_SERVER, {remove_password, gui_ctx:get_user_id()}),
    gui_ctx:clear_session(),
    session_logic:clear_expired_sessions(),
    Main = [
        #panel{
            class = <<"alert alert-success">>,
            style = <<"width: 30em; margin: 0 auto; text-align: center; margin-top: 2em;">>,
            body = [
                #h3{
                    body = <<"Successful logout">>
                },
                #p{
                    body = <<"Come back soon.">>
                },
                #button{
                    postback = to_login,
                    class = <<"btn btn-primary btn-block">>,
                    body = <<"Login page">>
                }
            ]
        },
        gui_utils:cookie_policy_popup_body(?PAGE_PRIVACY_POLICY)
    ],
    onepanel_gui_utils:body(Main).


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) -> ok;

event(to_login) ->
    gui_jq:redirect_to_login();

event(terminate) ->
    ok.