%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page is displayed when client asks for not existing resource.
%% @end
%% ===================================================================
-module(page_404).

-include("gui_modules/common.hrl").

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
%% @doc Page title.
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Error 404">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = [],
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #panel{
                style = <<"width: 50%; margin: 0 auto;">>,
                class = <<"alert alert-danger">>,
                body = [
                    #h3{
                        body = <<"Error 404">>
                    },
                    #p{
                        style = <<"margin-bottom: 2em;">>,
                        body = <<"Requested page could not be found on the server.">>
                    },
                    #link{
                        id = <<"to_login_button">>,
                        postback = to_login,
                        class = <<"btn btn-warning btn-block">>,
                        style = <<"width: 8em; font-weight: bold; margin: 0 auto;">>,
                        body = <<"Main page">>
                    }
                ]
            },
            gui_utils:cookie_policy_popup_body(?PAGE_PRIVACY_POLICY)
        ]
    },
    onepanel_gui_utils:body(Header, Main).


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

event(to_login) ->
    gui_jq:redirect_to_login(false);

event(terminate) ->
    ok.