%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page handles users' logging in.
%% @end
%% ===================================================================
-module(page_login).

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
%% @doc This will be placed instead of {{title}} tag in template.
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Log in">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{} | no_return().
%% ====================================================================
body() ->
    case gui_ctx:user_logged_in() of
        true ->
            gui_jq:redirect(?PAGE_ROOT);
        false ->
            case application:get_env(?APP_NAME, verify_gr_cert) of
                {ok, true} ->
                    case file:read_file(http_client:ca_bundle_location()) of
                        {ok, _} ->
                            render_login_page();
                        _ ->
                            render_ca_alert()
                    end;
                {ok, false} ->
                    render_login_page()
            end
    end.


render_ca_alert() ->
    #panel{
        class = <<"alert alert-error">>,
        style = <<"width: 30em; margin: 0 auto; text-align: center; margin-top: 2em;">>,
        body = [
            #h3{
                body = <<"Welcome to onepanel">>
            },
            #p{style = <<"color: red; font-weight: 600; margin-top: 15px;">>, body = [
                <<"Trusted CA bundle not found.">>,
                #br{},
                gui_str:format_bin("Please make sure it is present under path: ~s",
                    [http_client:ca_bundle_location()])
            ]},
            #button{
                id = <<"refresh_button">>,
                class = <<"btn btn-warning btn-block">>,
                style = <<"margin: 10px auto;">>,
                body = <<"Refresh">>,
                postback = refresh
            }
        ]}.


render_login_page() ->
    Header = [],
    Main = [
        #panel{
            class = <<"alert alert-success">>,
            style = <<"width: 30em; margin: 0 auto; text-align: center; margin-top: 2em;">>,
            body = [
                #h3{
                    body = <<"Welcome to onepanel">>
                },
                #form{
                    id = <<"login_form">>,
                    method = "post",
                    action = ?PAGE_LOGIN_VALIDATION,
                    style = <<"width: 15em; margin: 0 auto; padding-top: 1em; float: center">>,
                    body = [
                        #textbox{
                            id = <<"username">>,
                            name = <<"username">>,
                            class = <<"span">>,
                            placeholder = <<"Username">>
                        },
                        #password{
                            id = <<"password">>,
                            name = <<"password">>,
                            class = <<"span">>,
                            placeholder = <<"Password">>
                        },
                        #button{
                            id = <<"login_button">>,
                            type = <<"submit">>,
                            class = <<"btn btn-primary btn-block">>,
                            style = <<"margin: 0 auto;">>,
                            body = <<"Log in">>
                        }
                    ]
                }
            ]
        },
        gui_utils:cookie_policy_popup_body(?PAGE_PRIVACY_POLICY)
    ],
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
    gui_jq:focus(<<"username">>),
    gui_jq:bind_enter_to_change_focus(<<"username">>, <<"password">>),
    gui_jq:bind_enter_to_submit_button(<<"password">>, <<"login_button">>),
    ok;

event(refresh) ->
    gui_jq:redirect(<<"/login">>);

event(terminate) ->
    ok.