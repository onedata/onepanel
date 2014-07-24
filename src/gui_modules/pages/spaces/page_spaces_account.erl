%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page is a starting point for registration in Global Registry.
%% @end
%% ===================================================================

-module(page_spaces_account).
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
main() ->
    case gui_ctx:user_logged_in() of
        true ->
            case installer_utils:get_workers() of
                [] ->
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]};
                _ ->
                    case gr_utils:get_provider_id() of
                        undefined ->
                            case gui_ctx:get(?CURRENT_REGISTRATION_PAGE) of
                                undefined ->
                                    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK);
                                Page ->
                                    gui_jq:redirect(Page)
                            end,
                            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, <<"">>}, {custom, <<"">>}]};
                        ProviderId ->
                            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body(ProviderId)}, {custom, <<"">>}]}
                    end
            end;
        false ->
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
    <<"Settings">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_account_link),
    Content = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = #panel{
            style = <<"width: 50%; margin: 0 auto;">>,
            class = <<"alert alert-info">>,
            body = [
                #h3{
                    body = <<"Software is not installed">>
                },
                #p{
                    body = <<"Please complete installation process before registering in Global Registry as a provider.">>
                },
                #link{
                    id = <<"ok_button">>,
                    postback = to_main_page,
                    class = <<"btn btn-info">>,
                    style = <<"width: 80px; font-weight: bold;">>,
                    body = <<"OK">>
                }
            ]
        }
    },
    onepanel_gui_utils:body(Header, Content).


%% body/1
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body(ProviderId :: binary()) -> Result when
    Result :: #panel{}.
%% ====================================================================
body(ProviderId) ->
    #panel{
        style = <<"position: relative;">>,
        body = [
            onepanel_gui_utils:top_menu(registration_tab),

            #panel{
                style = <<"margin-top: 150px; text-align: center;">>,
                body = [
                    #panel{
                        style = <<"width: 50%; margin: 0 auto;">>,
                        body = #panel{
                            class = <<"alert alert-info">>,
                            body = [
                                #h3{
                                    body = <<"You are registered in Global Registry.">>
                                },
                                #p{
                                    body = <<"Your provider ID: <b>", ProviderId/binary, "</b>">>
                                },
                                #link{
                                    id = <<"ok_button">>,
                                    postback = to_main_page,
                                    class = <<"btn btn-info">>,
                                    style = <<"width: 80px; font-weight: bold;">>,
                                    body = <<"OK">>
                                }
                            ]
                        }
                    }
                ]
            }
        ] ++ onepanel_gui_utils:logotype_footer(120)
    }.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"ok_button">>),
    ok;

event(to_main_page) ->
    gui_jq:redirect(?PAGE_ROOT);

event(terminate) ->
    ok.