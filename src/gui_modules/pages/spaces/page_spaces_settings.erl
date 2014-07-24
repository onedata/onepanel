%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page allows to manage provider spaces.
%% @end
%% ===================================================================

-module(page_spaces_settings).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").

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
            ProviderId = gr_utils:get_provider_id(),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body(ProviderId)}, {custom, <<"">>}]};
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
    <<"Spaces setting">>.


%% body/1
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body(ProviderId :: binary() | undefined) -> Result when
    Result :: #panel{}.
%% ====================================================================
body(undefined) ->
    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_settings_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = #panel{
            style = <<"width: 50%; margin: 0 auto;">>,
            class = <<"alert alert-info">>,
            body = [
                #h3{
                    body = <<"Your are not registered">>
                },
                #p{
                    body = <<"Please complete registration process in Global Registry.">>
                },
                #link{
                    id = <<"ok_button">>,
                    postback = to_account_page,
                    class = <<"btn btn-info">>,
                    style = <<"width: 80px; font-weight: bold;">>,
                    body = <<"OK">>
                }
            ]
        }
    },
    onepanel_gui_utils:body(Header, Main);

body(_) ->
    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_settings_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #panel{
                id = <<"ok_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-success">>
            },
            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #h6{
                style = <<"font-size: x-large; margin-bottom: 3em;">>,
                body = <<"Spaces settings">>
            },
            #panel{
                style = <<"margin-bottom: 3em;">>,
                body = [
                    #button{
                        postback = create_space,
                        class = <<"btn btn-inverse btn-small">>,
                        style = <<"width: 8em; font-weight: bold; margin-right: 1em;">>,
                        body = <<"Create Space">>
                    },
                    #button{
                        postback = support_space,
                        class = <<"btn btn-inverse btn-small">>,
                        style = <<"width: 8em; font-weight: bold; margin-left: 1em">>,
                        body = <<"Support Space">>
                    }
                ]
            },
            #table{
                class = <<"table table-bordered">>,
                style = <<"width: 50%; margin: 0 auto;">>,
                body = settings_table()
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% settings_table/0
%% ====================================================================
%% @doc Renders settings table body.
-spec settings_table() -> Result when
    Result :: [#tr{}].
%% ====================================================================
settings_table() ->
    Header = #tr{
        cells = [
            #th{
                body = <<"Spaces">>,
                colspan = 2
            }
        ]
    },
    try
        Rows = [],
        [Header | Rows]
    catch
        _:_ -> [Header]
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
    ok;

event(to_account_page) ->
    gui_jq:redirect(?PAGE_SPACES_ACCOUNT);

event(terminate) ->
    ok.