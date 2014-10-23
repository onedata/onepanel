%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains useful functions commonly used in
%% Global Registry web pages.
%% @end
%% ===================================================================
-module(onepanel_gui_utils_adapter).

-include("gui_modules/common.hrl").

-export([top_menu/1, top_menu/2, top_menu/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% top_menu/1
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID will be highlighted as active.
%% @end
-spec top_menu(ActiveTabID :: any()) -> list().
%% ====================================================================
top_menu(ActiveTabID) ->
    top_menu(ActiveTabID, []).


%% top_menu/2
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID will be highlighted as active.
%% Submenu body (list of n2o elements) will be concatenated below the main menu.
%% @end
-spec top_menu(ActiveTabID :: any(), SubMenuBody :: any()) -> list().
%% ====================================================================
top_menu(ActiveTabID, Submenu) ->
    top_menu(ActiveTabID, Submenu, false).


top_menu(ActiveTabID, Submenu, Spinner) ->
    Process = fun(ActiveItem, List) ->
        lists:map(fun({ItemID, ListItem}) ->
            case ItemID of
                ActiveItem -> ListItem#li{class = <<"active">>};
                _ -> ListItem
            end
        end, List)
    end,

    MenuCaptions = Process(ActiveTabID, [
        {brand_tab, #li{body = #link{style = <<"padding: 18px;">>, url = ?PAGE_ROOT,
            body = [
                #span{style = <<"font-size: xx-large;">>, class = <<"fui-gear">>},
                #b{style = <<"font-size: x-large;">>, body = <<"onepanel">>}
            ]}
        }},
        {installation_tab, #li{body = [
            #link{style = <<"padding: 18px;">>, url = ?PAGE_INSTALLATION, body = <<"Installation">>}
        ]}}
    ]),

    MenuIcons = Process(ActiveTabID, [
        {account_settings_tab, #li{id = <<"account_settings_tab">>,
            body = onepanel_gui_utils:account_settings_tab(gui_ctx:get_user_id())}},
        {about_tab, #li{body = #link{style = <<"padding: 18px;">>, title = <<"About">>,
            url = ?PAGE_ABOUT, body = #span{class = <<"fui-info">>}}}},
        {logout_button, #li{body = #link{style = <<"padding: 18px;">>, title = <<"Log out">>,
            url = ?PAGE_LOGOUT, body = #span{class = <<"fui-power">>}}}}
    ]),

    SpinnerDisplay = case Spinner of
                         true -> <<"">>;
                         _ -> <<" display: none;">>
                     end,

    MessagesTop = case Submenu of
                      [] -> <<"55px">>;
                      _ -> <<"110px">>
                  end,

    [
        #panel{
            id = <<"main_spinner">>,
            style = <<"position: absolute; top: 15px; left: 15px; z-index: 1234; width: 32px;", SpinnerDisplay/binary>>,
            body = #image{
                image = <<"/images/spinner.gif">>
            }
        },
        #panel{class = <<"navbar navbar-fixed-top">>, body = [
            #panel{class = <<"navbar-inner">>, style = <<"border-bottom: 2px solid gray;">>, body = [
                #panel{class = <<"container">>, body = [
                    #list{class = <<"nav pull-left">>, body = MenuCaptions},
                    #list{class = <<"nav pull-right">>, body = MenuIcons}
                ]}
            ]}
        ] ++ Submenu},
        #panel{
            id = <<"ok_message">>,
            style = <<"position: fixed; width: 100%; top: ", MessagesTop/binary, "; display: none;">>,
            class = <<"dialog dialog-success">>
        },
        #panel{
            id = <<"error_message">>,
            style = <<"position: fixed; width: 100%; top: ", MessagesTop/binary, "; display: none;">>,
            class = <<"dialog dialog-danger">>
        }
    ] ++ gui_utils:cookie_policy_popup_body(?PAGE_PRIVACY_POLICY).