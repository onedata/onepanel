%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains useful functions commonly used in
%% provider web pages.
%% @end
%% ===================================================================
-module(onepanel_gui_utils_adapter).

-include("gui_modules/common.hrl").

-export([top_menu/1, top_menu/2, top_menu/3, top_menu/4]).

%% ====================================================================
%% API functions
%% ====================================================================

%% top_menu/1
%% ====================================================================
%% @doc Convenience function to render top menu in GUI pages.
%% Item with ActiveTabID will be highlighted as active.
%% @end
-spec top_menu(ActiveTabID :: atom()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID) ->
    top_menu(ActiveTabID, undefined).


%% top_menu/2
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID and ActiveLinkID will be highlighted as active.
%% Main page spinner will not be shown.
%% @end
-spec top_menu(ActiveTabID :: atom(), ActiveLinkID :: atom()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID, ActiveLinkID) ->
    top_menu(ActiveTabID, ActiveLinkID, []).


%% top_menu/3
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID and ActiveLinkID will be highlighted as active.
%% It allows to add submenu. Main page spinner will not be shown.
%% @end
-spec top_menu(ActiveTabID :: atom(), ActiveLinkID :: atom(), Submenu :: term()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID, ActiveLinkID, Submenu) ->
    top_menu(ActiveTabID, ActiveLinkID, Submenu, false).


%% top_menu/4
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID and ActiveLinkID will be highlighted as active.
%% It allows to show or hide main page spinner and add submenu.
%% @end
-spec top_menu(ActiveTabID :: atom(), ActiveLinkID :: atom(), Submenu :: term(), Spinner :: boolean()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID, ActiveLinkID, Submenu, Spinner) ->
    Process = fun(ActiveItem, List) ->
        lists:map(fun({ItemID, ListItem}) ->
            case ItemID of
                ActiveItem -> ListItem#li{class = <<"active">>};
                _ -> ListItem
            end
        end, List)
    end,


    % Define menu items with ids, so that proper tab can be made active via function parameter
    MenuCaptions = Process(ActiveTabID, [
        {brand_tab, #li{body = #link{style = <<"padding: 18px;">>, url = ?PAGE_ROOT,
            body = [
                #span{style = <<"font-size: xx-large;">>, class = <<"fui-gear">>},
                #b{style = <<"font-size: x-large;">>, body = provider_logic:get_provider_name()}
            ]}
        }},
        {software_tab, #li{body = [
            #link{style = "padding: 18px;", url = ?PAGE_INSTALLATION, body = <<"Software">>},
            #list{style = "top: 37px; width: 120px;", body = Process(ActiveLinkID, [
                {installation_link, #li{body = #link{url = ?PAGE_INSTALLATION, body = <<"Installation">>}}},
                {storage_link, #li{body = #link{url = ?PAGE_STORAGE, body = <<"Storage configuration">>}}}
            ])}
        ]}},
        {spaces_tab, #li{body = [
            #link{style = "padding: 18px;", url = ?PAGE_SPACES_ACCOUNT, body = <<"Spaces">>},
            #list{style = "top: 37px; width: 120px;", body = Process(ActiveLinkID, [
                {spaces_account_link, #li{body = #link{url = ?PAGE_SPACES_ACCOUNT, body = <<"Account">>}}},
                {spaces_dashboard_link, #li{body = #link{url = ?PAGE_SPACES_MANAGEMENT, body = <<"Management">>}}}
            ])}
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

    [
        #panel{
            id = <<"main_spinner">>,
            style = <<"position: absolute; top: 15px; left: 15px; z-index: 1234; width: 32px;", SpinnerDisplay/binary>>,
            body = #image{
                image = <<"/images/spinner.gif">>
            }
        },
        #panel{class = <<"navbar navbar-fixed-top">>, body = [
            #panel{class = <<"navbar-inner">>, style = <<"border-bottom: 1px solid gray;">>, body = [
                #panel{class = <<"container">>, body = [
                    #list{class = <<"nav pull-left">>, body = MenuCaptions},
                    #list{class = <<"nav pull-right">>, body = MenuIcons}
                ]}
            ]}
        ] ++ Submenu}
    ] ++ gui_utils:cookie_policy_popup_body(?PAGE_PRIVACY_POLICY).