%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains useful functions commonly used in
%% Onepanel GUI modules.
%% @end
%% ===================================================================

-module(onepanel_gui_utils).
-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include_lib("ctool/include/logging.hrl").

-export([body/1, body/2, body/3, top_menu/1, top_menu/2, account_settings_tab/1, logotype_footer/0]).
-export([get_error_message/1, get_installation_state/0, format_list/1, message/2, message/3]).
-export([change_page/2, maybe_redirect/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% body/1
%% ====================================================================
%% @doc Template function to render page body, without header and with
%% default page footer.
%% @end
-spec body(Main :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Main) ->
    body([], Main).


%% body/2
%% ====================================================================
%% @doc Template function to render page body, with default page footer.
%% @end
-spec body(Header :: term(), Main :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Header, Main) ->
    body(Header, Main, logotype_footer()).


%% body/3
%% ====================================================================
%% @doc Template function to render page body.
%% @end
-spec body(Header :: term(), Main :: term(), Footer :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Header, Main, Footer) ->
    [
        #header{id = <<"page-header">>, class = <<"page-row">>, body = Header},
        #main{id = <<"page-main">>, class = <<"page-row page-row-expanded">>, body = Main},
        #footer{id = <<"page-footer">>, class = <<"page-row">>, body = Footer}
    ].


%% logotype_footer/0
%% ====================================================================
%% @doc Convienience function to render logotype footer, coming after page content.
%% @end
-spec logotype_footer() -> Result when
    Result :: #panel{}.
%% ====================================================================
logotype_footer() ->
    #panel{style = <<"text-align: center; display: flex; justify-content: space-around; padding: 2em; margin-top: 3em;">>,
        body = [
            #image{class = <<"pull-left">>, image = <<"/images/innow-gosp-logo.png">>},
            #image{image = <<"/images/plgrid-plus-logo.png">>},
            #image{class = <<"pull-right">>, image = <<"/images/unia-logo.png">>}
        ]
    }.


%% top_menu/1
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
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
%% @end
-spec top_menu(ActiveTabID :: atom(), ActiveLinkID :: atom()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID, ActiveLinkID) ->
    top_menu(ActiveTabID, ActiveLinkID, []).


%% top_menu/2
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID and ActiveLinkID will be highlighted as active.
%% Submenu body (list of n2o elements) will be concatenated below the main menu.
%% @end
-spec top_menu(ActiveTabID :: atom(), ActiveLinkID :: atom(), SubMenuBody :: term()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID, ActiveLinkID, SubMenuBody) ->
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
                #b{style = <<"font-size: x-large;">>, body = <<"onepanel">>}
            ]}
        }},
        {software_tab, #li{body = [
            #link{style = "padding: 18px;", url = ?PAGE_INSTALLATION, body = <<"Software">>},
            #list{style = "top: 37px; width: 120px;", body = Process(ActiveLinkID, [
                {installation_link, #li{body = #link{url = ?PAGE_INSTALLATION, body = <<"Installation">>}}},
                {update_link, #li{body = #link{url = ?PAGE_UPDATE, body = <<"Update">>}}}
            ])}
        ]}},
        {spaces_tab, #li{body = [
            #link{style = "padding: 18px;", url = ?PAGE_SPACES_ACCOUNT, body = <<"Spaces">>},
            #list{style = "top: 37px; width: 120px;", body = Process(ActiveLinkID, [
                {spaces_account_link, #li{body = #link{url = ?PAGE_SPACES_ACCOUNT, body = <<"Account">>}}},
                {spaces_settings_link, #li{body = #link{url = ?PAGE_SPACES_SETTINGS, body = <<"Settings">>}}}
            ])}
        ]}}
    ]),

    MenuIcons = Process(ActiveTabID, [
        {account_settings_tab, #li{id = <<"account_settings_tab">>, body = account_settings_tab(gui_ctx:get_user_id())}},
        {about_tab, #li{body = #link{style = <<"padding: 18px;">>, title = <<"About">>,
            url = ?PAGE_ABOUT, body = #span{class = <<"fui-info">>}}}},
        {logout_button, #li{body = #link{style = <<"padding: 18px;">>, title = <<"Log out">>,
            url = ?PAGE_LOGOUT, body = #span{class = <<"fui-power">>}}}}
    ]),

    [
        #panel{
            id = <<"main_spinner">>,
            style = <<"position: absolute; top: 15px; left: 15px; z-index: 1234; width: 32px; display: none;">>,
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
        ] ++ SubMenuBody}
    ] ++ gui_utils:cookie_policy_popup_body(?PAGE_PRIVACY_POLICY).


%% account_settings_tab/1
%% ====================================================================
%% @doc Renders body of account settings tab.
%% @end
-spec account_settings_tab(Username :: binary()) -> Result when
    Result :: #link{}.
%% ====================================================================
account_settings_tab(Username) ->
    #link{
        style = <<"padding: 18px;">>,
        title = <<"Account settings">>,
        url = ?PAGE_ACCOUNT_SETTINGS,
        body = [
            Username,
            #span{
                class = <<"fui-user">>,
                style = <<"margin-left: 10px;">>
            }
        ]
    }.


%% get_error_message/1
%% ====================================================================
%% @doc Returns error message for given error id, that will be displayed
%% on page.
%% @end
-spec get_error_message(ErrorId :: atom()) -> Result when
    Result :: binary().
%% ====================================================================
get_error_message(?AUTHENTICATION_ERROR) ->
    <<"Invalid username or password.">>;
get_error_message(_) ->
    <<"Internal server error.">>.


%% get_installation_state/0
%% ====================================================================
%% @doc Returns current installation state read in first place from session
%% and in second place from database.
%% @end
-spec get_installation_state() -> Result when
    Result :: #?GLOBAL_CONFIG_RECORD{} | undefined.
%% ====================================================================
get_installation_state() ->
    case gui_ctx:get(?CONFIG_ID) of
        undefined ->
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, Record} -> {ok, Record};
                _ -> undefined
            end;
        Record -> {ok, Record}
    end.


%% change_page/2
%% ====================================================================
%% @doc Redirects to given page and saves it in user session.
%% @end
-spec change_page(Env :: atom(), Page :: string()) -> Result when
    Result :: ok.
%% ====================================================================
change_page(Env, Page) ->
    gui_ctx:put(Env, Page),
    gui_jq:redirect(Page).


%% maybe_redirect/3
%% ====================================================================
%% @doc Redirects to appropriate page read from user session.
%% @end
-spec maybe_redirect(Env :: atom(), Page :: string(), DefaultPage :: string()) -> Result when
    Result :: true | false.
%% ====================================================================
maybe_redirect(Env, CurrentPage, DefaultPage) ->
    case gui_ctx:get(Env) of
        CurrentPage ->
            false;
        undefined ->
            gui_jq:redirect(DefaultPage),
            true;
        Page ->
            gui_jq:redirect(Page),
            true
    end.


%% format_list/1
%% ====================================================================
%% @doc Returns list elements as a comma-delimited binary.
%% @end
-spec format_list(List :: [string()]) -> Result when
    Result :: binary().
%% ====================================================================
format_list([]) ->
    <<"">>;
format_list(Hosts) ->
    list_to_binary(string:join(Hosts, ", ")).


%% message/2
%% ====================================================================
%% @doc Renders a message in given element and allows to hide it with
%% default postback.
%% @end
-spec message(Id :: binary(), Message :: binary()) -> Result when
    Result :: ok.
%% ====================================================================
message(Id, Message) ->
    message(Id, Message, {close_message, Id}).


%% message/3
%% ====================================================================
%% @doc Renders a message in given element and allows to hide it with
%% custom postback.
%% @end
-spec message(Id :: binary(), Message :: binary(), Postback :: term()) -> Result when
    Result :: ok.
%% ====================================================================
message(Id, Message, Postback) ->
    Body = [
        Message,
        #link{
            title = <<"Close">>,
            style = <<"position: absolute; top: 1em; right: 1em;">>,
            class = <<"glyph-link">>,
            postback = Postback,
            body = #span{
                class = <<"fui-cross">>
            }
        }
    ],
    gui_jq:update(Id, Body),
    gui_jq:fade_in(Id, 300).
