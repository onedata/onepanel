%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains useful functions commonly used in
%% Onepanel GUI modules.
%% @end
%% ===================================================================

-module(onepanel_gui_utils).
-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include_lib("ctool/include/logging.hrl").

-export([body/1, body/2, body/3, top_menu/1, top_menu/2, logotype_footer/0]).
-export([get_error_message/1, get_installation_state/0, format_list/1, message/2]).
-export([change_page/2, maybe_redirect/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% body/1
%% ====================================================================
%% @doc Template function to render page body, without header and with
%% default page footer.
%% @end
-spec body(Content :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Content) ->
    body([], Content).


%% body/2
%% ====================================================================
%% @doc Template function to render page body, with default page footer.
%% @end
-spec body(Header :: term(), Content :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Header, Content) ->
    body(Header, Content, logotype_footer()).


%% body/3
%% ====================================================================
%% @doc Template function to render page body.
%% @end
-spec body(Header :: term(), Content :: term(), Footer :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Header, Content, Footer) ->
    [
        #header{id = <<"page-header">>, class = <<"page-row">>, body = Header},
        #main{id = <<"page-main">>, class = <<"page-row page-row-expanded">>, body = Content},
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
    #panel{style = <<"text-align: center; display: flex; justify-content: space-around; padding: 2em; margin-top: 3em;">>, body = [
        #image{class = <<"pull-left">>, image = <<"/images/innow-gosp-logo.png">>},
        #image{image = <<"/images/plgrid-plus-logo.png">>},
        #image{class = <<"pull-right">>, image = <<"/images/unia-logo.png">>}
    ]}.


%% top_menu/1
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID will be highlighted as active.
%% @end
-spec top_menu(ActiveTabID :: any()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID) ->
    top_menu(ActiveTabID, []).


%% top_menu/2
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID will be highlighted as active.
%% Submenu body (list of n2o elements) will be concatenated below the main menu.
%% @end
-spec top_menu(ActiveTabID :: atom(), SubMenuBody :: term()) -> Result when
    Result :: #panel{}.
%% ====================================================================
top_menu(ActiveTabID, SubMenuBody) ->
    % Define menu items with ids, so that proper tab can be made active via function parameter
    MenuCaptions =
        [
            {brand_tab, #li{body = #link{style = <<"padding: 18px;">>, url = ?PAGE_ROOT,
                body = [
                    #span{class = <<"fui-gear">>},
                    #b{style = <<"font-size: 20px;">>, body = <<"OnePanel">>}
                ]}
            }},
            {software_tab, #li{body = [
                #link{style = "padding: 18px;", body = "Software"},
                #list{style = "top: 37px; width: 120px;", body = [
                    #li{body = #link{url = ?PAGE_SOFTWARE_INSTALLATION, body = "Installation"}},
                    #li{body = #link{url = ?PAGE_SOFTWARE_UPDATE, body = "Update"}},
                    #li{body = #link{url = ?PAGE_SOFTWARE_SETTINGS, body = "Settings"}}
                ]}
            ]}},
            {spaces_tab, #li{body = [
                #link{style = "padding: 18px;", body = "Spaces"},
                #list{style = "top: 37px; width: 120px;", body = [
                    #li{body = #link{url = ?PAGE_SPACES_ACCOUNT, body = "Account"}},
                    #li{body = #link{url = ?PAGE_SPACES_SETTINGS, body = "Settings"}}
                ]}
            ]}}
        ],

    MenuIcons =
        [
            {manage_account_tab, #li{body = #link{style = <<"padding: 18px;">>, title = <<"Manage account">>,
                url = ?PAGE_MANAGE_ACCOUNT, body = [gui_ctx:get_user_id(), #span{class = <<"fui-user">>,
                    style = <<"margin-left: 10px;">>}]}}},
            {about_tab, #li{body = #link{style = <<"padding: 18px;">>, title = <<"About">>,
                url = ?PAGE_ABOUT, body = #span{class = <<"fui-info">>}}}},
            {logout_button, #li{body = #link{style = <<"padding: 18px;">>, title = <<"Log out">>,
                url = ?PAGE_LOGOUT, body = #span{class = <<"fui-power">>}}}}
        ],

    MenuCaptionsProcessed = lists:map(
        fun({TabID, ListItem}) ->
            case TabID of
                ActiveTabID -> ListItem#li{class = <<"active">>};
                _ -> ListItem
            end
        end, MenuCaptions),

    MenuIconsProcessed = lists:map(
        fun({TabID, ListItem}) ->
            case TabID of
                ActiveTabID -> ListItem#li{class = <<"active">>};
                _ -> ListItem
            end
        end, MenuIcons),

    #panel{class = <<"navbar navbar-fixed-top">>, body = [
        #panel{class = <<"navbar-inner">>, style = <<"border-bottom: 2px solid gray;">>, body = [
            #panel{class = <<"container">>, body = [
                #list{class = <<"nav pull-left">>, body = MenuCaptionsProcessed},
                #list{class = <<"nav pull-right">>, body = MenuIconsProcessed}
            ]}
        ]}
    ] ++ SubMenuBody}.


%% get_error_message/1
%% ====================================================================
%% @doc Returns error message for given error id, that will be displayed
%% on page.
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
-spec change_page(Env :: atom(), Page :: string()) -> Result when
    Result :: ok.
%% ====================================================================
change_page(Env, Page) ->
    gui_ctx:put(Env, Page),
    gui_jq:redirect(Page).


%% maybe_redirect/3
%% ====================================================================
%% @doc Redirects to appropriate page read from user session.
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
-spec format_list(List :: [string()]) -> Result when
    Result :: binary().
%% ====================================================================
format_list([]) ->
    <<"">>;
format_list(Hosts) ->
    list_to_binary(string:join(Hosts, ", ")).


%% message/2
%% ====================================================================
%% @doc Renders a message in given element.
-spec message(Id :: binary(), Message :: binary()) -> Result when
    Result :: ok.
%% ====================================================================
message(Id, Message) ->
    gui_jq:update(Id, Message),
    gui_jq:fade_in(Id, 300).

