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
-include("onepanel_modules/db/common.hrl").
-include_lib("ctool/include/logging.hrl").

% Functions to generate page elements
-export([top_menu/1, top_menu/2, logotype_footer/1, bind_enter_to_change_focus/2, bind_key_to_click/2]).
-export([get_error_message/1, get_installation_state/0, change_page/2, maybe_redirect/1, format_list/1, message/2]).


%% ====================================================================
%% API functions
%% ====================================================================

%% logotype_footer/1
%% ====================================================================
%% @doc Convienience function to render logotype footer, coming after page content.
%% @end
-spec logotype_footer(MarginTop :: integer()) -> list().
%% ====================================================================
logotype_footer(MarginTop) ->
    Height = integer_to_binary(MarginTop + 82),
    Margin = integer_to_binary(MarginTop),
    [
        #panel{style = <<"position: relative; height: ", Height/binary, "px;">>, body = [
            #panel{style = <<"text-align: center; z-index: -1; margin-top: ", Margin/binary, "px;">>, body = [
                #image{style = <<"margin: 10px 100px;">>, image = <<"/images/innow-gosp-logo.png">>},
                #image{style = <<"margin: 10px 100px;">>, image = <<"/images/plgrid-plus-logo.png">>},
                #image{style = <<"margin: 10px 100px;">>, image = <<"/images/unia-logo.png">>}
            ]}
        ]}
    ].


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
top_menu(ActiveTabID, SubMenuBody) ->
    % Define menu items with ids, so that proper tab can be made active via function parameter
    % see old_menu_captions()
    MenuCaptions =
        [
            {installation_tab, #li{body = [
                #link{style = <<"padding: 18px;">>, url = <<"/installation">>, body = <<"Installation">>}
            ]}},
            {registration_tab, #li{body = [
                #link{style = <<"padding: 18px;">>, url = <<"/registration">>, body = <<"Registration">>}
            ]}}
        ],

    MenuIcons =
        [
            {manage_account_tab, #li{body = #link{style = <<"padding: 18px;">>, title = <<"Manage account">>,
                url = <<"/manage_account">>, body = [gui_ctx:get_user_id(), #span{class = <<"fui-user">>,
                    style = <<"margin-left: 10px;">>}]}}},
            {about_tab, #li{body = #link{style = <<"padding: 18px;">>, title = <<"About">>,
                url = <<"/about">>, body = #span{class = <<"fui-info">>}}}},
            {logout_button, #li{body = #link{style = <<"padding: 18px;">>, title = <<"Log out">>,
                url = <<"/logout">>, body = #span{class = <<"fui-power">>}}}}
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


%% bind_enter_to_change_focus/2
%% ====================================================================
%% @doc Makes any enter keypresses on InputID (whenever it is focused)
%% change focus to selected target. This way, it allows
%% easy switch submission with enter key.
%% @end
-spec bind_enter_to_change_focus(InputID :: binary(), TargetID :: binary()) -> string().
%% ====================================================================
bind_enter_to_change_focus(InputID, TargetID) ->
    Script = <<"$('#", InputID/binary, "').bind('keydown', function (e){",
    "if (e.which == 13) { e.preventDefault(); document.getElementById('", TargetID/binary, "').focus(); } });">>,
    gui_jq:wire(Script, false).


%% bind_key_to_click/2
%% ====================================================================
%% @doc Makes any enter keypresses on page to click on selected target.
%% @end
-spec bind_key_to_click(KeyCode :: binary(), TargetID :: binary()) -> string().
%% ====================================================================
bind_key_to_click(KeyCode, TargetID) ->
    Script = <<"$(document).bind('keydown', function (e){",
    "if (e.which == ", KeyCode/binary, ") { e.preventDefault(); document.getElementById('", TargetID/binary, "').click(); } });">>,
    gui_jq:wire(Script, false).


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

change_step(_, _) -> ok.

%% change_page/2
%% ====================================================================
%% @doc Redirects to given page and saves it in user session.
-spec change_page(Env :: atom(), Page :: string()) -> no_return().
%% ====================================================================
change_page(Env, Page) ->
    gui_ctx:put(Env, Page),
    gui_jq:redirect(Page).


%% maybe_redirect/1
%% ====================================================================
%% @doc Redirects to current installation step read from user session.
-spec maybe_redirect(Page :: string()) -> true | false.
%% ====================================================================
maybe_redirect(CurrentPage) ->
    case gui_ctx:get(?INSTALL_STEP) of
        CurrentPage ->
            false;
        undefined ->
            gui_jq:redirect("/hosts_selection"),
            true;
        InstallStep ->
            gui_jq:redirect(InstallStep),
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
-spec message(Id :: binary(), Message :: binary()) -> no_return().
%% ====================================================================
message(Id, Message) ->
    gui_jq:update(Id, Message),
    gui_jq:fade_in(Id, 300).

