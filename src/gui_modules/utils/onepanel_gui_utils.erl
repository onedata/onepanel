%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains useful functions commonly used in
%% onepanel GUI modules.
%% @end
%% ===================================================================

-module(onepanel_gui_utils).
-include("gui_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

% Functions to generate page elements
-export([top_menu/1, top_menu/2, logotype_footer/1, bind_enter_to_change_focus/2, get_error_message/1, change_step/2]).


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


%% bind_enter_to_submit_button/2
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


%% change_step/2
%% ====================================================================
%% @doc Hides current installation step and displays next ('Diff' equals 1)
%% or previous ('Diff' equals -1) installaton step.
-spec change_step(CurrentStep :: integer(), Diff :: -1 | 1) -> no_return().
%% ====================================================================
change_step(CurrentStep, Diff) ->
    HideId = <<"step_", (integer_to_binary(CurrentStep))/binary>>,
    ShowId = <<"step_", (integer_to_binary(CurrentStep + Diff))/binary>>,
    gui_jq:hide(<<"error_message">>),
    gui_jq:slide_up(HideId, 800),
    gui_jq:delay(ShowId, integer_to_binary(500)),
    gui_jq:slide_down(ShowId, 800).


