%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page is a starting point for software update.
%% @end
%% ===================================================================

-module(page_update).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").
-include("onepanel_modules/updater/state.hrl").
-include("onepanel_modules/updater/stages.hrl").

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
                    case gui_ctx:get(?CURRENT_UPDATE_PAGE) of
                        undefined ->
                            State = updater:get_state(),
                            case updater_state:get_stage_and_job(State) of
                                {?STAGE_IDLE, _} ->
                                    onepanel_gui_utils:change_page(?CURRENT_UPDATE_PAGE, ?PAGE_VERSION_SELECTION);
                                _ ->
                                    onepanel_gui_utils:change_page(?CURRENT_UPDATE_PAGE, ?PAGE_UPDATE_SUMMARY)
                            end;
                        Page ->
                            gui_jq:redirect(Page)
                    end,
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
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
    <<"Update">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(software_tab, update_link),
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
                    body = <<"Please complete installation process before proceeding with update.">>
                },
                #link{
                    id = <<"next_button">>,
                    postback = to_main_page,
                    class = <<"btn btn-info">>,
                    style = <<"width: 80px; font-weight: bold;">>,
                    body = <<"OK">>
                }
            ]
        }
    },
    onepanel_gui_utils:body(Header, Content).


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

event(terminate) ->
    ok.