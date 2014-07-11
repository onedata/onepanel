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
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, <<"">>}, {custom, <<"">>}]};
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