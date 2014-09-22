%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page is a starting point for software update.
%% @end
%% ===================================================================
-module(page_update).

-include("gui_modules/common.hrl").
-include("onepanel_modules/updater/state.hrl").
-include("onepanel_modules/updater/stages.hrl").

-export([main/0, event/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% main/0
%% ====================================================================
%% @doc Template points to the template file, which will be filled with content.
%% @end
-spec main() -> Result when
    Result :: #dtl{}.
%% ====================================================================
main() ->
    case gui_ctx:user_logged_in() of
        true ->
            case installer_utils:get_workers() of
                [] ->
                    page_error:redirect_with_error(?SOFTWARE_NOT_INSTALLED_ERROR),
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
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
            gui_jq:redirect_to_login(),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    ok;

event(terminate) ->
    ok.