%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page is displayed in case of successful update.
%% @end
%% ===================================================================
-module(page_update_success).

-include("gui_modules/common.hrl").
-include("onepanel_modules/updater/common.hrl").

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_UPDATE_PAGE, ?PAGE_UPDATE_SUCCESS, ?PAGE_UPDATE) of
                true ->
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                _ ->
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}
            end;
        false ->
            gui_jq:redirect_to_login(true),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


%% title/0
%% ====================================================================
%% @doc This will be placed instead of {{title}} tag in template.
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Successful update">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(software_tab, update_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = #panel{
            style = <<"width: 50%; margin: 0 auto;">>,
            body = #panel{
                class = <<"alert alert-success">>,
                body = [
                    #h3{
                        body = <<"Successful update">>
                    },
                    case onepanel_utils:get_software_version() of
                        undefined -> #p{};
                        Version -> #p{
                            body = <<"Current software version: <b>", Version/binary, "</b>">>
                        }
                    end,
                    #link{
                        id = <<"ok_button">>,
                        postback = to_root_page,
                        style = <<"width: 80px;">>,
                        class = <<"btn btn-primary">>,
                        body = <<"OK">>
                    }
                ]
            }
        }
    },
    onepanel_gui_utils:body(Header, Main).


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
    gui_jq:bind_key_to_click(<<"13">>, <<"ok_button">>),
    ok;

event(to_root_page) ->
    gui_ctx:put(?CHOSEN_VERSION, undefined),
    gui_ctx:put(?CURRENT_UPDATE_PAGE, undefined),
    gui_jq:redirect(?PAGE_ROOT);

event(terminate) ->
    ok.