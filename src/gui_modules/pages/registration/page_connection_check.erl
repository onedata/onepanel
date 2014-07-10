%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page allows to check connection to Global Registry.
%% @end
%% ===================================================================

-module(page_connection_check).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK, ?PAGE_REGISTRATION) of
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
%% @doc Page title.
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Connection check">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    #panel{
        style = <<"position: relative;">>,
        body = [
            onepanel_gui_utils:top_menu(registration_tab),

            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #panel{
                style = <<"margin-top: 150px; text-align: center;">>,
                body = case installer_utils:get_workers() of
                           [] ->
                               #panel{
                                   style = <<"width: 50%; margin: 0 auto;">>,
                                   class = <<"alert alert-info">>,
                                   body = [
                                       #h3{
                                           body = <<"Software is not installed.">>
                                       },
                                       #p{
                                           body = <<"Please complete installation process in order to register in Global Registry as a provider.">>
                                       },
                                       #link{
                                           id = <<"next_button">>,
                                           postback = to_main_page,
                                           class = <<"btn btn-info">>,
                                           style = <<"width: 80px; font-weight: bold;">>,
                                           body = <<"OK">>
                                       }
                                   ]
                               };
                           _ ->
                               [
                                   #h6{
                                       style = <<"font-size: 18px;">>,
                                       body = <<"Step 1: Check your connection to Global Registry.">>
                                   },
                                   #panel{
                                       style = <<"margin-top: 30px; margin-bottom: 30px;">>,
                                       body = #button{
                                           id = <<"next_button">>,
                                           postback = next,
                                           class = <<"btn btn-inverse btn-small">>,
                                           style = <<"width: 80px; font-weight: bold;">>,
                                           body = <<"Next">>
                                       }
                                   }
                               ]
                       end

            }
        ] ++ onepanel_gui_utils:logotype_footer(120)
    }.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
    ok;

event(to_main_page) ->
    gui_jq:redirect(?PAGE_ROOT);

event(next) ->
    case gr_adapter:check_ip_address() of
        {ok, _} -> onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK);
        _ -> onepanel_gui_utils:message(<<"error_message">>, <<"Cannot connect to Global Registry.<br>
        Please check your network configuration or try again later.">>)
    end;

event(terminate) ->
    ok.