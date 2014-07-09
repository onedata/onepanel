%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page is a starting point for VeilCluster nodes installation.
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
            case onepanel_gui_utils:maybe_redirect(?REGISTER_PAGE, "/connection_check", "/connection_check") of
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
                body = [
                    #h6{
                        style = <<"font-size: 18px;">>,
                        body = <<"Step 1: Check your connection to Global Registry.">>
                    }
                    #panel{
                        style = <<"width: 50%; margin: 0 auto; margin-top: 30px; margin-bottom: 30px;">>,
                        body = [
                            #button{
                                id = <<"next_button">>,
                                postback = check_connection,
                                class = <<"btn btn-inverse btn-small">>,
                                style = <<"float: right; width: 80px; font-weight: bold;">>,
                                body = <<"Next">>
                            }
                        ]
                    }
                ]
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
    ok;

event(check_connection) ->
    case gr_adapter:check_ip_address() of
        {ok, _} -> onepanel_gui_utils:change_page(?REGISTER_PAGE, "/ports_check");
        _ -> onepanel_gui_utils:message(<<"error_message">>, <<"Cannot connect to Global Registry.<br>
        Please check your network configuration or try again later.">>)
    end;

event(terminate) ->
    ok.