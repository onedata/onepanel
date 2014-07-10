%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page is displayed in case of successful installation.
%% @end
%% ===================================================================

-module(page_installation_success).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUCCESS, ?PAGE_INSTALLATION) of
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
    <<"Successful installtion">>.


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
            onepanel_gui_utils:top_menu(installation_tab),

            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #panel{
                style = <<"margin-top: 150px; text-align: center;">>,
                body = [
                    #panel{
                        style = <<"width: 50%; margin: 0 auto;">>,
                        body = case gr_utils:get_provider_id() of
                                   undefined ->
                                       [
                                           #panel{
                                               class = <<"alert alert-success">>,
                                               body = [
                                                   #h3{
                                                       body = <<"Successful installation">>
                                                   },
                                                   #p{
                                                       body = <<"Would you like to register as a provider?">>
                                                   },
                                                   #link{
                                                       postback = to_main_page,
                                                       style = <<"width: 80px;">>,
                                                       class = <<"btn btn-info">>,
                                                       body = <<"Not now">>
                                                   },
                                                   #link{
                                                       id = <<"next_button">>,
                                                       postback = register,
                                                       style = <<"width: 80px;">>,
                                                       class = <<"btn btn-primary">>,
                                                       body = <<"Register">>
                                                   }
                                               ]
                                           }
                                       ];
                                   _ ->
                                       [
                                           #panel{
                                               class = <<"alert alert-success">>,
                                               body = [
                                                   #h3{
                                                       body = <<"Successful installation">>
                                                   },
                                                   #link{
                                                       id = <<"next_button">>,
                                                       postback = to_main_page,
                                                       style = <<"width: 80px;">>,
                                                       class = <<"btn btn-primary">>,
                                                       body = <<"OK">>
                                                   }
                                               ]
                                           }
                                       ]
                               end
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
    gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
    ok;

event(to_main_page) ->
    gui_ctx:put(?CURRENT_INSTALLATION_PAGE, undefined),
    gui_jq:redirect(?PAGE_ROOT);

event(register) ->
    gui_ctx:put(?CURRENT_INSTALLATION_PAGE, undefined),
    gui_jq:redirect(?PAGE_REGISTRATION);

event(terminate) ->
    ok.