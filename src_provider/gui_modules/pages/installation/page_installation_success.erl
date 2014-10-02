%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page is displayed in case of successful installation.
%% @end
%% ===================================================================
-module(page_installation_success).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUCCESS, ?PAGE_INSTALLATION) of
                true ->
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                _ ->
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}
            end;
        false ->
            gui_jq:redirect_to_login(),
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
    <<"Successful installation">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, installation_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = #panel{
            style = <<"width: 50%; margin: 0 auto;">>,
            body = case provider_logic:get_provider_id() of
                       undefined ->
                           [
                               #panel{
                                   class = <<"alert alert-success">>,
                                   body = [
                                       #h3{
                                           body = <<"Successful installation">>
                                       },
                                       #p{
                                           body = <<"Would you like to register as a provider?<br>"
                                           "<i>Provider</i> is anyone who decides to support storage spaces for <i>onedata</i> users.">>
                                       },
                                       #link{
                                           postback = to_root_page,
                                           style = <<"width: 8em;">>,
                                           class = <<"btn btn-inverse">>,
                                           body = <<"Not now">>
                                       },
                                       #link{
                                           id = <<"next_button">>,
                                           postback = register,
                                           style = <<"width: 8em;">>,
                                           class = <<"btn btn-primary">>,
                                           body = <<"Register">>
                                       }
                                   ]
                               }
                           ];
                       _ ->
                           #panel{
                               class = <<"alert alert-success">>,
                               body = [
                                   #h3{
                                       body = <<"Successful installation">>
                                   },
                                   #link{
                                       id = <<"next_button">>,
                                       postback = to_root_page,
                                       style = <<"width: 8em;">>,
                                       class = <<"btn btn-primary">>,
                                       body = <<"OK">>
                                   }
                               ]
                           }
                   end
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
    gui_ctx:put(?CONFIG_ID, undefined),
    gui_ctx:put(?CURRENT_INSTALLATION_PAGE, undefined),
    gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
    ok;

event(to_root_page) ->
    gui_jq:redirect(?PAGE_ROOT);

event(register) ->
    gui_ctx:put(?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK),
    gui_jq:redirect(?PAGE_SPACES_ACCOUNT);

event(terminate) ->
    ok.