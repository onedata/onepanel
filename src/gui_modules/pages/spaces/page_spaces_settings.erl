%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page allows to manage provider spaces.
%% @end
%% ===================================================================

-module(page_spaces_settings).
-export([main/0, event/1, api_event/3]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include_lib("ctool/include/logging.hrl").

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
            ProviderId = gr_utils:get_provider_id(),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body(ProviderId)}, {custom, custom()}]};
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
    <<"Spaces setting">>.


%% custom/0
%% ====================================================================
%% @doc This will be placed instead of {{custom}} tag in template.
-spec custom() -> binary().
%% ====================================================================
custom() ->
    <<"<script src='/js/bootbox.min.js' type='text/javascript' charset='utf-8'></script>">>.


%% body/1
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body(ProviderId :: binary() | undefined) -> Result when
    Result :: #panel{}.
%% ====================================================================
body(undefined) ->
    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_settings_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = #panel{
            style = <<"width: 50%; margin: 0 auto;">>,
            class = <<"alert alert-info">>,
            body = [
                #h3{
                    body = <<"Your are not registered">>
                },
                #p{
                    body = <<"Please complete registration process in Global Registry.">>
                },
                #link{
                    id = <<"ok_button">>,
                    postback = to_account_page,
                    class = <<"btn btn-info">>,
                    style = <<"width: 80px; font-weight: bold;">>,
                    body = <<"OK">>
                }
            ]
        }
    },
    onepanel_gui_utils:body(Header, Main);

body(_) ->
    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_settings_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #panel{
                id = <<"ok_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-success">>
            },
            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #h6{
                style = <<"font-size: x-large; margin-bottom: 3em;">>,
                body = <<"Spaces settings">>
            },
            #panel{
                style = <<"margin-bottom: 3em;">>,
                body = [
                    #button{
                        postback = create_space,
                        class = <<"btn btn-inverse btn-small">>,
                        style = <<"font-weight: bold; margin-right: 1em;">>,
                        body = <<"Create Space">>
                    },
                    #button{
                        postback = support_space,
                        class = <<"btn btn-inverse btn-small">>,
                        style = <<"font-weight: bold; margin-left: 1em">>,
                        body = <<"Support Space">>
                    }
                ]
            },
            #table{
                class = <<"table table-bordered">>,
                style = <<"width: 50%; margin: 0 auto;">>,
                body = settings_table()
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% settings_table/0
%% ====================================================================
%% @doc Renders settings table body.
-spec settings_table() -> Result when
    Result :: [#tr{}].
%% ====================================================================
settings_table() ->
    Header = #tr{
        cells = [
            #th{
                body = <<"Spaces">>,
                colspan = 2
            }
        ]
    },
    try
        Rows = [],
        [Header | Rows]
    catch
        _:_ -> [Header]
    end.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:wire(#api{name = "createSpace", tag = "createSpace"}, false),
    gui_jq:wire(#api{name = "supportSpace", tag = "supportSpace"}, false),
    onepanel_gui_utils:bind_key_to_click(<<"13">>, <<"button.confirm">>),
    ok;

event(to_account_page) ->
    gui_jq:redirect(?PAGE_SPACES_ACCOUNT);

event(create_space) ->
    Title = <<"Create Space">>,
    Message = <<"<div style=\"margin: 0 auto; width: 80%;\">",
    "<p id=\"create_space_alert\" style=\"width: 100%; color: red; font-size: medium; text-align: center; display: none;\"></p>",
    "<input id=\"create_space_name\" type=\"text\" style=\"width: 100%;\" placeholder=\"Name\">",
    "<input id=\"create_space_token\" type=\"text\" style=\"width: 100%;\" placeholder=\"Token\">",
    "</div>">>,
    Script = <<"var alert = $(\"#create_space_alert\");",
    "var name = $.trim($(\"#create_space_name\").val());",
    "var token = $.trim($(\"#create_space_token\").val());",
    "if(name.length == 0) { alert.html(\"Please provide Space name.\"); alert.fadeIn(300); return false; }",
    "else if(token.length == 0) { alert.html(\"Please provide Space token.\"); alert.fadeIn(300); return false; }",
    "else { createSpace([name, token]); return true; }">>,
    onepanel_gui_utils:dialog_popup(Title, Message, Script),
    gui_jq:wire(<<"box.on('shown',function(){ $(\"#create_space_name\").focus(); });">>);

event(support_space) ->
    Title = <<"Support Space">>,
    Message = <<"<div style=\"margin: 0 auto; width: 80%;\">",
    "<p id=\"support_space_alert\" style=\"width: 100%; color: red; font-size: medium; text-align: center; display: none;\"></p>",
    "<input id=\"support_space_token\" type=\"text\" style=\"width: 100%;\" placeholder=\"Token\">",
    "</div>">>,
    Script = <<"var alert = $(\"#support_space_alert\");",
    "var token = $.trim($(\"#support_space_token\").val());",
    "if(token.length == 0) { alert.html(\"Please provide Space token.\"); alert.fadeIn(300); return false; }",
    "else { supportSpace([token]); return true; }">>,
    onepanel_gui_utils:dialog_popup(Title, Message, Script),
    gui_jq:wire(<<"box.on('shown',function(){ $(\"#support_space_token\").focus(); });">>);

event(terminate) ->
    ok.


%% api_event/3
%% ====================================================================
%% @doc Handles page events.
-spec api_event(Name :: string(), Args :: string(), Req :: string()) -> no_return().
%% ====================================================================
api_event("createSpace", Args, _) ->
    [Name, Token] = mochijson2:decode(Args),
    case gr_adapter:create_space(Name, Token) of
        {ok, SpaceId} ->
            onepanel_gui_utils:message(<<"ok_message">>, <<"Operation success.<br>Created Space ID: ", SpaceId/binary>>);
        _ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Operation failure.<br>Please try again later.">>)
    end;

api_event("supportSpace", Args, _) ->
    [Token] = mochijson2:decode(Args),
    case gr_adapter:support_space(Token) of
        {ok, SpaceId} ->
            onepanel_gui_utils:message(<<"ok_message">>, <<"Operation success.<br>Supporting Space ID: ", SpaceId/binary>>);
        _ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Operation failure.<br>Please try again later.">>)
    end.