%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% The page allows to manage provider account.
%% @end
%% ===================================================================

-module(page_spaces_account).
-export([main/0, event/1, api_event/3]).
-include("gui_modules/common.hrl").
-include("onepanel_modules/user_logic.hrl").
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
            case installer_utils:get_workers() of
                [] ->
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]};
                _ ->
                    case gui_ctx:get(?CURRENT_REGISTRATION_PAGE) of
                        undefined ->
                            case dao:get_records(?PROVIDER_TABLE) of
                                {ok, [#?PROVIDER_RECORD{id = Id, urls = Urls, redirectionPoint = RedirectionPoint} | _]} when Id =/= undefined ->
                                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()},
                                        {body, body(Id, Urls, RedirectionPoint)}, {custom, custom()}]};
                                _ ->
                                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()},
                                        {body, body(undefined, [], undefined)}, {custom, custom()}]}
                            end;
                        Page ->
                            gui_jq:redirect(Page),
                            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
                    end
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
    <<"Account settings">>.


%% custom/0
%% ====================================================================
%% @doc This will be placed instead of {{custom}} tag in template.
-spec custom() -> binary().
%% ====================================================================
custom() ->
    <<"<script src='/js/bootbox.min.js' type='text/javascript' charset='utf-8'></script>">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_account_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = #panel{
            style = <<"width: 50%; margin: 0 auto;">>,
            class = <<"alert alert-info">>,
            body = [
                #h3{
                    body = <<"Software is not installed">>
                },
                #p{
                    body = <<"Please complete installation process before registering in Global Registry as a provider.">>
                },
                #link{
                    id = <<"ok_button">>,
                    postback = to_main_page,
                    class = <<"btn btn-info">>,
                    style = <<"width: 80px; font-weight: bold;">>,
                    body = <<"OK">>
                }
            ]
        }
    },
    onepanel_gui_utils:body(Header, Main).


%% body/3
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body(ProviderId :: binary() | undefined, Urls :: [binary()], RedirectionPoint :: binary()| undefined) -> Result when
    Result :: #panel{}.
%% ====================================================================
body(ProviderId, Urls, RedirectionPoint) ->
    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_account_link),
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
                body = <<"Account settings">>
            },
            settings_table(ProviderId, Urls, RedirectionPoint)
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% settings_table/0
%% ====================================================================
%% @doc Renders the body of settings table.
-spec settings_table(ProviderId :: binary()| undefined, Urls :: [binary()], RedirectionPoint :: binary()| undefined) -> Result when
    Result :: #table{}.
%% ====================================================================
settings_table(ProviderId, Urls, RedirectionPoint) ->
    DescriptionStyle = <<"border-width: 0; text-align: right; padding: 1em 1em; width: 50%;">>,
    MainStyle = <<"border-width: 0;  text-align: left; padding: 1em 1em;">>,
    #table{
        style = <<"border-width: 0; width: 100%;">>, body = [
            #tr{
                cells = [
                    #td{
                        style = DescriptionStyle,
                        body = #label{
                            class = <<"label label-large label-inverse">>,
                            body = <<"Provider ID">>
                        }
                    },
                    #td{
                        id = <<"providerId">>,
                        style = MainStyle,
                        body = providerId(ProviderId)
                    }
                ]
            },
            #tr{
                cells = [
                    #td{
                        style = <<DescriptionStyle/binary, " vertical-align: top;">>,
                        body = #label{
                            class = <<"label label-large label-inverse">>,
                            body = <<"URLs">>
                        }
                    },
                    #td{
                        id = <<"urls">>,
                        style = MainStyle,
                        body = urls(Urls)
                    }
                ]
            },
            #tr{
                cells = [
                    #td{
                        style = DescriptionStyle,
                        body = #label{
                            class = <<"label label-large label-inverse">>,
                            body = <<"Redirection point">>
                        }
                    },
                    #td{
                        id = <<"redirectionPoint">>,
                        style = MainStyle,
                        body = redirectionPoint(RedirectionPoint)
                    }
                ]
            }
        ]
    }.


%% providerId/1
%% ====================================================================
%% @doc Renders provider ID.
-spec providerId(ProviderId :: binary() | undefined) -> Result when
    Result :: #span{}.
%% ====================================================================
providerId(undefined) ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>,
            #link{
                title = <<"Register">>,
                style = <<"margin-left: 1em;">>,
                class = <<"glyph-link">>,
                postback = register,
                body = #span{
                    class = <<"fui-plus-inverted">>
                }
            }
        ]
    };

providerId(ProviderId) ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            ProviderId,
            #link{
                title = <<"Unregister">>,
                style = <<"margin-left: 1em;">>,
                class = <<"glyph-link">>,
                postback = unregister,
                body = #span{
                    class = <<"fui-cross-inverted">>
                }
            }
        ]
    }.


%% urls/1
%% ====================================================================
%% @doc Renders urls.
-spec urls(Urls :: [binary()]) -> Result when
    Result :: #p{}.
%% ====================================================================
urls([]) ->
    #p{body = <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>};

urls(Urls) ->
    #list{
        style = <<"list-style-type: none; margin: 0 auto;">>,
        body = lists:map(fun(Url) ->
            #li{body = #p{body = Url}}
        end, Urls)
    }.


%% redirectionPoint/1
%% ====================================================================
%% @doc Renders redirection point.
-spec redirectionPoint(RedirectionPoint :: binary() | undefined) -> Result when
    Result :: #p{}.
%% ====================================================================
redirectionPoint(undefined) ->
    #p{body = <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>};

redirectionPoint(RedirectionPoint) ->
    #p{body = RedirectionPoint}.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:wire(#api{name = "unregister", tag = "unregister"}, false),
    gui_jq:bind_key_to_click(<<"13">>, <<"ok_button">>),
    ok;

event(to_main_page) ->
    gui_jq:redirect(?PAGE_ROOT);

event(register) ->
    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK);

event(unregister) ->
    Message = <<"Are you sure you want to unregister from Global Registry?<br>This operation cannot be undone.">>,
    Script = <<"unregister();">>,
    onepanel_gui_utils:confirm_popup(Message, Script);

event(terminate) ->
    ok.


%% api_event/3
%% ====================================================================
%% @doc Handles page events.
-spec api_event(Name :: string(), Args :: string(), Req :: string()) -> no_return().
%% ====================================================================
api_event("unregister", _, _) ->
    case gr_adapter:unregister() of
        ok ->
            gui_jq:update(<<"providerId">>, providerId(undefined)),
            gui_jq:update(<<"urls">>, urls([])),
            gui_jq:update(<<"redirectionPoint">>, redirectionPoint(undefined)),
            onepanel_gui_utils:message(<<"ok_message">>, <<"You have been successfully unregistered from Global Registry.">>);
        _ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot unregister from Global Registry.">>)
    end.