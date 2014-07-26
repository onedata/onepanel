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
-include("onepanel_modules/space_logic.hrl").
-include_lib("ctool/include/logging.hrl").

-define(MESSAGE_STYLE, <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>).
-define(CONTENT_COLUMN_STYLE, <<"padding-right: 0">>).
-define(NAVIGATION_COLUMN_STYLE, <<"border-left-width: 0; width: 20px; padding-left: 0;">>).
-define(DESCRIPTION_STYLE, <<"border-width: 0; text-align: right; width: 10%; padding-left: 0; padding-right: 0;">>).
-define(MAIN_STYLE, <<"border-width: 0;  text-align: left; padding-left: 1em; width: 90%;">>).
-define(LABEL_STYLE, <<"margin: 0 auto;">>).
-define(PARAGRAPH_STYLE, <<"margin: 0 auto;">>).
-define(TABLE_STYLE, <<"border-width: 0; width: 100%; border-collapse: inherit;">>).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, state).
-record(?STATE, {}).

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
                style = ?MESSAGE_STYLE,
                class = <<"dialog dialog-success">>
            },
            #panel{
                id = <<"error_message">>,
                style = ?MESSAGE_STYLE,
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
                class = <<"table table-bordered table-striped">>,
                style = <<"width: 50%; margin: 0 auto; table-layout: fixed;">>,
                body = #tbody{
                    id = <<"spaces">>,
                    body = spaces_table_collapsed(<<"spaces">>)
                }
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% spaces_table_collapsed/1
%% ====================================================================
%% @doc Renders collapsed Spaces settings table.
-spec spaces_table_collapsed(TableId :: binary()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
spaces_table_collapsed(TableId) ->
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Spaces">>
            },
            #th{
                id = <<"space_all_spinner">>,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = expand_button(<<"Expand All">>, {spaces_table_expand, TableId, <<"space_all_spinner">>})
            }
        ]
    },
    try
        {ok, SpaceIds} = gr_adapter:get_provider_spaces(),
        Rows = lists:map(fun({SpaceId, Counter}) ->
            RowId = <<"space_", (integer_to_binary(Counter))/binary>>,
            #tr{
                id = RowId,
                cells = space_row_collapsed(SpaceId, RowId)
            }
        end, lists:zip(SpaceIds, tl(lists:seq(0, length(SpaceIds))))),
        [Header | Rows]
    catch
        _:_ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch supported Spaces.<br>Please try again later.">>),
            [Header]
    end.


%% spaces_table_expanded/1
%% ====================================================================
%% @doc Renders expanded Spaces settings table.
-spec spaces_table_expanded(TableId :: binary()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
spaces_table_expanded(TableId) ->
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Spaces">>
            },
            #th{
                id = <<"space_all_spinner">>,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = collapse_button(<<"Collapse All">>, {spaces_table_collapse, TableId, <<"space_all_spinner">>})
            }
        ]
    },
    try
        {ok, SpaceIds} = gr_adapter:get_provider_spaces(),
        Rows = lists:map(fun({SpaceId, Counter}) ->
            RowId = <<"space_", (integer_to_binary(Counter))/binary>>,
            #tr{
                id = RowId,
                cells = space_row_expanded(SpaceId, RowId)
            }
        end, lists:zip(SpaceIds, tl(lists:seq(0, length(SpaceIds))))),
        [Header | Rows]
    catch
        _:_ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch supported Spaces.<br>Please try again later.">>),
            [Header]
    end.


%% space_row_collapsed/2
%% ====================================================================
%% @doc Renders collapsed Space settings row.
-spec space_row_collapsed(SpaceId :: binary(), RowId :: binary()) -> Result when
    Result :: [#td{}].
%% ====================================================================
space_row_collapsed(SpaceId, RowId) ->
    SpinnerId = <<RowId/binary, "_spinner">>,
    [
        #td{
            style = ?CONTENT_COLUMN_STYLE,
            body = #table{
                style = ?TABLE_STYLE,
                body = [
                    #tr{
                        cells = [
                            #td{
                                style = ?DESCRIPTION_STYLE,
                                body = #label{
                                    style = ?LABEL_STYLE,
                                    class = <<"label label-large label-inverse">>,
                                    body = <<"Space ID">>
                                }
                            },
                            #td{
                                style = ?MAIN_STYLE,
                                body = #p{
                                    style = ?PARAGRAPH_STYLE,
                                    body = SpaceId
                                }
                            }
                        ]
                    }
                ]
            }
        },
        #td{
            id = SpinnerId,
            style = ?NAVIGATION_COLUMN_STYLE,
            body = expand_button({space_row_expand, SpaceId, RowId, SpinnerId})
        }
    ].


%% space_row_expanded/2
%% ====================================================================
%% @doc Renders expanded Space settings row.
-spec space_row_expanded(SpaceId :: binary(), RowId :: binary()) -> Result when
    Result :: [#td{}].
%% ====================================================================
space_row_expanded(SpaceId, RowId) ->
    try
        {ok, #?SPACE_DETAILS{name = Name}} = gr_adapter:get_space_details(SpaceId),
        SpinnerId = <<RowId/binary, "_spinner">>,
        ProvidersTableId = <<RowId/binary, "_providers">>,
        [
            #td{
                style = ?CONTENT_COLUMN_STYLE,
                body = [
                    #table{
                        style = ?TABLE_STYLE,
                        body = lists:map(fun({Description, Main}) ->
                            #tr{
                                cells = [
                                    #td{
                                        style = ?DESCRIPTION_STYLE,
                                        body = #label{
                                            style = ?LABEL_STYLE,
                                            class = <<"label label-large label-inverse">>,
                                            body = Description
                                        }
                                    },
                                    #td{
                                        style = ?MAIN_STYLE,
                                        body = #p{
                                            style = ?PARAGRAPH_STYLE,
                                            body = Main
                                        }
                                    }
                                ]
                            }
                        end, [{<<"Space ID">>, SpaceId}, {<<"Name">>, Name}])
                    },
                    #table{
                        class = <<"table table-bordered table-striped">>,
                        style = <<"width: 100%; margin: 0 auto; table-layout: fixed;">>,
                        body = #tbody{
                            id = ProvidersTableId,
                            body = providers_table_collapsed(SpaceId, ProvidersTableId)
                        }
                    }
                ]
            },
            #td{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = collapse_button({space_row_collapse, SpaceId, RowId, SpinnerId})
            }
        ]
    catch
        _:_ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch Space details.<br>Please try again later.">>),
            space_row_collapsed(SpaceId, RowId)
    end.


%% providers_table_collapsed/2
%% ====================================================================
%% @doc Renders collapsed providers table for given Space.
-spec providers_table_collapsed(SpaceId :: binary(), TableId :: binary()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
providers_table_collapsed(SpaceId, TableId) ->
    SpinnerId = <<TableId/binary, "_spinner">>,
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Providers">>
            },
            #th{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = expand_button(<<"Expand All">>, {providers_table_expand, SpaceId, TableId, SpinnerId})
            }
        ]
    },
    try
        {ok, ProviderIds} = gr_adapter:get_space_providers(SpaceId),
        Rows = lists:map(fun({ProviderId, Counter}) ->
            RowId = <<TableId/binary, "_", (integer_to_binary(Counter))/binary>>,
            #tr{
                id = RowId,
                cells = provider_row_collapsed(SpaceId, ProviderId, RowId)
            }
        end, lists:zip(ProviderIds, tl(lists:seq(0, length(ProviderIds))))),
        [Header | Rows]
    catch
        _:_ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch Space's providers.<br>Please try again later.">>),
            [Header]
    end.


%% providers_table_expanded/1
%% ====================================================================
%% @doc Renders expanded providers table for given Space.
-spec providers_table_expanded(SpaceId :: binary(), TableId :: binary()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
providers_table_expanded(SpaceId, TableId) ->
    SpinnerId = <<TableId/binary, "_spinner">>,
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Providers">>
            },
            #th{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = collapse_button(<<"Collapse All">>, {providers_table_collapse, SpaceId, TableId, SpinnerId})
            }
        ]
    },
    try
        {ok, ProviderIds} = gr_adapter:get_space_providers(SpaceId),
        Rows = lists:map(fun({ProviderId, Counter}) ->
            RowId = <<TableId/binary, "_", (integer_to_binary(Counter))/binary>>,
            #tr{
                id = RowId,
                cells = provider_row_expanded(SpaceId, ProviderId, RowId)
            }
        end, lists:zip(ProviderIds, tl(lists:seq(0, length(ProviderIds))))),
        [Header | Rows]
    catch
        _:_ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch Space's providers.<br>Please try again later.">>),
            [Header]
    end.


%% provider_row_collapsed/3
%% ====================================================================
%% @doc Renders collapsed provider row for given Space.
-spec provider_row_collapsed(SpaceId :: binary(), ProviderId :: binary(), RowId :: binary()) -> Result when
    Result :: [#td{}].
%% ====================================================================
provider_row_collapsed(SpaceId, ProviderId, RowId) ->
    SpinnerId = <<RowId/binary, "_spinner">>,
    [
        #td{
            style = ?CONTENT_COLUMN_STYLE,
            body = #table{
                style = ?TABLE_STYLE,
                body = [
                    #tr{
                        cells = [
                            #td{
                                style = ?DESCRIPTION_STYLE,
                                body = #label{
                                    style = ?LABEL_STYLE,
                                    class = <<"label label-large label-inverse">>,
                                    body = <<"Provider ID">>
                                }
                            },
                            #td{
                                style = ?MAIN_STYLE,
                                body = #p{
                                    style = ?PARAGRAPH_STYLE,
                                    body = ProviderId
                                }
                            }
                        ]
                    }
                ]
            }
        },
        #td{
            id = SpinnerId,
            style = ?NAVIGATION_COLUMN_STYLE,
            body = expand_button({provider_row_expand, SpaceId, ProviderId, RowId, SpinnerId})
        }
    ].


%% provider_row_expanded/3
%% ====================================================================
%% @doc Renders expanded provider row for given Space.
-spec provider_row_expanded(SpaceId :: binary(), ProviderId :: binary(), RowId :: binary()) -> Result when
    Result :: [#td{}].
%% ====================================================================
provider_row_expanded(SpaceId, ProviderId, RowId) ->
    try
        {ok, #?PROVIDER_DETAILS{urls = Urls, redirectionPoint = RedirectionPoint}} = gr_adapter:get_provider_details(SpaceId, ProviderId),
        SpinnerId = <<RowId/binary, "_spinner">>,
        [
            #td{
                style = ?CONTENT_COLUMN_STYLE,
                body = [
                    #table{
                        style = ?TABLE_STYLE,
                        body = [
                            #tr{
                                cells = [
                                    #td{
                                        style = ?DESCRIPTION_STYLE,
                                        body = #label{
                                            style = ?LABEL_STYLE,
                                            class = <<"label label-large label-inverse">>,
                                            body = <<"Provider ID">>
                                        }
                                    },
                                    #td{
                                        style = ?MAIN_STYLE,
                                        body = #p{
                                            style = ?PARAGRAPH_STYLE,
                                            body = ProviderId
                                        }
                                    }
                                ]
                            },
                            #tr{
                                cells = [
                                    #td{
                                        style = <<(?DESCRIPTION_STYLE)/binary, " vertical-align: top;">>,
                                        body = #label{
                                            style = ?LABEL_STYLE,
                                            class = <<"label label-large label-inverse">>,
                                            body = <<"URLs">>
                                        }
                                    },
                                    #td{
                                        style = ?MAIN_STYLE,
                                        body = #list{
                                            style = <<"list-style-type: none; margin: 0 auto;">>,
                                            body = lists:map(fun(Url) ->
                                                #li{body = #p{
                                                    style = ?PARAGRAPH_STYLE,
                                                    body = Url}
                                                }
                                            end, Urls)
                                        }
                                    }
                                ]
                            },
                            #tr{
                                cells = [
                                    #td{
                                        style = ?DESCRIPTION_STYLE,
                                        body = #label{
                                            style = ?LABEL_STYLE,
                                            class = <<"label label-large label-inverse">>,
                                            body = <<"Redirection point">>
                                        }
                                    },
                                    #td{
                                        style = ?MAIN_STYLE,
                                        body = #p{
                                            style = ?PARAGRAPH_STYLE,
                                            body = RedirectionPoint
                                        }
                                    }
                                ]
                            }
                        ]
                    }
                ]
            },
            #td{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = collapse_button({provider_row_collapse, SpaceId, ProviderId, RowId, SpinnerId})
            }
        ]
    catch
        _:_ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch Space's provider details.<br>Please try again later.">>),
            provider_row_collapsed(SpaceId, ProviderId, RowId)
    end.


expand_button(Postback) ->
    expand_button(<<"Expand">>, Postback).

expand_button(Title, Postback) ->
    #link{
        title = Title,
        class = <<"glyph-link">>,
        postback = Postback,
        body = #span{
            style = <<"font-size: large;  vertical-align: top;">>,
            class = <<"fui-triangle-down">>
        }
    }.

collapse_button(Postback) ->
    collapse_button(<<"Collapse">>, Postback).

collapse_button(Title, Postback) ->
    #link{
        title = Title,
        class = <<"glyph-link">>,
        postback = Postback,
        body = #span{
            style = <<"font-size: large; vertical-align: top;">>,
            class = <<"fui-triangle-up">>
        }
    }.

spinner() ->
    #image{image = <<"/images/spinner.gif">>, style = <<"width: 1.5em;">>}.


%% comet_loop/1
%% ====================================================================
%% @doc Handles spaces management actions.
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{} = State) ->
    NewState = try
        receive
            {spaces_table_collapse, TableId} ->
                gui_jq:update(TableId, spaces_table_collapsed(TableId)),
                gui_comet:flush(),
                State;

            {spaces_table_expand, TableId} ->
                gui_jq:update(TableId, spaces_table_expanded(TableId)),
                gui_comet:flush(),
                State;

            {space_row_collapse, SpaceId, RowId} ->
                gui_jq:update(RowId, space_row_collapsed(SpaceId, RowId)),
                gui_comet:flush(),
                State;

            {space_row_expand, SpaceId, RowId} ->
                gui_jq:update(RowId, space_row_expanded(SpaceId, RowId)),
                gui_comet:flush(),
                State;

            {providers_table_collapse, SpaceId, TableId} ->
                gui_jq:update(TableId, providers_table_collapsed(SpaceId, TableId)),
                gui_comet:flush(),
                State;

            {providers_table_expand, SpaceId, TableId} ->
                gui_jq:update(TableId, providers_table_expanded(SpaceId, TableId)),
                gui_comet:flush(),
                State;

            {provider_row_collapse, SpaceId, ProviderId, RowId} ->
                gui_jq:update(RowId, provider_row_collapsed(SpaceId, ProviderId, RowId)),
                gui_comet:flush(),
                State;

            {provider_row_expand, SpaceId, ProviderId, RowId} ->
                gui_jq:update(RowId, provider_row_expanded(SpaceId, ProviderId, RowId)),
                gui_comet:flush(),
                State
        end
               catch Type:Reason ->
                   ?error("Comet process exception: ~p:~p", [Type, Reason]),
                   onepanel_gui_utils:message(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Reason}
               end,
    comet_loop(NewState).


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#?STATE{}) end),
    put(?COMET_PID, Pid),
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

event({spaces_table_collapse, TableId, SpinnerId}) ->
    get(?COMET_PID) ! {spaces_table_collapse, TableId},
    gui_jq:update(SpinnerId, spinner());

event({spaces_table_expand, TableId, SpinnerId}) ->
    get(?COMET_PID) ! {spaces_table_expand, TableId},
    gui_jq:update(SpinnerId, spinner());

event({space_row_collapse, SpaceId, RowId, SpinnerId}) ->
    get(?COMET_PID) ! {space_row_collapse, SpaceId, RowId},
    gui_jq:update(SpinnerId, spinner());

event({space_row_expand, SpaceId, RowId, SpinnerId}) ->
    get(?COMET_PID) ! {space_row_expand, SpaceId, RowId},
    gui_jq:update(SpinnerId, spinner());

event({providers_table_collapse, SpaceId, TableId, SpinnerId}) ->
    get(?COMET_PID) ! {providers_table_collapse, SpaceId, TableId},
    gui_jq:update(SpinnerId, spinner());

event({providers_table_expand, SpaceId, TableId, SpinnerId}) ->
    get(?COMET_PID) ! {providers_table_expand, SpaceId, TableId},
    gui_jq:update(SpinnerId, spinner());

event({provider_row_collapse, SpaceId, ProviderId, RowId, SpinnerId}) ->
    get(?COMET_PID) ! {provider_row_collapse, SpaceId, ProviderId, RowId},
    gui_jq:update(SpinnerId, spinner());

event({provider_row_expand, SpaceId, ProviderId, RowId, SpinnerId}) ->
    get(?COMET_PID) ! {provider_row_expand, SpaceId, ProviderId, RowId},
    gui_jq:update(SpinnerId, spinner());

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