%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains useful functions commonly used in
%% onepanel GUI modules.
%% @end
%% ===================================================================
-module(onepanel_gui_utils).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include_lib("ctool/include/logging.hrl").

-export([body/1, body/2, body/3, body/4, account_settings_tab/1, logotype_footer/0, nav_buttons/1, nav_buttons/2]).
-export([collapse_button/1, collapse_button/2, expand_button/1, expand_button/2, breadcrumbs/1]).
-export([get_session_config/0, format_list/1, message/2, message/4]).
-export([change_page/2, maybe_redirect/3, event/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% body/1
%% ====================================================================
%% @equiv body([], Main).
%% @end
-spec body(Main :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Main) ->
    body([], Main).


%% body/2
%% ====================================================================
%% @equiv body(Header, Main, logotype_footer()).
%% @end
-spec body(Header :: term(), Main :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Header, Main) ->
    body(Header, Main, logotype_footer()).


%% body/3
%% ====================================================================
%% @equiv body(62, Header, Main, logotype_footer()).
%% @end
-spec body(Header :: term(), Main :: term(), Footer :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Header, Main, Footer) ->
    body(61, Header, Main, Footer).


%% body/4
%% ====================================================================
%% @doc Template function to render page body. 'Top' parameter defines
%% height where page content should start.
%% @end
-spec body(Top :: integer(), Header :: term(), Main :: term(), Footer :: term()) -> Result when
    Result :: list().
%% ====================================================================
body(Top, Header, Main, Footer) ->
    [
        #header{id = <<"page-header">>, class = <<"page-row">>, body = Header},
        #main{id = <<"page-main">>, class = <<"page-row page-row-expanded">>,
            body = #panel{
                style = <<"margin-top: ", (integer_to_binary(Top))/binary, "px; padding-top: 1px;">>,
                body = [
                    #panel{
                        id = <<"message">>,
                        style = <<"width: 100%; padding: 0.5em 0; margin: 0 auto; border: 0; display: none;">>,
                        class = <<"dialog">>
                    },
                    Main
                ]
            }
        },
        #footer{id = <<"page-footer">>, class = <<"page-row">>, body = Footer}
    ].


%% breadcrumbs/1
%% ====================================================================
%% @doc Renders breadcrumbs in submenu.
-spec breadcrumbs(Elements :: [{PageName :: binary(), PageAddress :: binary()}]) -> Result when
    Result :: [#panel{}].
%% ====================================================================
breadcrumbs(Elements) ->
    [{LastElementName, LastElementCtx, LastElementPage} | ReversedTail] = lists:reverse(Elements),
    [
        #panel{
            class = <<"navbar-inner">>,
            style = <<"border-bottom: 1px solid gray;">>,
            body = #panel{
                class = <<"container">>,
                body = #list{
                    style = <<"margin: 0 auto; background-color: inherit;">>,
                    class = <<"breadcrumb">>,
                    body = lists:map(fun({Name, Ctx, Page}) ->
                        #li{
                            body = #link{
                                class = <<"glyph-link">>,
                                delegate = ?MODULE,
                                postback = {change_page, Ctx, Page},
                                body = Name
                            }
                        }
                    end, lists:reverse(ReversedTail)) ++ [
                        #li{
                            class = <<"active">>,
                            body = #link{
                                style = <<"color: #1abc9c">>,
                                class = <<"glyph-link">>,
                                delegate = ?MODULE,
                                postback = {change_page, LastElementCtx, LastElementPage},
                                body = LastElementName
                            }
                        }
                    ]
                }
            }
        }
    ].


%% get_session_config/0
%% ====================================================================
%% @doc Returns current installation state read in first place from session
%% and in second place from database.
%% @end
-spec get_session_config() -> Result when
    Result :: #?GLOBAL_CONFIG_RECORD{} | {error, undefined}.
%% ====================================================================
get_session_config() ->
    case gui_ctx:get(?CONFIG_ID) of
        undefined ->
            case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
                {ok, Record} -> {ok, Record};
                _ -> {error, undefined}
            end;
        Record -> {ok, Record}
    end.


%% logotype_footer/0
%% ====================================================================
%% @doc Convienience function to render logotype footer, coming after page content.
%% @end
-spec logotype_footer() -> Result when
    Result :: #panel{}.
%% ====================================================================
logotype_footer() ->
    #panel{style = <<"text-align: center; margin-top: 2em; margin-bottom: 2em;">>,
        body = [
            #image{class = <<"pull-left">>, image = <<"/images/innow-gosp-logo.png">>},
            #image{image = <<"/images/plgrid-plus-logo.png">>},
            #image{class = <<"pull-right">>, image = <<"/images/unia-logo.png">>}
        ]
    }.


%% nav_buttons/1
%% ====================================================================
%% @doc Convienience function to render navigation buttons.
%% @end
-spec nav_buttons(Buttons :: [{Id, Event, Disabled, Body}]) -> Result when
    Id :: binary(),
    Event :: {postback, term()} | {actions, term()} | undefined,
    Disabled :: boolean(),
    Body :: binary(),
    Result :: #panel{}.
%% ====================================================================
nav_buttons(Buttons) ->
    nav_buttons(Buttons, <<"50%">>).


%% nav_buttons/2
%% ====================================================================
%% @doc Convienience function to render navigation buttons with custom
%% width.
%% @end
-spec nav_buttons(Buttons :: [{Id, Event, Disabled, Body}], Width :: binary()) -> Result when
    Id :: binary(),
    Event :: {postback, term()} | {actions, term()} | undefined,
    Disabled :: boolean(),
    Body :: binary(),
    Result :: #panel{}.
%% ====================================================================
nav_buttons(Buttons, Width) ->
    ButtonClass = <<"btn btn-inverse btn-small">>,
    ButtonStyle = <<"min-width: 8em; margin-left: 1em; margin-right: 1em; font-weight: bold;">>,
    #panel{
        style = <<"width: ", Width/binary, "; margin: 0 auto; margin-top: 3em;">>,
        body = lists:map(fun
            ({Id, {postback, Postback}, Disabled, Body}) ->
                #button{
                    id = Id,
                    postback = Postback,
                    class = ButtonClass,
                    style = ButtonStyle,
                    disabled = Disabled,
                    body = Body
                };
            ({Id, {actions, Actions}, Disabled, Body}) ->
                #button{
                    id = Id,
                    actions = Actions,
                    class = ButtonClass,
                    style = ButtonStyle,
                    disabled = Disabled,
                    body = Body
                };
            ({Id, _, Disabled, Body}) ->
                #button{
                    id = Id,
                    class = ButtonClass,
                    style = ButtonStyle,
                    disabled = Disabled,
                    body = Body
                };
            (_) ->
                #button{}
        end, Buttons)
    }.


%% collapse_button/1
%% ====================================================================
%% @doc Renders collapse button.
%% @end
-spec collapse_button(Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
collapse_button(Postback) ->
    collapse_button(<<"Collapse">>, Postback).


%% collapse_button/2
%% ====================================================================
%% @doc Renders collapse button.
%% @end
-spec collapse_button(Title :: binary(), Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
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


%% expand_button/1
%% ====================================================================
%% @doc Renders expand button.
%% @end
-spec expand_button(Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
expand_button(Postback) ->
    expand_button(<<"Expand">>, Postback).


%% expand_button/2
%% ====================================================================
%% @doc Renders expand button.
%% @end
-spec expand_button(Title :: binary(), Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
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


%% account_settings_tab/1
%% ====================================================================
%% @doc Renders body of account settings tab.
%% @end
-spec account_settings_tab(Username :: binary()) -> Result when
    Result :: #link{}.
%% ====================================================================
account_settings_tab(Username) ->
    #link{
        style = <<"padding: 18px;">>,
        title = <<"Account settings">>,
        url = ?PAGE_ACCOUNT_SETTINGS,
        body = [
            Username,
            #span{
                class = <<"fui-user">>,
                style = <<"margin-left: 10px;">>
            }
        ]
    }.


%% change_page/2
%% ====================================================================
%% @doc Redirects to given page and saves it in user session.
%% @end
-spec change_page(Ctx :: atom(), Page :: string()) -> Result when
    Result :: ok.
%% ====================================================================
change_page(Ctx, Page) ->
    gui_ctx:put(Ctx, Page),
    gui_jq:redirect(Page).


%% maybe_redirect/3
%% ====================================================================
%% @doc Redirects to appropriate page read from user session.
%% @end
-spec maybe_redirect(Env :: atom(), Page :: string(), DefaultPage :: string()) -> Result when
    Result :: true | false.
%% ====================================================================
maybe_redirect(Env, CurrentPage, DefaultPage) ->
    case gui_ctx:get(Env) of
        CurrentPage ->
            false;
        undefined ->
            gui_jq:redirect(DefaultPage),
            true;
        Page ->
            gui_jq:redirect(Page),
            true
    end.


%% event/1
%% ====================================================================
%% @doc Handles page changing events generated by breadcrumb links.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event({change_page, Ctx, Page}) ->
    change_page(Ctx, Page).


%% format_list/1
%% ====================================================================
%% @doc Returns list elements as a comma-delimited binary.
%% @end
-spec format_list(List :: [string()]) -> Result when
    Result :: binary().
%% ====================================================================
format_list([]) ->
    <<"">>;
format_list(Hosts) ->
    list_to_binary(string:join(Hosts, ", ")).


%% message/2
%% ====================================================================
%% @doc Renders a message below given element and allows to hide it with
%% default postback.
%% @end
-spec message(Type :: success | error, Message :: binary()) -> Result when
    Result :: ok.
%% ====================================================================
message(Type, Message) ->
    message(<<"message">>, Type, Message, {close_message, <<"message">>}).


%% message/4
%% ====================================================================
%% @doc Renders a message below given element and allows to hide it with
%% custom postback.
%% @end
-spec message(Id :: binary(), Type :: success | error, Message :: binary(), Postback :: term()) -> Result when
    Result :: ok.
%% ====================================================================
message(Id, Type, Message, Postback) ->
    Body = [
        Message,
        #link{
            id = <<"close_message_button">>,
            title = <<"Close">>,
            style = <<"position: absolute; top: 0.5em; right: 0.5em;">>,
            class = <<"glyph-link">>,
            postback = Postback,
            body = #span{
                class = <<"fui-cross">>
            }
        }
    ],
    case Type of
        success ->
            gui_jq:add_class(Id, <<"dialog-success">>),
            gui_jq:remove_class(Id, <<"dialog-danger">>);
        _ ->
            gui_jq:add_class(Id, <<"dialog-danger">>),
            gui_jq:remove_class(Id, <<"dialog-success">>)
    end,
    gui_jq:update(Id, Body),
    gui_jq:fade_in(Id, 300).
