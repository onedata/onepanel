%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to view space details.
%% @end
%% ===================================================================
-module(page_space_details).

-include("gui_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/global_registry/gr_spaces.hrl").

-export([main/0, event/1, comet_loop/1]).

%% Common page CCS styles
-define(CONTENT_COLUMN_STYLE, <<"padding-right: 0">>).
-define(NAVIGATION_COLUMN_STYLE, <<"border-left-width: 0; width: 20px; padding-left: 0;">>).
-define(PARAGRAPH_STYLE, <<"margin: 0 auto;">>).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {}).

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
            case provider_logic:get_provider_id() of
                undefined ->
                    page_error:redirect_with_error(?UNREGISTERED_PROVIDER_ERROR),
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                _ ->
                    case gui_ctx:url_param(<<"id">>) of
                        undefined ->
                            page_error:redirect_with_error(?SPACE_NOT_FOUND_ERROR),
                            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                        Id ->
                            SpaceId = gui_str:to_binary(Id),
                            case gr_providers:get_space_details(provider, SpaceId) of
                                {ok, SpaceDetails} ->
                                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body(SpaceDetails)}, {custom, <<"">>}]};
                                Other ->
                                    ?error("Cannot get details of Space with ID ~p: ~p", [SpaceId, Other]),
                                    page_error:redirect_with_error(?SPACE_PERMISSION_DENIED_ERROR),
                                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
                            end
                    end
            end;
        false ->
            gui_jq:redirect_to_login(true),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


%% title/0
%% ====================================================================
%% @doc Page title.
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Space details">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body(SpaceDetails :: #space_details{}) -> Result when
    Result :: #panel{}.
%% ====================================================================
body(SpaceDetails) ->
    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_settings_link, [], true),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 3em;">>,
                body = <<"Space details">>
            },
            space_details_table(SpaceDetails)
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% space_details_table/1
%% ====================================================================
%% @doc Renders the body of space details table
%% @end
-spec space_details_table(SpaceDetails :: #space_details{}) -> Result when
    Result :: #table{}.
%% ====================================================================
space_details_table(#space_details{id = SpaceId, name = SpaceName}) ->
    DescriptionStyle = <<"border-width: 0; vertical-align: top; text-align: right; padding: 1em 1em;">>,
    MainStyle = <<"border-width: 0;  text-align: left; padding: 1em 1em;">>,
    #table{
        style = <<"border-width: 0; width: 50%; margin: 0 auto;">>,
        body = lists:map(fun({DetailName, DetailValue}) ->
            #tr{
                cells = [
                    #td{
                        style = DescriptionStyle,
                        body = #label{
                            class = <<"label label-large label-inverse">>,
                            style = <<"cursor: auto;">>,
                            body = DetailName
                        }
                    },
                    #td{
                        style = MainStyle,
                        body = #p{
                            style = <<"margin: 0;">>,
                            body = DetailValue
                        }
                    }
                ]
            }
        end, [
            {<<"Name">>, SpaceName},
            {<<"Space ID">>, SpaceId}
        ])
    }.


%% ====================================================================
%% Events handling
%% ====================================================================

%% comet_loop/1
%% ====================================================================
%% @doc Handles spaces management actions.
%% @end
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{} = State) ->
    NewState = try
        receive
            ok -> ok
        end
               catch Type:Message ->
                   ?error("Comet process exception: ~p:~p", [Type, Message]),
                   onepanel_gui_utils:message(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Message}
               end,
    ?MODULE:comet_loop(NewState).


%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    try
        ok
    catch
        _:Reason ->
            ?error("Cannot fetch supported Spaces: ~p", [Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch supported Spaces.<br>Please try again later.">>)
    end;

event({message, Message}) ->
    get(?COMET_PID) ! Message,
    gui_jq:show(<<"main_spinner">>);

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.