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
-include_lib("ctool/include/global_registry/gr_users.hrl").
-include_lib("ctool/include/global_registry/gr_spaces.hrl").
-include_lib("ctool/include/global_registry/gr_providers.hrl").

-export([main/0, event/1, comet_loop/1]).

%% Common page CCS styles
-define(CONTENT_COLUMN_STYLE, <<"padding-right: 0">>).
-define(NAVIGATION_COLUMN_STYLE, <<"border-left-width: 0; width: 20px; padding-left: 0;">>).
-define(PARAGRAPH_STYLE, <<"margin: 0 auto;">>).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {providers_details, users_details}).

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
    <<"Space details">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body(SpaceDetails :: #space_details{}) -> Result when
    Result :: #panel{}.
%% ====================================================================
body(SpaceDetails) ->
    Header = onepanel_gui_utils_adapter:top_menu(spaces_tab, spaces_settings_link, [], true),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 2em;">>,
                body = <<"Space details">>
            },
            space_details_table(SpaceDetails) |
            lists:map(fun(TableId) ->
                #table{
                    class = <<"table table-bordered table-striped">>,
                    style = <<"width: 50%; margin: 0 auto; margin-top: 3em; table-layout: fixed;">>,
                    body = #tbody{
                        id = TableId,
                        style = <<"display: none;">>
                    }
                }
            end, [<<"providers_table">>, <<"users_table">>])
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
                            style = ?PARAGRAPH_STYLE,
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


%% providers_table_collapsed/1
%% ====================================================================
%% @doc Renders collapsed providers details table.
%% @end
-spec providers_table_collapsed(ProvidersDetails :: [{Id :: binary(), ProviderDetails :: #provider_details{}}]) -> Result when
    Result :: [#tr{}].
%% ====================================================================
providers_table_collapsed(ProvidersDetails) ->
    TableName = <<"Providers">>,
    NavigationBody = onepanel_gui_utils:expand_button(<<"Expand All">>, {message, expand_providers_table}),
    RenderRowFunction = fun provider_row_collapsed/2,
    table(ProvidersDetails, TableName, NavigationBody, RenderRowFunction).


%% providers_table_expanded/1
%% ====================================================================
%% @doc Renders expanded providers details table.
%% @end
-spec providers_table_expanded(ProvidersDetails :: [{Id :: binary(), ProviderDetails :: #provider_details{}}]) -> Result when
    Result :: [#tr{}].
%% ====================================================================
providers_table_expanded(ProvidersDetails) ->
    TableName = <<"Providers">>,
    NavigationBody = onepanel_gui_utils:collapse_button(<<"Collapse All">>, {message, collapse_providers_table}),
    RenderRowFunction = fun provider_row_expanded/2,
    table(ProvidersDetails, TableName, NavigationBody, RenderRowFunction).


%% users_table_collapsed/1
%% ====================================================================
%% @doc Renders collapsed users details table.
%% @end
-spec users_table_collapsed(UsersDetails :: [{Id :: binary(), UserDetails :: #user_details{}}]) -> Result when
    Result :: [#tr{}].
%% ====================================================================
users_table_collapsed(UsersDetails) ->
    TableName = <<"Users">>,
    NavigationBody = onepanel_gui_utils:expand_button(<<"Expand All">>, {message, expand_users_table}),
    RenderRowFunction = fun user_row_collapsed/2,
    table(UsersDetails, TableName, NavigationBody, RenderRowFunction).


%% users_table_expanded/1
%% ====================================================================
%% @doc Renders expanded users details table.
%% @end
-spec users_table_expanded(UsersDetails :: [{Id :: binary(), UserDetails :: #user_details{}}]) -> Result when
    Result :: [#tr{}].
%% ====================================================================
users_table_expanded(UsersDetails) ->
    TableName = <<"Users">>,
    NavigationBody = onepanel_gui_utils:collapse_button(<<"Collapse All">>, {message, collapse_users_table}),
    RenderRowFunction = fun user_row_expanded/2,
    table(UsersDetails, TableName, NavigationBody, RenderRowFunction).


%% table/4
%% ====================================================================
%% @doc Renders details table.
%% @end
-spec table(Details :: [{Id :: binary(), Details :: #provider_details{} | #user_details{}}], TableName :: binary(), NavigationBody :: #link{}, RenderRowFunction :: function()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
table(Details, TableName, NavigationBody, RenderRowFunction) ->
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = TableName
            },
            #th{
                style = ?NAVIGATION_COLUMN_STYLE,
                body = NavigationBody
            }
        ]
    },

    Rows = lists:foldl(fun({RowId, RowDetails}, RowsAcc) ->
        [#tr{
            id = RowId,
            cells = RenderRowFunction(RowId, RowDetails)
        } | RowsAcc]
    end, [], Details),

    [Header | Rows].


%% provider_row_collapsed/2
%% ====================================================================
%% @doc Renders collapsed provider details row.
%% @end
-spec provider_row_collapsed(RowId :: binary(), ProviderDetails :: #provider_details{}) -> Result when
    Result :: [#td{}].
%% ====================================================================
provider_row_collapsed(RowId, #provider_details{id = ProviderId} = ProviderDetails) ->
    Details = [{<<"ProviderId">>, #p{style = ?PARAGRAPH_STYLE, body = ProviderId}}],
    NavigationBody = onepanel_gui_utils:expand_button({message, {expand_provider_row, RowId, ProviderDetails}}),
    row(Details, NavigationBody).


%% provider_row_expanded/2
%% ====================================================================
%% @doc Renders expanded provider details row.
%% @end
-spec provider_row_expanded(RowId :: binary(), ProviderDetails :: #provider_details{}) -> Result when
    Result :: [#td{}].
%% ====================================================================
provider_row_expanded(RowId, #provider_details{id = ProviderId, redirection_point = RedirectionPoint, urls = URLs} = ProviderDetails) ->
    Details = [
        {<<"Provider ID">>, #p{style = ?PARAGRAPH_STYLE, body = ProviderId}},
        {<<"URLs">>, #list{
            style = <<"list-style-type: none; margin: 0 auto;">>,
            body = lists:map(fun(URL) ->
                #li{body = #p{
                    style = ?PARAGRAPH_STYLE,
                    body = URL}
                }
            end, lists:sort(URLs))
        }},
        {<<"Redirection point">>, #p{style = ?PARAGRAPH_STYLE, body = RedirectionPoint}}
    ],
    NavigationBody = onepanel_gui_utils:collapse_button({message, {collapse_provider_row, RowId, ProviderDetails}}),
    row(Details, NavigationBody).


%% user_row_collapsed/2
%% ====================================================================
%% @doc Renders collapsed user details row.
%% @end
-spec user_row_collapsed(RowId :: binary(), UserDetails :: #user_details{}) -> Result when
    Result :: [#td{}].
%% ====================================================================
user_row_collapsed(RowId, #user_details{name = UserName} = UserDetails) ->
    Details = [{<<"Name">>, #p{style = ?PARAGRAPH_STYLE, body = UserName}}],
    NavigationBody = onepanel_gui_utils:expand_button({message, {expand_user_row, RowId, UserDetails}}),
    row(Details, NavigationBody).


%% user_row_expanded/2
%% ====================================================================
%% @doc Renders expanded user details row.
%% @end
-spec user_row_expanded(RowId :: binary(), UserDetails :: #user_details{}) -> Result when
    Result :: [#td{}].
%% ====================================================================
user_row_expanded(RowId, #user_details{id = UserId, name = UserName} = UserDetails) ->
    Details = [
        {<<"Name">>, #p{style = ?PARAGRAPH_STYLE, body = UserName}},
        {<<"User ID">>, #p{style = ?PARAGRAPH_STYLE, body = UserId}}
    ],
    NavigationBody = onepanel_gui_utils:collapse_button({message, {collapse_user_row, RowId, UserDetails}}),
    row(Details, NavigationBody).


%% row/2
%% ====================================================================
%% @doc Renders details row.
%% @end
-spec row([{DetailName :: binary(), DetailBody :: term()}], NavigationBody :: term()) -> Result when
    Result :: [#td{}].
%% ====================================================================
row(Details, NavigationBody) ->
    [
        #td{
            style = ?CONTENT_COLUMN_STYLE,
            body = #table{
                style = <<"border-width: 0; width: 100%; border-collapse: inherit;">>,
                body = lists:map(fun({DetailName, DetailBody}) ->
                    #tr{
                        cells = [
                            #td{
                                style = <<"border-width: 0; text-align: right; width: 10%; padding-left: 0; padding-right: 0;">>,
                                body = #label{
                                    style = <<"margin: 0 auto; cursor: auto;">>,
                                    class = <<"label label-large label-inverse">>,
                                    body = DetailName
                                }
                            },
                            #td{
                                style = <<"border-width: 0;  text-align: left; padding-left: 1em; width: 90%;">>,
                                body = DetailBody
                            }
                        ]
                    }
                end, Details)
            }
        },
        #td{
            style = ?NAVIGATION_COLUMN_STYLE,
            body = NavigationBody
        }
    ].


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

comet_loop(#?STATE{providers_details = ProvidersDetails, users_details = UsersDetails} = State) ->
    NewState = try
        receive
            render_tables ->
                gui_jq:update(<<"providers_table">>, providers_table_collapsed(ProvidersDetails)),
                gui_jq:fade_in(<<"providers_table">>, 500),
                gui_jq:update(<<"users_table">>, users_table_collapsed(UsersDetails)),
                gui_jq:fade_in(<<"users_table">>, 500),
                gui_jq:wire(<<"$('#main_spinner').delay(500).hide(0);">>, false),
                State;

            Event ->
                case Event of
                    collapse_providers_table ->
                        gui_jq:update(<<"providers_table">>, providers_table_collapsed(ProvidersDetails));
                    expand_providers_table ->
                        gui_jq:update(<<"providers_table">>, providers_table_expanded(ProvidersDetails));
                    {collapse_provider_row, RowId, ProviderDetails} ->
                        gui_jq:update(RowId, provider_row_collapsed(RowId, ProviderDetails));
                    {expand_provider_row, RowId, ProviderDetails} ->
                        gui_jq:update(RowId, provider_row_expanded(RowId, ProviderDetails));
                    collapse_users_table ->
                        gui_jq:update(<<"users_table">>, users_table_collapsed(UsersDetails));
                    expand_users_table ->
                        gui_jq:update(<<"users_table">>, users_table_expanded(UsersDetails));
                    {collapse_user_row, RowId, UserDetails} ->
                        gui_jq:update(RowId, user_row_collapsed(RowId, UserDetails));
                    {expand_user_row, RowId, UserDetails} ->
                        gui_jq:update(RowId, user_row_expanded(RowId, UserDetails));
                    _ ->
                        ok
                end,
                gui_jq:hide(<<"main_spinner">>),
                State

        after ?COMET_PROCESS_RELOAD_DELAY ->
            State
        end
               catch Type:Message ->
                   ?error("Comet process exception: ~p:~p", [Type, Message]),
                   onepanel_gui_utils:message(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Message}
               end,
    gui_comet:flush(),
    ?MODULE:comet_loop(NewState).


%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    try
        SpaceId = gui_str:to_binary(gui_ctx:url_param(<<"id">>)),

        {ok, ProviderIds} = gr_spaces:get_providers(provider, SpaceId),
        {ProvidersDetails, ProvidersCounter} = lists:foldl(fun(ProviderId, {ProvidersDetailsAcc, Id}) ->
            {ok, ProviderDetails} = gr_spaces:get_provider_details(provider, SpaceId, ProviderId),
            {
                [{<<"provider_", (integer_to_binary(Id + 1))/binary>>, ProviderDetails} | ProvidersDetailsAcc],
                Id + 1
            }
        end, {[], 0}, ProviderIds),

        {ok, UserIds} = gr_spaces:get_users(provider, SpaceId),
        {UsersDetails, UsersCounter} = lists:foldl(fun(UserId, {UsersDetailsAcc, Id}) ->
            {ok, UserDetails} = gr_spaces:get_user_details(provider, SpaceId, UserId),
            {
                [{<<"user_", (integer_to_binary(Id + 1))/binary>>, UserDetails} | UsersDetailsAcc],
                Id + 1
            }
        end, {[], 0}, UserIds),

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#?STATE{providers_details = ProvidersDetails, users_details = UsersDetails})
        end),
        put(?COMET_PID, Pid),
        Pid ! render_tables
    catch
        _:Reason ->
            ?error("Cannot fetch Space details: ~p", [Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch Space details.<br>Please try again later.">>)
    end;

event({message, Message}) ->
    get(?COMET_PID) ! Message,
    gui_jq:show(<<"main_spinner">>);

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.