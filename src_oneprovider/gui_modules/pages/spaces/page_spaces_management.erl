%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to manage provider spaces.
%% @end
%% ===================================================================
-module(page_spaces_management).

-include("gui_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/global_registry/gr_spaces.hrl").

-export([main/0, event/1, api_event/3, comet_loop/1]).

%% Common page CCS styles
-define(CONTENT_COLUMN_STYLE, <<"padding-right: 0">>).
-define(NAVIGATION_COLUMN_STYLE, <<"border-left-width: 0; width: 20px; padding-left: 0;">>).
-define(PARAGRAPH_STYLE, <<"margin: 0 auto;">>).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {counter, spaces_details}).

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
                    page_error:redirect_with_error(<<"unregistered_provider_error">>),
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                _ ->
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, custom()}]}
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
    <<"Spaces management">>.


%% custom/0
%% ====================================================================
%% @doc This will be placed instead of {{custom}} tag in template.
%% @end
-spec custom() -> binary().
%% ====================================================================
custom() ->
    <<"<script src='/flatui/bootbox.min.js' type='text/javascript' charset='utf-8'></script>">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils_adapter:top_menu(spaces_tab, spaces_dashboard_link, [], true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Spaces settings">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"Supported <i>Spaces</i> are presented in the table below.">>
            },
            onepanel_gui_utils:nav_buttons([
                {<<"create_space_button">>, {postback, create_space}, true, <<"Create Space">>},
                {<<"support_space_button">>, {postback, support_space}, true, <<"Support Space">>}
            ], <<"20em">>),
            #table{
                class = <<"table table-bordered table-striped">>,
                style = <<"width: 50%; margin: 0 auto; margin-top: 3em; table-layout: fixed;">>,
                body = #tbody{
                    id = <<"spaces_table">>,
                    style = <<"display: none;">>
                }
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% spaces_table_collapsed/1
%% ====================================================================
%% @doc Renders collapsed Spaces settings table.
%% @end
-spec spaces_table_collapsed(SpacesDetails :: [{Id :: binary(), SpaceDetails :: #space_details{}}]) -> Result when
    Result :: [#tr{}].
%% ====================================================================
spaces_table_collapsed(SpacesDetails) ->
    NavigationBody = onepanel_gui_utils:expand_button(<<"Expand All">>, {message, expand_spaces_table}),
    RenderRowFunction = fun space_row_collapsed/2,
    spaces_table(SpacesDetails, NavigationBody, RenderRowFunction).


%% spaces_table_expanded/1
%% ====================================================================
%% @doc Renders expanded Spaces settings table.
%% @end
-spec spaces_table_expanded(SpacesDetails :: [{Id :: binary(), SpaceDetails :: #space_details{}}]) -> Result when
    Result :: [#tr{}].
%% ====================================================================
spaces_table_expanded(SpacesDetails) ->
    NavigationBody = onepanel_gui_utils:collapse_button(<<"Collapse All">>, {message, collapse_spaces_table}),
    RenderRowFunction = fun space_row_expanded/2,
    spaces_table(SpacesDetails, NavigationBody, RenderRowFunction).


%% spaces_table/3
%% ====================================================================
%% @doc Renders Spaces settings table.
%% @end
-spec spaces_table(SpacesDetails :: [{Id :: binary(), SpaceDetails :: #space_details{}}], NavigationBody :: #link{}, RenderRowFunction :: function()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
spaces_table(SpacesDetails, NavigationBody, RenderRowFunction) ->
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Spaces">>
            },
            #th{
                style = ?NAVIGATION_COLUMN_STYLE,
                body = NavigationBody
            }
        ]
    },

    Rows = lists:foldl(fun({RowId, SpaceDetails}, RowsAcc) ->
        [#tr{
            id = RowId,
            cells = RenderRowFunction(RowId, SpaceDetails)
        } | RowsAcc]
    end, [], SpacesDetails),

    [Header | Rows].


%% space_row_collapsed/2
%% ====================================================================
%% @doc Renders collapsed Space settings row.
%% @end
-spec space_row_collapsed(RowId :: binary(), SpaceDetails :: #space_details{}) -> Result when
    Result :: [#td{}].
%% ====================================================================
space_row_collapsed(RowId, #space_details{id = SpaceId, name = SpaceName} = SpaceDetails) ->
    [
        #td{
            style = ?CONTENT_COLUMN_STYLE,
            body = #p{
                style = ?PARAGRAPH_STYLE,
                body = <<"<b>", SpaceName/binary, "</b> (", SpaceId/binary, ")">>
            }
        },
        #td{
            style = ?NAVIGATION_COLUMN_STYLE,
            body = onepanel_gui_utils:expand_button({message, {expand_space_row, RowId, SpaceDetails}})
        }
    ].


%% space_row_expanded/2
%% ====================================================================
%% @doc Renders expanded Space settings row.
%% @end
-spec space_row_expanded(RowId :: binary(), SpaceDetails :: #space_details{}) -> Result when
    Result :: [#td{}].
%% ====================================================================
space_row_expanded(RowId, #space_details{id = SpaceId, name = SpaceName} = SpaceDetails) ->
    SettingsIcons = lists:map(fun({LinkTitle, LinkPostback, SpanClass}) ->
        #link{
            title = LinkTitle,
            style = <<"font-size: large; margin-right: 1em;">>,
            class = <<"glyph-link">>,
            postback = LinkPostback,
            body = #span{
                class = SpanClass
            }
        }
    end, [
        {<<"Get details">>, {get_details, SpaceDetails}, <<"fui-info">>},
        {<<"Revoke support">>, {revoke_space_support, RowId, SpaceDetails}, <<"fui-trash">>}
    ]),
    [
        #td{
            style = ?CONTENT_COLUMN_STYLE,
            body = #table{
                style = <<"border-width: 0; width: 100%; border-collapse: inherit;">>,
                body = lists:map(fun({Description, Main}) ->
                    #tr{
                        cells = [
                            #td{
                                style = <<"border-width: 0; text-align: right; width: 10%; padding-left: 0; padding-right: 0;">>,
                                body = #label{
                                    style = <<"margin: 0 auto; cursor: auto;">>,
                                    class = <<"label label-large label-inverse">>,
                                    body = Description
                                }
                            },
                            #td{
                                style = <<"border-width: 0;  text-align: left; padding-left: 1em; width: 90%;">>,
                                body = #p{
                                    style = ?PARAGRAPH_STYLE,
                                    body = Main
                                }
                            }
                        ]
                    }
                end, [
                    {<<"Name">>, SpaceName},
                    {<<"Space ID">>, SpaceId},
                    {<<"Settings">>, SettingsIcons}
                ])
            }
        },
        #td{
            style = ?NAVIGATION_COLUMN_STYLE,
            body = onepanel_gui_utils:collapse_button({message, {collapse_space_row, RowId, SpaceDetails}})
        }
    ].


%% space_size_parameter/2
%% ====================================================================
%% @doc Renders space size parameter in window popups.
%% @end
-spec space_size_parameter() -> Result when
    Result :: binary().
%% ====================================================================
space_size_parameter() ->
    <<"<div id=\"space_size_div\" style=\"width: 100%;\">",
    "   <input id=\"create_space_size\" class=\"pull-left\" type=\"text\" style=\"width: 50%;\" placeholder=\"Size\">",
    "   <fieldset class=\"pull-right\" style=\"width: 41%; line-height: 40px;\">"
    "       <label id=\"size_mb\" class=\"radio checked\" style=\"display: inline-block; padding-left: 25px; margin-right: 10px;\">",
    "           <input type=\"radio\" name=\"optionsRadios2\" id=\"optionsRadios1\" value=\"option1\" data-toggle=\"radio\" checked=\"checked\">MB",
    "       </label>",
    "       <label id=\"size_gb\" class=\"radio\" style=\"display: inline-block; padding-left: 25px; margin-right: 10px;\">",
    "           <input type=\"radio\" name=\"optionsRadios2\" id=\"optionsRadios2\" value=\"option1\" data-toggle=\"radio\">GB",
    "       </label>",
    "       <label id=\"size_tb\" class=\"radio\" style=\"display: inline-block; padding-left: 25px; margin-right: 10px;\">",
    "           <input type=\"radio\" name=\"optionsRadios2\" id=\"optionsRadios3\" value=\"option1\" data-toggle=\"radio\">TB",
    "       </label>",
    "   </fieldset>",
    "</div>">>.


%% add_space_row/2
%% ====================================================================
%% @doc Adds collapsed Space settings row to Spaces settings table.
%% @end
-spec add_space_row(RowId :: binary(), SpaceDetails :: #space_details{}) -> Result when
    Result :: ok.
%% ====================================================================
add_space_row(RowId, SpaceDetails) ->
    Row = #tr{
        id = RowId,
        cells = space_row_collapsed(RowId, SpaceDetails)
    },
    gui_jq:insert_bottom(<<"spaces_table">>, Row).


%% initialize_radio_buttons/0
%% ====================================================================
%% @doc Initializes radio buttons - required by flatui.
%% @end
-spec initialize_radio_buttons() -> Result when
    Result :: ok.
%% ====================================================================
initialize_radio_buttons() ->
    gui_jq:wire(<<"$('[data-toggle=\"radio\"]').each(function () {
      var $radio = $(this);
      $radio.radio();
    });">>).


%% space_size_check/0
%% ====================================================================
%% @doc Returns script used to check correctness of space size parameter.
%% @end
-spec space_size_check() -> Result when
    Result :: binary().
%% ====================================================================
space_size_check() ->
    <<"else if(size.length == 0) { alert.html(\"Please provide Space size.\"); alert.fadeIn(300); return false; }",
    "else if(isNaN(size) || parseInt(size, 10) <= 0) { alert.html(\"Space size should be a positive number.\"); alert.fadeIn(300); return false; }">>.


%% space_size_multiply/0
%% ====================================================================
%% @doc Returns script used to multiply space size parameter.
%% @end
-spec space_size_multiply() -> Result when
    Result :: binary().
%% ====================================================================
space_size_multiply() ->
    <<"if($(\"#size_mb\").hasClass(\"checked\")) { size = 1024 * 1024 * size; }",
    "else if($(\"#size_gb\").hasClass(\"checked\")) { size = 1024 * 1024 * 1024 * size; }",
    "else { size = 1024 * 1024 * 1024 * 1024 * size; }">>.


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

comet_loop(#?STATE{counter = Counter, spaces_details = SpacesDetails} = State) ->
    NewState = try
                   receive
                       {create_space, Name, Token, Size} ->
                           NextState =
                               try
                                   RowId = <<"space_", (integer_to_binary(Counter + 1))/binary>>,
                                   {ok, SpaceId} = gr_providers:create_space(provider,
                                       [{<<"name">>, Name}, {<<"token">>, Token}, {<<"size">>, integer_to_binary(Size)}]),
                                   {ok, SpaceDetails} = gr_providers:get_space_details(provider, SpaceId),
                                   add_space_row(RowId, SpaceDetails),
                                   onepanel_gui_utils:message(success, <<"Created Space's ID: <b>", SpaceId/binary, "</b>">>),
                                   State#?STATE{counter = Counter + 1, spaces_details = [{RowId, SpaceDetails} | SpacesDetails]}
                               catch
                                   _:Reason ->
                                       ?error("Cannot create Space ~p associated with token ~p: ~p", [Name, Token, Reason]),
                                       onepanel_gui_utils:message(error, <<"Cannot create Space <b>", Name/binary, "</b> associated with token <b>", Token/binary, "</b>.<br>
                            Please try again later.">>),
                                       State
                               end,
                           gui_jq:prop(<<"create_space_button">>, <<"disabled">>, <<"">>),
                           NextState;

                       {support_space, Token, Size} ->
                           NextState =
                               try
                                   RowId = <<"space_", (integer_to_binary(Counter + 1))/binary>>,
                                   {ok, SpaceId} = gr_providers:support_space(provider,
                                       [{<<"token">>, Token}, {<<"size">>, integer_to_binary(Size)}]),
                                   {ok, SpaceDetails} = gr_providers:get_space_details(provider, SpaceId),
                                   add_space_row(RowId, SpaceDetails),
                                   onepanel_gui_utils:message(success, <<"Supported Space's ID: <b>", SpaceId/binary, "</b>">>),
                                   State#?STATE{counter = Counter + 1, spaces_details = [{RowId, SpaceDetails} | SpacesDetails]}
                               catch
                                   _:Reason ->
                                       ?error("Cannot support Space associated with token ~p: ~p", [Token, Reason]),
                                       onepanel_gui_utils:message(error, <<"Cannot support Space associated with token <b>", Token/binary, "</b>.<br>
                            Please try again later.">>),
                                       State
                               end,
                           gui_jq:prop(<<"support_space_button">>, <<"disabled">>, <<"">>),
                           NextState;

                       {revoke_space_support, RowId, SpaceId} ->
                           NextState =
                               case gr_providers:revoke_space_support(provider, SpaceId) of
                                   ok ->
                                       onepanel_gui_utils:message(success, <<"Space: <b>", SpaceId/binary, "</b> is no longer supported.">>),
                                       gui_jq:remove(RowId),
                                       State#?STATE{spaces_details = proplists:delete(RowId, SpacesDetails)};
                                   Other ->
                                       ?error("Cannot revoke support for Space ~p: ~p", [SpaceId, Other]),
                                       onepanel_gui_utils:message(error, <<"Cannot revoke support for Space <b>", SpaceId/binary, "</b>.<br>Please try again later.">>),
                                       State
                               end,
                           NextState;

                       render_spaces_table ->
                           gui_jq:update(<<"spaces_table">>, spaces_table_collapsed(SpacesDetails)),
                           gui_jq:fade_in(<<"spaces_table">>, 500),
                           gui_jq:prop(<<"create_space_button">>, <<"disabled">>, <<"">>),
                           gui_jq:prop(<<"support_space_button">>, <<"disabled">>, <<"">>),
                           State;

                       Event ->
                           case Event of
                               collapse_spaces_table ->
                                   gui_jq:update(<<"spaces_table">>, spaces_table_collapsed(SpacesDetails));
                               expand_spaces_table ->
                                   gui_jq:update(<<"spaces_table">>, spaces_table_expanded(SpacesDetails));
                               {collapse_space_row, RowId, SpaceDetails} ->
                                   gui_jq:update(RowId, space_row_collapsed(RowId, SpaceDetails));
                               {expand_space_row, RowId, SpaceDetails} ->
                                   gui_jq:update(RowId, space_row_expanded(RowId, SpaceDetails));
                               _ ->
                                   ok
                           end,
                           State

                   after ?COMET_PROCESS_RELOAD_DELAY ->
                       State
                   end
               catch Type:Message ->
                   ?error_stacktrace("Comet process exception: ~p:~p", [Type, Message]),
                   onepanel_gui_utils:message(error, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Message}
               end,
    gui_jq:wire(<<"$('#main_spinner').delay(300).hide(0);">>, false),
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
        {ok, SpaceIds} = gr_providers:get_spaces(provider),
        {SpacesDetails, Counter} = lists:foldl(fun(SpaceId, {SpacesDetailsAcc, Id}) ->
            {ok, SpaceDetails} = gr_providers:get_space_details(provider, SpaceId),
            {
                [{<<"space_", (integer_to_binary(Id + 1))/binary>>, SpaceDetails} | SpacesDetailsAcc],
                Id + 1
            }
        end, {[], 0}, SpaceIds),

        gui_jq:wire(#api{name = "createSpace", tag = "createSpace"}, false),
        gui_jq:wire(#api{name = "supportSpace", tag = "supportSpace"}, false),
        gui_jq:wire(#api{name = "revokeSpaceSupport", tag = "revokeSpaceSupport"}, false),
        gui_jq:bind_key_to_click_on_class(<<"13">>, <<"confirm">>),

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#?STATE{counter = Counter, spaces_details = SpacesDetails})
        end),
        put(?COMET_PID, Pid),
        Pid ! render_spaces_table
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch supported Spaces.<br>Please try again later.">>)
    end;

event(create_space) ->
    Title = <<"Create Space">>,
    Message = <<"<div style=\"margin: 0 auto; width: 80%;\">",
    "<p id=\"create_space_alert\" style=\"width: 100%; color: red; font-size: medium; text-align: center; display: none;\"></p>",
    "<input id=\"create_space_name\" type=\"text\" style=\"width: 100%;\" placeholder=\"Name\">",
    "<input id=\"create_space_token\" type=\"text\" style=\"width: 100%;\" placeholder=\"Token\">",
    (space_size_parameter())/binary,
    "</div>">>,
    Script = <<"var alert = $(\"#create_space_alert\");",
    "var name = $.trim($(\"#create_space_name\").val());",
    "var token = $.trim($(\"#create_space_token\").val());",
    "var size = $.trim($(\"#create_space_size\").val());",
    "if(name.length == 0) { alert.html(\"Please provide Space name.\"); alert.fadeIn(300); return false; }",
    "else if(token.length == 0) { alert.html(\"Please provide Space token.\"); alert.fadeIn(300); return false; }",
    (space_size_check())/binary,
    "else { ", (space_size_multiply())/binary,
    "          createSpace([name, token, size]); return true;",
    "     }">>,
    ConfirmButtonClass = <<"btn-inverse">>,
    gui_jq:dialog_popup(Title, Message, Script, ConfirmButtonClass),
    gui_jq:wire(<<"box.on('shown',function(){ $(\"#create_space_name\").focus(); });">>),
    initialize_radio_buttons();

event(support_space) ->
    Title = <<"Support Space">>,
    Message = <<"<div style=\"margin: 0 auto; width: 80%;\">",
    "<select id=\"storage_type\" class=\"select\" onchange=(function(){alert($(\"#storage_type\").val())})()>",
    "<option value=\"0\">lol</option>",
    "<option value=\"1\">My Friends</option>",
    "</select>",
    "<p id=\"support_space_alert\" style=\"width: 100%; color: red; font-size: medium; text-align: center; display: none;\"></p>",
    "<input id=\"support_space_token\" type=\"text\" style=\"width: 100%;\" placeholder=\"Token\">",
    (space_size_parameter())/binary,
    "</div>">>,
    Script = <<"var alert = $(\"#support_space_alert\");",
    "var token = $.trim($(\"#support_space_token\").val());",
    "var size = $.trim($(\"#create_space_size\").val());",
    "if(token.length == 0) { alert.html(\"Please provide Space token.\"); alert.fadeIn(300); return false; }",
    (space_size_check())/binary,
    "else { ", (space_size_multiply())/binary,
    "          supportSpace([token, size]); return true;",
    "     }">>,
    ConfirmButtonClass = <<"btn-inverse">>,
    gui_jq:dialog_popup(Title, Message, Script, ConfirmButtonClass),
    gui_jq:wire(<<"box.on('shown',function(){ $(\"#support_space_token\").focus(); });">>),
    initialize_radio_buttons();

event({revoke_space_support, RowId, #space_details{id = SpaceId}}) ->
    Title = <<"Revoke Space support">>,
    Message = <<"Are you sure you want to stop supporting Space: <b>", SpaceId/binary, "</b>?<br>This operation cannot be undone.">>,
    Script = <<"revokeSpaceSupport(['", SpaceId/binary, "','", RowId/binary, "']);">>,
    ConfirmButtonClass = <<"btn-inverse">>,
    gui_jq:dialog_popup(Title, Message, Script, ConfirmButtonClass);

event({get_details, #space_details{id = SpaceId}}) ->
    gui_jq:redirect(<<"/spaces?id=", SpaceId/binary>>);

event({message, Message}) ->
    get(?COMET_PID) ! Message,
    gui_jq:show(<<"main_spinner">>);

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.


%% api_event/3
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec api_event(Name :: string(), Args :: string(), Req :: string()) -> no_return().
%% ====================================================================
api_event("createSpace", Args, _) ->
    [Name, Token, Size] = mochijson2:decode(Args),
    get(?COMET_PID) ! {create_space, Name, Token, Size},
    gui_jq:show(<<"main_spinner">>),
    gui_jq:prop(<<"create_space_button">>, <<"disabled">>, <<"disabled">>);

api_event("supportSpace", Args, _) ->
    [Token, Size] = mochijson2:decode(Args),
    get(?COMET_PID) ! {support_space, Token, Size},
    gui_jq:show(<<"main_spinner">>),
    gui_jq:prop(<<"support_space_button">>, <<"disabled">>, <<"disabled">>);

api_event("revokeSpaceSupport", Args, _) ->
    [SpaceId, RowId] = mochijson2:decode(Args),
    get(?COMET_PID) ! {revoke_space_support, RowId, SpaceId},
    gui_jq:show(<<"main_spinner">>).
