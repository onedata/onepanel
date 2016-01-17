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
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/global_registry/gr_spaces.hrl").
-include_lib("ctool/include/global_registry/gr_openid.hrl").

-export([main/0, event/1, api_event/3, comet_loop/1]).

%% Convenience record abbreviation
-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

%% Common page CCS styles
-define(CONTENT_COLUMN_STYLE, <<"padding-right: 0">>).
-define(NAVIGATION_COLUMN_STYLE, <<"border-left-width: 0; width: 20px; padding-left: 0;">>).
-define(PARAGRAPH_STYLE, <<"margin: 0 auto;">>).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {counter, spaces_details, workers}).

-record(document, {key, rev, value, links}).
-record(storage, {name, helpers}).
-record(helper_init, {name, args}).

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
    <<
        "<script src='/flatui/bootbox.min.js' type='text/javascript' charset='utf-8'></script>",
        "<script src='/js/spaces_management.js' type='text/javascript' charset='utf-8'></script>"
    >>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    {ok, #?CONFIG{workers = Hosts}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
    Workers = onepanel_utils:get_nodes("worker", Hosts),
    {ok, Storages} = onepanel_utils:dropwhile_failure(Workers, storage, list, [], ?RPC_TIMEOUT),
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
                {<<"create_space_button">>, {postback, {create_space, Storages}}, true, <<"Create Space">>},
                {<<"support_space_button">>, {postback, {support_space, Storages}}, true, <<"Support Space">>}
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


%% storage_dropdown/3
%% ====================================================================
%% @doc Renders storage list dropdown.
%% @end
-spec storage_dropdown(Storages :: list(), CephForm :: binary(), DioForm :: binary()) ->
    Result when Result :: binary().
%% ====================================================================
storage_dropdown(Storages, CephForm, DioForm) ->
    Options = lists:map(fun(#document{key = StorageId,
        value = #storage{name = SName, helpers = [#helper_init{name = HName}]}
    }) ->
        <<"<option value=\"", HName/binary, ":", StorageId/binary, "\">", SName/binary, "</option>">>
    end, Storages),
    Header = <<"<div id=\"storage_list\" style=\"display: flex;\">",
        "<label for=\"storage_type\" style=\"font-size: larger; line-height: 2em; padding-right: 0.5em;\">Storage:</label>",
        "<select id=\"storage_type\" class=\"select\" onchange=(function(){",
        "storage_type=$(\"#storage_type\").val();",
        "$(\"#ceph_form\").remove();",
        "$(\"#dio_form\").remove();",
        "if(storage_type.slice(0,4)==\"Ceph\"){",
        CephForm/binary,
        "}",
        "else{",
        DioForm/binary,
        "}",
        "})()>">>,
    lists:foldl(fun(Part, Acc) ->
        <<Acc/binary, Part/binary>>
    end, Header, Options ++ [<<"</select></div>">>]).

%% get_storage_id/1
%% ====================================================================
%% @doc Returns storage ID from option ID.
%% @end
-spec get_storage_id(OptionId :: binary()) -> StorageId :: binary().
%% ====================================================================
get_storage_id(<<"Ceph:", StorageId/binary>>) ->
    StorageId;
get_storage_id(<<"DirectIO:", StorageId/binary>>) ->
    StorageId.

%% maybe_add_ceph_user/6
%% ====================================================================
%% @doc For Ceph storage user adds his details to provider database.
%% @end
-spec maybe_add_ceph_user(Workers :: [node()], IsCeph :: boolean(), UserId :: binary(),
    StorageId :: binary(), Username :: binary(), Key :: binary()) -> ok.
%% ====================================================================
maybe_add_ceph_user(_, false, _, _, _, _) ->
    ok;
maybe_add_ceph_user(Workers, true, UserId, StorageId, Username, Key) ->
    installer_storage:add_ceph_user(Workers, UserId, StorageId, Username, Key).

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

comet_loop(#?STATE{counter = Counter, spaces_details = SpacesDetails, workers = Workers} = State) ->
    NewState = try
        receive
            {create_space, StorageType, IsCeph, Name, Token, Size, Username, Key} ->
                NextState =
                    try
                        RowId = <<"space_", (integer_to_binary(Counter + 1))/binary>>,
                        {ok, #token_issuer{client_type = <<"user">>, client_id = UserId}} =
                            gr_providers:get_token_issuer(provider, Token),
                        {ok, SpaceId} = gr_providers:create_space(provider,
                            [{<<"name">>, Name}, {<<"token">>, Token}, {<<"size">>, integer_to_binary(Size)}]),
                        {ok, SpaceDetails} = gr_providers:get_space_details(provider, SpaceId),
                        StorageId = get_storage_id(StorageType),
                        installer_storage:add_space_storage_mapping(Workers, SpaceId, StorageId),
                        maybe_add_ceph_user(Workers, IsCeph, UserId, StorageId, Username, Key),
                        add_space_row(RowId, SpaceDetails),
                        onepanel_gui_utils:message(success, <<"Created Space's ID: <b>", SpaceId/binary, "</b>">>),
                        State#?STATE{counter = Counter + 1, spaces_details = [{RowId, SpaceDetails} | SpacesDetails]}
                    catch
                        _:Reason ->
                            ?error_stacktrace("Cannot create Space ~p associated with token ~p: ~p", [Name, Token, Reason]),
                            onepanel_gui_utils:message(error, <<"Cannot create Space <b>", Name/binary, "</b> associated with token <b>", Token/binary, "</b>.<br>
                            Please try again later.">>),
                            State
                    end,
                gui_jq:prop(<<"create_space_button">>, <<"disabled">>, <<"">>),
                NextState;

            {support_space, StorageType, IsCeph, Token, Size, Username, Key} ->
                NextState =
                    try
                        RowId = <<"space_", (integer_to_binary(Counter + 1))/binary>>,
                        {ok, #token_issuer{client_type = <<"user">>, client_id = UserId}} =
                            gr_providers:get_token_issuer(provider, Token),
                        {ok, SpaceId} = gr_providers:support_space(provider,
                            [{<<"token">>, Token}, {<<"size">>, integer_to_binary(Size)}]),
                        {ok, SpaceDetails} = gr_providers:get_space_details(provider, SpaceId),
                        StorageId = get_storage_id(StorageType),
                        installer_storage:add_space_storage_mapping(Workers, SpaceId, StorageId),
                        maybe_add_ceph_user(Workers, IsCeph, UserId, StorageId, Username, Key),
                        add_space_row(RowId, SpaceDetails),
                        onepanel_gui_utils:message(success, <<"Supported Space's ID: <b>", SpaceId/binary, "</b>">>),

                        State#?STATE{counter = Counter + 1, spaces_details = [{RowId, SpaceDetails} | SpacesDetails]}
                    catch
                        _:Reason ->
                            ?error_stacktrace("Cannot support Space associated with token ~p: ~p", [Token, Reason]),
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
        {ok, #?CONFIG{workers = Hosts}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        Workers = onepanel_utils:get_nodes("worker", Hosts),
        {ok, SpaceIds} = gr_providers:get_spaces(provider),
        {SpacesDetails, Counter} = lists:foldl(fun(SpaceId, {SpacesDetailsAcc, Id}) ->
            {ok, SpaceDetails} = gr_providers:get_space_details(provider, SpaceId),
            {
                [{<<"space_", (integer_to_binary(Id + 1))/binary>>, SpaceDetails} | SpacesDetailsAcc],
                Id + 1
            }
        end, {[], 0}, SpaceIds),

        gui_jq:wire(#api{name = "create_space", tag = "create_space"}, false),
        gui_jq:wire(#api{name = "support_space", tag = "support_space"}, false),
        gui_jq:wire(#api{name = "revoke_space_support", tag = "revoke_space_support"}, false),
        gui_jq:bind_key_to_click_on_class(<<"13">>, <<"confirm">>),

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#?STATE{counter = Counter, spaces_details = SpacesDetails, workers = Workers})
        end),
        put(?COMET_PID, Pid),
        Pid ! render_spaces_table
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch supported Spaces.<br>Please try again later.">>)
    end;

event({create_space, Storages}) ->
    Title = <<"Create Space">>,
    Message = <<"<div style=\"margin: 0 auto; width: 80%;\">",
        "<p id=\"space_alert\" style=\"width: 100%; color: red; font-size: medium; text-align: center; display: none;\"></p>",
        (storage_dropdown(Storages, <<"ceph_create_form();">>, <<"dio_create_form();">>))/binary,
        "</div>">>,
    Script = <<"create_space_check();">>,
    ConfirmButtonClass = <<"btn-inverse">>,
    gui_jq:dialog_popup(Title, Message, Script, ConfirmButtonClass),
    case Storages of
        [#document{value = #storage{helpers = [#helper_init{name = <<"Ceph">>} | _]}} | _] ->
            gui_jq:wire(<<"box.on('shown',ceph_create_form);">>);
        [#document{value = #storage{helpers = [#helper_init{name = <<"DirectIO">>} | _]}} | _] ->
            gui_jq:wire(<<"box.on('shown',dio_create_form);">>);
        _ -> ok
    end;

event({support_space, Storages}) ->
    Title = <<"Support Space">>,
    Message = <<"<div style=\"margin: 0 auto; width: 80%;\">",
        "<p id=\"space_alert\" style=\"width: 100%; color: red; font-size: medium; text-align: center; display: none;\"></p>",
        (storage_dropdown(Storages, <<"ceph_support_form();">>, <<"dio_support_form();">>))/binary,
        "</div>">>,
    Script = <<"support_space_check();">>,
    ConfirmButtonClass = <<"btn-inverse">>,
    gui_jq:dialog_popup(Title, Message, Script, ConfirmButtonClass),
    case Storages of
        [#document{value = #storage{helpers = [#helper_init{name = <<"Ceph">>} | _]}} | _] ->
            gui_jq:wire(<<"box.on('shown',ceph_support_form);">>);
        [#document{value = #storage{helpers = [#helper_init{name = <<"DirectIO">>} | _]}} | _] ->
            gui_jq:wire(<<"box.on('shown',dio_support_form);">>);
        _ -> ok
    end;

event({revoke_space_support, RowId, #space_details{id = SpaceId}}) ->
    Title = <<"Revoke Space support">>,
    Message = <<"Are you sure you want to stop supporting Space: <b>", SpaceId/binary, "</b>?<br>This operation cannot be undone.">>,
    Script = <<"revoke_space_support(['", SpaceId/binary, "','", RowId/binary, "']);">>,
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
api_event("create_space", Args, _) ->
    [StorageType, IsCeph, Name, Token, Size, Username, Key] = mochijson2:decode(Args),
    get(?COMET_PID) ! {create_space, StorageType, IsCeph, Name, Token, Size, Username, Key},
    gui_jq:show(<<"main_spinner">>),
    gui_jq:prop(<<"create_space_button">>, <<"disabled">>, <<"disabled">>);

api_event("support_space", Args, _) ->
    [StorageType, IsCeph, Token, Size, Username, Key] = mochijson2:decode(Args),
    get(?COMET_PID) ! {support_space, StorageType, IsCeph, Token, Size, Username, Key},
    gui_jq:show(<<"main_spinner">>),
    gui_jq:prop(<<"support_space_button">>, <<"disabled">>, <<"disabled">>);

api_event("revoke_space_support", Args, _) ->
    [SpaceId, RowId] = mochijson2:decode(Args),
    get(?COMET_PID) ! {revoke_space_support, RowId, SpaceId},
    gui_jq:show(<<"main_spinner">>).