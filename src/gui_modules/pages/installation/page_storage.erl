%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to add storage during software components installation.
%% @end
%% ===================================================================

-module(page_storage).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").

%% Number of added storage paths
-define(STORAGE_PATHS_SIZE, storage_paths_size).

%% Convenience record abbreviation
-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_STORAGE, ?PAGE_INSTALLATION) of
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
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Storage configuration">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(software_tab, installation_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 4: Storage configuration.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"<i>Worker</i> components save and retrieve user's data from network file system"
                " storages. To configure application's storage please provide paths to storages"
                " below. It is required, that each storge is available for all <i>worker</i> components"
                " at the same absolute path in file system.">>
            },
            #table{
                class = <<"table table-striped">>,
                style = <<"width: 50%; margin: 0 auto;">>,
                body = [
                    #tbody{
                        id = <<"storage_paths_table">>,
                        body = storage_paths_table_body()}
                ]
            },
            onepanel_gui_utils:nav_buttons([
                {<<"back_button">>, {postback, back}, <<"Back">>},
                {<<"next_button">>, {postback, next}, <<"Next">>}
            ])
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% storage_paths_table_body/0
%% ====================================================================
%% @doc Renders storage table body.
%% @end
-spec storage_paths_table_body() -> Result
    when Result :: [#tr{}].
%% ====================================================================
storage_paths_table_body() ->
    try
        {ok, Db} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, Session} = onepanel_gui_utils:get_session_config(),
        State = case Db#?CONFIG.storage_paths of
                    [] -> deletable;
                    _ -> none
                end,

        {Body, _} = lists:foldl(fun(StoragePath, {List, Id}) ->
            {
                [storage_paths_table_row(StoragePath, Id, true, State) | List],
                Id + 1
            }
        end, {[], 1}, lists:sort(fun(StoragePath1, StoragePath2) ->
            StoragePath1 > StoragePath2
        end, Session#?CONFIG.storage_paths)),

        Size = length(Session#?CONFIG.storage_paths),
        gui_ctx:put(?STORAGE_PATHS_SIZE, Size + 1),

        case State of
            none ->
                gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
                Body;
            _ -> Body ++ [storage_paths_table_row(<<"">>, Size + 1, undefined, addable)]
        end
    catch
        _:_ -> []
    end.


%% storage_paths_table_row/3
%% ====================================================================
%% @doc Renders storage table row. 'StoragePath' is a value that will
%% be placed in textbox with suffix id equals 'Id'. When 'Disabled'
%% equals true user cannot write in textbox.
%% @end
-spec storage_paths_table_row(StoragePath, Id, Disabled, State) -> Result when
    StoragePath :: string() | binary(),
    Id :: integer(),
    Disabled :: true | undefined,
    State :: addable | deletable | none,
    Result :: #tr{}.
%% ====================================================================
storage_paths_table_row(StoragePath, Id, Disabled, Deletable) ->
    BinaryId = integer_to_binary(Id),
    TextboxId = <<"storage_path_textbox_", BinaryId/binary>>,
    {AddStoragePathDisplay, DeleteStoragePathDisplay} = case Deletable of
                                                            addable -> {<<"">>, <<" display: none;">>};
                                                            deletable -> {<<" display: none;">>, <<"">>};
                                                            _ -> {<<" display: none;">>, <<" display: none;">>}
                                                        end,
    gui_jq:bind_enter_to_submit_button(TextboxId, <<"add_storage_path_", BinaryId/binary>>),
    #tr{
        id = <<"storage_path_row_", BinaryId/binary>>,
        cells = [
            #th{
                style = <<"text-align: center; vertical-align: inherit; padding-bottom: 0;">>,
                body = #textbox{
                    id = TextboxId,
                    value = gui_str:html_encode(StoragePath),
                    disabled = Disabled,
                    placeholder = <<"Storage path">>,
                    style = <<"width: 100%;">>
                }
            },
            #th{
                id = <<"add_storage_path_th_", BinaryId/binary>>,
                style = <<"text-align: center; vertical-align: inherit; padding: 0; width: 2em;", AddStoragePathDisplay/binary>>,
                body = #link{
                    id = <<"add_storage_path_", BinaryId/binary>>,
                    actions = gui_jq:form_submit_action(<<"add_storage_path_", BinaryId/binary>>,
                        {add_storage_path, BinaryId}, [TextboxId]),
                    class = <<"glyph-link">>,
                    body = #span{
                        class = <<"fui-plus">>,
                        style = <<"font-size: large;">>
                    }
                }
            },
            #th{
                id = <<"delete_storage_path_th_", BinaryId/binary>>,
                style = <<"text-align: center; vertical-align: inherit; padding: 0; width: 2em;", DeleteStoragePathDisplay/binary>>,
                body = #link{
                    id = <<"delete_storage_path_", BinaryId/binary>>,
                    actions = gui_jq:form_submit_action(<<"delete_storage_path_", BinaryId/binary>>,
                        {delete_storage_path, BinaryId}, [TextboxId]),
                    class = <<"glyph-link">>,
                    body = #span{
                        class = <<"fui-cross">>,
                        style = <<"font-size: large;">>
                    }
                }
            }
        ]
    }.


%% check_storage_paths/2
%% ====================================================================
%% @doc Checks wheter all storage paths are available for all workers.
%% @end
-spec check_storage_paths(Hosts :: [string()], StoragePath :: [string()]) -> Result when
    Result :: ok | error.
%% ====================================================================
check_storage_paths(_, []) ->
    ok;
check_storage_paths(Hosts, [StoragePath | StoragePaths]) ->
    case installer_storage:check_storage_path_on_hosts(Hosts, StoragePath) of
        ok ->
            check_storage_paths(Hosts, StoragePaths);
        {error, {hosts, ErrorHosts}} ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Storage: ", (list_to_binary(StoragePath))/binary,
            ", is not available on hosts: ", (onepanel_gui_utils:format_list(ErrorHosts))/binary>>),
            error;
        _ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Storage: ", (list_to_binary(StoragePath))/binary,
            ", is not available on all hosts">>),
            error
    end.


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
    Counter = gui_ctx:get(?STORAGE_PATHS_SIZE),
    gui_jq:focus(<<"storage_path_textbox_", (integer_to_binary(Counter))/binary>>),
    ok;

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_SYSTEM_LIMITS);

event({add_storage_path, BinaryId}) ->
    #?CONFIG{workers = Workers, storage_paths = StoragePaths} = Config = gui_ctx:get(?CONFIG_ID),
    StoragePath = binary_to_list(gui_ctx:postback_param(<<"storage_path_textbox_", BinaryId/binary>>)),
    case StoragePath of
        "" -> event(next);
        _ ->
            case lists:member(StoragePath, StoragePaths) of
                true -> onepanel_gui_utils:message(<<"error_message">>, <<"Storage path already added.">>);
                _ ->
                    case installer_storage:check_storage_path_on_hosts(Workers, StoragePath) of
                        ok ->
                            Counter = gui_ctx:get(?STORAGE_PATHS_SIZE),
                            gui_ctx:put(?STORAGE_PATHS_SIZE, Counter + 1),
                            gui_jq:hide(<<"error_message">>),
                            gui_jq:hide(<<"add_storage_path_th_", BinaryId/binary>>),
                            gui_jq:show(<<"delete_storage_path_th_", BinaryId/binary>>),
                            gui_jq:prop(<<"storage_path_textbox_", BinaryId/binary>>, <<"disabled">>, <<"disabled">>),
                            gui_jq:insert_bottom(<<"storage_paths_table">>, storage_paths_table_row(<<"">>, Counter + 1, undefined, addable)),
                            gui_jq:focus(<<"storage_path_textbox_", (integer_to_binary(Counter + 1))/binary>>),
                            gui_ctx:put(?CONFIG_ID, Config#?CONFIG{storage_paths = [StoragePath | StoragePaths]});
                        {error, {hosts, Hosts}} ->
                            onepanel_gui_utils:message(<<"error_message">>, <<"Storage is not available on hosts: ",
                            (onepanel_gui_utils:format_list(Hosts))/binary>>);
                        _ ->
                            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot add storage path.">>)
                    end
            end
    end;

event({delete_storage_path, BinaryId}) ->
    #?CONFIG{storage_paths = StoragePaths} = Config = gui_ctx:get(?CONFIG_ID),
    StoragePath = binary_to_list(gui_ctx:postback_param(<<"storage_path_textbox_", BinaryId/binary>>)),
    gui_jq:hide(<<"error_message">>),
    gui_jq:remove(<<"storage_path_row_", BinaryId/binary>>),
    gui_ctx:put(?CONFIG_ID, Config#?CONFIG{storage_paths = lists:delete(StoragePath, StoragePaths)});

event(next) ->
    #?CONFIG{workers = Workers, storage_paths = StoragePaths} = gui_ctx:get(?CONFIG_ID),
    case StoragePaths of
        [] ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Please add at least one storage.">>);
        _ ->
            case check_storage_paths(Workers, StoragePaths) of
                ok ->
                    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUMMARY);
                _ ->
                    error
            end
    end;

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.