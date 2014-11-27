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

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

%% Convenience record abbreviation
-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {counter = 0, db_config = #?CONFIG{}, session_config = #?CONFIG{}}).

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
    <<"Storage configuration">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Breadcrumbs = onepanel_gui_utils:breadcrumbs([
        {<<"Hosts selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_HOST_SELECTION},
        {<<"Primary CCM selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CCM_SELECTION},
        {<<"Application ports check">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_APP_PORTS_CHECK},
        {<<"System limits">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_SYSTEM_LIMITS},
        {<<"Storage configuration">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_STORAGE}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, installation_link, Breadcrumbs, true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 5: Storage configuration.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"<i>Worker</i> components save and retrieve user's data from network file system"
                " storages. In order to configure application's storage please provide paths to storages"
                " below. It is required that each storage is available for all <i>worker</i> components"
                " at the same absolute path in file system.">>
            },
            #table{
                class = <<"table table-striped">>,
                style = <<"width: 50%; margin: 0 auto;">>,
                body = #tbody{
                    id = <<"storage_paths_table">>,
                    style = <<"display: none;">>
                }
            },
            onepanel_gui_utils:nav_buttons([
                {<<"back_button">>, {postback, back}, false, <<"Back">>},
                {<<"next_button">>, {postback, next}, true, <<"Next">>}
            ])
        ]
    },
    onepanel_gui_utils:body(?SUBMENU_HEIGHT, Header, Main, onepanel_gui_utils:logotype_footer()).


%% storage_paths_table/2
%% ====================================================================
%% @doc Renders storage table body.
%% @end
-spec storage_paths_table(DbConfig :: #?CONFIG{}, SessionConfig :: #?CONFIG{}) -> Result
    when Result :: [#tr{}].
%% ====================================================================
storage_paths_table(#?CONFIG{storage_paths = DbStoragePaths}, #?CONFIG{storage_paths = SessionStoragePaths}) ->
    State = case DbStoragePaths of
                [] -> removable;
                _ -> none
            end,

    {Body, _} = lists:foldl(fun(StoragePath, {List, Id}) ->
        {
            [storage_paths_table_row(StoragePath, Id, true, State) | List],
            Id + 1
        }
    end, {[], 1}, lists:sort(fun(StoragePath1, StoragePath2) ->
        StoragePath1 > StoragePath2
    end, SessionStoragePaths)),

    case State of
        none ->
            gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
            Body;
        _ ->
            Body ++ [storage_paths_table_row(<<"">>, length(SessionStoragePaths) + 1, undefined, addable)]
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
    State :: addable | removable | none,
    Result :: #tr{}.
%% ====================================================================
storage_paths_table_row(StoragePath, Id, Disabled, Deletable) ->
    BinaryId = integer_to_binary(Id),
    TextboxId = <<"storage_path_textbox_", BinaryId/binary>>,
    {AddStoragePathDisplay, RemoveStoragePathDisplay} = case Deletable of
                                                            addable -> {<<"">>, <<" display: none;">>};
                                                            removable -> {<<" display: none;">>, <<"">>};
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
            } |
            lists:map(fun({Prefix, Title, Display, Postback, Label}) ->
                #th{
                    id = <<Prefix/binary, "th_", BinaryId/binary>>,
                    title = Title,
                    style = <<"text-align: center; vertical-align: inherit; padding: 0; width: 2em;", Display/binary>>,
                    body = #link{
                        title = Title,
                        actions = gui_jq:form_submit_action(<<Prefix/binary, BinaryId/binary>>, Postback, [TextboxId]),
                        class = <<"glyph-link">>,
                        body = #span{
                            id = <<Prefix/binary, BinaryId/binary>>,
                            class = Label,
                            style = <<"font-size: large;">>
                        }
                    }
                }
            end, [
                {<<"add_storage_path_">>, <<"Add">>, AddStoragePathDisplay, {add_storage_path, BinaryId}, <<"fui-plus">>},
                {<<"remove_storage_path_">>, <<"Remove">>, RemoveStoragePathDisplay, {remove_storage_path, BinaryId}, <<"fui-cross">>}
            ])
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
            onepanel_gui_utils:message(error, <<"Storage: ", (list_to_binary(StoragePath))/binary,
            ", is not available on hosts: ", (onepanel_gui_utils:format_list(ErrorHosts))/binary>>),
            error;
        _ ->
            onepanel_gui_utils:message(error, <<"Storage: ", (list_to_binary(StoragePath))/binary,
            ", is not available on all hosts">>),
            error
    end.


%% ====================================================================
%% Events handling
%% ====================================================================

%% comet_loop/1
%% ====================================================================
%% @doc Handles user's application configuration preferences.
%% @end
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{counter = Counter, db_config = DbConfig, session_config = #?CONFIG{workers = Workers, storage_paths = StoragePaths} = SessionConfig} = State) ->
    NewState = try
        receive
            render_storage_paths_table ->
                gui_jq:update(<<"storage_paths_table">>, storage_paths_table(DbConfig, SessionConfig)),
                gui_jq:fade_in(<<"storage_paths_table">>, 500),
                gui_jq:focus(<<"storage_path_textbox_", (integer_to_binary(Counter + 1))/binary>>),
                gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
                State;

            next ->
                NextState = case StoragePaths of
                                [] ->
                                    onepanel_gui_utils:message(error, <<"Please add at least one storage.">>),
                                    State;
                                _ ->
                                    case check_storage_paths(Workers, StoragePaths) of
                                        ok ->
                                            gui_ctx:put(?CONFIG_ID, SessionConfig),
                                            onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUMMARY),
                                            State;
                                        _ ->
                                            State
                                    end
                            end,
                gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
                gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"">>),
                NextState;

            back ->
                gui_ctx:put(?CONFIG_ID, SessionConfig),
                onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_SYSTEM_LIMITS),
                State;

            {add_storage_path, StorageId, StoragePath} ->
                NextState = case lists:member(StoragePath, StoragePaths) of
                                true ->
                                    onepanel_gui_utils:message(error, <<"Storage path already added.">>),
                                    State;
                                _ ->
                                    case installer_storage:check_storage_path_on_hosts(Workers, StoragePath) of
                                        ok ->
                                            gui_jq:hide(<<"add_storage_path_th_", StorageId/binary>>),
                                            gui_jq:show(<<"remove_storage_path_th_", StorageId/binary>>),
                                            gui_jq:prop(<<"storage_path_textbox_", StorageId/binary>>, <<"disabled">>, <<"disabled">>),
                                            gui_jq:insert_bottom(<<"storage_paths_table">>, storage_paths_table_row(<<"">>, Counter + 2, undefined, addable)),
                                            gui_jq:focus(<<"storage_path_textbox_", (integer_to_binary(Counter + 2))/binary>>),
                                            State#?STATE{counter = Counter + 1, session_config = SessionConfig#?CONFIG{storage_paths = [StoragePath | StoragePaths]}};
                                        {error, {hosts, Hosts}} ->
                                            onepanel_gui_utils:message(error, <<"Storage is not available on hosts: ",
                                            (onepanel_gui_utils:format_list(Hosts))/binary>>),
                                            State;
                                        _ ->
                                            onepanel_gui_utils:message(error, <<"Cannot add storage path.">>),
                                            State
                                    end
                            end,
                NextState;

            {remove_storage_path, StorageId, StoragePath} ->
                gui_jq:remove(<<"storage_path_row_", StorageId/binary>>),
                gui_jq:focus(<<"storage_path_textbox_", (integer_to_binary(Counter + 1))/binary>>),
                State#?STATE{session_config = SessionConfig#?CONFIG{storage_paths = lists:delete(StoragePath, StoragePaths)}}

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
        {ok, DbConfig} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, SessionConfig} = onepanel_gui_utils:get_session_config(),

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#?STATE{counter = length(SessionConfig#?CONFIG.storage_paths), db_config = DbConfig, session_config = SessionConfig})
        end),
        put(?COMET_PID, Pid),
        Pid ! render_storage_paths_table
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event({add_storage_path, StorageId}) ->
    StoragePath = binary_to_list(gui_ctx:postback_param(<<"storage_path_textbox_", StorageId/binary>>)),
    gui_jq:show(<<"main_spinner">>),
    case StoragePath of
        "" -> get(?COMET_PID) ! next;
        _ -> get(?COMET_PID) ! {add_storage_path, StorageId, StoragePath}
    end;

event({remove_storage_path, StorageId}) ->
    StoragePath = binary_to_list(gui_ctx:postback_param(<<"storage_path_textbox_", StorageId/binary>>)),
    gui_jq:show(<<"main_spinner">>),
    get(?COMET_PID) ! {remove_storage_path, StorageId, StoragePath};

event(next) ->
    gui_jq:show(<<"main_spinner">>),
    gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"disabled">>),
    gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"disabled">>),
    get(?COMET_PID) ! next;

event(back) ->
    get(?COMET_PID) ! back;

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.