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
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

%% Convenience record abbreviation
-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {workers, storage_type, storages = []}).

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
            case installer_utils_adapter:get_workers() of
                [] ->
                    page_error:redirect_with_error(?SOFTWARE_NOT_INSTALLED_ERROR),
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
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, storage_link, [], true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Storage configuration.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"<i>Worker</i> components save and retrieve user's data from network file system"
                " storages.">>
            },
            #panel{
                style = <<"display: inline-block">>,
                body = [
                    #panel{
                        class = <<"btn-group">>,
                        body = storage_type_dropdown()
                    },
                    ceph_storage(),
                    dio_storage()
                ]
            },
            #table{
                class = <<"table table-striped">>,
                style = <<"width: 50%; margin: 0 auto;">>,
                body = #tbody{
                    id = <<"storage_paths_table">>,
                    style = <<"display: none;">>
                }
            }
        ]
    },
    onepanel_gui_utils:body(61, Header, Main, onepanel_gui_utils:logotype_footer()).


%% storage_type_dropdown/0
%% ====================================================================
%% @doc 
%% @end
-spec storage_type_dropdown() -> Result when
    Result :: [term()].
%% ====================================================================
storage_type_dropdown() ->
    [
        <<"<i class=\"dropdown-arrow dropdown-arrow-inverse\"></i>">>,
        #button{
            class = <<"btn btn-inverse dropdown-toggle">>,
            data_fields = [{<<"data-toggle">>, <<"dropdown">>}],
            body = [
                #span{
                    id = <<"storage_type_label">>,
                    style = <<"padding-right: 1em; min-width: 10em;">>,
                    class = <<"filter-option pull-left">>,
                    body = <<"Storage type: <b>Ceph</b>">>
                },
                #span{
                    class = <<"caret pull-right">>
                }
            ]
        },
        #list{
            id = <<"storage_type_dropdown">>,
            class = <<"dropdown-menu dropdown-inverse">>,
            style = <<"overflow-y: auto; max-height: 20em;">>,
            body = storage_type_list(<<"Ceph">>, [<<"Direct IO">>, <<"Ceph">>])
        }
    ].


%% storage_type_list/2
%% ====================================================================
%% @doc
%% @end
-spec storage_type_list(StorageType :: binary(), StorageTypes :: [binary()]) -> Result when
    Result :: [#li{}].
%% ====================================================================
storage_type_list(StorageType, StorageTypes) ->
    {Body, _} = lists:foldl(fun(Type, {List, Id}) ->
        TypeId = <<"storage_type_li_", (integer_to_binary(Id))/binary>>,
        {
            [#li{
                id = TypeId,
                actions = gui_jq:postback_action(TypeId, {set_storage_type, Type, StorageTypes}),
                class = case Type of
                    StorageType -> <<"active">>;
                    _ -> <<"">>
                end,
                body = #link{
                    style = <<"text-align: left;">>,
                    body = http_utils:html_encode(Type)
                }
            }, List],
            Id + 1
        }
    end, {[], 1}, StorageTypes),
    Body.

ceph_storage() ->
    #panel{
        id = <<"ceph_storage">>,
        style = <<"margin-top: 0.75em;">>,
        actions = gui_jq:form_submit_action(<<"ceph_submit">>,
            ceph_submit, [<<"ceph_storage_name">>, <<"ceph_mon_host">>,
                <<"ceph_cluster_name">>, <<"ceph_pool_name">>]),
        body = [
            #textbox{
                id = <<"ceph_storage_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Storage name">>
            },
            #textbox{
                id = <<"ceph_mon_host">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Monitor host">>
            },
            #textbox{
                id = <<"ceph_cluster_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Cluster name">>
            },
            #textbox{
                id = <<"ceph_pool_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Pool name">>
            },
            #button{
                id = <<"ceph_submit">>,
                class = <<"btn btn-inverse">>,
                style = <<"width: 10em;">>,
                body = <<"Add">>
            }
        ]
    }.

dio_storage() ->
    #panel{
        id = <<"dio_storage">>,
        style = <<"margin-top: 0.75em; display: none;">>,
        actions = gui_jq:form_submit_action(<<"dio_submit">>,
            dio_submit, [<<"dio_storage_name">>, <<"dio_mount_point">>]),
        body = [
            #textbox{
                id = <<"dio_storage_name">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Storage name">>
            },
            #textbox{
                id = <<"dio_mount_point">>,
                style = <<"width: 30em; display: block">>,
                placeholder = <<"Mount point">>
            },
            #button{
                id = <<"dio_submit">>,
                class = <<"btn btn-inverse">>,
                style = <<"width: 10em;">>,
                body = <<"Add">>
            }
        ]
    }.

%% storages_table/1
%% ====================================================================
%% @doc Renders storage table body.
%% @end
-spec storages_table(Storages :: list()) -> Result
    when Result :: [#tr{}].
%% ====================================================================
storages_table(Storages) ->
    ?dump(Storages),
    [].


%%%% storage_paths_table_row/3
%%%% ====================================================================
%%%% @doc Renders storage table row. 'StoragePath' is a value that will
%%%% be placed in textbox with suffix id equals 'Id'. When 'Disabled'
%%%% equals true user cannot write in textbox.
%%%% @end
%%-spec storage_paths_table_row(StoragePath, Id, Disabled, State) -> Result when
%%    StoragePath :: string() | binary(),
%%    Id :: integer(),
%%    Disabled :: true | undefined,
%%    State :: addable | removable | none,
%%    Result :: #tr{}.
%%%% ====================================================================
%%storage_paths_table_row(StoragePath, Id, Disabled, Deletable) ->
%%    BinaryId = integer_to_binary(Id),
%%    TextboxId = <<"storage_path_textbox_", BinaryId/binary>>,
%%    {AddStoragePathDisplay, RemoveStoragePathDisplay} = case Deletable of
%%        addable -> {<<"">>, <<" display: none;">>};
%%        removable -> {<<" display: none;">>, <<"">>};
%%        _ -> {<<" display: none;">>, <<" display: none;">>}
%%    end,
%%    gui_jq:bind_enter_to_submit_button(TextboxId, <<"add_storage_path_", BinaryId/binary>>),
%%    #tr{
%%        id = <<"storage_path_row_", BinaryId/binary>>,
%%        cells = [
%%            #th{
%%                style = <<"text-align: center; vertical-align: inherit; padding-bottom: 0;">>,
%%                body = #textbox{
%%                    id = TextboxId,
%%                    value = http_utils:html_encode(StoragePath),
%%                    disabled = Disabled,
%%                    placeholder = <<"Storage path">>,
%%                    style = <<"width: 100%;">>
%%                }
%%            } |
%%            lists:map(fun({Prefix, Title, Display, Postback, Label}) ->
%%                #th{
%%                    id = <<Prefix/binary, "th_", BinaryId/binary>>,
%%                    title = Title,
%%                    style = <<"text-align: center; vertical-align: inherit; padding: 0; width: 2em;", Display/binary>>,
%%                    body = #link{
%%                        title = Title,
%%                        actions = gui_jq:form_submit_action(<<Prefix/binary, BinaryId/binary>>, Postback, [TextboxId]),
%%                        class = <<"glyph-link">>,
%%                        body = #span{
%%                            id = <<Prefix/binary, BinaryId/binary>>,
%%                            class = Label,
%%                            style = <<"font-size: large;">>
%%                        }
%%                    }
%%                }
%%            end, [
%%                {<<"add_storage_path_">>, <<"Add">>, AddStoragePathDisplay, {add_storage_path, BinaryId}, <<"fui-plus">>},
%%                {<<"remove_storage_path_">>, <<"Remove">>, RemoveStoragePathDisplay, {remove_storage_path, BinaryId}, <<"fui-cross">>}
%%            ])
%%        ]
%%    }.

clear_textboxes() ->
    lists:foreach(fun(Id) ->
        gui_jq:set_value(Id, <<"''">>)
    end, [<<"ceph_storage_name">>, <<"ceph_mon_host">>, <<"ceph_cluster_name">>,
        <<"ceph_pool_name">>, <<"dio_storage_name">>, <<"dio_mount_point">>]).

strip(Text) ->
    list_to_binary(string:strip(binary_to_list(Text))).

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

comet_loop(#?STATE{storage_type = StorageType, workers = Workers} = State) ->
    NewState = try
        receive
            render_storages_table ->
                {ok, Storages} = onepanel_utils:dropwhile_failure(Workers, storage,
                    list, [], ?RPC_TIMEOUT),
                gui_jq:update(<<"storage_paths_table">>, storages_table(Storages)),
                gui_jq:fade_in(<<"storage_paths_table">>, 500),
                gui_jq:focus(<<"ceph_storage_name">>),
                State;

            {set_storage_type, StorageType} ->
                State;

            {set_storage_type, <<"Ceph">> = SType} ->
                gui_jq:show(<<"ceph_storage">>),
                gui_jq:hide(<<"dio_storage">>),
                clear_textboxes(),
                gui_jq:focus(<<"ceph_storage_name">>),
                gui_jq:bind_enter_to_submit_button(<<"ceph_pool_name">>, <<"ceph_submit">>),
                State#?STATE{storage_type = SType};

            {set_storage_type, <<"Direct IO">> = SType} ->
                gui_jq:show(<<"dio_storage">>),
                gui_jq:hide(<<"ceph_storage">>),
                clear_textboxes(),
                gui_jq:focus(<<"dio_storage_name">>),
                gui_jq:bind_enter_to_submit_button(<<"dio_mount_point">>, <<"dio_submit">>),
                State#?STATE{storage_type = SType};

            {ceph_submit, <<>>, _, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide storage name.">>),
                State;

            {ceph_submit, _, <<>>, _, _} ->
                onepanel_gui_utils:message(error, <<"Please provide monitor host.">>),
                State;

            {ceph_submit, _, _, <<>>, _} ->
                onepanel_gui_utils:message(error, <<"Please provide cluster name.">>),
                State;

            {ceph_submit, _, _, _, <<>>} ->
                onepanel_gui_utils:message(error, <<"Please provide pool name.">>),
                State;

            {ceph_submit, StorageName, MonHost, ClusterName, PoolName} ->
                case installer_storage:add_ceph_storage(Workers, StorageName, MonHost, ClusterName, PoolName) of
                    ok ->
                        onepanel_gui_utils:message(success, <<"Storage successfully added.">>),
                        clear_textboxes(),
                        gui_jq:focus(<<"ceph_storage_name">>),
                        self() ! render_storages_table;
                    error ->
                        onepanel_gui_utils:message(error, <<"There has been an error while adding storage. Please try again later.">>)
                end,
                State;

            {dio_submit, <<>>, _} ->
                onepanel_gui_utils:message(error, <<"Please provide storage name.">>),
                State;

            {dio_submit, _, <<>>} ->
                onepanel_gui_utils:message(error, <<"Please provide mount point.">>),
                State;

            {dio_submit, StorageName, MountPoint} ->
                case installer_storage:add_dio_storage(Workers, StorageName, MountPoint) of
                    ok ->
                        onepanel_gui_utils:message(success, <<"Storage successfully added.">>),
                        clear_textboxes(),
                        gui_jq:focus(<<"dio_storage_name">>),
                        self() ! render_storages_table;
                    {error, {hosts, [EHost | EHosts]}} ->
                        BHosts = lists:foldl(fun(H, Acc) ->
                            <<Acc/binary, ", ", (list_to_binary(H))/binary>>
                        end, list_to_binary(EHost), EHosts),
                        onepanel_gui_utils:message(error, <<"Storage not avaliable on following hosts: ", BHosts/binary>>);
                    {error, _} ->
                        onepanel_gui_utils:message(error, <<"There has been an error while adding storage. Please try again later.">>)
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
        {ok, #?CONFIG{workers = Workers}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#?STATE{workers = onepanel_utils:get_nodes("worker", Workers)})
        end),
        put(?COMET_PID, Pid),
        Pid ! render_storages_table
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event({set_storage_type, Type, StorageTypes}) ->
    gui_jq:show(<<"main_spinner">>),
    gui_jq:update(<<"storage_type_label">>, <<"Storage type: <b>", Type/binary, "</b>">>),
    gui_jq:update(<<"storage_type_dropdown">>, storage_type_list(Type, StorageTypes)),
    get(?COMET_PID) ! {set_storage_type, Type};

event(ceph_submit) ->
    gui_jq:show(<<"main_spinner">>),
    StorageName = gui_ctx:postback_param(<<"ceph_storage_name">>),
    MonHost = gui_ctx:postback_param(<<"ceph_mon_host">>),
    ClusterName = gui_ctx:postback_param(<<"ceph_cluster_name">>),
    PoolName = gui_ctx:postback_param(<<"ceph_pool_name">>),
    get(?COMET_PID) ! {ceph_submit, strip(StorageName), strip(MonHost), strip(ClusterName), strip(PoolName)};

event(dio_submit) ->
    gui_jq:show(<<"main_spinner">>),
    StorageName = gui_ctx:postback_param(<<"dio_storage_name">>),
    MountPoint = gui_ctx:postback_param(<<"dio_mount_point">>),
    get(?COMET_PID) ! {dio_submit, strip(StorageName), strip(MountPoint)};

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.
