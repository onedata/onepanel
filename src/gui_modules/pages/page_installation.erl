%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code
%% The page handles VeilCluster installation.
%% @end
%% ===================================================================

-module(page_installation).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").
-include("onepanel_modules/db_logic.hrl").
-include_lib("ctool/include/logging.hrl").

%% Record that holds current page state, that is installation configuration saved in database and user preferences from web page
-record(page_state, {counter = 1, main_ccm = undefined, ccms = sets:new(), workers = sets:new(), dbs = sets:new(), storage_paths = sets:new()}).

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
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]};
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
    <<"Installation">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    #panel{style = <<"position: relative;">>, body = [
        onepanel_gui_utils:top_menu(installation_tab),
        #panel{id = <<"error_message">>, style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
            class = <<"dialog dialog-danger">>},

        #panel{id = <<"step_1">>, style = <<"margin-top: 150px; text-align: center;">>, body = [
            #h6{style = <<"font-size: 18px;">>, body = <<"Step 1: Select cluster and database hosts.">>},
            #table{class = <<"table table-bordered">>, style = <<"width: 50%; margin: 0 auto; margin-top: 20px;">>, body = hosts_table_body()},
            #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
                #button{postback = {next, 1}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold;">>, body = <<"Next">>}
            ]}
        ]},

        #panel{id = <<"step_2">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
            #h6{style = <<"font-size: 18px;">>, body = <<"Step 2: Select primary Central Cluster Manager host.">>},
            main_ccm_dropdown_body(),
            #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
                #button{postback = {prev, 2}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-right: 200px;">>, body = <<"Back">>},
                #button{postback = {next, 2}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-left: 200px;">>, body = <<"Next">>}
            ]}
        ]},

        #panel{id = <<"step_3">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
            #h6{style = <<"font-size: 18px;">>, body = <<"Step 3: Add storage.">>},
            #table{class = <<"table table-striped">>, style = <<"width: 50%; margin: 0 auto; margin-top: 20px;">>, body = [
                #tbody{id = <<"storage_paths_table">>, body = storage_paths_table_body()}
            ]},
            #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
                #button{postback = {prev, 3}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-right: 200px;">>, body = <<"Back">>},
                #button{id = <<"next_3_button">>, postback = {next, 3}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-left: 200px;">>, body = <<"Next">>}
            ]}
        ]},

        #panel{id = <<"step_4">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
            #h6{style = <<"font-size: 18px;">>, body = <<"Step 4: Installation summary.">>},
            #table{class = <<"table table-striped">>, style = <<"width: 50%; margin: 0 auto; margin-top: 20px; margin-bottom: 50px;">>, body = [
                #tbody{id = <<"summary_table">>, body = []}
            ]},
            #panel{id = <<"progress">>, style = <<"margin-top: 30px; width: 50%; margin: 0 auto; display: none;">>, body = [
                #p{id = <<"progress_text">>, style = <<"font-weight: 300;">>, body = <<"">>},
                #panel{class = <<"progress">>, body = #panel{id = <<"bar">>, class = <<"bar">>, style = <<"width: 0%;">>}}
            ]},
            #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
                #button{postback = {prev, 4}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-right: 200px;">>, body = <<"Back">>},
                #button{id = <<"install_button">>, postback = install, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-left: 200px;">>, body = <<"Install">>}
            ]}
        ]},

        #panel{id = <<"step_5">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
            #panel{style = <<"width: 50%; margin: 0 auto;">>, body = registration_body()}
        ]}

    ] ++ onepanel_gui_utils:logotype_footer(120)}.


%% comet_loop/1
%% ====================================================================
%% @doc Handles messages that change installation preferences.
-spec comet_loop(PageState :: #page_state{}) -> no_return().
%% ====================================================================
comet_loop(#page_state{counter = Counter, main_ccm = MainCCM, ccms = CCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths} = PageState) ->
    ?debug("Main CCM: ~p", [MainCCM]),
    ?debug("CCMs: ~p", [sets:to_list(CCMs)]),
    ?debug("Workers: ~p", [sets:to_list(Workers)]),
    ?debug("Dbs: ~p", [sets:to_list(Dbs)]),
    ?debug("Storages: ~p", [sets:to_list(StoragePaths)]),
    try
        receive
            {ccm_checkbox_toggled, Host, HostId} ->
                case sets:is_element(Host, CCMs) of
                    true ->
                        case Host of
                            MainCCM ->
                                comet_loop(PageState#page_state{main_ccm = undefined, ccms = sets:del_element(Host, CCMs)});
                            _ ->
                                comet_loop(PageState#page_state{ccms = sets:del_element(Host, CCMs)})
                        end;
                    false ->
                        case sets:is_element(Host, Workers) of
                            true ->
                                comet_loop(PageState#page_state{ccms = sets:add_element(Host, CCMs)});
                            false ->
                                WorkerCheckboxId = <<"worker_checkbox_", HostId/binary>>,
                                gui_jq:click(WorkerCheckboxId),
                                gui_comet:flush(),
                                comet_loop(PageState#page_state{ccms = sets:add_element(Host, CCMs), workers = sets:add_element(Host, Workers)})
                        end
                end;

            {worker_checkbox_toggled, Host, HostId} ->
                case sets:is_element(Host, Workers) of
                    true ->
                        case sets:is_element(Host, CCMs) of
                            true ->
                                CCMCheckboxId = <<"ccm_checkbox_", HostId/binary>>,
                                gui_jq:click(CCMCheckboxId),
                                gui_comet:flush(),
                                comet_loop(PageState#page_state{ccms = sets:del_element(Host, CCMs), workers = sets:del_element(Host, Workers)});
                            false ->
                                comet_loop(PageState#page_state{workers = sets:del_element(Host, Workers)})
                        end;
                    _ ->
                        comet_loop(PageState#page_state{workers = sets:add_element(Host, Workers)})
                end;

            {db_checkbox_toggled, Host, _} ->
                case sets:is_element(Host, Dbs) of
                    true ->
                        comet_loop(PageState#page_state{dbs = sets:del_element(Host, Dbs)});
                    _ ->
                        comet_loop(PageState#page_state{dbs = sets:add_element(Host, Dbs)})
                end;

            {set_main_ccm, NewMainCCM} ->
                comet_loop(PageState#page_state{main_ccm = NewMainCCM});

            {add_storage_path, "", _} ->
                self() ! {next, 3},
                comet_loop(PageState);

            {add_storage_path, StoragePath, BinaryId} ->
                case sets:is_element(StoragePath, StoragePaths) of
                    true ->
                        error_message(<<"Storage path already added.">>),
                        comet_loop(PageState);
                    _ ->
                        case install_storage:check_storage_path_on_hosts(sets:to_list(Workers), StoragePath) of
                            ok ->
                                gui_jq:hide(<<"error_message">>),
                                gui_jq:hide(<<"add_storage_path_th_", BinaryId/binary>>),
                                gui_jq:show(<<"delete_storage_path_th_", BinaryId/binary>>),
                                gui_jq:wire(<<"$('#storage_path_textbox_", BinaryId/binary, "').prop('disabled', 'disabled');">>),
                                gui_jq:insert_bottom(<<"storage_paths_table">>, storage_paths_table_row(<<"">>, Counter + 1, undefined)),
                                gui_jq:focus(<<"storage_path_textbox_", (integer_to_binary(Counter + 1))/binary>>),
                                gui_comet:flush(),
                                comet_loop(PageState#page_state{counter = Counter + 1, storage_paths = sets:add_element(StoragePath, StoragePaths)});
                            {error, {hosts, Hosts}} ->
                                error_message(<<"Storage is not available on hosts: ", (format_list(Hosts))/binary>>),
                                comet_loop(PageState);
                            _ ->
                                error_message(<<"Cannot add storage path.">>),
                                comet_loop(PageState)
                        end
                end;

            {delete_storage_path, StoragePath, BinaryId} ->
                gui_jq:hide(<<"error_message">>),
                gui_jq:remove(<<"storage_path_row_", BinaryId/binary>>),
                gui_comet:flush(),
                comet_loop(PageState#page_state{storage_paths = sets:del_element(StoragePath, StoragePaths)});

            {next, 1} ->
                case (sets:size(CCMs) > 0) andalso (sets:size(Dbs) > 0) of
                    true ->
                        CCMsList = sets:to_list(CCMs),
                        NewMainCCM = case MainCCM of
                                         undefined -> hd(CCMsList);
                                         _ -> MainCCM
                                     end,
                        update_main_ccm_dropdown(NewMainCCM, CCMsList),
                        onepanel_gui_utils:change_step(1, 1),
                        gui_comet:flush(),
                        comet_loop(PageState#page_state{main_ccm = NewMainCCM});
                    _ ->
                        error_message(<<"Please select at least one CCM and database host.">>),
                        comet_loop(PageState)
                end;

            {next, 3} ->
                case sets:size(StoragePaths) > 0 of
                    true ->
                        case check_storage_paths(sets:to_list(Workers), sets:to_list(StoragePaths)) of
                            ok ->
                                onepanel_gui_utils:change_step(3, 1),
                                gui_jq:update(<<"summary_table">>, summary_table_body(get_page_state_diff(get_prev_page_state(), PageState))),
                                gui_comet:flush();
                            _ -> error
                        end;
                    _ ->
                        error_message(<<"Please add at least one storage.">>)
                end,
                comet_loop(PageState);

            install ->
                install(get_page_state_diff(get_prev_page_state(), PageState)),
                comet_loop(PageState);

            Other ->
                ?error("Comet process received unknown message: ~p", [Other]),
                comet_loop(PageState)
        end
    catch Type:Reason ->
        ?error("Comet process exception: ~p:~p", [Type, Reason]),
        error_message(<<"There has been an error in comet process. Please refresh the page.">>)
    end.


%% error_message/1
%% ====================================================================
%% @doc Renders an error message on top of a page.
-spec error_message(Message :: binary()) -> no_return().
%% ====================================================================
error_message(Message) ->
    gui_jq:update(<<"error_message">>, Message),
    gui_jq:fade_in(<<"error_message">>, 300),
    gui_comet:flush().


%% hosts_table_body/0
%% ====================================================================
%% @doc Renders hosts table body in first step of installation
-spec hosts_table_body() -> Result when
    Result :: [#tr{}].
%% ====================================================================
hosts_table_body() ->
    #page_state{ccms = CCMs, workers = Workers, dbs = Dbs} = get_prev_page_state(),
    hosts_table_body(CCMs, Workers, Dbs).

hosts_table_body(CCMs, Workers, Dbs) ->
    Hosts = lists:sort(install_utils:get_hosts()),
    ColumnStyle = <<"text-align: center; vertical-align: inherit;">>,
    Header = #tr{cells = [
        #th{body = <<"Host">>, style = ColumnStyle},
        #th{body = <<"CCM">>, style = ColumnStyle},
        #th{body = <<"Worker">>, style = ColumnStyle},
        #th{body = <<"Database">>, style = ColumnStyle}
    ]},
    Rows = lists:map(fun({Host, Id}) ->
        HostId = integer_to_binary(Id),
        Checkboxes = [
            {<<"ccm_checkbox_">>, sets:is_element(Host, CCMs), sets:size(CCMs) =/= 0},
            {<<"worker_checkbox_">>, sets:is_element(Host, Workers), sets:is_element(Host, Workers)},
            {<<"db_checkbox_">>, sets:is_element(Host, Dbs), sets:size(Dbs) =/= 0}
        ],
        #tr{id = <<"row_", HostId/binary>>, cells = [
            #td{body = <<"<b>", (list_to_binary(Host))/binary, "</b>">>, style = ColumnStyle} |
            lists:map(fun({Prefix, Checked, Disabled}) ->
                #td{body = #label{id = <<Prefix/binary, HostId/binary>>, class = <<"checkbox no-label">>, for = <<Prefix/binary, HostId/binary>>,
                    style = <<"width: 20px; margin: 0 auto;">>,
                    actions = gui_jq:postback_action(<<Prefix/binary, HostId/binary>>, {binary_to_atom(<<Prefix/binary, "toggled">>, latin1), Host, HostId, Disabled}),
                    body = [
                        #span{class = <<"icons">>},
                        #custom_checkbox{id = <<Prefix/binary, HostId/binary>>, data_fields = [{<<"data-toggle">>, <<"checkbox">>}],
                            value = <<"">>, checked = Checked, disabled = Disabled}
                    ]}, style = ColumnStyle}
            end, Checkboxes)
        ]}
    end, lists:zip(lists:sort(Hosts), lists:seq(1, length(Hosts)))),
    [Header | Rows].


%% main_ccm_dropdown_body/0
%% ====================================================================
%% @doc Renders main CCM dropdown body and highlights current choice
%% in second step of installation.
-spec main_ccm_dropdown_body() -> Result when
    Result :: #panel{}.
%% ====================================================================
main_ccm_dropdown_body() ->
    #page_state{main_ccm = MainCCM, ccms = CCMs} = get_prev_page_state(),
    main_ccm_dropdown_body(MainCCM, sets:to_list(CCMs), MainCCM =/= undefined).

main_ccm_dropdown_body(MainCCM, CCMs, Disabled) ->
    #panel{class = <<"btn-group">>, style = <<"margin: 12px 15px;">>, body = [
        <<"<i class=\"dropdown-arrow dropdown-arrow-inverse\"></i>">>,
        #button{id = <<"ccms_button">>, disabled = Disabled, class = <<"btn btn-inverse btn-small dropdown-toggle">>, style = <<"width: 280px;">>,
            data_fields = [{<<"data-toggle">>, <<"dropdown">>}], body = [
                #span{id = <<"ccms_label">>, class = <<"filter-option pull-left">>, body = <<"<b>Primary CCM host</b>">>},
                #span{class = <<"caret pull-right">>}
            ]},
        #list{id = <<"ccms_dropdown">>, class = <<"dropdown-menu dropdown-inverse">>,
            style = <<"overflow-y: auto; max-height: 200px;">>, body = ccms_list_body(MainCCM, CCMs)}
    ]}.


%% ccms_list_body/2
%% ====================================================================
%% @doc Renders CCMs' list body in second step of installation.
-spec ccms_list_body(MainCCM :: string(), CCMs :: [string()]) -> Result when
    Result :: [#li{}].
%% ====================================================================
ccms_list_body(_, []) ->
    [];
ccms_list_body(MainCCM, CCMs) ->
    lists:map(
        fun({CCM, Index}) ->
            Class = case CCM of
                        MainCCM -> <<"active">>;
                        _ -> <<"">>
                    end,
            CCMId = <<"ccm_li_", (integer_to_binary(Index))/binary>>,
            #li{id = CCMId, actions = gui_jq:postback_action(CCMId, {set_main_ccm, CCM, CCMs}),
                class = Class, body = #link{style = <<"text-align: left;">>, body = CCM}}
        end, lists:zip(lists:sort(CCMs), lists:seq(1, length(CCMs)))).


%% update_main_ccm_dropdown/2
%% ====================================================================
%% @doc Updates main CCM dropdown body in second step of installation.
-spec update_main_ccm_dropdown(MainCCM :: string(), CCMs :: [string()]) -> no_return().
%% ====================================================================
update_main_ccm_dropdown(MainCCM, CCMs) ->
    gui_jq:update(<<"ccms_label">>, <<"Primary CCM host: <b>", (list_to_binary(MainCCM))/binary, "</b>">>),
    gui_jq:update(<<"ccms_dropdown">>, ccms_list_body(MainCCM, CCMs)).


%% storage_paths_table_body/0
%% ====================================================================
%% @doc Renders storage table body in third step of installation.
-spec storage_paths_table_body() -> Result
    when Result :: [#tr{}].
%% ====================================================================
storage_paths_table_body() ->
    #page_state{storage_paths = StoragePaths} = get_prev_page_state(),
    storage_paths_table_body(sets:size(StoragePaths) =/= 0, sets:to_list(StoragePaths)).

storage_paths_table_body(_, []) ->
    [storage_paths_table_row(<<"">>, 1, undefined)];

storage_paths_table_body(Disabled, StoragePaths) ->
    lists:map(fun({StoragePath, Id}) ->
        storage_paths_table_row(StoragePath, Id, true)
    end, lists:zip(lists:sort(StoragePaths), lists:seq(1, length(StoragePaths))))
    ++ case Disabled of true -> []; _ -> [storage_paths_table_row(<<"">>, length(StoragePaths) + 1, undefined)] end.


%% storage_paths_table_row/3
%% ====================================================================
%% @doc Renders storage table row in third step of installation.
%% 'StoragePath' is a value that will be place in textbox with suffix
%% id equals 'Id'. When 'Disabled' equals true user cannot write in
%% textbox.
-spec storage_paths_table_row(StoragePath :: string() | binary(), Id :: integer(), Disabled :: true | undefined) -> Result
    when Result :: #tr{}.
%% ====================================================================
storage_paths_table_row(StoragePath, Id, Disabled) ->
    BinaryId = integer_to_binary(Id),
    TextboxId = <<"storage_path_textbox_", BinaryId/binary>>,
    {AddStoragePathDisplay, DeleteStoragePathDisplay} = case Disabled of
                                                            undefined -> {<<"">>, <<" display: none;">>};
                                                            _ -> {<<" display: none;">>, <<" display: none;">>}
                                                        end,
    gui_jq:bind_enter_to_submit_button(TextboxId, <<"add_storage_path_", BinaryId/binary>>),
    #tr{id = <<"storage_path_row_", BinaryId/binary>>, cells = [
        #th{body = #textbox{id = TextboxId, value = StoragePath, disabled = Disabled,
            placeholder = <<"Storage path">>, style = <<"width: 100%;">>},
            style = <<"text-align: center; vertical-align: inherit; padding-bottom: 0;">>},
        #th{id = <<"add_storage_path_th_", BinaryId/binary>>, body = #link{id = <<"add_storage_path_", BinaryId/binary>>,
            actions = gui_jq:form_submit_action(<<"add_storage_path_", BinaryId/binary>>,
                {add_storage_path, BinaryId}, [TextboxId]), class = <<"glyph-link">>,
            body = #span{class = <<"fui-plus">>, style = <<"font-size: 20px;">>}},
            style = <<"text-align: center; vertical-align: inherit; padding: 0; width: 20px;", AddStoragePathDisplay/binary>>},
        #th{id = <<"delete_storage_path_th_", BinaryId/binary>>, body = #link{id = <<"delete_storage_path_", BinaryId/binary>>,
            actions = gui_jq:form_submit_action(<<"delete_storage_path_", BinaryId/binary>>,
                {delete_storage_path, BinaryId}, [TextboxId]), class = <<"glyph-link">>,
            body = #span{class = <<"fui-cross">>, style = <<"font-size: 20px;">>}},
            style = <<"text-align: center; vertical-align: inherit; padding: 0; width: 20px;", DeleteStoragePathDisplay/binary>>}
    ]}.


%% summary_table_body/1
%% ====================================================================
%% @doc Renders summary table body in fourth step of installation.
-spec summary_table_body(PageState :: #page_state{}) -> Result
    when Result :: [#tr{}].
%% ====================================================================
summary_table_body(#page_state{main_ccm = MainCCM, ccms = CCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths}) ->
    [
        #tr{id = <<"summary_ccm">>, cells = [
            #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
                body = #p{style = <<"text-align: center; margin-bottom: 0;">>, body = <<"Primary CCM host">>}},
            #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
                body = #p{style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>, body =
                case MainCCM of undefined -> <<"-">>; _ -> list_to_binary(MainCCM) end
                }}
        ]},
        summary_table_row(<<"summary_ccms">>, <<"Optional CCM hosts">>, format_set(sets:del_element(MainCCM, CCMs))),
        summary_table_row(<<"summary_workers">>, <<"Worker hosts">>, format_set(Workers)),
        summary_table_row(<<"summary_Dbs">>, <<"Database hosts">>, format_set(Dbs)),
        summary_table_row(<<"summary_storages">>, <<"Storage paths">>, format_set(StoragePaths))
    ].


%% summary_table_row/3
%% ====================================================================
%% @doc Renders summary table row in fourth step of installation.
%% 'Description' is showed in first column and 'Details' in second one.
-spec summary_table_row(Id :: binary(), Description :: binary(), Details :: binary()) -> Result
    when Result :: #tr{}.
%% ====================================================================
summary_table_row(Id, Description, Details) ->
    #tr{id = Id, cells = [
        #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
            body = #p{style = <<"text-align: center; margin-bottom: 0;">>, body = Description}},
        #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>, body = Details}
    ]}.


%% registration_body/0
%% ====================================================================
%% @doc Renders registration body in fifth step of installation.
-spec registration_body() -> Result
    when Result :: [#panel{}].
%% ====================================================================
registration_body() ->
    case install_utils:get_provider_id() of
        undefined ->
            [
                #panel{class = <<"alert alert-success">>, body = [
                    #h3{body = <<"Successful installation.">>},
                    #p{body = <<"Would you like to register as a provider?">>},
                    #link{postback = finish, style = <<"width: 80px;">>, class = <<"btn btn-info">>, body = <<"Not now">>},
                    #link{postback = register, style = <<"width: 80px;">>, class = <<"btn btn-primary">>, body = <<"Register">>}
                ]}
            ];
        _ ->
            [
                #panel{class = <<"alert alert-success">>, body = [
                    #h3{body = <<"Successful installation.">>},
                    #link{postback = finish, style = <<"width: 80px;">>, class = <<"btn btn-primary">>, body = <<"OK">>}
                ]}
            ]
    end.


%% format_list/1
%% ====================================================================
%% @doc Returns list elements as a comma-delimited binary.
-spec format_list(List :: [string()]) -> Result when
    Result :: no_return().
%% ====================================================================
format_list([]) ->
    <<"">>;
format_list([Host | Hosts]) ->
    list_to_binary(lists:foldl(fun(Item, Acc) -> Acc ++ ", " ++ Item end, Host, Hosts)).


%% format_set/1
%% ====================================================================
%% @doc Returns set elements as a comma-delimited binary.
-spec format_set(Set :: sets:set()) -> Result when
    Result :: binary().
%% ====================================================================
format_set(Set) ->
    case sets:to_list(Set) of
        [] -> #p{body = <<"-">>, style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>};
        List ->
            lists:foldr(fun(Item, Acc) ->
                [#p{body = list_to_binary(Item), style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>} | Acc]
            end, [], List)
    end.


%% get_prev_page_state/0
%% ====================================================================
%% @doc Helper function that retrieves installation configuration from database.
-spec get_prev_page_state() -> Result when
    Result :: #page_state{}.
%% ====================================================================
get_prev_page_state() ->
    case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
        {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = undefined, opt_ccms = OptCCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths}} ->
            #page_state{ccms = sets:from_list(OptCCMs), workers = sets:from_list(Workers), dbs = sets:from_list(Dbs), storage_paths = sets:from_list(StoragePaths)};
        {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCM, opt_ccms = OptCCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths}} ->
            #page_state{main_ccm = MainCCM, ccms = sets:from_list([MainCCM | OptCCMs]), workers = sets:from_list(Workers), dbs = sets:from_list(Dbs), storage_paths = sets:from_list(StoragePaths)};
        _ ->
            #page_state{}
    end.


%% get_page_state_diff/2
%% ====================================================================
%% @doc Returns page state difference, that is difference between installation
%% configuration saved in database and current user's installation preferences.
-spec get_page_state_diff(PrevPageState :: #page_state{}, CurrPageState :: #page_state{}) -> Result when
    Result :: #page_state{}.
%% ====================================================================
get_page_state_diff(PrevPageState, CurrPageState) ->
    MainCCM = case PrevPageState#page_state.main_ccm =:= CurrPageState#page_state.main_ccm of
                  true -> undefined;
                  _ -> CurrPageState#page_state.main_ccm
              end,
    CCMs = sets:subtract(CurrPageState#page_state.ccms, PrevPageState#page_state.ccms),
    Workers = sets:subtract(CurrPageState#page_state.workers, PrevPageState#page_state.workers),
    Dbs = sets:subtract(CurrPageState#page_state.dbs, PrevPageState#page_state.dbs),
    StoragePaths = sets:subtract(CurrPageState#page_state.storage_paths, PrevPageState#page_state.storage_paths),
    #page_state{main_ccm = MainCCM, ccms = CCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths}.


%% check_storage_paths/2
%% ====================================================================
%% @doc Checks wheter all storage paths are available for all workers.
-spec check_storage_paths(Hosts :: [string()], StoragePath :: [string()]) -> Result when
    Result :: ok | error.
%% ====================================================================
check_storage_paths(_, []) ->
    ok;
check_storage_paths(Hosts, [StoragePath | StoragePaths]) ->
    case install_storage:check_storage_path_on_hosts(Hosts, StoragePath) of
        ok ->
            check_storage_paths(Hosts, StoragePaths);
        {error, {hosts, ErrorHosts}} ->
            error_message(<<"Storage: ", (list_to_binary(StoragePath))/binary, ", is not available on hosts: ", (format_list(ErrorHosts))/binary>>),
            error;
        _ ->
            error_message(<<"Storage: ", (list_to_binary(StoragePath))/binary, ", is not available on all hosts">>),
            error
    end.


%% install/1
%% ====================================================================
%% @doc Template installation method. Runs necessary installation
%% steps one by one.
-spec install(PageState :: #page_state{}) -> Result when
    Result :: ok | error.
%% ====================================================================
install(#page_state{main_ccm = undefined, workers = Workers, storage_paths = StoragePaths}) ->
    try
        WorkersList = sets:to_list(Workers),
        StoragePathsList = sets:to_list(StoragePaths),
        case sets:size(Workers) =:= 0 of
            true ->
                error_message(<<"Nothing to install.">>),
                throw(error);
            _ -> ok
        end,
        update_progress_bar(0, 3, <<"Installing worker nodes...">>),
        install_workers(WorkersList),
        update_progress_bar(1, 3, <<"Adding storage configuration...">>),
        add_storage(WorkersList, StoragePathsList),
        update_progress_bar(2, 3, <<"Starting worker nodes...">>),
        start_workers(WorkersList),
        update_progress_bar(3, 3, <<"Done">>),
        onepanel_gui_utils:change_step(4, 1),
        gui_comet:flush(),
        ok
    catch
        _:_ -> error
    end;

install(#page_state{main_ccm = MainCCM, ccms = CCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths}) ->
    try
        OptCCMsList = sets:to_list(sets:del_element(MainCCM, CCMs)),
        WorkersList = sets:to_list(Workers),
        DbsList = sets:to_list(Dbs),
        StoragePathsList = sets:to_list(StoragePaths),
        update_progress_bar(0, 8, <<"Installing database nodes...">>),
        install_dbs(DbsList),
        update_progress_bar(1, 8, <<"Starting database nodes...">>),
        start_dbs(DbsList),
        update_progress_bar(2, 8, <<"Installing CCM nodes...">>),
        install_ccms([MainCCM | OptCCMsList]),
        update_progress_bar(3, 8, <<"Starting CCM nodes...">>),
        start_ccms(MainCCM, OptCCMsList),
        update_progress_bar(4, 8, <<"Installing worker nodes...">>),
        install_workers(WorkersList),
        update_progress_bar(5, 8, <<"Adding storage configuration...">>),
        add_storage(WorkersList, StoragePathsList),
        update_progress_bar(6, 8, <<"Starting worker nodes...">>),
        start_workers(WorkersList),
        update_progress_bar(7, 8, <<"Finalizing installation...">>),
        finalize_installation(MainCCM),
        update_progress_bar(8, 8, <<"Done">>),
        onepanel_gui_utils:change_step(4, 1),
        gui_comet:flush(),
        ok
    catch
        _:_ -> error
    end.


%% install_dbs/1
%% ====================================================================
%% @doc Calls underlying installation function and in case of an error
%% renders appropriate message and throws exception.
-spec install_dbs(Dbs :: [string()]) -> Result when
    Result :: ok | no_return().
%% ====================================================================
install_dbs(Dbs) ->
    case install_db:install([{hosts, Dbs}]) of
        ok -> ok;
        {error, {hosts, ErrorHosts}} ->
            error_message(<<"Database nodes were not installed on following hosts: ", (format_list(ErrorHosts))/binary>>),
            throw(error);
        _ ->
            error_message(<<"Could not install database nodes.">>),
            throw(error)
    end.


%% start_dbs/1
%% ====================================================================
%% @doc Calls underlying installation function and in case of an error
%% renders appropriate message and throws exception.
-spec start_dbs(Dbs :: [string()]) -> Result when
    Result :: ok | no_return().
%% ====================================================================
start_dbs(Dbs) ->
    case install_db:start([{hosts, Dbs}]) of
        ok -> ok;
        {error, {hosts, ErrorHosts}} ->
            error_message(<<"Database nodes were not started on following hosts: ", (format_list(ErrorHosts))/binary>>),
            throw(error);
        _ ->
            error_message(<<"Could not start database nodes.">>),
            throw(error)
    end.


%% install_ccms/1
%% ====================================================================
%% @doc Calls underlying installation function and in case of an error
%% renders appropriate message and throws exception.
-spec install_ccms(CCMs :: [string()]) -> Result when
    Result :: ok | no_return().
%% ====================================================================
install_ccms(CCMs) ->
    case install_ccm:install([{hosts, CCMs}]) of
        ok -> ok;
        {error, {hosts, ErrorHosts}} ->
            error_message(<<"CCM nodes were not installed on following hosts: ", (format_list(ErrorHosts))/binary>>),
            throw(error);
        _ ->
            error_message(<<"Could not install CCM nodes.">>),
            throw(error)
    end.


%% start_ccms/2
%% ====================================================================
%% @doc Calls underlying installation function and in case of an error
%% renders appropriate message and throws exception.
-spec start_ccms(MainCCM :: string(), OptCCMs :: [string()]) -> Result when
    Result :: ok | no_return().
%% ====================================================================
start_ccms(MainCCM, OptCCMs) ->
    case install_ccm:start([{main_ccm, MainCCM}, {opt_ccms, OptCCMs}]) of
        ok -> ok;
        {error, {hosts, ErrorHosts}} ->
            error_message(<<"CCM nodes were not started on following hosts: ", (format_list(ErrorHosts))/binary>>),
            throw(error);
        _ ->
            error_message(<<"Could not start CCM nodes.">>),
            throw(error)
    end.


%% install_workers/1
%% ====================================================================
%% @doc Calls underlying installation function and in case of an error
%% renders appropriate message and throws exception.
-spec install_workers(Workers :: [string()]) -> Result when
    Result :: ok | no_return().
%% ====================================================================
install_workers(Workers) ->
    case install_worker:install([{hosts, Workers}]) of
        ok -> ok;
        {error, {hosts, ErrorHosts}} ->
            error_message(<<"Worker nodes were not installed on following hosts: ", (format_list(ErrorHosts))/binary>>),
            throw(error);
        _ ->
            error_message(<<"Could not install worker nodes.">>),
            throw(error)
    end.


%% start_workers/1
%% ====================================================================
%% @doc Calls underlying installation function and in case of an error
%% renders appropriate message and throws exception.
-spec start_workers(Workers :: [string()]) -> Result when
    Result :: ok | no_return().
%% ====================================================================
start_workers(Workers) ->
    case install_worker:start([{workers, Workers}]) of
        ok -> ok;
        {error, {hosts, ErrorHosts}} ->
            error_message(<<"Worker nodes were not started on following hosts: ", (format_list(ErrorHosts))/binary>>),
            throw(error);
        _ ->
            error_message(<<"Could not start worker nodes.">>),
            throw(error)
    end.


%% add_storage/2
%% ====================================================================
%% @doc Calls underlying installation function and in case of an error
%% renders appropriate message and throws exception.
-spec add_storage(Hosts :: [string()], StoragePaths :: [string()]) -> Result when
    Result :: ok | no_return().
%% ====================================================================
add_storage(Hosts, StoragePaths) ->
    lists:foreach(fun(StoragePath) ->
        case install_storage:add_storage_path(Hosts, StoragePath) of
            ok -> ok;
            {error, {hosts, ErrorHosts}} ->
                error_message(<<"Storage path ", (list_to_binary(StoragePath))/binary, " were not added on following hosts: ", (format_list(ErrorHosts))/binary>>),
                throw(error);
            _ ->
                error_message(<<"Could not add storage path ", (list_to_binary(StoragePath))/binary>>),
                throw(error)
        end
    end, StoragePaths).


%% finalize_installation/2
%% ====================================================================
%% @doc Waits until cluster control panel nodes are up and running.
-spec finalize_installation(MainCCM :: string()) -> Result when
    Result :: ok.
%% ====================================================================
finalize_installation(MainCCM) ->
    case install_utils:get_control_panel_hosts(MainCCM) of
        {ok, [_ | _]} ->
            ok;
        _ ->
            timer:sleep(1000),
            finalize_installation(MainCCM)
    end.


%% update_progress_bar/3
%% ====================================================================
%% @doc Updates installation progress bar. Parameter 'Elapsed' equals
%% amount of completed installation steps, 'Window' equals amount of
%% all installation steps. This function displays also text asociated
%% with progress bar.
-spec update_progress_bar(Elapsed :: integer(), Window :: integer(), Text :: binary()) -> no_return().
%% ====================================================================
update_progress_bar(Window, Window, _) ->
    gui_jq:wire(<<"$('#bar').width('0%');">>),
    gui_jq:wire(<<"$('#progress_text').text('');">>),
    gui_jq:hide(<<"progress">>),
    gui_comet:flush();
update_progress_bar(0, _, Text) ->
    gui_jq:wire(<<"$('#progress_text').text('", Text/binary, "');">>),
    gui_jq:show(<<"progress">>),
    gui_comet:flush();
update_progress_bar(Elapsed, Window, Text) ->
    Progress = <<"'", (integer_to_binary(round(Elapsed / Window * 100)))/binary, "%'">>,
    gui_jq:wire(<<"$('#progress_text').text('", Text/binary, "');">>),
    gui_jq:wire(<<"$('#bar').width(", Progress/binary, ");">>),
    gui_comet:flush().


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(get_prev_page_state()) end),
    put(comet_pid, Pid);

event({ccm_checkbox_toggled, _, _, true}) ->
    ok;

event({ccm_checkbox_toggled, HostName, HostId, _}) ->
    get(comet_pid) ! {ccm_checkbox_toggled, HostName, HostId};

event({worker_checkbox_toggled, _, _, true}) ->
    ok;

event({worker_checkbox_toggled, HostName, HostId, _}) ->
    get(comet_pid) ! {worker_checkbox_toggled, HostName, HostId};

event({db_checkbox_toggled, _, _, true}) ->
    ok;

event({db_checkbox_toggled, HostName, HostId, _}) ->
    get(comet_pid) ! {db_checkbox_toggled, HostName, HostId};

event({next, 2}) ->
    onepanel_gui_utils:change_step(2, 1);

event({next, Step}) ->
    get(comet_pid) ! {next, Step};

event({prev, Step}) ->
    onepanel_gui_utils:change_step(Step, -1);

event({set_main_ccm, CCM, CCMs}) ->
    update_main_ccm_dropdown(CCM, CCMs),
    get(comet_pid) ! {set_main_ccm, CCM};

event({add_storage_path, BinaryId}) ->
    StoragePath = binary_to_list(gui_ctx:postback_param(<<"storage_path_textbox_", BinaryId/binary>>)),
    get(comet_pid) ! {add_storage_path, StoragePath, BinaryId};

event({delete_storage_path, BinaryId}) ->
    StoragePath = binary_to_list(gui_ctx:postback_param(<<"storage_path_textbox_", BinaryId/binary>>)),
    get(comet_pid) ! {delete_storage_path, StoragePath, BinaryId};

event(install) ->
    get(comet_pid) ! install;

event(finish) ->
    gui_jq:redirect(<<"/">>);

event(register) ->
    gui_jq:redirect(<<"/registration">>);

event(terminate) ->
    ok.
