%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code
%% @end
%% ===================================================================

-module(page_installation).
-compile(export_all).
-include("gui_modules/common.hrl").
-include("spanel_modules/db.hrl").

-record(page_state, {counter = 1, main_ccm = undefined, ccms = sets:new(), workers = sets:new(), dbs = sets:new(), storage_paths = sets:new()}).

%% Template points to the template file, which will be filled with content
main() -> case gui_utils:user_logged_in() of
            true ->
              #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}]};
            false ->
              gui_utils:redirect_to_login(true),
              #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}]}
          end.

%% Page title
title() -> <<"Installation">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
  #panel{style = <<"position: relative;">>, body = [
    gui_utils:top_menu(installation_tab),
    #panel{id = <<"error_message">>, style = <<"position: fixed; width: 100%; top: 55px; z-index: 2; display: none;">>,
      class = <<"dialog dialog-danger">>},
    #panel{id = <<"success_message">>, style = case wf:q(<<"x">>) of
                                                 undefined -> <<"display: none;">>;
                                                 _ -> <<"position: fixed; width: 100%; top: 55px; z-index: 1;">>
                                               end,
      class = <<"dialog dialog-success">>, body = <<"Installation successful.">>},

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
        #tbody{id = <<"storage_table">>, body = storage_table_body()}
      ]},
      #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
        #button{postback = {prev, 3}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-right: 200px;">>, body = <<"Back">>},
        #button{postback = {next, 3}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-left: 200px;">>, body = <<"Next">>}
      ]}
    ]},

    #panel{id = <<"step_4">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
      #h6{style = <<"font-size: 18px;">>, body = <<"Step 4: Installation summary.">>},
      #table{class = <<"table table-striped">>, style = <<"width: 50%; margin: 0 auto; margin-top: 20px; margin-bottom: 50px;">>, body = [
        #tbody{id = <<"summary_table">>, body = summary_table_body()}
      ]},
      #panel{id = <<"progress">>, style = <<"margin-top: 30px; width: 50%; margin: 0 auto; display: none;">>, body = [
        #p{id = <<"progress_text">>, style = <<"font-weight: 300;">>, body = <<"">>},
        #panel{class = <<"progress">>, body = #panel{id = <<"bar">>, class = <<"bar">>, style = <<"width: 0%;">>}}
      ]},
      #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
        #button{id = <<"back_button">>, postback = {prev, 4}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-right: 200px;">>, body = <<"Back">>},
        #button{id = <<"install_button">>, postback = install, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-left: 200px;">>, body = <<"Install">>}
      ]}
    ]},

    #panel{id = <<"step_5">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
      #h6{style = <<"font-size: 18px;">>, body = <<"Step 5: Connectivity checkup.">>},
      #table{class = <<"table table-bordered">>, style = <<"width: 50%; margin: 0 auto; margin-top: 20px; margin-bottom: 50px;">>, body = [
        #tbody{id = <<"ports_table">>, body = ports_table_body()}
      ]},
      #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
        #button{postback = {next, 5}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold;">>, body = <<"Next">>}
      ]}
    ]},

    #panel{id = <<"step_6">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
      #panel{id = <<"registration">>, body = registration_body()}
    ]}

  ]}.

comet_loop(#page_state{counter = Counter, main_ccm = MainCCM, ccms = CCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths} = PageState) ->
  lager:info("CCM: ~p", [MainCCM]),
  lager:info("CCMs: ~p", [sets:to_list(CCMs)]),
  lager:info("Workers: ~p", [sets:to_list(Workers)]),
  lager:info("Dbs: ~p~n", [sets:to_list(Dbs)]),
  lager:info("Storages: ~p~n~n", [sets:to_list(StoragePaths)]),
  try
    receive
      {ccm_checkbox_toggled, Host, HostId} ->
        case sets:is_element(Host, CCMs) of
          true ->
            case Host of
              MainCCM -> comet_loop(PageState#page_state{main_ccm = undefined, ccms = sets:del_element(Host, CCMs)});
              _ -> comet_loop(PageState#page_state{ccms = sets:del_element(Host, CCMs)})
            end;
          false ->
            case sets:is_element(Host, Workers) of
              true -> ok;
              false ->
                WorkerCheckboxId = "worker_checkbox_" ++ binary_to_list(HostId),
                wf:wire(#jquery{target = WorkerCheckboxId, method = ["checkbox"], args = ["\"check\""]}),
                gui_utils:flush()
            end,
            comet_loop(PageState#page_state{ccms = sets:add_element(Host, CCMs)})
        end;

      {worker_checkbox_toggled, Host, HostId} ->
        case sets:is_element(Host, Workers) of
          true ->
            case sets:is_element(Host, CCMs) of
              true ->
                CCMCheckboxId = "ccm_checkbox_" ++ binary_to_list(HostId),
                wf:wire(#jquery{target = CCMCheckboxId, method = ["checkbox"], args = ["\"uncheck\""]}),
                gui_utils:flush();
              false -> ok
            end,
            comet_loop(PageState#page_state{workers = sets:del_element(Host, Workers)});
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

      {add_storage, StoragePath, Id} ->
        case sets:is_element(StoragePath, StoragePaths) of
          true ->
            error_message(<<"Storage already added.">>),
            comet_loop(PageState);
          _ ->
            case install_storage:check_storage_on_hosts(sets:to_list(Workers), StoragePath) of
              ok ->
                wf:wire(#jquery{target = "error_message", method = ["hide"], args = []}),
                wf:wire(#jquery{target = "add_storage_row_" ++ integer_to_list(Id), method = ["hide"], args = []}),
                wf:wire(#jquery{target = "delete_storage_row_" ++ integer_to_list(Id), method = ["show"], args = []}),
                wf:wire(#jquery{target = "storage_textbox_" ++ integer_to_list(Id), method = ["prop"], args = ["\"disabled\"", "\"disabled\""]}),
                gui_utils:insert_bottom("storage_table", storage_table_row(<<"">>, Counter + 1, undefined)),
                gui_utils:flush(),
                comet_loop(PageState#page_state{counter = Counter + 1, storage_paths = sets:add_element(StoragePath, StoragePaths)});
              {error, Hosts} ->
                error_message(<<"Storage is not available on hosts: ", (format_error_message(Hosts))/binary>>),
                comet_loop(PageState)
            end
        end;

      {delete_storage, StoragePath, Id} ->
        wf:wire(#jquery{target = "storage_row_" ++ integer_to_list(Id), method = ["remove"], args = []}),
        gui_utils:flush(),
        comet_loop(PageState#page_state{storage_paths = sets:del_element(StoragePath, StoragePaths)});

      {next, 1} ->
        case (sets:size(CCMs) > 0) andalso (sets:size(Dbs) > 0) of
          true ->
            update_main_ccm_dropdown(MainCCM, sets:to_list(CCMs)),
            change_step(1, 1),
            gui_utils:flush(),
            comet_loop(PageState);
          _ ->
            error_message(<<"Please select at least one CCM and database host.">>),
            comet_loop(PageState)
        end;

      {next, 2} ->
        case MainCCM of
          undefined ->
            error_message(<<"Please select primary Central Cluster Manager host.">>);
          _ ->
            change_step(2, 1),
            gui_utils:flush()
        end,
        comet_loop(PageState);

      {next, 3} ->
        case sets:size(StoragePaths) > 0 of
          true ->
            case check_storage(sets:to_list(Workers), sets:to_list(StoragePaths)) of
              ok ->
                change_step(3, 1),
                gui_utils:update("summary_table", summary_table_body(PageState)),
                gui_utils:flush();
              _ -> error
            end;
          _ ->
            error_message(<<"Please add at least one storage.">>)
        end,
        comet_loop(PageState);

      {next, 5} ->
        try
          {ok, Hosts} = install_utils:get_control_panel_hosts(MainCCM),
          lists:foreach(fun(Host) ->
            {ok, #port{gui = Gui, rest = Rest}} = dao:get_record(ports, Host),
            ok = global_registry:check_port(Host, Gui, "gui"),
            ok = global_registry:check_port(Host, Rest, "rest")
          end, Hosts),
          case global_registry:register() of
            ok ->
              gui_utils:update("registration", registration_body()),
              gui_utils:flush();
            _ -> error_message(<<"Registration failure. Please reinstall VeilCluster.">>)
          end,
          change_step(5, 1),
          gui_utils:flush()
        catch
          _:_ -> error_message(<<"Some ports are not available. Please change configuration.">>)
        end,
        comet_loop(PageState);

      {next, 6} ->
        wf:redirect(<<"/installation">>),
        gui_utils:flush(),
        comet_loop(PageState);

      install ->
        install(get_saved_page_state(), {MainCCM, sets:to_list(sets:del_element(MainCCM, CCMs)), sets:to_list(Workers),
          sets:to_list(Dbs), sets:to_list(StoragePaths)}),
        comet_loop(PageState);

      Other ->
        lager:error("Comet process received unknown message: ~p", [Other]),
        comet_loop(PageState)
    end
  catch _:_ ->
    error_message(<<"There has been an error in comet process. Please refresh the page.">>)
  end.

% Displays error message
error_message(Message) ->
  gui_utils:update("error_message", Message),
  wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]}),
  gui_utils:flush().

% Returns elements of list as a comma-delimited string
format_error_message([]) ->
  <<"">>;
format_error_message([Host | Hosts]) ->
  list_to_binary(lists:foldl(fun(Item, Acc) -> Acc ++ ", " ++ Item end, Host, Hosts)).

% Renders hosts table bidy in first step of installation
hosts_table_body() ->
  #page_state{ccms = CCMs, workers = Workers, dbs = Dbs} = get_saved_page_state(),
  hosts_table_body(CCMs, Workers, Dbs).

hosts_table_body(CCMs, Workers, Dbs) ->
  Hosts = install_utils:get_hosts(),
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
        #td{body = #custom_checkbox{
          id = <<Prefix/binary, HostId/binary>>,
          class = <<"checkbox no-label">>,
          style = <<"width: 20px; margin: 0 auto;">>,
          checked = Checked,
          disabled = Disabled,
          postback = {binary_to_atom(<<Prefix/binary, "toggled">>, latin1), Host, HostId}
        }, style = ColumnStyle}
      end, Checkboxes)
    ]}
  end, lists:zip(lists:sort(Hosts), lists:seq(1, length(Hosts)))),
  [Header | Rows].

% Renders main ccm dropdown body and highlights current choice in second step of installation
main_ccm_dropdown_body() ->
  #page_state{main_ccm = MainCCM, ccms = CCMs} = get_saved_page_state(),
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

% Renders ccms' list body
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
      #li{id = CCMId, actions = #event{type = "click", postback = {set_main_ccm, CCM, CCMs}, target = CCMId},
        class = Class, body = #link{style = <<"text-align: left;">>, body = CCM}}
    end, lists:zip(lists:sort(CCMs), lists:seq(1, length(CCMs)))).

% Updates main ccm dropdown body
update_main_ccm_dropdown(MainCCM, CCMs) ->
  case MainCCM of
    undefined -> gui_utils:update("ccms_label", <<"<b>Primary CCM host</b>">>);
    _ -> gui_utils:update("ccms_label", <<"Primary CCM host: <b>", (list_to_binary(MainCCM))/binary, "</b>">>)
  end,
  gui_utils:update("ccms_dropdown", ccms_list_body(MainCCM, CCMs)).

% Renders storage table body
storage_table_body() ->
  #page_state{main_ccm = MainCCM, storage_paths = StoragePaths} = get_saved_page_state(),
  lists:map(fun({StoragePath, Id}) ->
    storage_table_row(StoragePath, Id, true)
  end, lists:zip(lists:sort(sets:to_list(StoragePaths)), lists:seq(1, sets:size(StoragePaths))))
  ++ if MainCCM =/= undefined -> []; true -> [storage_table_row(<<"">>, sets:size(StoragePaths) + 1, undefined)] end.

% Renders storage table row
storage_table_row(StoragePath, Id, Disabled) ->
  StorageId = list_to_atom("storage_textbox_" ++ integer_to_list(Id)),
  {AddStorageDisplay, DeleteStorageDisplay} = case Disabled of
                                                undefined -> {<<"">>, <<" display: none;">>};
                                                _ -> {<<" display: none;">>, <<" display: none;">>}
                                              end,
  #tr{id = <<"storage_row_", (integer_to_binary(Id))/binary>>, cells = [
    #th{body = #textbox{id = StorageId, value = StoragePath, disabled = Disabled, placeholder = <<"Storage path">>, style = <<"width: 100%;">>},
      style = <<"text-align: center; vertical-align: inherit; padding-bottom: 0;">>},
    #th{id = <<"add_storage_row_", (integer_to_binary(Id))/binary>>,
      body = #link{postback = {add_storage, Id, StorageId}, source = [StorageId], class = <<"glyph-link">>,
        body = #span{class = <<"fui-plus">>, style = <<"font-size: 20px;">>}},
      style = <<"text-align: center; vertical-align: inherit; padding: 0; width: 20px;", AddStorageDisplay/binary>>},
    #th{id = <<"delete_storage_row_", (integer_to_binary(Id))/binary>>,
      body = #link{postback = {delete_storage, Id, StorageId}, source = [StorageId], class = <<"glyph-link">>,
        body = #span{class = <<"fui-cross">>, style = <<"font-size: 20px;">>}},
      style = <<"text-align: center; vertical-align: inherit; padding: 0; width: 20px;", DeleteStorageDisplay/binary>>}
  ]}.

% Renders summary teble body
summary_table_body() ->
  summary_table_body(get_saved_page_state()).
summary_table_body(PageState) ->
  PrevPageState = get_saved_page_state(),
  #page_state{main_ccm = MainCCM, ccms = CCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths} =
    get_page_state_diff(PrevPageState, PageState),
  [
    #tr{id = <<"summary_ccm">>, cells = [
      #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
        body = #p{style = <<"text-align: center; margin-bottom: 0;">>, body = <<"Primary CCM host">>}},
      #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
        body = #p{style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>, body =
        case MainCCM of undefined -> <<"-">>; _ -> list_to_binary(MainCCM) end
        }}
    ]},
    summary_table_row(<<"summary_ccms">>, <<"Optional CCM hosts">>, format_sets_items(sets:del_element(MainCCM, CCMs))),
    summary_table_row(<<"summary_workers">>, <<"Worker hosts">>, format_sets_items(Workers)),
    summary_table_row(<<"summary_Dbs">>, <<"Database hosts">>, format_sets_items(Dbs)),
    summary_table_row(<<"summary_storages">>, <<"Storage paths">>, format_sets_items(StoragePaths))
  ].

% Renders summary table row
summary_table_row(Id, Description, Details) ->
  #tr{id = Id, cells = [
    #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
      body = #p{style = <<"text-align: center; margin-bottom: 0;">>, body = Description}},
    #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>, body = Details}
  ]}.

% Renders ports table bidy in fifth step of installation
ports_table_body() ->
  ColumnStyle = <<"text-align: center; vertical-align: inherit;">>,
  [
    #tr{cells = [
      #th{body = <<"Host">>, style = ColumnStyle},
      #th{body = <<"Port">>, style = ColumnStyle},
      #th{body = <<"Description">>, style = ColumnStyle},
      #th{body = <<"Status">>, style = ColumnStyle}
    ]}
  ].

ports_table_body(MainCCM) ->
  [Header] = ports_table_body(),
  Body = case install_utils:get_control_panel_hosts(MainCCM) of
           {ok, []} -> [];
           {ok, Hosts} -> lists:foldl(fun({Host, Id}, Acc) ->
             Acc ++ ports_table_rows(Host, Id) end, [], lists:zip(Hosts, lists:seq(1, length(Hosts))));
           _ -> []
         end,
  [Header | Body].

ports_table_rows(Host, Start) ->
  ColumnStyle = <<"text-align: center; vertical-align: inherit;">>,
  {ok, #port{gui = GuiPort, rest = RestPort}} = dao:get_record(ports, Host),
  lists:map(fun({{Type, Port}, Id}) ->
    Active = global_registry:check_port(Host, Port, Type),
    #tr{cells = [
      #th{body = list_to_binary(Host), style = ColumnStyle},
      #th{body = case Active of
                   ok ->
                     #textbox{maxlength = 6, value = integer_to_binary(Port), disabled = true,
                       style = <<"width: 60px; margin-bottom: 0px; text-align: center;">>};
                   _ ->
                     BId = <<"port_button_", (integer_to_binary(Start + Id))/binary>>,
                     TBId = list_to_atom("port_textbox_" ++ integer_to_list(Start + Id)),
                     #panel{body = [
                       #textbox{id = TBId, maxlength = 6, placeholder = integer_to_binary(Port),
                         style = <<"width: 60px; margin-bottom: 0px; margin-right: 20px; text-align: center;">>},
                       #button{id = BId, postback = {check_port, Host, Type, Start + Id}, source = [TBId],
                         class = <<"btn btn-inverse btn-small">>,
                         style = <<"font-weight: bold;">>, body = <<"Check">>}
                     ]}
                 end, style = ColumnStyle},
      #th{body = case Type of "gui" -> "GUI"; _ -> "REST" end, style = ColumnStyle},
      #th{body = case Active of
                   ok ->
                     #button{id = <<"port_status_", (integer_to_binary(Start + Id))/binary>>, class = <<"btn btn-primary btn-small">>,
                       disabled = true, style = <<"font-weight: bold;">>, body = <<"OK">>};
                   _ ->
                     #button{id = <<"port_status_", (integer_to_binary(Start + Id))/binary>>, class = <<"btn btn-danger btn-small btn-primary">>,
                       disabled = true, style = <<"font-weight: bold;">>, body = <<"Error">>}
                 end, style = ColumnStyle}
    ]}
  end, [{{"gui", GuiPort}, 0}, {{"rest", RestPort}, 1}]).

registration_body() ->
  case dao:get_record(configurations, last) of
    {ok, #configuration{providerId = ProviderId}} when ProviderId =/= undefined ->
      #panel{class = <<"alert alert-success login-page">>, body = [
        #h3{class = <<"">>, body = <<"Successful registration.">>},
        #p{class = <<"login-info">>, body = <<"Your provider ID: ", ProviderId/binary>>},
        #button{postback = finish, class = <<"btn btn-primary btn-block">>, body = <<"Finish">>}
      ]};
    _ ->
      #panel{class = <<"alert alert-danger login-page">>, body = [
        #h3{body = <<"Error">>},
        #p{class = <<"login-info">>, style = <<"font-weight: bold;">>, body =
        <<"Registration failed. Please reinstall VeilCluster and try again.">>
        },
        #p{class = <<"login-info">>, body = wf:q(<<"details">>)},
        #button{postback = finish, class = <<"btn btn-warning btn-block">>, body = <<"Finish">>}
      ]}
  end.

% Returns set items as a comma-delimited binary
format_sets_items(Set) ->
  case sets:to_list(Set) of
    [] -> #p{body = <<"-">>, style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>};
    List ->
      lists:foldr(fun(Item, Acc) ->
        [#p{body = list_to_binary(Item), style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>} | Acc]
      end, [], List)
  end.

% Displays hides current installation step and displays next or previous one
change_step(Step, Diff) ->
  HideId = "step_" ++ integer_to_list(Step),
  ShowId = "step_" ++ integer_to_list(Step + Diff),
  wf:wire(#jquery{target = "error_message", method = ["hide"], args = []}),
  wf:wire(#jquery{target = "success_message", method = ["hide"], args = []}),
  wf:wire(#jquery{target = HideId, method = ["slideUp"], args = ["\"slow\""]}),
  wf:wire(#jquery{target = ShowId, method = ["delay"], args = [800]}),
  wf:wire(#jquery{target = ShowId, method = ["slideDown"], args = ["\"slow\""]}).

% Create page state using configuration loaded from Db
get_saved_page_state() ->
  case dao:get_record(configurations, last) of
    {ok, #configuration{main_ccm = undefined, opt_ccms = OptCCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths}} ->
      #page_state{ccms = sets:from_list(OptCCMs), workers = sets:from_list(Workers),
        dbs = sets:from_list(Dbs), storage_paths = sets:from_list(StoragePaths)};
    {ok, #configuration{main_ccm = MainCCM, opt_ccms = OptCCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths}} ->
      #page_state{main_ccm = MainCCM, ccms = sets:from_list([MainCCM | OptCCMs]), workers = sets:from_list(Workers),
        dbs = sets:from_list(Dbs), storage_paths = sets:from_list(StoragePaths)};
    _ ->
      #page_state{}
  end.

% Returns page state difference
get_page_state_diff(#page_state{main_ccm = undefined}, PageState) ->
  PageState;
get_page_state_diff(#page_state{workers = PrevWorkers}, #page_state{workers = CurrWorkers}) ->
  #page_state{workers = sets:subtract(CurrWorkers, PrevWorkers)}.

% Checks wheter all storage paths are available for all workers
check_storage(_, []) ->
  ok;
check_storage(Hosts, [StoragePath | StoragePaths]) ->
  case install_storage:check_storage_on_hosts(Hosts, StoragePath) of
    ok -> check_storage(Hosts, StoragePaths);
    {error, ErrorHosts} ->
      error_message(<<"Storage: ", (list_to_binary(StoragePath))/binary, ", is not available on hosts: ", (format_error_message(ErrorHosts))/binary>>),
      error
  end.

% Main installation function
install(#page_state{main_ccm = undefined}, {MainCCM, OptCCMs, Workers, Dbs, StoragePaths}) ->
  try
    update_progress_bar(0, 8, "Installing database nodes..."),
    install_dbs(Dbs),
    update_progress_bar(1, 8, "Starting database nodes..."),
    start_dbs(Dbs),
    update_progress_bar(2, 8, "Installing CCM nodes..."),
    install_ccms(MainCCM, OptCCMs, Dbs),
    update_progress_bar(3, 8, "Starting CCM nodes..."),
    start_ccms([MainCCM | OptCCMs]),
    update_progress_bar(4, 8, "Installing worker nodes..."),
    install_workers(MainCCM, OptCCMs, Workers, Dbs),
    update_progress_bar(5, 8, "Adding storage configuration..."),
    add_storage(Workers, StoragePaths),
    update_progress_bar(6, 8, "Starting worker nodes..."),
    start_workers(Workers),
    update_progress_bar(7, 8, "Finalizing installation..."),
    finalize_installation(MainCCM),
    gui_utils:update("ports_table", ports_table_body(MainCCM)),
    update_progress_bar(8, 8, "Done"),
    gui_utils:flush(),
    change_step(4, 1),
    gui_utils:flush()
  catch
    _:_ -> error
  end;
install(#page_state{workers = InstalledWorkers}, {MainCCM, OptCCMs, Workers, Dbs, StoragePaths}) ->
  WorkersToInstall = lists:filter(fun(Worker) -> not sets:is_element(Worker, InstalledWorkers) end, Workers),
  try
    case WorkersToInstall of
      [] ->
        error_message(<<"Nothing to install.">>),
        throw(error);
      _ -> ok
    end,
    update_progress_bar(0, 3, "Installing worker nodes..."),
    install_workers(MainCCM, OptCCMs, WorkersToInstall, Dbs),
    update_progress_bar(1, 3, "Adding storage configuration..."),
    add_storage(Workers, StoragePaths),
    update_progress_bar(2, 3, "Starting worker nodes..."),
    start_workers(WorkersToInstall),
    update_progress_bar(3, 3, "Done"),
    wf:redirect(<<"/installation?x=success">>),
    gui_utils:flush(),
    ok
  catch
    _:_ -> error
  end.

finalize_installation(MainCCM) ->
  case install_utils:get_control_panel_hosts(MainCCM) of
    {ok, [_ | _]} ->
      db_logic:initialize_ports_table(MainCCM);
    _ ->
      timer:sleep(1000),
      finalize_installation(MainCCM)
  end.

% Installs database nodes on hosts
install_dbs(Dbs) ->
  case install_db:install(Dbs, []) of
    ok -> ok;
    {error, ErrorHosts} ->
      error_message(<<"Database nodes were not installed on following hosts: ", (format_error_message(ErrorHosts))/binary>>),
      throw(error)
  end.

% Starts database nodes on hosts
start_dbs(Dbs) ->
  case install_db:start(Dbs, []) of
    ok -> ok;
    {error, ErrorHosts} ->
      error_message(<<"Database nodes were not started on following hosts: ", (format_error_message(ErrorHosts))/binary>>),
      throw(error)
  end.

% Installs CCM nodes on hosts
install_ccms(MainCCM, OptCCMs, Dbs) ->
  case install_ccm:install([MainCCM | OptCCMs], [{main_ccm, MainCCM}, {opt_ccms, OptCCMs}, {dbs, Dbs}]) of
    ok -> ok;
    {error, ErrorHosts} ->
      error_message(<<"CCM nodes were not installed on following hosts: ", (format_error_message(ErrorHosts))/binary>>),
      throw(error)
  end.

% Starts CCM nodes on hosts
start_ccms(Hosts) ->
  case install_ccm:start(Hosts, []) of
    ok -> ok;
    {error, ErrorHosts} ->
      error_message(<<"CCM nodes were not started on following hosts: ", (format_error_message(ErrorHosts))/binary>>),
      throw(error)
  end.

% Installs worker nodes on hosts
install_workers(MainCCM, OptCCMs, Workers, Dbs) ->
  case install_worker:install(Workers, [{main_ccm, MainCCM}, {opt_ccms, OptCCMs}, {dbs, Dbs}]) of
    ok -> ok;
    {error, ErrorHosts} ->
      error_message(<<"Worker nodes were not installed on following hosts: ", (format_error_message(ErrorHosts))/binary>>),
      throw(error)
  end.

% Installs worker nodes on hosts
start_workers(Hosts) ->
  case install_worker:start(Hosts, []) of
    ok -> ok;
    {error, ErrorHosts} ->
      error_message(<<"Worker nodes were not started on following hosts: ", (format_error_message(ErrorHosts))/binary>>),
      throw(error)
  end.

% Adds storage on hosts
add_storage(Hosts, StoragePaths) ->
  case install_storage:add_storage_paths_on_hosts(Hosts, StoragePaths) of
    ok -> ok;
    {error, ErrorHosts} ->
      error_message(<<"Storage paths were not added on following hosts: ", (format_error_message(ErrorHosts))/binary>>),
      throw(error)
  end.

update_progress_bar(Window, Window, _) ->
  wf:wire(#jquery{target = "bar", method = ["width"], args = ["\"0%\""]}),
  wf:wire(#jquery{target = "progress_text", method = ["text"], args = ["\"\""]}),
  wf:wire(#jquery{target = "progress", method = ["hide"], args = []}),
  gui_utils:flush();
update_progress_bar(0, _, Text) ->
  wf:wire(#jquery{target = "progress_text", method = ["text"], args = ["\"" ++ Text ++ "\""]}),
  wf:wire(#jquery{target = "progress", method = ["show"], args = []}),
  gui_utils:flush();
update_progress_bar(Elapsed, Window, Text) ->
  Progress = "\"" ++ integer_to_list(round(Elapsed / Window * 100)) ++ "%\"",
  wf:wire(#jquery{target = "progress_text", method = ["text"], args = ["\"" ++ Text ++ "\""]}),
  wf:wire(#jquery{target = "bar", method = ["width"], args = [Progress]}),
  gui_utils:flush().

% =====================
% Event handling

event(init) ->
  PageState = get_saved_page_state(),
  {ok, Pid} = gui_utils:comet(fun() -> comet_loop(PageState) end),
  put(comet_pid, Pid);

event({ccm_checkbox_toggled, HostName, HostId}) ->
  get(comet_pid) ! {ccm_checkbox_toggled, HostName, HostId};

event({worker_checkbox_toggled, HostName, HostId}) ->
  get(comet_pid) ! {worker_checkbox_toggled, HostName, HostId};

event({db_checkbox_toggled, HostName, HostId}) ->
  get(comet_pid) ! {db_checkbox_toggled, HostName, HostId};

event({next, Step}) ->
  get(comet_pid) ! {next, Step};

event({prev, Step}) ->
  change_step(Step, -1);

event({set_main_ccm, CCM, CCMs}) ->
  update_main_ccm_dropdown(CCM, CCMs),
  get(comet_pid) ! {set_main_ccm, CCM};

event({add_storage, Id, StorageId}) ->
  StoragePath = wf:q(StorageId),
  get(comet_pid) ! {add_storage, StoragePath, Id};

event({delete_storage, Id, StorageId}) ->
  StoragePath = wf:q(StorageId),
  get(comet_pid) ! {delete_storage, StoragePath, Id};

event(install) ->
  get(comet_pid) ! install;

event({check_port, Host, Type, Id}) ->
  TextboxId = "port_textbox_" ++ integer_to_list(Id),
  Port = wf:q(list_to_atom(TextboxId)),
  case Port of
    [] -> ok;
    _ -> case global_registry:check_port(Host, list_to_integer(Port), Type) of
           ok ->
             Record = case Type of
                        "gui" -> #port{host = Host, gui = list_to_integer(Port)};
                        _ -> #port{host = Host, rest = list_to_integer(Port)}
                      end,
             dao:update_record(ports, Record),
             wf:wire(#jquery{target = "port_button_" ++ integer_to_list(Id), method = ["hide"], args = []}),
             wf:wire(#jquery{target = "port_status_" ++ integer_to_list(Id), method = ["toggleClass"], args = ["\"btn-danger\""]}),
             wf:wire(#jquery{target = "port_status_" ++ integer_to_list(Id), method = ["text"], args = ["\"OK\""]}),
             wf:wire(#jquery{target = TextboxId, method = ["prop"], args = ["\"disabled\"", "\"disabled\""]}),
             wf:wire(#jquery{target = TextboxId, method = ["css"], args = ["\"margin-right\"", "\"\""]});
           _ -> ok
         end
  end;

event(finish) ->
  wf:redirect(<<"/installation">>).