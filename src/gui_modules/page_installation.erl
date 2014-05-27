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

-define(COLUMN_STYLE, "text-align: center; vertical-align: inherit;").
-define(EMPTY, <<"---------">>).

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
      #h6{style = <<"font-size: 18px;">>, body = <<"Step 1: Select cluster and database nodes.">>},
      #table{class = <<"table table-bordered">>, style = <<"width: 50%; margin: 0 auto; margin-top: 20px;">>, body = nodes_table_body()},
      #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
        #button{postback = {next, 1}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold;">>, body = <<"Next">>}
      ]}
    ]}
%%
%%     #panel{id = <<"step_2">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
%%       #h6{style = <<"font-size: 18px;">>, body = <<"Step 2: Select primary Central Cluster Manager node.">>},
%%       #panel{class = <<"btn-group">>, style = <<"margin: 12px 15px;">>, body = [
%%         <<"<i class=\"dropdown-arrow dropdown-arrow-inverse\"></i>">>,
%%         #button{id = <<"ccms_button">>, class = <<"btn btn-inverse btn-small dropdown-toggle">>, style = <<"width: 280px;">>,
%%           data_fields = [{<<"data-toggle">>, <<"dropdown">>}], body = [
%%             #span{id = <<"ccms_label">>, class = <<"filter-option pull-left">>, body = <<"<b>Primary CCM node</b>">>},
%%             #span{class = <<"caret pull-right">>}
%%           ]},
%%         #list{id = <<"ccms_dropdown">>, class = <<"dropdown-menu dropdown-inverse">>,
%%           style = <<"overflow-y: auto; max-height: 200px;">>, body = ccms_dropdown_body(undefined, [])}
%%       ]},
%%       #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
%%         #button{postback = {prev, 2}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-right: 200px;">>, body = <<"Back">>},
%%         #button{postback = {next, 2}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-left: 200px;">>, body = <<"Next">>}
%%       ]}
%%     ]},
%%
%%     #panel{id = <<"step_3">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
%%       #h6{style = <<"font-size: 18px;">>, body = <<"Step 3: Add storage.">>},
%%       #table{class = <<"table table-striped">>, style = <<"width: 50%; margin: 0 auto; margin-top: 20px;">>, body = [
%%         #tbody{id = <<"storage_table">>, body = storage_table_body()}
%%       ]},
%%       #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
%%         #button{postback = {prev, 3}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-right: 200px;">>, body = <<"Back">>},
%%         #button{postback = {next, 3}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-left: 200px;">>, body = <<"Next">>}
%%       ]}
%%     ]},
%%
%%     #panel{id = <<"step_4">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
%%       #h6{style = <<"font-size: 18px;">>, body = <<"Step 4: Installation summary.">>},
%%       #table{class = <<"table table-striped">>, style = <<"width: 50%; margin: 0 auto; margin-top: 20px; margin-bottom: 50px;">>, body = [
%%         #tbody{id = <<"summary_table">>, body = summary_table_body()}
%%       ]},
%%       #panel{id = <<"progress">>, style = <<"margin-top: 30px; width: 50%; margin: 0 auto; display: none;">>, body = [
%%         #p{id = <<"progress_text">>, style = <<"font-weight: 300;">>, body = <<"">>},
%%         #panel{class = <<"progress">>, body = #panel{id = <<"bar">>, class = <<"bar">>, style = <<"width: 0%;">>}}
%%       ]},
%%       #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
%%         #button{id = <<"back_button">>, postback = {prev, 4}, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-right: 200px;">>, body = <<"Back">>},
%%         #button{id = <<"install_button">>, postback = install, class = <<"btn btn-inverse btn-small">>, style = <<"width: 80px; font-weight: bold; margin-left: 200px;">>, body = <<"Install">>}
%%       ]}
%%     ]}

  ]}.

%% comet_loop(BackgroundPid, #page_state{counter = Counter, ccm = CCM, ccms = CCMs, workers = Workers, databases = Databases, storages = Storages} = PageState) ->
%%   lager:info("CCM: ~p", [CCM]),
%%   lager:info("CCMs: ~p", [sets:to_list(CCMs)]),
%%   lager:info("Workers: ~p", [sets:to_list(Workers)]),
%%   lager:info("Databases: ~p~n~n", [sets:to_list(Databases)]),
%%   try
%%     receive
%%       {update_progress_bar, Window, Window} ->
%%         wf:wire(#jquery{target = "bar", method = ["width"], args = ["\"0%\""]}),
%%         wf:wire(#jquery{target = "progress_text", method = ["text"], args = ["\"\""]}),
%%         wf:wire(#jquery{target = "progress", method = ["hide"], args = []}),
%%         gui_utils:flush(),
%%         comet_loop(BackgroundPid, PageState);
%%       {update_progress_bar, 0, Text} ->
%%         wf:wire(#jquery{target = "progress_text", method = ["text"], args = ["\"" ++ Text ++ "\""]}),
%%         wf:wire(#jquery{target = "progress", method = ["show"], args = []}),
%%         comet_loop(BackgroundPid, PageState);
%%       {update_progress_bar, Elapsed, Window} ->
%%         Progress = "\"" ++ integer_to_list(round(Elapsed / Window * 100)) ++ "%\"",
%%         wf:wire(#jquery{target = "bar", method = ["width"], args = [Progress]}),
%%         gui_utils:flush(),
%%         comet_loop(BackgroundPid, PageState);
%%
%%       {ccm_checkbox_toggled, NodeName, NodeId} ->
%%         case sets:is_element(NodeName, CCMs) of
%%           true ->
%%             comet_loop(BackgroundPid, PageState#page_state{ccms = sets:del_element(NodeName, CCMs)});
%%           false ->
%%             case sets:is_element(NodeName, Workers) of
%%               true -> ok;
%%               false ->
%%                 WorkerCheckboxId = "worker_checkbox_" ++ binary_to_list(NodeId),
%%                 wf:wire(#jquery{target = WorkerCheckboxId, method = ["checkbox"], args = ["\"check\""]}),
%%                 gui_utils:flush()
%%             end,
%%             comet_loop(BackgroundPid, PageState#page_state{ccms = sets:add_element(NodeName, CCMs)})
%%         end;
%%
%%       {worker_checkbox_toggled, NodeName, NodeId} ->
%%         case sets:is_element(NodeName, Workers) of
%%           true ->
%%             case sets:is_element(NodeName, CCMs) of
%%               true ->
%%                 CCMCheckboxId = "ccm_checkbox_" ++ binary_to_list(NodeId),
%%                 wf:wire(#jquery{target = CCMCheckboxId, method = ["checkbox"], args = ["\"uncheck\""]}),
%%                 gui_utils:flush();
%%               false -> ok
%%             end,
%%             comet_loop(BackgroundPid, PageState#page_state{workers = sets:del_element(NodeName, Workers)});
%%           _ ->
%%             comet_loop(BackgroundPid, PageState#page_state{workers = sets:add_element(NodeName, Workers)})
%%         end;
%%
%%       {database_checkbox_toggled, NodeName, _NodeId} ->
%%         case sets:is_element(NodeName, Databases) of
%%           true ->
%%             comet_loop(BackgroundPid, PageState#page_state{databases = sets:del_element(NodeName, Databases)});
%%           _ ->
%%             comet_loop(BackgroundPid, PageState#page_state{databases = sets:add_element(NodeName, Databases)})
%%         end;
%%
%%       {set_ccm, MainCCM} ->
%%         comet_loop(BackgroundPid, PageState#page_state{ccm = MainCCM});
%%
%%       {add_storage, StoragePath, Id} ->
%%         case sets:is_element(StoragePath, Storages) of
%%           true ->
%%             error_message(<<"Storage already added.">>),
%%             comet_loop(BackgroundPid, PageState);
%%           _ ->
%%             case gen_server:call(?Installer_Name, {check_storage, StoragePath}, ?GEN_SERVER_TIMEOUT) of
%%               ok ->
%%                 wf:wire(#jquery{target = "error_message", method = ["hide"], args = []}),
%%                 wf:wire(#jquery{target = "add_storage_row_" ++ integer_to_list(Id), method = ["hide"], args = []}),
%%                 wf:wire(#jquery{target = "delete_storage_row_" ++ integer_to_list(Id), method = ["show"], args = []}),
%%                 wf:wire(#jquery{target = "storage_textbox_" ++ integer_to_list(Id), method = ["prop"], args = ["\"disabled\"", "\"disabled\""]}),
%%                 gui_utils:insert_bottom("storage_table", storage_table_row(<<"">>, Counter + 1, undefined)),
%%                 gui_utils:flush(),
%%                 comet_loop(BackgroundPid, PageState#page_state{counter = Counter + 1, storages = sets:add_element(StoragePath, Storages)});
%%               _ ->
%%                 error_message(<<"Storage not available.">>),
%%                 comet_loop(BackgroundPid, PageState)
%%             end
%%         end;
%%
%%       {delete_storage, StoragePath, Id} ->
%%         wf:wire(#jquery{target = "storage_row_" ++ integer_to_list(Id), method = ["remove"], args = []}),
%%         gui_utils:flush(),
%%         comet_loop(BackgroundPid, PageState#page_state{storages = sets:del_element(StoragePath, Storages)});
%%
%%       {next, 1} ->
%%         case (sets:size(CCMs) > 0) andalso (sets:size(Databases) > 0) of
%%           true ->
%%             NewCCM = case sets:is_element(CCM, CCMs) of
%%                        true -> CCM;
%%                        _ -> undefined
%%                      end,
%%             update_ccms_dropdown(NewCCM, sets:to_list(CCMs)),
%%             case PageState#page_state.saved of
%%               true ->
%%                 wf:wire(#jquery{target = "ccms_button", method = ["prop"], args = ["\"disabled\"", "\"disabled\""]});
%%               _ -> ok
%%             end,
%%             change_step(1, 1),
%%             gui_utils:flush(),
%%             comet_loop(BackgroundPid, PageState);
%%           _ ->
%%             error_message(<<"Please select at least one CCM and database node.">>),
%%             comet_loop(BackgroundPid, PageState)
%%         end;
%%
%%       {next, 2} ->
%%         case CCM of
%%           undefined -> error_message(<<"Please select primary Central Cluster Manager node.">>);
%%           _ ->
%%             change_step(2, 1),
%%             gui_utils:flush()
%%         end,
%%         comet_loop(BackgroundPid, PageState);
%%
%%       {next, 3} ->
%%         case sets:size(Storages) > 0 of
%%           true ->
%%             change_step(3, 1),
%%             gui_utils:update("summary_table", summary_table_body(PageState)),
%%             gui_utils:flush();
%%           _ -> error_message(<<"Please add at least one storage.">>)
%%         end,
%%         comet_loop(BackgroundPid, PageState);
%%
%%       install ->
%%         PrevPageState = get_page_state(),
%%         PrevDBNode = case sets:to_list(PrevPageState#page_state.databases) of
%%                        [] -> undefined;
%%                        [P | _] -> binary_to_list(P)
%%                      end,
%%         PageStateDiff = get_page_state_diff(PrevPageState, PageState),
%%         #page_state{ccm = DiffCCM, ccms = DiffCCMs, workers = DiffWorkers, databases = DiffDatabases, storages = DiffStorages} = PageStateDiff,
%%         {ok, Ref} = application:get_env(?APP_Name, page_state),
%%         case {DiffCCM, sets:size(DiffCCMs), sets:size(DiffWorkers), sets:size(DiffDatabases), sets:size(DiffStorages)} of
%%           {undefined, 0, 0, 0, 0} -> error_message(<<"Nothing to install.">>);
%%           Other ->
%%             lager:info("Other: ~p", [Other]),
%%             case gen_server:call(?Installer_Name, {install_db_nodes, BackgroundPid, PrevDBNode, sets:to_list(DiffDatabases)}, ?GEN_SERVER_TIMEOUT_LONG) of
%%               ok ->
%%                 case gen_server:call(?Installer_Name, {install_ccms, BackgroundPid, [DiffCCM | sets:to_list(DiffCCMs)], DiffCCM, sets:to_list(DiffCCMs), sets:to_list(DiffDatabases), sets:to_list(DiffStorages)}, ?GEN_SERVER_TIMEOUT_LONG) of
%%                   ok ->
%%                     case gen_server:call(?Installer_Name, {install_workers, BackgroundPid, sets:to_list(DiffWorkers), CCM, sets:to_list(sets:subtract(CCMs, sets:add_element(CCM, sets:new()))), sets:to_list(Databases), sets:to_list(Storages)}, ?GEN_SERVER_TIMEOUT_LONG) of
%%                       ok ->
%%                         lager:info("=====> Saving page state: ~p", [PageState]),
%%                         gen_server:call({global, ?Db_Name}, {insert, Ref, page_state, PageState#page_state{saved = true}}, ?GEN_SERVER_TIMEOUT),
%%                         wf:redirect(<<"/installation?x=success">>),
%%                         gui_utils:flush();
%%                       {error, WorkersInstalled, WorkersNotInstalled} ->
%%                         NewCCMs = sets:union(PrevPageState#page_state.databases, sets:from_list(WorkersInstalled)),
%%                         gen_server:call({global, ?Db_Name}, {insert, Ref, page_state, PrevPageState#page_state{ccms = NewCCMs}}, ?GEN_SERVER_TIMEOUT),
%%                         error_message(<<"Following worker nodes were not installed: ", (format_error_message(WorkersNotInstalled))/binary>>);
%%                       _ -> error_message(<<"Internal server error. Please refresh the page.">>)
%%                     end;
%%                   {error, CCMsInstalled, CCMsNotInstalled} ->
%%                     NewCCMs = sets:union(PrevPageState#page_state.databases, sets:from_list(CCMsInstalled)),
%%                     gen_server:call({global, ?Db_Name}, {insert, Ref, page_state, PrevPageState#page_state{ccms = NewCCMs}}, ?GEN_SERVER_TIMEOUT),
%%                     error_message(<<"Following CCM nodes were not installed: ", (format_error_message(CCMsNotInstalled))/binary>>);
%%                   _ -> error_message(<<"Internal server error. Please refresh the page.">>)
%%                 end;
%%               {error, DbInstalled, DbNotInstalled} ->
%%                 NewDatabases = sets:union(PrevPageState#page_state.databases, sets:from_list(DbInstalled)),
%%                 gen_server:call({global, ?Db_Name}, {insert, Ref, page_state, PrevPageState#page_state{databases = NewDatabases}}, ?GEN_SERVER_TIMEOUT),
%%                 error_message(<<"Following database nodes were not installed: ", (format_error_message(DbNotInstalled))/binary>>);
%%               _ -> error_message(<<"Internal server error. Please refresh the page.">>)
%%             end
%%         end,
%%         comet_loop(BackgroundPid, PageState);
%%
%%       Other ->
%%         lager:error("Comet process received unknown message: ~p", [Other]),
%%         comet_loop(BackgroundPid, PageState)
%%     end
%%   catch Type:Msg ->
%%     lager:error("Error in comet process: ~p : ~p", [Type, Msg]),
%%     error_message(<<"There has been an error in comet process. Please refresh the page.">>)
%%   end.
%%
%% background_loop(Elapsed, Window) ->
%%   try
%%     receive
%%       {create_progress_bar, Text, Size} ->
%%         wf:wire(#jquery{target = "progress_text", method = ["text"], args = ["\"" ++ Text ++ "\""]}),
%%         wf:wire(#jquery{target = "progress", method = ["show"], args = []}),
%%         background_loop(0, Size);
%%
%%       {update_progress_bar, Text} ->
%%         case Elapsed =:= Window of
%%           true ->
%%             wf:wire(#jquery{target = "bar", method = ["width"], args = ["\"0%\""]}),
%%             wf:wire(#jquery{target = "progress_text", method = ["text"], args = ["\"\""]}),
%%             wf:wire(#jquery{target = "progress", method = ["hide"], args = []}),
%%             gui_utils:flush(),
%%             background_loop(0, 0);
%%           false ->
%%             Progress = "\"" ++ integer_to_list(round(Elapsed / Window * 100)) ++ "%\"",
%%             wf:wire(#jquery{target = "progress_text", method = ["text"], args = ["\"" ++ Text ++ "\""]}),
%%             wf:wire(#jquery{target = "bar", method = ["width"], args = [Progress]}),
%%             gui_utils:flush(),
%%             background_loop(Elapsed + 1, Window)
%%         end;
%%
%%       Other ->
%%         lager:error("Install process received unknown message: ~p", [Other]),
%%         background_loop(Elapsed, Window)
%%     end
%%   catch Type:Msg ->
%%     lager:error("Error in installation process: ~p : ~p", [Type, Msg])
%%   end.
%%
%% % Displays error message
%% error_message(Message) ->
%%   gui_utils:update("error_message", Message),
%%   wf:wire(#jquery{target = "error_message", method = ["fadeIn"], args = [300]}),
%%   gui_utils:flush().
%%
%% format_error_message([]) ->
%%   <<"">>;
%% format_error_message([Node | Nodes]) ->
%%   lists:foldl(fun(Item, Acc) -> <<Item/binary, ", ", Acc/binary>> end, Node, Nodes).
%%
% Renders nodes table in first step of installation
nodes_table_body() ->
  case db_logic:get_record(configurations, last) of
    {ok, #configuration{main_ccm = undefined, opt_ccms = OptCCMs, workers = Workers, dbs = Dbs}} ->
      nodes_table_body(sets:from_list(OptCCMs), sets:from_list(Workers), sets:from_list(Dbs));
    {ok, #configuration{main_ccm = MainCCM, opt_ccms = OptCCMs, workers = Workers, dbs = Dbs}} ->
      nodes_table_body(sets:from_list([MainCCM | OptCCMs]), sets:from_list(Workers), sets:from_list(Dbs));
    _ -> nodes_table_body(sets:new(), sets:new(), sets:new())
  end.
nodes_table_body(CCMs, Workers, Databases) ->
  Hostnames = case gen_server:call(?SPANEL_NAME, get_hostnames, ?GEN_SERVER_TIMEOUT) of
                {ok, Answer} -> Answer;
                _ -> []
              end,
  Header = #tr{cells = [
    #th{body = <<"Node">>, style = <<?COLUMN_STYLE>>},
    #th{body = <<"CCM">>, style = <<?COLUMN_STYLE>>},
    #th{body = <<"Worker">>, style = <<?COLUMN_STYLE>>},
    #th{body = <<"Database">>, style = <<?COLUMN_STYLE>>}
  ]},
  Rows = lists:map(fun({Hostname, Id}) ->
    HostId = integer_to_binary(Id),
    Checkboxes = [
      {<<"ccm_checkbox_">>, sets:is_element(Hostname, CCMs), sets:size(CCMs) =/= 0},
      {<<"worker_checkbox_">>, sets:is_element(Hostname, Workers), sets:is_element(Hostname, Workers)},
      {<<"database_checkbox_">>, sets:is_element(Hostname, Databases), sets:size(Databases) =/= 0}
    ],
    #tr{id = <<"row_", HostId/binary>>, cells = [
      #td{body = <<"<b>", (list_to_binary(Hostname))/binary, "</b>">>, style = <<?COLUMN_STYLE>>} |
      lists:map(fun({Prefix, Checked, Disabled}) ->
        #td{body = #custom_checkbox{
          id = <<Prefix/binary, HostId/binary>>,
          class = <<"checkbox no-label">>,
          style = <<"width: 20px; margin: 0 auto;">>,
          checked = Checked,
          disabled = Disabled,
          postback = {binary_to_atom(<<Prefix/binary, "toggled">>, latin1), Hostname, HostId}
        }, style = <<?COLUMN_STYLE>>}
      end, Checkboxes)
    ]}
  end, lists:zip(lists:sort(Hostnames), lists:seq(1, length(Hostnames)))),
  [Header | Rows].
%%
%% % Renders ccm dropdown and highlights current choice in second step of installation
%% ccms_dropdown_body(_, []) ->
%%   [];
%% ccms_dropdown_body(ActiveCCM, CCMs) ->
%%   lists:map(
%%     fun({CCM, Index}) ->
%%       Class = case CCM of
%%                 ActiveCCM -> <<"active">>;
%%                 _ -> <<"">>
%%               end,
%%       CCMId = <<"ccm_li_", (integer_to_binary(Index))/binary>>,
%%       #li{id = CCMId, actions = #event{type = "click", postback = {set_ccm, CCM, CCMs}, target = CCMId},
%%         class = Class, body = #link{style = <<"text-align: left;">>, body = CCM}}
%%     end, lists:zip(lists:sort(CCMs), lists:seq(1, length(CCMs)))).
%%
%% % Updates ccm dropdown
%% update_ccms_dropdown(CCM, CCMs) ->
%%   case CCM of
%%     undefined -> gui_utils:update("ccms_label", <<"<b>Primary CCM node</b>">>);
%%     _ -> gui_utils:update("ccms_label", <<"Primary CCM node: <b>", CCM/binary, "</b>">>)
%%   end,
%%   gui_utils:update("ccms_dropdown", ccms_dropdown_body(CCM, CCMs)).
%%
%% storage_table_body() ->
%%   #page_state{saved = Saved, storages = Storages} = get_page_state(),
%%   lists:map(fun({Storage, Id}) ->
%%     storage_table_row(Storage, Id, true)
%%   end, lists:zip(lists:sort(sets:to_list(Storages)), lists:seq(1, sets:size(Storages))))
%%   ++ if Saved =:= true -> []; true -> [storage_table_row(<<"">>, sets:size(Storages) + 1, undefined)] end.
%%
%% % Renders storage table row
%% storage_table_row(Storage, Id, Disabled) ->
%%   StorageId = list_to_atom("storage_textbox_" ++ integer_to_list(Id)),
%%   {AddStorageDisplay, DeleteStorageDisplay} = case Disabled of
%%                                                 undefined -> {<<"">>, <<" display: none;">>};
%%                                                 _ -> {<<" display: none;">>, <<" display: none;">>}
%%                                               end,
%%   #tr{id = <<"storage_row_", (integer_to_binary(Id))/binary>>, cells = [
%%     #th{body = #textbox{id = StorageId, value = Storage, disabled = Disabled, placeholder = <<"Storage path">>, style = <<"width: 100%;">>},
%%       style = <<"text-align: center; vertical-align: inherit; padding-bottom: 0;">>},
%%     #th{id = <<"add_storage_row_", (integer_to_binary(Id))/binary>>,
%%       body = #link{postback = {add_storage, Id, StorageId}, source = [StorageId], class = <<"glyph-link">>,
%%         body = #span{class = <<"fui-plus">>, style = <<"font-size: 20px;">>}},
%%       style = <<"text-align: center; vertical-align: inherit; padding: 0; width: 20px;", AddStorageDisplay/binary>>},
%%     #th{id = <<"delete_storage_row_", (integer_to_binary(Id))/binary>>,
%%       body = #link{postback = {delete_storage, Id, StorageId}, source = [StorageId], class = <<"glyph-link">>,
%%         body = #span{class = <<"fui-cross">>, style = <<"font-size: 20px;">>}},
%%       style = <<"text-align: center; vertical-align: inherit; padding: 0; width: 20px;", DeleteStorageDisplay/binary>>}
%%   ]}.
%%
%% % Renders summary teble
%% summary_table_body() ->
%%   summary_table_body(get_page_state()).
%% summary_table_body(PageState) ->
%%   PrevPageState = get_page_state(),
%%   #page_state{ccm = CCM, ccms = CCMs, workers = Workers, databases = Databases, storages = Storages} =
%%     get_page_state_diff(PrevPageState, PageState),
%%   [
%%     #tr{id = <<"summary_ccm">>, cells = [
%%       #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
%%         body = #p{style = <<"text-align: center; margin-bottom: 0;">>, body = <<"Primary CCM node">>}},
%%       #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
%%         body = #p{style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>, body =
%%         case CCM of undefined -> ?EMPTY; _ -> CCM end
%%         }}
%%     ]},
%%     summary_table_row(<<"summary_ccms">>, <<"Optional CCM nodes">>, format_sets_items(CCMs)),
%%     summary_table_row(<<"summary_workers">>, <<"Worker nodes">>, format_sets_items(Workers)),
%%     summary_table_row(<<"summary_databases">>, <<"Database nodes">>, format_sets_items(Databases)),
%%     summary_table_row(<<"summary_storages">>, <<"Storage paths">>, format_sets_items(Storages))
%%   ].
%%
%% summary_table_row(Id, Description, Details) ->
%%   #tr{id = Id, cells = [
%%     #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
%%       body = #p{style = <<"text-align: center; margin-bottom: 0;">>, body = Description}},
%%     #th{style = <<"width: 50%; vertical-align: inherit; padding: 0;">>, body = Details}
%%   ]}.
%%
%% format_sets_items(Set) ->
%%   case sets:to_list(Set) of
%%     [] -> #p{body = ?EMPTY, style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>};
%%     List ->
%%       lists:foldr(fun(Item, Acc) ->
%%         [#p{body = Item, style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>} | Acc]
%%       end, [], List)
%%   end.
%%
%% % Displays hides current installation step and displays next or previous one
%% change_step(Step, Diff) ->
%%   HideId = "step_" ++ integer_to_list(Step),
%%   ShowId = "step_" ++ integer_to_list(Step + Diff),
%%   wf:wire(#jquery{target = "error_message", method = ["hide"], args = []}),
%%   wf:wire(#jquery{target = "success_message", method = ["hide"], args = []}),
%%   wf:wire(#jquery{target = HideId, method = ["slideUp"], args = ["\"slow\""]}),
%%   wf:wire(#jquery{target = ShowId, method = ["delay"], args = [800]}),
%%   wf:wire(#jquery{target = ShowId, method = ["slideDown"], args = ["\"slow\""]}).
%%
%% % Returns installation prefernces saved in db
%% get_page_state() ->
%%   try
%%     {ok, Ref} = application:get_env(?APP_Name, page_state),
%%     {ok, Data} = gen_server:call({global, ?Db_Name}, {select, Ref, page_state}, ?GEN_SERVER_TIMEOUT),
%%     Data
%%   catch
%%     _:_ ->
%%       lager:error("Can not fetch previous installation data."),
%%       #page_state{}
%%   end.
%%
%% % Returns page state difference
%% get_page_state_diff(PrevPageState, PageState) ->
%%   #page_state{ccm = PCCM, ccms = PCCMs, workers = PWorkers, databases = PDatabases, storages = PStorages} = PrevPageState,
%%   #page_state{ccm = CCM, ccms = CCMs, workers = Workers, databases = Databases, storages = Storages} = PageState,
%%   #page_state{
%%     ccm = if CCM =:= PCCM -> undefined; true -> CCM end,
%%     ccms = sets:subtract(sets:subtract(CCMs, PCCMs), sets:add_element(CCM, sets:new())),
%%     workers = sets:subtract(Workers, PWorkers),
%%     databases = sets:subtract(Databases, PDatabases),
%%     storages = sets:subtract(Storages, PStorages)
%%   }.
%%
%% %% get_page_state_diff_for_install(PrevPageState, PageState) ->
%% %%   #page_state{ccm = PCCM, ccms = PCCMs, workers = PWorkers, databases = PDatabases, storages = PStorages} = PrevPageState,
%% %%   #page_state{ccm = CCM, ccms = CCMs, workers = Workers, databases = Databases, storages = Storages} = PageState,
%% %%   case PrevPageState#page_state.saved of
%% %%     true -> #page_state{
%% %%       ccm = PCCM,
%% %%       ccms = sets:subtract(PCCMs, sets:add_element(PCCM, sets:new())),
%% %%       workers = sets:subtract(Workers, PWorkers),
%% %%       databases = sets:subtract(Databases, PDatabases),
%% %%       storages = PStorages
%% %%     };
%% %%     _ -> #page_state{
%% %%       ccm = if CCM =:= PCCM -> undefined; true -> CCM end,
%% %%       ccms = sets:subtract(sets:subtract(CCMs, PCCMs), sets:add_element(CCM, sets:new())),
%% %%       workers = sets:subtract(Workers, PWorkers),
%% %%       databases = sets:subtract(Databases, PDatabases),
%% %%       storages = sets:subtract(Storages, PStorages)
%% %%     }
%% %%   end.

% =====================
% Event handling

event(init) -> ok.

%% event(init) ->
%%   {ok, BackgroundPid} = gui_utils:comet(fun() -> background_loop(0, 0) end),
%%   {ok, CometPid} = gui_utils:comet(fun() -> comet_loop(BackgroundPid, get_page_state()) end),
%%   lager:info("Comet pid: ~p", [CometPid]),
%%   lager:info("backgorund pid: ~p", [BackgroundPid]),
%%   put(comet_pid, CometPid);
%%
%% event({ccm_checkbox_toggled, NodeName, NodeId}) ->
%%   get(comet_pid) ! {ccm_checkbox_toggled, NodeName, NodeId};
%%
%% event({worker_checkbox_toggled, NodeName, NodeId}) ->
%%   get(comet_pid) ! {worker_checkbox_toggled, NodeName, NodeId};
%%
%% event({database_checkbox_toggled, NodeName, NodeId}) ->
%%   get(comet_pid) ! {database_checkbox_toggled, NodeName, NodeId};
%%
%% event({next, Step}) ->
%%   get(comet_pid) ! {next, Step};
%%
%% event({set_ccm, CCM, CCMs}) ->
%%   update_ccms_dropdown(CCM, CCMs),
%%   get(comet_pid) ! {set_ccm, CCM};
%%
%% event({add_storage, Id, StorageId}) ->
%%   StoragePath = wf:q(StorageId),
%%   get(comet_pid) ! {add_storage, StoragePath, Id};
%%
%% event({delete_storage, Id, StorageId}) ->
%%   StoragePath = wf:q(StorageId),
%%   get(comet_pid) ! {delete_storage, StoragePath, Id};
%%
%% event(install) ->
%%   get(comet_pid) ! install;
%%
%% event({prev, Step}) ->
%%   change_step(Step, -1).