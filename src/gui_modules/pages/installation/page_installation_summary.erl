%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page displays installation summary and starts installer process.
%% @end
%% ===================================================================

-module(page_installation_summary).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/stages.hrl").
-include_lib("ctool/include/logging.hrl").

%% Convenience record abbreviation
-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

%% Default time in miliseconds for next progress bar update
-define(DEFAULT_NEXT_UPDATE, 1000).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, state).
-record(?STATE, {step, steps, step_progress, next_update}).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUMMARY, ?PAGE_INSTALLATION) of
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
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Installtion summary">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    #panel{
        style = <<"position: relative;">>,
        body = [
            onepanel_gui_utils:top_menu(installation_tab),

            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #panel{
                style = <<"margin-top: 150px; text-align: center;">>,
                body = [
                    #h6{
                        style = <<"font-size: 18px;">>,
                        body = <<"Step 5: Installation summary.">>
                    },
                    #table{
                        class = <<"table table-striped">>,
                        style = <<"width: 50%; margin: 0 auto; margin-top: 30px; margin-bottom: 30px;">>,
                        body = [
                            #tbody{
                                id = <<"summary_table">>,
                                body = summary_table_body()
                            }
                        ]
                    },
                    #panel{
                        id = <<"progress">>,
                        style = <<"text-align: left; margin-top: 30px; width: 50%; margin: 0 auto; display: none;">>,
                        body = [
                            #p{
                                id = <<"progress_text">>,
                                style = <<"font-weight: 300;">>,
                                body = <<"">>
                            },
                            #panel{
                                class = <<"progress">>,
                                body = #panel{
                                    id = <<"bar">>,
                                    class = <<"bar">>,
                                    style = <<"width: 0%;">>
                                }
                            }
                        ]
                    },
                    #panel{
                        style = <<"width: 50%; margin: 0 auto; margin-top: 30px; margin-bottom: 30px;">>,
                        body = [
                            #button{
                                id = <<"back_button">>,
                                postback = back,
                                class = <<"btn btn-inverse btn-small">>,
                                style = <<"float: left; width: 80px; font-weight: bold;">>,
                                body = <<"Back">>
                            },
                            #button{
                                id = <<"install_button">>,
                                postback = install,
                                class = <<"btn btn-inverse btn-small">>,
                                style = <<"float: right; width: 80px; font-weight: bold;">>,
                                body = <<"Install">>
                            }
                        ]
                    }
                ]
            }
        ] ++ onepanel_gui_utils:logotype_footer(120)
    }.


%% summary_table_body/1
%% ====================================================================
%% @doc Renders summary table body.
-spec summary_table_body() -> Result
    when Result :: [#tr{}].
%% ====================================================================
summary_table_body() ->
    #?CONFIG{main_ccm = MainCCM, ccms = CCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths} = to_install(),
    [
        #tr{
            id = <<"summary_main_ccm">>,
            cells = [
                #th{
                    style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
                    body = #p{
                        style = <<"text-align: center; margin-bottom: 0;">>,
                        body = <<"Primary CCM host">>
                    }
                },
                #th{
                    style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
                    body = #p{
                        style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>,
                        body = case MainCCM of
                                   undefined -> <<"-">>;
                                   _ -> list_to_binary(MainCCM)
                               end
                    }}
            ]},
        summary_table_row(<<"summary_ccms">>, <<"CCM hosts">>, format(CCMs)),
        summary_table_row(<<"summary_workers">>, <<"Worker hosts">>, format(Workers)),
        summary_table_row(<<"summary_Dbs">>, <<"Database hosts">>, format(Dbs)),
        summary_table_row(<<"summary_storages">>, <<"Storage paths">>, format(StoragePaths))
    ].


%% summary_table_row/3
%% ====================================================================
%% @doc Renders summary table row. 'Description' is showed in first
%% column and 'Details' in second one.
-spec summary_table_row(Id :: binary(), Description :: binary(), Details :: binary()) -> Result
    when Result :: #tr{}.
%% ====================================================================
summary_table_row(Id, Description, Details) ->
    #tr{
        id = Id,
        cells = [
            #th{
                style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
                body = #p{
                    style = <<"text-align: center; margin-bottom: 0;">>,
                    body = Description
                }
            },
            #th{
                style = <<"width: 50%; vertical-align: inherit; padding: 0;">>,
                body = Details
            }
        ]
    }.


%% format/1
%% ====================================================================
%% @doc Formats list of hosts, which will be displayed in 'Details'
%% column of installation summary tabel.
-spec format(Hosts :: [string()]) -> Result
    when Result :: #p{} | [#p{}].
%% ====================================================================
format([]) ->
    #p{
        body = <<"-">>,
        style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>
    };
format(Hosts) ->
    lists:map(fun(Host) ->
        #p{
            body = list_to_binary(Host),
            style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>
        }
    end, Hosts).


%% to_install/0
%% ====================================================================
%% @doc Returns components to be installed. It is a difference between
%% configuration saved in user session and database.
-spec to_install() -> Result
    when Result :: #?CONFIG{}.
%% ====================================================================
to_install() ->
    try
        {ok, Db} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, Session} = onepanel_gui_utils:get_installation_state(),

        MainCCM = case Db#?CONFIG.main_ccm =:= Session#?CONFIG.main_ccm of
                      true -> undefined;
                      _ -> Session#?CONFIG.main_ccm
                  end,
        CCMs = lists:sort(Session#?CONFIG.ccms -- Db#?CONFIG.ccms),
        Workers = lists:sort(Session#?CONFIG.workers -- Db#?CONFIG.workers),
        Dbs = lists:sort(Session#?CONFIG.dbs -- Db#?CONFIG.dbs),
        StoragePaths = lists:sort(Session#?CONFIG.storage_paths -- Db#?CONFIG.storage_paths),

        #?CONFIG{main_ccm = MainCCM, ccms = CCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths}
    catch
        _:_ -> #?CONFIG{}
    end.


%% comet_loop/1
%% ====================================================================
%% @doc Handles installer process messages and updates progress bar.
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{step = Step, steps = Steps, step_progress = StepProgress, next_update = NextUpdate} = State) ->
    NewState = try
        receive
            {init, InitSteps} ->
                gui_jq:hide(<<"error_message">>),
                gui_jq:prop(<<"install_button">>, <<"disabled">>, <<"disabled">>),
                gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"disabled">>),
                gui_jq:set_width(<<"bar">>, <<"0%">>),
                gui_jq:show(<<"progress">>),
                gui_comet:flush(),
                timer:send_after(?DEFAULT_NEXT_UPDATE, {update, 0}),
                State#?STATE{step = 0, steps = InitSteps, step_progress = 0, next_update = ?DEFAULT_NEXT_UPDATE};

            {change_step, NewStep, Text} ->
                gui_jq:show(<<"progress">>),
                Progress = <<(integer_to_binary(round(100 * NewStep / Steps)))/binary, "%">>,
                gui_jq:update(<<"progress_text">>, <<Text/binary, " <b>( ", Progress/binary, " )</b>">>),
                gui_jq:set_width(<<"bar">>, Progress),
                gui_comet:flush(),
                timer:send_after(?DEFAULT_NEXT_UPDATE, {update, NewStep, Text}),
                State#?STATE{step = NewStep, step_progress = 0, next_update = ?DEFAULT_NEXT_UPDATE};

            {update, Step, Text} ->
                NewStepProgress = StepProgress + (1 - StepProgress) / 2,
                Progress = <<(integer_to_binary(round(100 * (Step + NewStepProgress) / Steps)))/binary, "%">>,
                gui_jq:update(<<"progress_text">>, <<Text/binary, " <b>( ", Progress/binary, " )</b>">>),
                gui_jq:set_width(<<"bar">>, Progress),
                gui_comet:flush(),
                timer:send_after(NextUpdate, {update, Step, Text}),
                State#?STATE{step_progress = NewStepProgress, next_update = 2 * NextUpdate};

            {update, _, _} ->
                State;

            finish ->
                gui_jq:update(<<"progress_text">>, <<"">>),
                gui_jq:set_width(<<"bar">>, <<"100%">>),
                onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUCCESS),
                gui_comet:flush(),
                State#?STATE{step = undefined};

            {error, Text} ->
                gui_jq:update(<<"summary_table">>, summary_table_body()),
                onepanel_gui_utils:message(<<"error_message">>, Text),
                gui_jq:prop(<<"install_button">>, <<"disabled">>, <<"">>),
                gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"">>),
                gui_jq:hide(<<"progress">>),
                gui_comet:flush(),
                State#?STATE{step = -1}
        end
               catch Type:Reason ->
                   ?error("Comet process exception: ~p:~p", [Type, Reason]),
                   onepanel_gui_utils:message(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Reason}
               end,
    comet_loop(NewState).


%% get_error_message/1
%% ====================================================================
%% @doc Returns error message for given stage and job of installation.
-spec get_error_message({State :: atom(), Job :: atom()}) -> binary().
%% ====================================================================
get_error_message({?STAGE_DB, ?JOB_INSTALL}) -> <<"Database nodes were not installed on following hosts: ">>;
get_error_message({?STAGE_DB, ?JOB_START}) -> <<"Database nodes were not started on following hosts: ">>;
get_error_message({?STAGE_CCM, ?JOB_INSTALL}) -> <<"CCM nodes were not installed on following hosts: ">>;
get_error_message({?STAGE_CCM, ?JOB_START}) -> <<"CCM nodes were not started on following hosts: ">>;
get_error_message({?STAGE_WORKER, ?JOB_INSTALL}) -> <<"Worker nodes were not installed on following hosts: ">>;
get_error_message({?STAGE_WORKER, ?JOB_START}) -> <<"Worker nodes were not started on following hosts: ">>;
get_error_message({?STAGE_STORAGE, ?JOB_ADD_STORAGE_PATHS}) -> <<"Cannot add storage paths on following hosts: ">>;
get_error_message(_) -> <<"">>.


%% get_info_message/1
%% ====================================================================
%% @doc Returns information for given stage and job of installation which
%% will be displayed above installation progress bar.
-spec get_info_message({State :: atom(), Job :: atom()}) -> Result when
    Result :: binary().
%% ====================================================================
get_info_message({?STAGE_DB, ?JOB_INSTALL}) -> <<"Current stage: <b>Installing database nodes</b>">>;
get_info_message({?STAGE_DB, ?JOB_START}) -> <<"Current stage: <b>Starting database nodes</b>">>;
get_info_message({?STAGE_CCM, ?JOB_INSTALL}) -> <<"Current stage: <b>Installing Central Cluster Manager nodes</b>">>;
get_info_message({?STAGE_CCM, ?JOB_START}) -> <<"Current stage: <b>Starting Central Cluster Manager nodes</b>">>;
get_info_message({?STAGE_WORKER, ?JOB_INSTALL}) -> <<"Current stage: <b>Installing worker nodes</b>">>;
get_info_message({?STAGE_WORKER, ?JOB_START}) -> <<"Current stage: <b>Starting worker nodes</b>">>;
get_info_message({?STAGE_STORAGE, ?JOB_ADD_STORAGE_PATHS}) -> <<"Current stage: <b>Adding storage paths</b>">>;
get_info_message(_) -> <<"">>.


%% installation_progress/1
%% ====================================================================
%% @doc Callback function called by installer gen_server which updates
%% installation progress bar.
-spec installation_progress(Event :: atom(), State :: #?i_state{}, Pid :: pid()) -> no_return().
%% ====================================================================
installation_progress(?EVENT_ERROR, State, Pid) ->
    case installer:get_error(State) of
        {error, {hosts, Hosts}} ->
            Pid ! {error, <<(get_error_message(installer:get_stage_and_job(State)))/binary, (onepanel_gui_utils:format_list(Hosts))/binary, ".<br>Please try again.">>};
        _ ->
            Pid ! {error, <<"An error occurred during installation.<br>Please try again.">>}
    end;

installation_progress(?EVENT_STATE_CHANGED, State, Pid) ->
    {Stage, Job} = installer:get_stage_and_job(State),
    case Stage of
        ?STAGE_IDLE -> Pid ! finish;
        _ -> Pid ! {change_step, installer:get_job_index(Stage, Job) - 1, get_info_message({Stage, Job})}
    end.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"install_button">>),
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#?STATE{}) end),
    installer:set_callback(fun(Event, State) -> installation_progress(Event, State, Pid) end),
    put(?COMET_PID, Pid);

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_ADD_STORAGE);

event(install) ->
    ToInstall = to_install(),
    case ToInstall#?CONFIG.workers of
        [] ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Nothing to install.">>),
            gui_ctx:put(?CURRENT_INSTALLATION_PAGE, ?PAGE_HOST_SELECTION);
        _ ->
            Pid = get(?COMET_PID),
            Pid ! {init, length(installer:get_flatten_stages())},
            Fields = record_info(fields, ?CONFIG),
            [_ | Values] = tuple_to_list(ToInstall),
            Config = lists:zip(Fields, Values),
            installer:install(Config, fun(Event, State) -> installation_progress(Event, State, Pid) end)
    end;

event(terminate) ->
    ok.