%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page displays summary of VeilCluster nodes installation and
%% executes installation steps.
%% @end
%% ===================================================================

-module(page_installation_summary).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/db/common.hrl").
-include_lib("ctool/include/logging.hrl").

-define(CONFIG, ?GLOBAL_CONFIG_RECORD).
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
            case onepanel_gui_utils:maybe_redirect("/installation_summary") of
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
                        style = <<"width: 50%; margin: 0 auto; margin-top: 20px;">>,
                        body = [
                            #tbody{
                                id = <<"summary_table">>,
                                body = summary_table_body()
                            }
                        ]
                    },
                    #panel{
                        id = <<"progress">>,
                        style = <<"margin-top: 30px; width: 50%; margin: 0 auto; margin-top: 30px; display: none;">>,
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
                                id = <<"prev_button">>,
                                postback = back,
                                class = <<"btn btn-inverse btn-small">>,
                                style = <<"float: left; width: 80px; font-weight: bold;">>,
                                body = <<"Back">>},
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
    #?CONFIG{main_ccm = MainCCM, opt_ccms = OptCCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths} = to_install(),
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
        summary_table_row(<<"summary_opt_ccms">>, <<"Optional CCM hosts">>, format(OptCCMs)),
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
        OptCCMs = Session#?CONFIG.opt_ccms -- Db#?CONFIG.opt_ccms,
        Workers = Session#?CONFIG.workers -- Db#?CONFIG.workers,
        Dbs = Session#?CONFIG.dbs -- Db#?CONFIG.dbs,
        StoragePaths = Session#?CONFIG.storage_paths -- Db#?CONFIG.storage_paths,

        #?CONFIG{main_ccm = MainCCM, opt_ccms = OptCCMs, workers = Workers, dbs = Dbs, storage_paths = StoragePaths}
    catch
        _:_ -> #?CONFIG{}
    end.


install(#?CONFIG{main_ccm = undefined, workers = Workers, storage_paths = StoragePaths}, State, Pid) ->
    try
        Step = case State of
                   init ->
                       Pid ! {init, 1, 1000, <<"Installing worker nodes...">>},
                       1;
                   _ ->
                       Pid ! {change_step, 5, 1000, <<"Installing worker nodes...">>},
                       5
               end,
%%         {worker_install, ok} = {worker_install, installer_worker:install([{hosts, Workers}])},
        ?info("Worker"),
        timer:sleep(5000),

        Pid ! {change_step, Step + 1, 1000, <<"Adding storage paths...">>},
%%         lists:foreach(fun(StoragePath) ->
%%             {storage_paths, ok, _} = {storage_paths, installer_storage:add_storage_path(Workers, StoragePath), StoragePath}
%%         end, StoragePaths),
        ?info("Storage"),
        timer:sleep(5000),

        Pid ! {change_step, Step + 2, 10, <<"Starting worker nodes...">>},
%%         {worker_start, ok} = {worker_start, installer_worker:start([{workers, Workers}])},
        ?info("Worker"),
        timer:sleep(5000),

        ok
    catch
        _:{badmatch, {worker_install, {error, {hosts, Hosts}}}} ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Worker nodes were not installed on following hosts: ",
            (onepanel_gui_utils:format_list(Hosts))/binary, ". Please try again.">>);

        _:{badmatch, {storage_paths, {error, {hosts, Hosts}}, StoragePath}} ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Storage path ", (list_to_binary(StoragePath))/binary,
            " was not added on following hosts: ", (onepanel_gui_utils:format_list(Hosts))/binary, ". Please try again.">>);

        _:{badmatch, {worker_start, {error, {hosts, Hosts}}}} ->
            onepanel_gui_utils:message(<<"error_message">>, <<"CCM nodes were not installed on following hosts: ",
            (onepanel_gui_utils:format_list(Hosts))/binary, ". Please try again.">>)
    end;

install(#?CONFIG{main_ccm = MainCCM, opt_ccms = OptCCMs, dbs = Dbs} = Config, _, Pid) ->
    try
        ?info("Pid: ~p", [Pid]),
        Pid ! {init, 8, 1000, <<"Installing database nodes...">>},
%%         {db_install, ok} = {db_install, installer_db:install([{hosts, Dbs}])},
        ?info("Db"),
        timer:sleep(5000),

        Pid ! {change_step, 2, 10, <<"Starting database nodes...">>},
%%         {db_start, ok} = {db_start, installer_db:start([{hosts, Dbs}])},
        ?info("Db"),
        timer:sleep(5000),

        Pid ! {change_step, 3, 1000, <<"Installing CCM nodes...">>},
%%         {ccm_install, ok} = {ccm_install, installer_ccm:install([{hosts, [MainCCM | OptCCMs]}])},
        ?info("CCM"),
        timer:sleep(5000),

        Pid ! {change_step, 4, 10, <<"Starting CCM nodes...">>},
%%         {ccm_start, ok} = {ccm_start, installer_ccm:start([{main_ccm, MainCCM}, {opt_ccms, OptCCMs}])},
        ?info("CCM"),
        timer:sleep(5000),

        {workers, ok} = {workers, install(Config#?CONFIG{main_ccm = undefined}, continue, Pid)},

        Pid ! {change_step, 8, 1000, <<"Finalizing installation...">>},
%%         ok = finalize_installation(MainCCM),
        timer:sleep(5000),

        Pid ! {finish, <<"Done">>},
        timer:sleep(5000),

        ok
    catch
        _:{badmatch, {db_install, {error, {hosts, Hosts}}}} ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Database nodes were not installed on following hosts: ",
            (onepanel_gui_utils:format_list(Hosts))/binary, ". Please try again.">>);

        _:{badmatch, {db_start, {error, {hosts, Hosts}}}} ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Database nodes were not started on following hosts: ",
            (onepanel_gui_utils:format_list(Hosts))/binary, ". Please try again.">>);

        _:{badmatch, {ccm_install, {error, {hosts, Hosts}}}} ->
            onepanel_gui_utils:message(<<"error_message">>, <<"CCM nodes were not installed on following hosts: ",
            (onepanel_gui_utils:format_list(Hosts))/binary, ". Please try again.">>);

        _:{badmatch, {ccm_start, {error, {hosts, Hosts}}}} ->
            onepanel_gui_utils:message(<<"error_message">>, <<"CCM nodes were not started on following hosts: ",
            (onepanel_gui_utils:format_list(Hosts))/binary, ". Please try again.">>);

        _:{badmatch, {workers, _}} ->
            ok;

        _:Reason ->
            ?error("Installation failure: ~p", [Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"An error occurred during installation. Please try again.">>)
    end.


%% finalize_installation/2
%% ====================================================================
%% @doc Waits until cluster control panel nodes are up and running.
-spec finalize_installation(MainCCM :: string()) -> Result when
    Result :: ok.
%% ====================================================================
finalize_installation(MainCCM) ->
    case installer_utils:get_control_panel_hosts(MainCCM) of
        {ok, [_ | _]} ->
            ok;
        _ ->
            timer:sleep(1000),
            finalize_installation(MainCCM)
    end.


%% comet_loop/1
%% ====================================================================
%% @doc Handles installation process and updates progress bar.
-spec comet_loop(State :: atom()) -> no_return().
%% ====================================================================
comet_loop(#?STATE{step = Step, steps = Steps, step_progress = StepProgress, next_update = NextUpdate} = State) ->
    ?info("LOOP"),
    try
        receive
            {init, InitSteps, InitNextUpdate, Text} ->
                ?info("INIT"),
                gui_jq:set_text(<<"progress_text">>, Text),
                gui_jq:set_width(<<"bar">>, <<"0%">>),
                gui_jq:show(<<"progress">>),
                gui_comet:flush(),
                timer:send_after(InitNextUpdate, {update, 1}),
                comet_loop(State#?STATE{step = 1, steps = InitSteps, step_progress = 0, next_update = InitNextUpdate});

            {change_step, NewStep, InitNextUpdate, Text} ->
                gui_jq:set_text(<<"progress_text">>, Text),
                Progress = <<(integer_to_binary(round(100 * Step / Steps)))/binary, "%">>,
                gui_jq:set_width(<<"bar">>, Progress),
                gui_comet:flush(),
                timer:send_after(InitNextUpdate, {update, NewStep}),
                comet_loop(State#?STATE{step = NewStep, step_progress = 0, next_update = InitNextUpdate});

            {update, Step} ->
                NewStepProgress = StepProgress + (1 - StepProgress) / 2,
                Progress = <<(integer_to_binary(round(100 * (Step + NewStepProgress - 1) / Steps)))/binary, "%">>,
                gui_jq:set_width(<<"bar">>, Progress),
                gui_comet:flush(),
                timer:send_after(NextUpdate, {update, Step}),
                comet_loop(State#?STATE{step_progress = NewStepProgress, next_update = 2 * NextUpdate});

            {update, _} ->
                comet_loop(State);

            {finish, Text} ->
                gui_jq:set_text(<<"progress_text">>, Text),
                gui_jq:set_width(<<"bar">>, <<"100%">>),
                gui_jq:hide(<<"progress">>),
                gui_comet:flush(),
                comet_loop(State#?STATE{step = undefined});

            {disable_button, ButtonId} ->
                ?info("Disable button"),
                gui_jq:prop(ButtonId, <<"disabled">>, <<"disabled">>),
                gui_comet:flush(),
                comet_loop(State)
        end
    catch Type:Reason ->
        ?error("Comet process exception: ~p:~p", [Type, Reason]),
        onepanel_gui_utils:message(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>)
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
    put(comet_pid, Pid);

event(back) ->
    onepanel_gui_utils:change_page(?INSTALL_STEP, "/add_storage");

event(install) ->
    ?info("Install"),
    ToInstall = to_install(),
    {ok, #?CONFIG{storage_paths = StoragePaths}} = onepanel_gui_utils:get_installation_state(),
    ?info("Here"),
    case ToInstall#?CONFIG.workers of
        [] ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Nothing to install.">>);
        _ ->
            gui_jq:prop(<<"install_button">>, <<"disabled">>, <<"disabled">>),
            gui_jq:prop(<<"prev_button">>, <<"disabled">>, <<"disabled">>),
            ?info("HERE"),
            Pid = get(comet_pid),
            Ans = spawn(fun() -> install(ToInstall#?CONFIG{storage_paths = StoragePaths}, init, Pid) end),
            ?info("Ans: ~p", [Ans])
    end;

event(terminate) ->
    ok.