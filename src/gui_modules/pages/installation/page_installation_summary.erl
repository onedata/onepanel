%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page displays installation summary and starts installer process.
%% @end
%% ===================================================================
-module(page_installation_summary).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/stages.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

%% Convenience record abbreviation
-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

%% Default time in miliseconds for next progress bar update
-define(NEXT_UPDATE_DELAY, 1000).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {step = 0, steps = 0, step_progress = 0, next_update = ?NEXT_UPDATE_DELAY, config = #?CONFIG{}, error_state}).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUMMARY, ?PAGE_INSTALLATION) of
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
    <<"Installation summary">>.


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
        {<<"Primary CM selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CM_SELECTION},
        {<<"Application ports check">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_APP_PORTS_CHECK},
        {<<"System limits">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_SYSTEM_LIMITS},
        {<<"Installation summary">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUMMARY}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, installation_link, Breadcrumbs, true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 5: Installation summary.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"Current application configuration is summarised in the table below.">>
            },
            #table{
                class = <<"table table-striped">>,
                style = <<"width: 50%; margin: 0 auto;">>,
                body = #tbody{
                    id = <<"summary_table">>,
                    style = <<"display: none;">>
                }
            },
            #panel{
                id = <<"progress">>,
                style = <<"text-align: left; margin-top: 3em; width: 50%; margin: 0 auto; margin-top: 3em; display: none;">>,
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
            onepanel_gui_utils:nav_buttons([
                {<<"back_button">>, {postback, back}, false, <<"Back">>},
                {<<"install_button">>, {postback, install}, true, <<"Install">>}
            ])
        ]
    },
    onepanel_gui_utils:body(?SUBMENU_HEIGHT, Header, Main, onepanel_gui_utils:logotype_footer()).


%% summary_table/1
%% ====================================================================
%% @doc Renders summary table body.
%% @end
-spec summary_table(Config :: #?CONFIG{}) -> Result
    when Result :: [#tr{}].
%% ====================================================================
summary_table(#?CONFIG{main_cm = MainCM, cms = CMs, workers = Workers, dbs = Dbs}) ->
    lists:map(fun({Id, Description, Details}) ->
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
        }
    end, [
        {<<"summary_main_cm">>, <<"Primary CM host">>, format(MainCM)},
        {<<"summary_cms">>, <<"CM hosts">>, format(CMs)},
        {<<"summary_workers">>, <<"Worker hosts">>, format(Workers)},
        {<<"summary_dbs">>, <<"Database hosts">>, format(Dbs)}
    ]).


%% format/1
%% ====================================================================
%% @doc Formats list of hosts, which will be displayed in 'Details'
%% column of installation summary tabel.
%% @end
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
            body = http_utils:html_encode(Host),
            style = <<"text-align: center; margin-bottom: 0; font-weight: 400;">>
        }
    end, Hosts).


%% get_error_message/1
%% ====================================================================
%% @doc Returns error message for given stage and job of installation.
%% @end
-spec get_error_message({State :: atom(), Job :: atom()}) -> binary().
%% ====================================================================
get_error_message({?STAGE_DB, ?JOB_INSTALL}) -> <<"Database components were not installed on following hosts: ">>;
get_error_message({?STAGE_DB, ?JOB_START}) -> <<"Database components were not started on following hosts: ">>;
get_error_message({?STAGE_CM, ?JOB_INSTALL}) -> <<"CM components were not installed on following hosts: ">>;
get_error_message({?STAGE_CM, ?JOB_START}) -> <<"CM components were not started on following hosts: ">>;
get_error_message({?STAGE_WORKER, ?JOB_INSTALL}) -> <<"Worker components were not installed on following hosts: ">>;
get_error_message({?STAGE_WORKER, ?JOB_START}) -> <<"Worker components were not started on following hosts: ">>;
get_error_message(_) -> <<"">>.


%% get_info_message/1
%% ====================================================================
%% @doc Returns information for given stage and job of installation which
%% will be displayed above installation progress bar.
%% @end
-spec get_info_message({State :: atom(), Job :: atom()}) -> Result when
    Result :: binary().
%% ====================================================================
get_info_message({?STAGE_DB, ?JOB_INSTALL}) -> <<"Current stage: <b>Installing database components</b>">>;
get_info_message({?STAGE_DB, ?JOB_START}) -> <<"Current stage: <b>Starting database components</b>">>;
get_info_message({?STAGE_CM, ?JOB_INSTALL}) ->
    <<"Current stage: <b>Installing Cluster Manager components</b>">>;
get_info_message({?STAGE_CM, ?JOB_START}) -> <<"Current stage: <b>Starting Cluster Manager components</b>">>;
get_info_message({?STAGE_WORKER, ?JOB_INSTALL}) -> <<"Current stage: <b>Installing worker components</b>">>;
get_info_message({?STAGE_WORKER, ?JOB_START}) -> <<"Current stage: <b>Starting worker components</b>">>;
get_info_message({?STAGE_FINAL, ?JOB_FINALIZE_INSTALLATION}) -> <<"Current stage: <b>Finalizing installation</b>">>;
get_info_message(_) -> <<"">>.


%% installation_progress/1
%% ====================================================================
%% @doc Callback function called by installer gen_server which updates
%% installation progress bar.
%% @end
-spec installation_progress(Event :: atom(), State :: #?I_STATE{}, Pid :: pid()) -> no_return().
%% ====================================================================
installation_progress(?EVENT_ERROR, State, Pid) ->
    case installer:get_error(State) of
        {error, {hosts, Hosts}} ->
            Pid ! {error, <<(get_error_message(installer:get_stage_and_job(State)))/binary, (onepanel_gui_utils:format_list(Hosts))/binary,
            ".<br>Please try again later.">>, State};
        {error, Reason} when is_list(Reason) ->
            Pid ! {error, list_to_binary(Reason), State};
        _ ->
            Pid ! {error, <<"An error occurred during installation.<br>Please try again later.">>, State}
    end;

installation_progress(?EVENT_STATE_CHANGED, State, Pid) ->
    {Stage, Job} = installer:get_stage_and_job(State),
    case Stage of
        ?STAGE_IDLE -> Pid ! finish;
        ?STAGE_FINAL ->
            Pid ! {change_step, installer:get_job_index(Stage, Job) - 1, get_info_message({Stage, Job}), 5 * ?NEXT_UPDATE_DELAY};
        _ ->
            Pid ! {change_step, installer:get_job_index(Stage, Job) - 1, get_info_message({Stage, Job}), ?NEXT_UPDATE_DELAY}
    end.


%% ====================================================================
%% Events handling
%% ====================================================================

%% comet_loop/1
%% ====================================================================
%% @doc Handles installer process messages and updates progress bar.
%% @end
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{step = Step, steps = Steps, step_progress = StepProgress, next_update = NextUpdate,
    config = Config, error_state = ErrorState} = State) ->
    NewState =
        try
            receive
                render_summary_table ->
                    gui_jq:update(<<"summary_table">>, summary_table(Config)),
                    gui_jq:fade_in(<<"summary_table">>, 500),
                    case Config#?CONFIG.workers of
                        [] ->
                            ok;
                        _ ->
                            gui_jq:prop(<<"install_button">>, <<"disabled">>, <<"">>)
                    end,
                    State;

                install ->
                    Username = gui_ctx:get_user_id(),
                    case gen_server:call(?ONEPANEL_SERVER, {get_password, Username}) of
                        {ok, Password} when is_binary(Password) ->
                            self() ! {init, <<"Current stage:">>},
                            Fields = record_info(fields, ?CONFIG),
                            [_ | Values] = tuple_to_list(Config#?CONFIG{main_cm = case Config#?CONFIG.main_cm of
                                                                                       [] -> undefined;
                                                                                       [MainCM | _] -> MainCM
                                                                                   end}),
                            NewConfig = [{username, Username}, {password, Password} | lists:zip(Fields, Values)],
                            Pid = self(),
                            installer:install(NewConfig, ErrorState, fun(Event, InstallerState) ->
                                installation_progress(Event, InstallerState, Pid)
                            end);
                        Other ->
                            ?error("Cannot get password to administrative database for user ~p: ~p", [Username, Other]),
                            onepanel_gui_utils:message(error, <<"Cannot get password to administrative database for user: ", Username/binary>>)
                    end,
                    State;

                {init, Text} ->
                    gui_jq:remove(<<"top_menu_message">>),
                    gui_jq:prop(<<"install_button">>, <<"disabled">>, <<"disabled">>),
                    gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"disabled">>),
                    gui_jq:update(<<"progress_text">>, Text),
                    gui_jq:set_width(<<"bar">>, <<"0%">>),
                    gui_jq:show(<<"progress">>),
                    State;

                {change_step, NewStep, Text, NewNextUpdate} ->
                    gui_jq:show(<<"progress">>),
                    gui_jq:prop(<<"install_button">>, <<"disabled">>, <<"disabled">>),
                    gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"disabled">>),
                    Progress = <<(integer_to_binary(round(99 * NewStep / Steps)))/binary, "%">>,
                    gui_jq:update(<<"progress_text">>, <<Text/binary, " <b>(", Progress/binary, ")</b>">>),
                    gui_jq:set_width(<<"bar">>, Progress),
                    timer:send_after(NextUpdate, {update, NewStep, Text}),
                    State#?STATE{step = NewStep, step_progress = 0, next_update = NewNextUpdate};

                {update, Step, Text} ->
                    NewStepProgress = StepProgress + (1 - StepProgress) / 2,
                    Progress = <<(integer_to_binary(round(99 * (Step + NewStepProgress) / Steps)))/binary, "%">>,
                    gui_jq:update(<<"progress_text">>, <<Text/binary, " <b>(", Progress/binary, ")</b>">>),
                    gui_jq:set_width(<<"bar">>, Progress),
                    timer:send_after(NextUpdate, {update, Step, Text}),
                    State#?STATE{step_progress = NewStepProgress, next_update = 2 * NextUpdate};

                {update, _, _} ->
                    State;

                finish ->
                    gui_jq:update(<<"progress_text">>, <<"">>),
                    gui_jq:set_width(<<"bar">>, <<"100%">>),
                    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_INSTALLATION_SUCCESS),
                    State#?STATE{step = undefined};

                {error, Text, NewErrorState} ->
                    gui_jq:update(<<"summary_table">>, summary_table(Config)),
                    onepanel_gui_utils:message(error, Text),
                    gui_jq:prop(<<"install_button">>, <<"disabled">>, <<"">>),
                    gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"">>),
                    gui_jq:hide(<<"progress">>),
                    State#?STATE{step = -1, error_state = NewErrorState}

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
    gui_jq:bind_key_to_click(<<"13">>, <<"install_button">>),
    try
        {ok, DbConfig} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, SessionConfig} = onepanel_gui_utils:get_session_config(),

        Config = #?CONFIG{
            main_cm = [SessionConfig#?CONFIG.main_cm] -- [DbConfig#?CONFIG.main_cm],
            cms = lists:sort(SessionConfig#?CONFIG.cms -- DbConfig#?CONFIG.cms),
            workers = lists:sort(SessionConfig#?CONFIG.workers -- DbConfig#?CONFIG.workers),
            dbs = lists:sort(SessionConfig#?CONFIG.dbs -- DbConfig#?CONFIG.dbs)
        },

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#?STATE{config = Config, steps = length(installer:get_flatten_stages())})
        end),
        put(?COMET_PID, Pid),
        Pid ! render_summary_table,

        case installer:get_stage_and_job(installer:get_state()) of
            {?STAGE_INIT, _} -> ok;
            _ -> Pid ! {init, <<"Getting installation progress...">>}
        end,
        installer:set_callback(fun(Event, State) -> installation_progress(Event, State, Pid) end)
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_SYSTEM_LIMITS);

event(install) ->
    get(?COMET_PID) ! install;

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.