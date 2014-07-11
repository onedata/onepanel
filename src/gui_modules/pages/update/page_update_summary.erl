%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page displays update summary and starts updater process.
%% @end
%% ===================================================================

-module(page_update_summary).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/updater/common.hrl").
-include("onepanel_modules/updater/state.hrl").
-include("onepanel_modules/updater/stages.hrl").
-include_lib("ctool/include/logging.hrl").

%% Current 'force_reload_checkbox' state
-define(FORCE_RELOAD, force_reload).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, state).
-record(?STATE, {}).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_UPDATE_PAGE, ?PAGE_UPDATE_SUMMARY, ?PAGE_UPDATE) of
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
    <<"Update summary">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    #version{major = Major, minor = Minor, patch = Patch} = Version = gui_ctx:get(?CHOSEN_VERSION),
    ChosenVersionName = <<(integer_to_binary(Major))/binary, ".", (integer_to_binary(Minor))/binary, ".", (integer_to_binary(Patch))/binary>>,
    State = updater:get_state(),
    {UpdatePanelDisplay, UpdateProgressDisplay} = case updater_state:get_stage_and_job(State) of
                                                      {?STAGE_IDLE, _} -> {<<"">>, <<" display: none;">>};
                                                      _ -> {<<" display: none;">>, <<"">>}
                                                  end,
    #panel{
        style = <<"position: relative;">>,
        body = [
            onepanel_gui_utils:top_menu(update_tab),

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
                        body = <<"Step 2: Update summary.">>
                    },
                    #h6{
                        style = <<"font-size: 16px; margin-top: 30px; margin-bottom: 30px;">>,
                        body = <<"Software version to update to: <b>", ChosenVersionName/binary, "</b>">>
                    },
                    #panel{
                        id = <<"update_pannel">>,
                        style = <<"margin-top: 30px; width: 260px; height: 100px; margin: 0 auto;", UpdatePanelDisplay/binary>>,
                        body = [
                            #button{
                                id = <<"update_button">>,
                                class = <<"btn btn-primary">>,
                                postback = {update, Version},
                                style = <<"float: left; width: 80px;">>,
                                body = <<"Update">>
                            },
                            #panel{
                                class = <<"btn-group pull-right">>,
                                style = <<"float: right; padding: 7px 14px 0px;">>,
                                body = [
                                    #label{
                                        id = <<"force_reload_checkbox">>,
                                        class = <<"checkbox">>,
                                        for = <<"force_reload_checkbox">>,
                                        style = <<"display: block; font-weight: bold;">>,
                                        actions = gui_jq:postback_action(<<"force_reload_checkbox">>, force_reload_checkbox_toggled),
                                        body = [
                                            #span{
                                                class = <<"icons">>
                                            },
                                            #checkbox{
                                                id = <<"force_reload_checkbox">>,
                                                data_fields = [{<<"data-toggle">>, <<"checkbox">>}]
                                            },
                                            <<"Force node reload">>
                                        ]
                                    }
                                ]
                            }
                        ]
                    },
                    #panel{
                        id = <<"update_progress">>,
                        style = UpdateProgressDisplay,
                        body = [
                            #panel{
                                id = <<"stage_progress">>,
                                style = <<"text-align: left; margin-top: 30px; width: 50%; margin: 0 auto;">>,
                                body = [
                                    #p{
                                        id = <<"stage_progress_text">>,
                                        style = <<"font-weight: 300;">>,
                                        body = <<"Current stage">>
                                    },
                                    #panel{
                                        class = <<"progress">>,
                                        body = #panel{
                                            id = <<"stage_bar">>,
                                            class = <<"bar">>,
                                            style = <<"width: 0%;">>
                                        }
                                    }
                                ]
                            },
                            #panel{
                                id = <<"job_progress">>,
                                style = <<"text-align: left; margin-top: 30px; width: 50%; margin: 0 auto;">>,
                                body = [
                                    #p{
                                        id = <<"job_progress_text">>,
                                        style = <<"font-weight: 300;">>,
                                        body = <<"Current job">>
                                    },
                                    #panel{
                                        class = <<"progress">>,
                                        body = #panel{
                                            id = <<"job_bar">>,
                                            class = <<"bar">>,
                                            style = <<"width: 0%;">>
                                        }
                                    }
                                ]
                            },
                            #button{
                                id = <<"abort_button">>,
                                class = <<"btn btn-danger">>,
                                postback = abort,
                                style = <<"width: 80px; margin-top: 30px;">>,
                                body = <<"Abort">>
                            }
                        ]
                    }
                ]
            }
        ] ++ onepanel_gui_utils:logotype_footer(120)
    }.


%% translate_stage/1
%% ====================================================================
%% @doc Translates stage ID to human-readable version
-spec translate_stage(StageId :: atom()) -> binary().
%% ====================================================================
translate_stage(?STAGE_IDLE) -> <<"Idle">>;
translate_stage(?STAGE_INIT) -> <<"Initializing">>;
translate_stage(?STAGE_DAO_UPDATER_LOAD) -> <<"Loading database updater">>;
translate_stage(?STAGE_DAO_SETUP_VIEWS) -> <<"Setting up database views">>;
translate_stage(?STAGE_DAO_REFRESH_VIEWS) -> <<"Refreshing database views">>;
translate_stage(?STAGE_DEPLOY_FILES) -> <<"Deploying files">>;
translate_stage(?STAGE_SOFT_RELOAD) -> <<"Applying soft reload">>;
translate_stage(?STAGE_HARD_RELOAD) -> <<"Applying hard reload">>;
translate_stage(?STAGE_FORCE_RELOAD) -> <<"Applying force reload">>;
translate_stage(?STAGE_NODE_RESTART) -> <<"Restarting nodes">>;
translate_stage(?STAGE_ROLLBACK) -> <<"Rollbacking">>;
translate_stage(?STAGE_DAO_POST_SETUP_VIEWS) -> <<"Applying post update database views setup">>;
translate_stage(?STAGE_REPAIR_NODES) -> <<"Repairing nodes">>;
translate_stage(_) -> <<"">>.


%% translate_job/1
%% ====================================================================
%% @doc Translates job ID to human-readable version
-spec translate_job(JobId :: atom()) -> binary().
%% ====================================================================
translate_job(?JOB_DOWNLOAD_BINARY) -> <<"Downloading binary">>;
translate_job(?JOB_LOAD_EXPORTS) -> <<"Loading exports">>;
translate_job(?JOB_RELOAD_EXPORTS) -> <<"Reloading exports">>;
translate_job(?JOB_INSTALL_PACKAGE) -> <<"Installing package">>;
translate_job(?JOB_DEFAULT) -> <<"Updating">>;
translate_job(?JOB_MOVE_BEAMS) -> <<"Moving beam files">>;
translate_job(?JOB_LOAD_BEAMS) -> <<"Loading beam files">>;
translate_job(?JOB_PRE_UPDATE) -> <<"Applying preupdate">>;
translate_job(?JOB_INSTALL_VIEW_SOURCES) -> <<"Installing view sources">>;
translate_job(?JOB_INSTALL_VIEWS) -> <<"Installing database views">>;
translate_job(?JOB_BACKUP) -> <<"Backuping">>;
translate_job(?JOB_DEPLOY) -> <<"Deploying">>;
translate_job(?JOB_CLEANUP_VIEWS) -> <<"Cleaning up database views">>;
translate_job(?JOB_CHECK_CONNECTIVITY) -> <<"Checking connectivity">>;
translate_job(_) -> <<"">>.


%% update_progress/3
%% ====================================================================
%% @doc Updater callback.
-spec update_progress(Pid :: pid(), Event :: atom(), State :: atom()) -> no_return().
%% ====================================================================
update_progress(_Pid, Event, State) ->
    ?info("Update progress!"),
    {Stage, Job} = updater_state:get_stage_and_job(State),
    ?info("Event: ~p", [Event]),
    ?info("Job: ~p", [translate_job(Job)]),
    ?info("Stage: ~p", [translate_stage(Stage)]).
%%     Pid ! {update, Job, Stage}.


%% comet_loop/1
%% ====================================================================
%% @doc Handles installation process and updates progress bar.
-spec comet_loop(State :: #?STATE{}) -> no_return().
%% ====================================================================
comet_loop(#?STATE{} = _State) ->
    try
        receive
            ok -> ok
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
    gui_jq:bind_key_to_click(<<"13">>, <<"update_button">>),

    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#?STATE{}) end),
    updater:set_callback(fun(Event, State) -> update_progress(Pid, Event, State) end),
    put(?COMET_PID, Pid),

    put(?FORCE_RELOAD, false),
    ok;

event(force_reload_checkbox_toggled) ->
    ForceReload = get(?FORCE_RELOAD),
    ?info("Force reload: ~p", [ForceReload]),
    put(?FORCE_RELOAD, not ForceReload);

event({update, Version}) ->
    gui_jq:prop(<<"update_button">>, <<"disabled">>, <<"disabled">>),
    gui_jq:prop(<<"force_reload_checkbox">>, <<"disabled">>, <<"disabled">>),
    gui_jq:show(<<"update_progress">>),
    ForceReload = get(?FORCE_RELOAD),
    ?info("Version: ~p", [Version]),
    ?info("Force reload: ~p", [ForceReload]),
    updater:update_to(Version, ForceReload);

event(terminate) ->
    ok.