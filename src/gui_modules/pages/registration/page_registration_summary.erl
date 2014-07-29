%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page displays registration summary and starts registration process.
%% @end
%% ===================================================================

-module(page_registration_summary).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

%% Default time in miliseconds for next progress bar update
-define(DEFAULT_NEXT_UPDATE, 50).

%% Time in miliseconds after which an error message will be displayed if registration process has not returned result
-define(REGISTRATION_TIMEOUT, 30000).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, state).
-record(?STATE, {counter = 0, progress, next_update}).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUMMARY, ?PAGE_REGISTRATION) of
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
    <<"Registration summary">>.


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
            onepanel_gui_utils:top_menu(registration_tab),

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
                        body = <<"Step 3: Registration summary.">>
                    },
                    #p{
                        body = <<"You software configuration has been successfully verified.">>
                    },
                    #panel{
                        id = <<"progress">>,
                        style = <<"margin-top: 30px; width: 50%; margin: 0 auto; margin-top: 30px; display: none;">>,
                        body = [
                            #p{
                                style = <<"font-weight: 300;">>,
                                body = <<"Registering...">>
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
                                id = <<"register_button">>,
                                postback = register,
                                class = <<"btn btn-inverse btn-small">>,
                                style = <<"float: right; width: 80px; font-weight: bold;">>,
                                body = <<"Register">>
                            }
                        ]
                    }
                ]
            }
        ] ++ onepanel_gui_utils:logotype_footer(120)
    }.


%% comet_loop/1
%% ====================================================================
%% @doc Handles connection process messages and updates progress bar.
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{counter = Counter, progress = Progress, next_update = NextUpdate} = State) ->
    NewState = try
        receive
            init ->
                gui_jq:hide(<<"error_message">>),
                gui_jq:set_width(<<"bar">>, <<"0%">>),
                gui_jq:show(<<"progress">>),
                gui_comet:flush(),
                timer:send_after(?REGISTRATION_TIMEOUT, {timeout, Counter + 1}),
                timer:send_after(?DEFAULT_NEXT_UPDATE, {update, Counter + 1}),
                State#?STATE{counter = Counter + 1, progress = 0, next_update = ?DEFAULT_NEXT_UPDATE};

            {update, Counter} ->
                NewProgress = (Progress + 1) / 2,
                NewProgressBinary = <<(integer_to_binary(round(100 * NewProgress)))/binary, "%">>,
                gui_jq:set_width(<<"bar">>, NewProgressBinary),
                gui_comet:flush(),
                timer:send_after(2 * NextUpdate, {update, Counter}),
                State#?STATE{progress = NewProgress, next_update = 2 * NextUpdate};

            {update, _} ->
                State;

            {timeout, Counter} ->
                self() ! registration_failure,
                State;

            {timeout, _} ->
                State;

            registration_success ->
                gui_jq:set_width(<<"bar">>, <<"100%">>),
                onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUCCESS),
                gui_comet:flush(),
                State;

            registration_failure ->
                onepanel_gui_utils:message(<<"error_message">>, <<"Cannot register in Global Registry. Please try again later.">>),
                gui_jq:hide(<<"progress">>),
                gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"">>),
                gui_jq:prop(<<"register_button">>, <<"disabled">>, <<"">>),
                gui_comet:flush(),
                State
        end
               catch Type:Reason ->
                   ?error("Comet process exception: ~p:~p", [Type, Reason]),
                   onepanel_gui_utils:message(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Reason}
               end,
    comet_loop(NewState).


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"register_button">>),
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#?STATE{}) end),
    put(?COMET_PID, Pid);

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK);

event(register) ->
    Pid = get(?COMET_PID),
    Pid ! init,
    spawn(fun() ->
        timer:sleep(2000),
        case gr_adapter:register() of
            {ok, _} -> Pid ! registration_success;
            _ -> Pid ! registration_failure
        end
    end),
    gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"disabled">>),
    gui_jq:prop(<<"register_button">>, <<"disabled">>, <<"disabled">>);

event(terminate) ->
    ok.