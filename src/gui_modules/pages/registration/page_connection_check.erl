%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to check connection to Global Registry.
%% @end
%% ===================================================================

-module(page_connection_check).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% Default time in miliseconds for next progress bar update
-define(DEFAULT_NEXT_UPDATE, 500).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {step, steps, status}).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK, ?PAGE_SPACES_ACCOUNT) of
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
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Connection check">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_account_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #h6{
                style = <<"font-size: x-large; margin-bottom: 3em;">>,
                body = <<"Step 1: Check your connection to Global Registry.">>
            },
            #panel{
                id = <<"progress">>,
                style = <<"width: 50%; margin: 0 auto; display: none;">>,
                body = [
                    #p{
                        body = <<"Connecting...">>
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
                style = <<"margin-top: 3em;">>,
                body = #button{
                    id = <<"next_button">>,
                    postback = next,
                    class = <<"btn btn-inverse btn-small">>,
                    style = <<"width: 8em; font-weight: bold;">>,
                    body = <<"Next">>
                }
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% comet_loop/1
%% ====================================================================
%% @doc Handles connection process messages and updates progress bar.
%% @end
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{step = Step, steps = Steps, status = Status} = State) ->
    NewState = try
        receive
            {init, InitSteps} ->
                gui_jq:hide(<<"error_message">>),
                gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"disabled">>),
                gui_jq:set_width(<<"bar">>, <<"0%">>),
                gui_jq:show(<<"progress">>),
                gui_comet:flush(),
                timer:send_after(?DEFAULT_NEXT_UPDATE, update),
                State#?STATE{step = 1, steps = InitSteps, status = connecting};

            update ->
                Progress = <<(integer_to_binary(round(100 * Step / Steps)))/binary, "%">>,
                gui_jq:set_width(<<"bar">>, Progress),
                gui_comet:flush(),
                case Step of
                    Steps ->
                        gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
                        case Status of
                            connection_success ->
                                timer:sleep(?DEFAULT_NEXT_UPDATE),
                                gui_jq:set_width(<<"bar">>, <<"100%">>),
                                onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK),
                                gui_comet:flush(),
                                State;
                            _ ->
                                gui_jq:hide(<<"progress">>),
                                gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
                                onepanel_gui_utils:message(<<"error_message">>, <<"Cannot connect to Global Registry.<br>
                                Please check your network configuration and try again later.">>),
                                gui_comet:flush(),
                                State#?STATE{status = idle}
                        end;
                    _ ->
                        gui_comet:flush(),
                        timer:send_after(?DEFAULT_NEXT_UPDATE, update),
                        State#?STATE{step = Step + 1}
                end;

            {set_status, NewStatus} ->
                State#?STATE{status = NewStatus}
        end
               catch Type:Message ->
                   ?error("Comet process exception: ~p:~p", [Type, Message]),
                   onepanel_gui_utils:message(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Message}
               end,
    comet_loop(NewState).


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#?STATE{}) end),
    put(?COMET_PID, Pid);

event(next) ->
    Pid = get(?COMET_PID),
    Pid ! {init, round(?CONNECTION_TIMEOUT / ?DEFAULT_NEXT_UPDATE)},
    spawn(fun() ->
        timer:sleep(1000),
        case gr_providers:check_ip_address(provider, ?CONNECTION_TIMEOUT) of
            {ok, _} -> Pid ! {set_status, connection_success};
            _ -> Pid ! {set_status, connection_error}
        end
    end),
    gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"disabled">>);

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.