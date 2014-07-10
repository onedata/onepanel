%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page allows to check connection to Global Registry.
%% @end
%% ===================================================================

-module(page_connection_check).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

-define(STATE, state).
-define(UPDATE_TIME, 500).

-record(?STATE, {step, steps, status}).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK, ?PAGE_REGISTRATION) of
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
    <<"Connection check">>.


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
                body = case installer_utils:get_workers() of
                           [] ->
                               #panel{
                                   style = <<"width: 50%; margin: 0 auto;">>,
                                   class = <<"alert alert-info">>,
                                   body = [
                                       #h3{
                                           body = <<"Software is not installed.">>
                                       },
                                       #p{
                                           body = <<"Please complete installation process in order to register in Global Registry as a provider.">>
                                       },
                                       #link{
                                           id = <<"next_button">>,
                                           postback = to_main_page,
                                           class = <<"btn btn-info">>,
                                           style = <<"width: 80px; font-weight: bold;">>,
                                           body = <<"OK">>
                                       }
                                   ]
                               };
                           _ ->
                               [
                                   #h6{
                                       style = <<"font-size: 18px;">>,
                                       body = <<"Step 1: Check your connection to Global Registry.">>
                                   },
                                   #panel{
                                       id = <<"progress">>,
                                       style = <<"margin-top: 30px; width: 50%; margin: 0 auto; margin-top: 30px; display: none;">>,
                                       body = [
                                           #p{
                                               style = <<"font-weight: 300;">>,
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
                                       style = <<"margin-top: 30px; margin-bottom: 30px;">>,
                                       body = #button{
                                           id = <<"next_button">>,
                                           postback = next,
                                           class = <<"btn btn-inverse btn-small">>,
                                           style = <<"width: 80px; font-weight: bold;">>,
                                           body = <<"Next">>
                                       }
                                   }
                               ]
                       end

            }
        ] ++ onepanel_gui_utils:logotype_footer(120)
    }.


%% comet_loop/1
%% ====================================================================
%% @doc Handles connection process and updates progress bar.
-spec comet_loop(State :: #?STATE{}) -> no_return().
%% ====================================================================
comet_loop(#?STATE{step = Step, steps = Steps, status = Status} = State) ->
    try
        receive
            {init, InitSteps} ->
                gui_jq:hide(<<"error_message">>),
                gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"disabled">>),
                gui_jq:set_width(<<"bar">>, <<"0%">>),
                gui_jq:show(<<"progress">>),
                gui_comet:flush(),
                timer:send_after(?UPDATE_TIME, update),
                comet_loop(State#?STATE{step = 1, steps = InitSteps, status = connecting});

            update ->
                Progress = <<(integer_to_binary(round(100 * Step / Steps)))/binary, "%">>,
                ?info("Progress: ~p", [Progress]),
                gui_jq:set_width(<<"bar">>, Progress),
                gui_comet:flush(),
                case Step of
                    Steps ->
                        case Status of
                            connection_success ->
                                timer:sleep(?UPDATE_TIME),
                                gui_jq:set_width(<<"bar">>, <<"100%">>),
                                onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK),
                                gui_comet:flush();
                            connection_error ->
                                gui_jq:hide(<<"progress">>),
                                gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
                                onepanel_gui_utils:message(<<"error_message">>, <<"Cannot connect to Global Registry.<br>
                                Please check your network configuration and try again later.">>),
                                gui_comet:flush(),
                                comet_loop(State#?STATE{status = idle});
                            connecting ->
                                timer:send_after(?UPDATE_TIME, update),
                                comet_loop(State);
                            _ ->
                                comet_loop(State)
                        end;
                    _ ->
                        gui_comet:flush(),
                        timer:send_after(?UPDATE_TIME, update),
                        comet_loop(State#?STATE{step = Step + 1})
                end;

            {set_status, NewStatus} ->
                comet_loop(State#?STATE{status = NewStatus})
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
    gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#?STATE{}) end),
    put(comet_pid, Pid);

event(to_main_page) ->
    gui_jq:redirect(?PAGE_ROOT);

event(next) ->
    Pid = get(comet_pid),
    spawn(fun() ->
        Pid ! {init, round(?CONNECTION_TIMEOUT / ?UPDATE_TIME)},
        case gr_adapter:check_ip_address() of
            {ok, _} -> Pid ! {set_status, connection_success};
            _ -> Pid ! {set_status, connection_error}
        end
    end);

event(terminate) ->
    ok.