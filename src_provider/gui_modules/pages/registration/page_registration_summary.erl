%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page displays registration summary and starts registration process.
%% @end
%% ===================================================================
-module(page_registration_summary).

-include("gui_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).


%% Time in miliseconds after which an error message will be displayed if registration process has not returned result
-define(REGISTRATION_TIMEOUT, 30000).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {pid}).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUMMARY, ?PAGE_SPACES_ACCOUNT) of
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
%% @doc Page title.
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Registration summary">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils_adapter:top_menu(spaces_tab, spaces_account_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 3: Registration summary.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"You software configuration has been successfully verified.">>
            },
            #panel{
                id = <<"progress">>,
                style = <<"width: 50%; margin: 0 auto; display: none; justify-content: center;">>,
                body = [
                    #panel{
                        body = #image{
                            style = <<"width: 2em;">>,
                            image = <<"/images/spinner.gif">>
                        }
                    },
                    #p{
                        style = <<"margin-left: 1em;">>,
                        body = <<"Registering...">>
                    }
                ]
            },
            onepanel_gui_utils:nav_buttons([
                {<<"back_button">>, {postback, back}, false, <<"Back">>},
                {<<"register_button">>, {postback, register}, false, <<"Register">>}
            ])
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% ====================================================================
%% Events handling
%% ====================================================================

%% comet_loop_init/0
%% ====================================================================
%% @doc Initializes comet loop.
%% @end
-spec comet_loop_init() -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop_init() ->
    process_flag(trap_exit, true),
    comet_loop(#?STATE{}).


%% comet_loop/1
%% ====================================================================
%% @doc Handles connection process messages and updates progress bar.
%% @end
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{pid = Pid} = State) ->
    NewState = try
        receive
            register ->
                NewPid = spawn_link(fun() ->
                    {ok, _} = provider_logic:register()
                end),
                erlang:send_after(?REGISTRATION_TIMEOUT, self(), registration_failure),
                State#?STATE{pid = NewPid};

            registration_failure ->
                onepanel_gui_utils:message(<<"error_message">>, <<"Cannot register in <i>Global Registry</i>.<br>Please try again later.">>),
                gui_jq:hide(<<"progress">>),
                gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"">>),
                gui_jq:prop(<<"register_button">>, <<"disabled">>, <<"">>),
                gui_comet:flush(),
                State;

            {'EXIT', Pid, normal} ->
                onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUCCESS),
                gui_comet:flush(),
                State;

            {'EXIT', Pid, _} ->
                self() ! registration_failure,
                State;

            _ ->
                State

        after ?COMET_PROCESS_RELOAD_DELAY ->
            State
        end
               catch Type:Message ->
                   ?error("Comet process exception: ~p:~p", [Type, Message]),
                   onepanel_gui_utils:message(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Message}
               end,
    ?MODULE:comet_loop(NewState).


%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"register_button">>),
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop_init() end),
    put(?COMET_PID, Pid);

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK);

event(register) ->
    get(?COMET_PID) ! register,
    gui_jq:css(<<"progress">>, <<"display">>, <<"flex">>),
    gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"disabled">>),
    gui_jq:prop(<<"register_button">>, <<"disabled">>, <<"disabled">>);

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.