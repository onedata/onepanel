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

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {attempt = 1, pid}).

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
%% @doc This will be placed instead of {{title}} tag in template.
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
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 1: Connection check">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"To establish test connection to <i>Global Registry</i> please press <i>Next</i> button.">>
            },
            #panel{
                id = <<"progress">>,
                style = <<"width: 50%; margin: 0 auto; display: none;">>,
                body = [
                    #panel{
                        body = #image{
                            style = <<"width: 2em;">>,
                            image = <<"/images/spinner.gif">>
                        }
                    },
                    #p{
                        style = <<"margin-left: 1em;">>,
                        body = <<"Connecting...">>
                    }
                ]
            },
            onepanel_gui_utils:nav_buttons([{<<"next_button">>, {postback, connect}, false, <<"Next">>}])
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

comet_loop(#?STATE{attempt = Attempt, pid = Pid} = State) ->
    NewState = try
        receive
            connect ->
                NewPid = spawn_link(fun() ->
                    ok = installer_utils:check_ip_addresses()
                end),
                erlang:send_after(?CONNECTION_TIMEOUT, self(), {connection_failure, Attempt}),
                State#?STATE{pid = NewPid};

            {connection_failure, Attempt} ->
                onepanel_gui_utils:message(<<"error_message">>, <<"Cannot connect to Global Registry.<br>
                Please check your network configuration and try again later.">>),
                gui_jq:hide(<<"progress">>),
                gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
                State#?STATE{attempt = Attempt + 1};

            {'EXIT', Pid, normal} ->
                onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK),
                State;

            {'EXIT', Pid, _} ->
                self() ! connection_failure,
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
    gui_comet:flush(),
    ?MODULE:comet_loop(NewState).


%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop_init() end),
    put(?COMET_PID, Pid);

event(connect) ->
    get(?COMET_PID) ! connect,
    gui_jq:show(<<"progress">>),
    gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"disabled">>);

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.