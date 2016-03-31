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
%% @doc This will be placed instead of {{title}} tag in template.
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
    Breadcrumbs = onepanel_gui_utils:breadcrumbs([
        {<<"Connection check">>, ?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK},
        {<<"Ports configuration">>, ?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK},
        {<<"Registration summary">>, ?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUMMARY}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(spaces_tab, spaces_account_link, Breadcrumbs),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 3: Registration summary.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"Your software configuration has been successfully verified.<br>"
                "Please enter your <i>onedata</i> provider name.">>
            },
            #textbox{
                id = <<"client_name">>,
                placeholder = <<"Provider name">>,
                style = <<"margin: 0 auto; width: 25%;">>
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
                        body = <<"Registering...">>
                    }
                ]
            },
            onepanel_gui_utils:nav_buttons([
                {<<"back_button">>, {postback, back}, false, <<"Back">>},
                {<<"register_button">>, {actions, gui_jq:form_submit_action(<<"register_button">>, register, [<<"client_name">>])}, false, <<"Register">>}
            ])
        ]
    },
    onepanel_gui_utils:body(?SUBMENU_HEIGHT, Header, Main).


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
    NewState =
        try
            receive
                {register, ClientName} ->
                    RedirectionPoint = gui_ctx:get(redirection_point),
                    NewPid = spawn_link(fun() ->
                        {ok, _} = provider_logic:register(RedirectionPoint, ClientName)
                    end),
                    State#?STATE{pid = NewPid};

                {'EXIT', Pid, normal} ->
                    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUCCESS),
                    State;

                {'EXIT', Pid, _} ->
                    onepanel_gui_utils:message(error, <<"Cannot register in <i>Global Registry</i>.<br>Please try again later.">>),
                    gui_jq:hide(<<"progress">>),
                    gui_jq:show(<<"client_name">>),
                    gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"">>),
                    gui_jq:prop(<<"register_button">>, <<"disabled">>, <<"">>),
                    State;

                _ ->
                    State

            after ?COMET_PROCESS_RELOAD_DELAY ->
                State
            end
        catch Type:Message ->
            ?error_stacktrace("Comet process exception: ~p:~p", [Type, Message]),
            onepanel_gui_utils:message(error, <<"There has been an error in comet process. Please refresh the page.">>),
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
    gui_jq:focus(<<"client_name">>),
    gui_jq:bind_key_to_click(<<"13">>, <<"register_button">>),
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop_init() end),
    put(?COMET_PID, Pid);

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK);

event(register) ->
    case gui_ctx:postback_param(<<"client_name">>) of
        <<>> ->
            onepanel_gui_utils:message(error, <<"Please enter provider name.">>);
        ClientName ->
            get(?COMET_PID) ! {register, ClientName},
            gui_jq:hide(<<"client_name">>),
            gui_jq:show(<<"progress">>),
            gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"disabled">>),
            gui_jq:prop(<<"register_button">>, <<"disabled">>, <<"disabled">>)
    end;

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.