%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to check connection to onezone.
%% @end
%% ===================================================================
-module(page_connection_check).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Convenience record abbreviation
-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK, ?PAGE_SPACES_ACCOUNT) of
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
    <<"Connection check">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Breadcrumbs = onepanel_gui_utils:breadcrumbs([
        {<<"Connection check">>, ?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(spaces_tab, spaces_account_link, Breadcrumbs),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 1: Connection check">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"Please provider <i>onezone</i> domain and press <i>Next</i> button in order to check connection.">>
            },
            #table{
                id = <<"redirection_point_table">>,
                style = <<"width: 50%; margin: 0 auto; margin-bottom: 3em; border-spacing: 1em; border-collapse: inherit;">>,
                body = [
                    #tr{
                        cells = [
                            #td{
                                style = <<"border-width: 0; width: 50%; text-align: right;">>,
                                body = #label{
                                    style = <<"margin: 0 auto; cursor: auto;">>,
                                    class = <<"label label-large label-inverse">>,
                                    body = <<"onezone domain">>
                                }
                            },
                            #td{
                                style = <<"border-width: 0; width: 50%; text-align: left;">>,
                                body = #textbox{
                                    id = <<"onezone_domain_textbox">>,
                                    style = <<"margin: 0 auto; padding: 1px;">>,
                                    class = <<"span">>,
                                    placeholder = <<"onezone domain">>
                                }
                            }
                        ]
                    }
                ]
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
            onepanel_gui_utils:nav_buttons([{<<"next_button">>,
                {actions, gui_jq:form_submit_action(<<"next_button">>, connect,
                    <<"onezone_domain_textbox">>)}, false, <<"Next">>}])
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
                {connect, ""} ->
                    onepanel_gui_utils:message(error, <<"Please provide onezone domain.">>),
                    gui_jq:hide(<<"progress">>),
                    gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
                    State;
                {connect, OzDomain} ->
                    NewPid = spawn_link(fun() ->
                        application:set_env(?APP_NAME, oz_domain, OzDomain),
                        {ok, _} = installer_utils:check_ip_address(),
                        AppStr = ?APP_STR,
                        lists:foreach(fun(Node) ->
                            case string:tokens(atom_to_list(Node), "@") of
                                [AppStr | _] ->
                                    rpc:call(Node, application, set_env, [?APP_NAME, oz_domain, OzDomain]),
                                    ok = rpc:call(Node, app_config, set, [?APP_NAME, oz_domain, OzDomain]);
                                _ ->
                                    ok
                            end
                        end, onepanel_utils:get_nodes()),
                        {ok, #?CONFIG{workers = Workers}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
                        lists:foreach(fun(Node) ->
                            rpc:call(Node, application, set_env, [op_worker, oz_domain, OzDomain])
                        end, onepanel_utils:get_nodes(?WORKER_NAME, Workers)),
                        lists:foreach(fun(Node) ->
                            ok = rpc:call(Node, app_config, set, [op_worker, oz_domain, OzDomain])
                        end, onepanel_utils:get_nodes(?APP_STR, Workers))
                    end),
                    State#?STATE{pid = NewPid};

                {'EXIT', Pid, normal} ->
                    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK),
                    State;

                {'EXIT', Pid, _} ->
                    onepanel_gui_utils:message(error, <<"Cannot connect to onezone.<br>
                    This may occur due to NAT or PAT translation mechanisms. Please check your network configuration or try again later.">>),
                    gui_jq:hide(<<"progress">>),
                    gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
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
    gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop_init() end),
    put(?COMET_PID, Pid);

event(connect) ->
    OzDomain = gui_ctx:postback_param(<<"onezone_domain_textbox">>),
    get(?COMET_PID) ! {connect, binary_to_list(OzDomain)},
    gui_jq:show(<<"progress">>),
    gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"disabled">>);

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.
