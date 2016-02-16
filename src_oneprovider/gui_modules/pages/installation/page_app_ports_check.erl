%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to check whether all application ports are free.
%% @end
%% ===================================================================
-module(page_app_ports_check).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

%% Convenience record abbreviation
-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {hosts, ports}).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_APP_PORTS_CHECK, ?PAGE_INSTALLATION) of
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
    <<"Application ports check">>.

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
        {<<"Primary CM selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CCM_SELECTION},
        {<<"Application ports check">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_APP_PORTS_CHECK}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, installation_link, Breadcrumbs, true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 3: Application ports check.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"Before proceeding with installation please ensure that all ports required by",
                " application are free.">>
            },
            #table{
                id = <<"ports_table">>,
                class = <<"table table-bordered">>,
                style = <<"width: 50%; margin: 0 auto;">>
            },
            #panel{
                id = <<"nav_buttons">>,
                body = onepanel_gui_utils:nav_buttons([
                    {<<"back_button">>, {postback, back}, false, <<"Back">>}
                ])
            }
        ]
    },
    onepanel_gui_utils:body(?SUBMENU_HEIGHT, Header, Main, onepanel_gui_utils:logotype_footer()).

%% ports_table/2
%% ====================================================================
%% @doc Renders ports table body.
%% @end
-spec ports_table(Hosts :: [{Host :: string(), HostPorts :: [boolean()]}], Ports :: [integer()]) -> Result
    when Result :: [#tr{}].
%% ====================================================================
ports_table(Hosts, Ports) ->
    ColumnStyle = <<"text-align: center; vertical-align: inherit;">>,

    Header = [
        #tr{
            cells = [
                #th{
                    body = <<"Host">>,
                    style = ColumnStyle,
                    rowspan = 2
                },
                #th{
                    body = <<"Ports">>,
                    style = ColumnStyle,
                    colspan = length(Ports)
                }
            ]
        },
        #tr{
            cells = lists:map(fun(Body) ->
                #th{
                    body = Body,
                    style = ColumnStyle
                }
            end, lists:map(fun(Port) -> integer_to_binary(Port) end, Ports))
        }
    ],

    Rows = lists:map(fun({Host, HostPorts}) ->
        #tr{
            cells = [
                #td{
                    body = <<"<b>", (list_to_binary(Host))/binary, "</b>">>,
                    style = ColumnStyle
                } | lists:map(fun
                    (ok) ->
                        #td{style = ColumnStyle, body = #span{
                            class = <<"icomoon-checkmark">>,
                            style = <<"color: green;">>
                        }};
                    (_) ->
                        #td{style = ColumnStyle, body = #span{
                            class = <<"icomoon-close">>,
                            style = <<"color: red;">>
                        }}
                end, HostPorts)
            ]
        }
    end, Hosts),

    [Header | Rows].

%% ====================================================================
%% Events handling
%% ====================================================================

%% comet_loop/1
%% ====================================================================
%% @doc Handles user's application configuration preferences.
%% @end
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{hosts = Hosts, ports = Ports} = State) ->
    NewState =
        try
            receive
                render_ports_table ->
                    {HostsPorts, Status} = installer_utils:check_ports(Hosts, Ports),
                    gui_jq:update(<<"ports_table">>, ports_table(HostsPorts, Ports)),
                    gui_jq:fade_in(<<"ports_table">>, 500),
                    case Status of
                        ok ->
                            gui_jq:update(<<"nav_buttons">>, onepanel_gui_utils:nav_buttons([
                                {<<"back_button">>, {postback, back}, false, <<"Back">>},
                                {<<"next_button">>, {postback, next}, false, <<"Next">>}
                            ]));
                        _ ->
                            gui_jq:update(<<"nav_buttons">>, onepanel_gui_utils:nav_buttons([
                                {<<"back_button">>, {postback, back}, false, <<"Back">>},
                                {<<"recheck_button">>, {postback, recheck}, false, <<"Recheck">>}
                            ]))
                    end,
                    State

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
    try
        Ports = onepanel_utils:get_application_ports(),
        {ok, #?CONFIG{workers = ConfiguredWorkers}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, #?CONFIG{workers = Workers}} = onepanel_gui_utils:get_session_config(),
        Hosts = lists:usort(Workers -- ConfiguredWorkers),

        gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#?STATE{hosts = Hosts, ports = Ports})
        end),
        put(?COMET_PID, Pid),
        Pid ! render_ports_table
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CCM_SELECTION);

event(next) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_SYSTEM_LIMITS);

event(recheck) ->
    get(?COMET_PID) ! render_ports_table;

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.