%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to select hosts during software components installation.
%% @end
%% ===================================================================
-module(page_hosts_selection).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

%% Convenience record abbreviation
-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {hosts = [], db_config = #?CONFIG{}, session_config = #?CONFIG{}}).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_HOST_SELECTION, ?PAGE_INSTALLATION) of
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
    <<"Hosts selection">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Breadcrumbs = onepanel_gui_utils:breadcrumbs([
        {<<"Hosts selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_HOST_SELECTION}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, installation_link, Breadcrumbs, true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 1: Hosts selection.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"The table below presents a list of hosts where software package installation has been detected.<br>"
                " In order to configure application please distribute software components over available hosts by selecting"
                " checkboxes.">>
            },
            #table{
                id = <<"hosts_table">>,
                class = <<"table table-bordered">>,
                style = <<"width: 50%; margin: 0 auto; display: none;">>
            },
            onepanel_gui_utils:nav_buttons([{<<"next_button">>, {postback, {message, next}}, true, <<"Next">>}])
        ]
    },
    onepanel_gui_utils:body(?SUBMENU_HEIGHT, Header, Main).


%% hosts_table/3
%% ====================================================================
%% @doc Renders hosts table body.
%% @end
-spec hosts_table(Hosts :: [string()], DbConfig :: #?CONFIG{}, PageConfig :: #?CONFIG{}) -> Result when
    Result :: [#tr{}].
%% ====================================================================
hosts_table(Hosts, DbConfig, PageConfig) ->
    ColumnStyle = <<"text-align: center; vertical-align: inherit;">>,

    Header = #tr{
        cells = lists:map(fun(ColumnName) ->
            #th{
                body = ColumnName,
                style = ColumnStyle
            }
        end, [<<"Host">>, <<"CM">>, <<"Worker">>, <<"Database">>])
    },

    Rows = lists:map(fun({Host, Id}) ->
        HostId = integer_to_binary(Id),
        #tr{
            cells = [
                #td{
                    body = <<"<b>", (http_utils:html_encode(Host))/binary, "</b>">>,
                    style = case installer_utils:check_host_domain_name(Host) of
                                ok -> ColumnStyle;
                                _ -> <<ColumnStyle/binary, " color: red;">>
                            end
                } | lists:map(fun({Prefix, Checked, Disabled}) ->
                    flatui_checkbox:init_checkbox(<<Prefix/binary, "checkbox_", HostId/binary>>),
                    #td{
                        style = ColumnStyle,
                        body = #flatui_checkbox{
                            label_id = <<Prefix/binary, "label_", HostId/binary>>,
                            label_style = <<"width: 20px; margin: 0 auto;">>,
                            label_class = <<"checkbox no-label">>,
                            id = <<Prefix/binary, "checkbox_", HostId/binary>>,
                            checked = Checked,
                            disabled = Disabled,
                            delegate = ?MODULE,
                            postback = {message, {binary_to_atom(<<Prefix/binary, "checkbox_toggled">>, latin1), Host, HostId}}
                        }
                    }
                end, [
                    {<<"cm_">>, lists:member(Host, PageConfig#?CONFIG.cms), DbConfig#?CONFIG.main_cm =/= undefined},
                    {<<"worker_">>, lists:member(Host, PageConfig#?CONFIG.workers), lists:member(Host, DbConfig#?CONFIG.workers)},
                    {<<"db_">>, lists:member(Host, PageConfig#?CONFIG.dbs), DbConfig#?CONFIG.dbs =/= []}
                ])
            ]
        }
    end, lists:zip(lists:sort(Hosts), tl(lists:seq(0, length(Hosts))))),

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

comet_loop(#?STATE{hosts = Hosts, db_config = DbConfig, session_config = #?CONFIG{main_cm = MainCM, cms = CMs, workers = Workers, dbs = Dbs} = SessionConfig} = State) ->
    NewState =
        try
            receive
                render_hosts_table ->
                    gui_jq:update(<<"hosts_table">>, hosts_table(Hosts, DbConfig, SessionConfig)),
                    gui_jq:fade_in(<<"hosts_table">>, 500),
                    case lists:all(fun(Host) -> installer_utils:check_host_domain_name(Host) =:= ok end, Hosts) of
                        true ->
                            gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>);
                        _ ->
                            onepanel_gui_utils:message(error, <<"Before proceeding with installation please ensure",
                            " domain name for each host is fully qualified.<br>Change domain name and reinstall software",
                            " package on hosts marked in red.">>)
                    end,
                    State;

                next ->
                    case Dbs of
                        [] ->
                            onepanel_gui_utils:message(error, <<"Please select at least one host for database component.">>);
                        _ ->
                            case MainCM of
                                undefined ->
                                    case CMs of
                                        [_ | _] ->
                                            gui_ctx:put(?CONFIG_ID, SessionConfig#?CONFIG{main_cm = hd(lists:sort(CMs)), cms = CMs, workers = Workers, dbs = Dbs}),
                                            onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CM_SELECTION);
                                        _ ->
                                            onepanel_gui_utils:message(error, <<"Please select at least one host for CM component.">>)
                                    end;
                                _ ->
                                    gui_ctx:put(?CONFIG_ID, SessionConfig#?CONFIG{main_cm = MainCM, cms = CMs, workers = Workers, dbs = Dbs}),
                                    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CM_SELECTION)
                            end
                    end,
                    State;

                {cm_checkbox_toggled, Host, HostId} ->
                    case lists:member(Host, CMs) of
                        true ->
                            case Host of
                                MainCM ->
                                    State#?STATE{session_config = SessionConfig#?CONFIG{main_cm = undefined, cms = lists:delete(Host, CMs)}};
                                _ ->
                                    State#?STATE{session_config = SessionConfig#?CONFIG{cms = lists:delete(Host, CMs)}}
                            end;
                        false ->
                            case lists:member(Host, Workers) of
                                true ->
                                    State#?STATE{session_config = SessionConfig#?CONFIG{cms = [Host | CMs]}};
                                false ->
                                    gui_jq:add_class(<<"worker_label_", HostId/binary>>, <<"checked">>),
                                    State#?STATE{session_config = SessionConfig#?CONFIG{cms = [Host | CMs], workers = [Host | Workers]}}
                            end
                    end;

                {worker_checkbox_toggled, Host, HostId} ->
                    case lists:member(Host, Workers) of
                        true ->
                            case lists:member(Host, CMs) of
                                true ->
                                    gui_jq:remove_class(<<"cm_label_", HostId/binary>>, <<"checked">>),
                                    case Host of
                                        MainCM ->
                                            State#?STATE{session_config = SessionConfig#?CONFIG{main_cm = undefined, cms = lists:delete(Host, CMs), workers = lists:delete(Host, Workers)}};
                                        _ ->
                                            State#?STATE{session_config = SessionConfig#?CONFIG{cms = lists:delete(Host, CMs), workers = lists:delete(Host, Workers)}}
                                    end;
                                false ->
                                    State#?STATE{session_config = SessionConfig#?CONFIG{workers = lists:delete(Host, Workers)}}
                            end;
                        _ ->
                            State#?STATE{session_config = SessionConfig#?CONFIG{workers = [Host | Workers]}}
                    end;

                {db_checkbox_toggled, Host, _} ->
                    case lists:member(Host, Dbs) of
                        true ->
                            State#?STATE{session_config = SessionConfig#?CONFIG{dbs = lists:delete(Host, Dbs)}};
                        _ ->
                            State#?STATE{session_config = SessionConfig#?CONFIG{dbs = [Host | Dbs]}}
                    end;

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
        Hosts = onepanel_utils:get_hosts(),
        {ok, DbConfig} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, SessionConfig} = onepanel_gui_utils:get_session_config(),
        NewSessionConfig = case {SessionConfig#?CONFIG.main_cm, Hosts} of
                               {undefined, [_]} ->
                                   SessionConfig#?CONFIG{main_cm = hd(Hosts), cms = Hosts, workers = Hosts, dbs = Hosts};
                               _ ->
                                   SessionConfig
                           end,

        gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#?STATE{hosts = Hosts, db_config = DbConfig, session_config = NewSessionConfig})
        end),
        put(?COMET_PID, Pid),
        Pid ! render_hosts_table
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event({message, Message}) ->
    get(?COMET_PID) ! Message;

event(recheck) ->
    get(?COMET_PID) ! render_hosts_table;

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.