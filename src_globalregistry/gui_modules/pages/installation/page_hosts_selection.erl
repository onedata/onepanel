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
-record(?STATE, {hosts = [], gr_checkbox_id, db_config = #?CONFIG{}, session_config = #?CONFIG{}}).

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
%% @doc Page title.
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
    Header = onepanel_gui_utils_adapter:top_menu(installation_tab, [], true),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 1: Hosts selection.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"The table below presents a list of hosts where software package installation has been detected.<br>"
                " In order to configure application please distribute software components throughout available hosts by selecting"
                " corresponding checkboxes.">>
            },
            #table{
                id = <<"hosts_table">>,
                class = <<"table table-bordered">>,
                style = <<"width: 50%; margin: 0 auto; display: none;">>
            },
            onepanel_gui_utils:nav_buttons([{<<"next_button">>, {postback, {message, next}}, true, <<"Next">>}])
        ]
    },
    onepanel_gui_utils:body(Header, Main).


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
        end, [<<"Host">>, <<"Global Registry">>, <<"Database">>])
    },

    Rows = lists:map(fun({Host, Id}) ->
        HostId = integer_to_binary(Id),
        #tr{
            cells = [
                #td{
                    body = <<"<b>", (gui_str:html_encode(Host))/binary, "</b>">>,
                    style = ColumnStyle
                } | lists:map(fun({Prefix, Checked, Disabled}) ->
                    #td{
                        style = ColumnStyle,
                        body = #custom_checkbox{
                            id = <<Prefix/binary, HostId/binary>>,
                            style = <<"width: 20px; margin: 0 auto;">>,
                            class = <<"checkbox no-label">>,
                            checked = Checked,
                            disabled = Disabled,
                            postback = {message, {binary_to_atom(<<Prefix/binary, "toggled">>, latin1), Host, HostId, Disabled}}
                        }
                    }
                end, [
                    {<<"gr_checkbox_">>, Host =:= PageConfig#?CONFIG.gr, DbConfig#?CONFIG.gr =/= undefined},
                    {<<"db_checkbox_">>, lists:member(Host, PageConfig#?CONFIG.dbs), DbConfig#?CONFIG.dbs =/= []}
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

comet_loop(#?STATE{hosts = Hosts, gr_checkbox_id = GrId, db_config = DbConfig, session_config = #?CONFIG{gr = GR, dbs = Dbs} = SessionConfig} = State) ->
    NewState = try
        receive
            render_hosts_table ->
                gui_jq:update(<<"hosts_table">>, hosts_table(Hosts, DbConfig, SessionConfig)),
                gui_jq:fade_in(<<"hosts_table">>, 500),
                gui_jq:wire(<<"$('#main_spinner').delay(500).hide(0);">>, false),
                gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
                State;

            next ->
                case Dbs of
                    [] ->
                        onepanel_gui_utils:message(<<"error_message">>, <<"Please select at least one host for database component.">>);
                    _ ->
                        case GR of
                            undefined ->
                                onepanel_gui_utils:message(<<"error_message">>, <<"Please select host for Global Registry component.">>);
                            _ ->
                                gui_ctx:put(?CONFIG_ID, SessionConfig#?CONFIG{gr = GR, dbs = Dbs}),
                                onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_SYSTEM_LIMITS)
                        end
                end,
                State;

            {gr_checkbox_toggled, GR, _, false} ->
                State#?STATE{gr_checkbox_id = undefined, session_config = SessionConfig#?CONFIG{gr = undefined}};

            {gr_checkbox_toggled, Host, HostId, false} ->
                case GrId of
                    undefined -> ok;
                    _ -> gui_jq:click(<<"gr_checkbox_", GrId/binary>>)
                end,
                State#?STATE{gr_checkbox_id = HostId, session_config = SessionConfig#?CONFIG{gr = Host}};

            {db_checkbox_toggled, Host, _, false} ->
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
    try
        Hosts = onepanel_utils:get_hosts(),
        {ok, DbConfig} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, SessionConfig} = onepanel_gui_utils:get_session_config(),
        State = case {SessionConfig#?CONFIG.gr, Hosts} of
                    {undefined, [_]} ->
                        #?STATE{hosts = Hosts, gr_checkbox_id = <<"gr_checkbox_1">>,
                            db_config = DbConfig, session_config = SessionConfig#?CONFIG{gr = hd(Hosts), dbs = Hosts}};
                    _ ->
                        #?STATE{hosts = Hosts, db_config = DbConfig, session_config = SessionConfig}
                end,

        gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(State)
        end),
        put(?COMET_PID, Pid),
        Pid ! render_hosts_table
    catch
        _:Reason ->
            ?error("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event({message, Message}) ->
    get(?COMET_PID) ! Message;

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.
