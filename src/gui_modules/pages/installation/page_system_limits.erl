%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to set system ulimits during software components
%% installation.
%% @end
%% ===================================================================

-module(page_system_limits).
-export([main/0, event/1, comet_loop/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% Convenience record abbreviation
-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {installed_hosts, system_limits}).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_SYSTEM_LIMITS, ?PAGE_INSTALLATION) of
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
    <<"System limits">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(software_tab, installation_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 3: System limits configuration.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"Proper system limits configuration is essential for <i>database</i> components to"
                " work correctly. Therefore, unless necessary, it is recommended not to change"
                " default values.">>
            },
            #table{
                id = <<"system_limits_table">>,
                class = <<"table table-bordered">>,
                style = <<"width: 50%; margin: 0 auto;">>
            },
            #panel{
                id = <<"nav_buttons">>
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% system_limits_table_body/2
%% ====================================================================
%% @doc Renders system limits table body.
%% @end
-spec system_limits_table_body(InstalledHosts :: [string()], SystemLimits :: [{Host :: string(), Id :: binary(), OpenFilesLimit :: integer(), ProcessesLimit :: integer()}]) -> Result
    when Result :: [#tr{}].
%% ====================================================================
system_limits_table_body(InstalledHosts, SystemLimits) ->
    ColumnStyle = <<"text-align: center; vertical-align: inherit;">>,

    Header = #tr{
        cells = lists:map(fun(Body) ->
            #th{
                body = Body,
                style = ColumnStyle
            }
        end, [<<"Host">>, <<"Open files limit">>, <<"Processes limit">>])
    },

    Rows = lists:map(fun({Host, Id, OpenFilesLimit, ProcessesLimit}) ->
        #tr{
            cells = [
                #td{
                    body = <<"<b>", (list_to_binary(Host))/binary, "</b>">>,
                    style = ColumnStyle
                } | lists:map(fun({TextboxId, Limit}) ->
                    #td{
                        style = ColumnStyle,
                        body = #textbox{
                            id = TextboxId,
                            style = <<"text-align: center; margin: 0 auto;">>,
                            class = <<"span1">>,
                            value = integer_to_binary(Limit),
                            disabled = case lists:member(Host, InstalledHosts) of
                                           true -> true;
                                           _ -> undefined
                                       end
                        }
                    }
                end, [
                    {<<"open_files_textbox_", Id/binary>>, OpenFilesLimit},
                    {<<"processes_textbox_", Id/binary>>, ProcessesLimit}
                ])
            ]
        }
    end, SystemLimits),

    [Header | Rows].


%% validate_limit/1
%% ====================================================================
%% @doc Checks whether given limit is a positive number.
%% @end
-spec validate_limit(Limit :: string()) -> Result
    when Result :: true | false.
%% ====================================================================
validate_limit(Limit) ->
    Regex = <<"[1-9][0-9]*">>,
    case re:run(Limit, Regex, [{capture, first, binary}]) of
        {match, [Limit]} -> true;
        _ -> false
    end.


%% open_files_limit_value/1
%% ====================================================================
%% @doc Returns default open files limit in case of unsupported value.
%% @end
-spec open_files_limit_value(OpenFilesLimit :: term()) -> Result
    when Result :: integer().
%% ====================================================================
open_files_limit_value(OpenFilesLimit) when is_integer(OpenFilesLimit) ->
    OpenFilesLimit;

open_files_limit_value(OpenFilesLimit) when is_binary(OpenFilesLimit) ->
    binary_to_integer(OpenFilesLimit);

open_files_limit_value(_) ->
    ?DEFAULT_OPEN_FILES.


%% processes_limit_value/1
%% ====================================================================
%% @doc Returns default processes limit in case of unsupported value.
%% @end
-spec processes_limit_value(ProcessesLimit :: term()) -> Result
    when Result :: integer().
%% ====================================================================
processes_limit_value(ProcessesLimit) when is_integer(ProcessesLimit) ->
    ProcessesLimit;

processes_limit_value(ProcessesLimit) when is_binary(ProcessesLimit) ->
    binary_to_integer(ProcessesLimit);

processes_limit_value(_) ->
    ?DEFAULT_PROCESSES.

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

comet_loop(#?STATE{installed_hosts = InstalledHosts, system_limits = SystemLimits} = State) ->
    NewState = try
        receive
            render_system_limits_table ->
                TextboxIds = lists:foldl(fun({_, Id, _, _}, TextboxIdsAcc) ->
                    [<<"open_files_textbox_", Id/binary>>, <<"processes_textbox_", Id/binary>> | TextboxIdsAcc]
                end, [], SystemLimits),
                gui_jq:update(<<"system_limits_table">>, system_limits_table_body(InstalledHosts, SystemLimits)),
                gui_jq:update(<<"nav_buttons">>, onepanel_gui_utils:nav_buttons([
                    {<<"back_button">>, {postback, back}, <<"Back">>},
                    {<<"next_button">>, {actions, gui_jq:form_submit_action(<<"next_button">>, {set_system_limits, SystemLimits}, TextboxIds)}, <<"Next">>}
                ])),
                gui_jq:hide(<<"main_spinner">>),
                gui_comet:flush(),
                State;

            {set_system_limits, NewSystemLimits} ->
                case lists:foldl(fun({Host, OpenFilesId, OpenFilesLimit, ProcessesId, ProcessesLimit}, {OpenFilesStatus, ProcessesStatus}) ->
                    {
                        try
                            true = validate_limit(OpenFilesLimit),
                            ok = dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{open_files_limit, open_files_limit_value(OpenFilesLimit)}]),
                            gui_jq:css(OpenFilesId, <<"border-color">>, <<"green">>),
                            OpenFilesStatus
                        catch
                            _:_ ->
                                gui_jq:css(OpenFilesId, <<"border-color">>, <<"red">>),
                                error
                        end,
                        try
                            true = validate_limit(ProcessesLimit),
                            ok = dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{processes_limit, processes_limit_value(ProcessesLimit)}]),
                            gui_jq:css(ProcessesId, <<"border-color">>, <<"green">>),
                            ProcessesStatus
                        catch
                            _:_ ->
                                gui_jq:css(ProcessesId, <<"border-color">>, <<"red">>),
                                error
                        end
                    }
                end, {ok, ok}, NewSystemLimits) of
                    {ok, ok} ->
                        onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_STORAGE);
                    _ ->
                        onepanel_gui_utils:message(<<"error_message">>, <<"Cannot set system limits for some hosts.<br>Remember that system limit should be a positive number.">>)
                end,
                gui_jq:hide(<<"main_spinner">>),
                gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
                gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"">>),
                gui_comet:flush(),
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
    try
        {ok, DbConfig} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        InstalledHosts = DbConfig#?CONFIG.ccms ++ DbConfig#?CONFIG.workers ++ DbConfig#?CONFIG.dbs,
        {ok, SessionConfig} = onepanel_gui_utils:get_session_config(),
        SessionHosts = lists:usort(SessionConfig#?CONFIG.ccms ++ SessionConfig#?CONFIG.workers ++ SessionConfig#?CONFIG.dbs),

        SystemLimits = lists:map(fun({Host, Id}) ->
            {OpenFilesLimit, ProcessesLimit} = case dao:get_record(?LOCAL_CONFIG_TABLE, Host) of
                                                   {ok, #?LOCAL_CONFIG_RECORD{open_files_limit = Limit1, processes_limit = Limit2}} ->
                                                       {Limit1, Limit2};
                                                   {error, <<"Record not found.">>} ->
                                                       {?DEFAULT_OPEN_FILES, ?DEFAULT_PROCESSES};
                                                   _ ->
                                                       throw("Cannot get local configuration for host: " ++ Host)
                                               end,
            {Host, integer_to_binary(Id), open_files_limit_value(OpenFilesLimit), processes_limit_value(ProcessesLimit)}
        end, lists:zip(SessionHosts, tl(lists:seq(0, length(SessionHosts))))),

        gui_jq:show(<<"main_spinner">>),
        gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#?STATE{installed_hosts = InstalledHosts, system_limits = SystemLimits})
        end),
        put(?COMET_PID, Pid),
        Pid ! render_system_limits_table
    catch
        _:Reason ->
            ?error("Cannot fetch application configuration: ~p", [Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_MAIN_PRIMARY_SELECTION);

event({set_system_limits, SystemLimits}) ->
    NewSystemLimits = lists:map(fun({Host, Id, _, _}) ->
        OpenFilesId = <<"open_files_textbox_", Id/binary>>,
        OpenFilesLimit = gui_ctx:postback_param(OpenFilesId),
        ProcessesId = <<"processes_textbox_", Id/binary>>,
        ProcessesLimit = gui_ctx:postback_param(ProcessesId),
        {Host, OpenFilesId, OpenFilesLimit, ProcessesId, ProcessesLimit}
    end, SystemLimits),
    gui_jq:show(<<"main_spinner">>),
    gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"disabled">>),
    gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"disabled">>),
    get(?COMET_PID) ! {set_system_limits, NewSystemLimits};

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.