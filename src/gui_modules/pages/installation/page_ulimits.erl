%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page allows to set system ulimits in third step of VeilCluster
%% nodes installation.
%% @end
%% ===================================================================

-module(page_ulimits).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").

-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_ULIMITS, ?PAGE_INSTALLATION) of
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
    <<"Ulimits">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Disabled = case installer_utils:get_workers() of
                   [] -> undefined;
                   _ -> true
               end,
    #?CONFIG{ccms = CCMs, workers = Workers, dbs = Dbs} = gui_ctx:get(?CONFIG_ID),
    Hosts = lists:usort(CCMs ++ Workers ++ Dbs),
    {TextboxIds, _} = lists:foldl(fun(_, {Ids, Id}) ->
        HostId = integer_to_binary(Id),
        {[<<"open_files_textbox_", HostId/binary>>, <<"processes_textbox_", HostId/binary>> | Ids], Id + 1}
    end, {[], 1}, Hosts),

    #panel{
        style = <<"position: relative;">>,
        body = [
            onepanel_gui_utils:top_menu(installation_tab),

            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #panel{
                style = <<"margin-top: 150px; text-align: center;">>,
                body = [
                    #h6{
                        style = <<"font-size: 18px;">>,
                        body = <<"Step 3: Set system limits.">>
                    },
                    #table{
                        class = <<"table table-bordered">>,
                        style = <<"width: 50%; margin: 0 auto; margin-top: 20px;">>,
                        body = ulimits_table_body(Hosts, Disabled)
                    },
                    #panel{
                        style = <<"width: 50%; margin: 0 auto; margin-top: 30px; margin-bottom: 30px;">>,
                        body = [
                            #button{
                                id = <<"prev_button">>,
                                postback = back,
                                class = <<"btn btn-inverse btn-small">>,
                                style = <<"float: left; width: 80px; font-weight: bold;">>,
                                body = <<"Back">>
                            },
                            #button{
                                id = <<"next_button">>,
                                actions = gui_jq:form_submit_action(<<"next_button">>, {set_ulimits, Hosts, Disabled}, TextboxIds),
                                class = <<"btn btn-inverse btn-small">>,
                                style = <<"float: right; width: 80px; font-weight: bold;">>,
                                body = <<"Next">>
                            }
                        ]
                    }
                ]
            }
        ] ++ onepanel_gui_utils:logotype_footer(120)
    }.


%% ulimits_table_body/2
%% ====================================================================
%% @doc Renders system limits table body.
-spec ulimits_table_body(Hosts :: [string()], Disabled :: true | undefined) -> Result
    when Result :: [#tr{}].
%% ====================================================================
ulimits_table_body(Hosts, Disabled) ->
    ColumnStyle = <<"text-align: center; vertical-align: inherit;">>,
    Header = #tr{
        cells = [
            #th{
                body = <<"Host">>,
                style = ColumnStyle
            },
            #th{
                body = <<"Open files limit">>,
                style = ColumnStyle
            },
            #th{
                body = <<"Processes limit">>,
                style = ColumnStyle
            }
        ]
    },
    try
        Rows = lists:map(fun({Host, Id}) ->
            HostId = integer_to_binary(Id),
            {OpenFilesLimit, ProcessesLimit} =
                case dao:get_record(?LOCAL_CONFIG_TABLE, Host) of
                    {ok, #?LOCAL_CONFIG_RECORD{open_files_limit = undefined, processes_limit = undefined}} ->
                        {?DEFAULT_OPEN_FILES, ?DEFAULT_PROCESSES};
                    {ok, #?LOCAL_CONFIG_RECORD{open_files_limit = Limit, processes_limit = undefined}} ->
                        {Limit, ?DEFAULT_PROCESSES};
                    {ok, #?LOCAL_CONFIG_RECORD{open_files_limit = undefined, processes_limit = Limit}} ->
                        {?DEFAULT_OPEN_FILES, Limit};
                    {ok, #?LOCAL_CONFIG_RECORD{open_files_limit = Limit1, processes_limit = Limit2}} ->
                        {Limit1, Limit2};
                    _ ->
                        {?DEFAULT_OPEN_FILES, ?DEFAULT_PROCESSES}
                end,
            Textboxes = [
                {
                    <<"open_files_textbox_", HostId/binary>>,
                    OpenFilesLimit
                },
                {
                    <<"processes_textbox_", HostId/binary>>,
                    ProcessesLimit
                }
            ],

            #tr{
                id = <<"row_", HostId/binary>>,
                cells = [
                    #td{
                        body = <<"<b>", (list_to_binary(Host))/binary, "</b>">>,
                        style = ColumnStyle
                    } | lists:map(fun({TextboxId, Text}) ->
                        #td{
                            style = ColumnStyle,
                            body = #textbox{
                                id = TextboxId,
                                style = <<"text-align: center;">>,
                                class = <<"span1">>,
                                value = list_to_binary(Text),
                                disabled = Disabled
                            }
                        }
                    end, Textboxes)
                ]
            }
        end, lists:zip(lists:sort(Hosts), lists:seq(1, length(Hosts)))),

        [Header | Rows]
    catch
        _:_ -> [Header]
    end.


%% validate_limit/1
%% ====================================================================
%% @doc Checks whether given limit is a positive number.
-spec validate_limit(Limit :: string()) -> Result
    when Result :: true | false.
%% ====================================================================
validate_limit(Limit) ->
    Regex = "[1-9][0-9]*",
    case re:run(Limit, Regex) of
        {match, _} -> true;
        _ -> false
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
    ok;

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_MAIN_CCM_SELECTION);

event({set_ulimits, _, true}) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_ADD_STORAGE);

event({set_ulimits, Hosts, _}) ->
    case lists:foldl(fun(Host, {Status, Id}) ->
        HostId = integer_to_binary(Id),
        OpenFilesId = <<"open_files_textbox_", HostId/binary>>,
        ProcessesId = <<"processes_textbox_", HostId/binary>>,
        OpenFilesLimit = gui_str:to_list(gui_ctx:postback_param(OpenFilesId)),
        ProcessesLimit = gui_str:to_list(gui_ctx:postback_param(ProcessesId)),
        {
            case validate_limit(OpenFilesLimit) of
                true ->
                    gui_jq:css(OpenFilesId, <<"border-color">>, <<"green">>),
                    dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{open_files_limit, OpenFilesLimit}]),
                    case validate_limit(ProcessesLimit) of
                        true ->
                            gui_jq:css(ProcessesId, <<"border-color">>, <<"green">>),
                            rpc:call(erlang:list_to_atom(?APP_STR ++ "@" ++ Host), installer_utils, set_ulimits,
                                [list_to_integer(OpenFilesLimit), list_to_integer(ProcessesLimit)]),
                            Status;
                        _ ->
                            gui_jq:css(ProcessesId, <<"border-color">>, <<"red">>),
                            error
                    end;
                _ ->
                    gui_jq:css(OpenFilesId, <<"border-color">>, <<"red">>),
                    case validate_limit(ProcessesLimit) of
                        true ->
                            gui_jq:css(ProcessesId, <<"border-color">>, <<"green">>),
                            dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{processes_limit, ProcessesLimit}]),
                            error;
                        _ ->
                            gui_jq:css(ProcessesId, <<"border-color">>, <<"red">>),
                            error
                    end
            end,
            Id + 1
        }
    end, {ok, 1}, Hosts) of
        {ok, _} ->
            onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_ADD_STORAGE);
        _ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"System limit should be a positive number.">>)
    end;

event(terminate) ->
    ok.