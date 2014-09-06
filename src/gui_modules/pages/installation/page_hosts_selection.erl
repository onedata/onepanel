%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to select hosts during VeilCluster nodes installation.
%% @end
%% ===================================================================

-module(page_hosts_selection).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").

%% Convenience record abbreviation
-define(CONFIG, ?GLOBAL_CONFIG_RECORD).

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
    <<"Hosts selection">>.


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
            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #h6{
                style = <<"font-size: x-large; margin-bottom: 3em;">>,
                body = <<"Step 1: Select cluster and database hosts.">>
            },
            #table{
                class = <<"table table-bordered">>,
                style = <<"width: 50%; margin: 0 auto;">>,
                body = hosts_table_body()
            },
            #panel{
                style = <<"margin-top: 3em;">>,
                body = #button{
                    id = <<"next_button">>,
                    postback = next,
                    class = <<"btn btn-inverse btn-small">>,
                    style = <<"width: 8em; font-weight: bold;">>,
                    body = <<"Next">>
                }
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% hosts_table_body/0
%% ====================================================================
%% @doc Renders hosts table body.
%% @end
-spec hosts_table_body() -> Result when
    Result :: [#tr{}].
%% ====================================================================
hosts_table_body() ->
    ColumnStyle = <<"text-align: center; vertical-align: inherit;">>,
    Header = #tr{
        cells = [
            #th{
                body = <<"Host">>,
                style = ColumnStyle
            },
            #th{
                body = <<"CCM">>,
                style = ColumnStyle
            },
            #th{
                body = <<"Worker">>,
                style = ColumnStyle
            },
            #th{
                body = <<"Database">>,
                style = ColumnStyle
            }
        ]
    },
    try
        Hosts = lists:sort(onepanel_utils:get_hosts()),

        {ok, Db} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, Session} = onepanel_gui_utils:get_installation_state(),

        NewSession = case Hosts of
                         [_] -> Session#?CONFIG{main_ccm = hd(Hosts), ccms = Hosts, workers = Hosts, dbs = Hosts};
                         _ -> Session
                     end,
        gui_ctx:put(?CONFIG_ID, NewSession),

        Rows = lists:map(fun({Host, Id}) ->
            HostId = integer_to_binary(Id),
            Checkboxes = [
                {
                    <<"ccm_checkbox_">>,
                    lists:member(Host, NewSession#?CONFIG.ccms),
                    Db#?CONFIG.main_ccm =/= undefined
                },
                {
                    <<"worker_checkbox_">>,
                    lists:member(Host, NewSession#?CONFIG.workers),
                    lists:member(Host, Db#?CONFIG.workers)
                },
                {
                    <<"db_checkbox_">>,
                    lists:member(Host, NewSession#?CONFIG.dbs),
                    Db#?CONFIG.dbs =/= []
                }
            ],

            #tr{
                id = <<"row_", HostId/binary>>,
                cells = [
                    #td{
                        body = <<"<b>", (list_to_binary(Host))/binary, "</b>">>,
                        style = ColumnStyle
                    } | lists:map(fun({Prefix, Checked, Disabled}) ->
                        #td{
                            style = ColumnStyle,
                            body = #label{
                                id = <<Prefix/binary, HostId/binary>>,
                                class = <<"checkbox no-label">>,
                                for = <<Prefix/binary, HostId/binary>>,
                                style = <<"width: 2em; margin: 0 auto;">>,
                                actions = gui_jq:postback_action(<<Prefix/binary, HostId/binary>>,
                                    {binary_to_atom(<<Prefix/binary, "toggled">>, latin1), Host, HostId, Disabled}),
                                body = [
                                    #span{
                                        class = <<"icons">>
                                    },
                                    #custom_checkbox{
                                        id = <<Prefix/binary, HostId/binary>>,
                                        data_fields = [{<<"data-toggle">>, <<"checkbox">>}],
                                        value = <<"">>,
                                        checked = Checked,
                                        disabled = Disabled
                                    }
                                ]
                            }
                        }
                    end, Checkboxes)
                ]
            }
        end, lists:zip(lists:sort(Hosts), lists:seq(1, length(Hosts)))),

        [Header | Rows]
    catch
        _:_ -> [Header]
    end.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
    ok;

event({ccm_checkbox_toggled, _, _, true}) ->
    ok;

event({ccm_checkbox_toggled, Host, HostId, _}) ->
    #?CONFIG{main_ccm = MainCCM, ccms = CCMs, workers = Workers} = Config = gui_ctx:get(?CONFIG_ID),
    case lists:member(Host, CCMs) of
        true ->
            case Host of
                MainCCM ->
                    gui_ctx:put(?CONFIG_ID, Config#?CONFIG{main_ccm = undefined, ccms = lists:delete(Host, CCMs)});
                _ ->
                    gui_ctx:put(?CONFIG_ID, Config#?CONFIG{ccms = lists:delete(Host, CCMs)})
            end;
        false ->
            case lists:member(Host, Workers) of
                true ->
                    gui_ctx:put(?CONFIG_ID, Config#?CONFIG{ccms = [Host | CCMs]});
                false ->
                    WorkerCheckboxId = <<"worker_checkbox_", HostId/binary>>,
                    gui_jq:click(WorkerCheckboxId),
                    gui_ctx:put(?CONFIG_ID, Config#?CONFIG{ccms = [Host | CCMs], workers = [Host | Workers]})
            end
    end;

event({worker_checkbox_toggled, _, _, true}) ->
    ok;

event({worker_checkbox_toggled, Host, HostId, _}) ->
    #?CONFIG{main_ccm = MainCCM, ccms = CCMs, workers = Workers} = Config = gui_ctx:get(?CONFIG_ID),
    case lists:member(Host, Workers) of
        true ->
            case lists:member(Host, CCMs) of
                true ->
                    CCMCheckboxId = <<"ccm_checkbox_", HostId/binary>>,
                    gui_jq:click(CCMCheckboxId),
                    case Host of
                        MainCCM ->
                            gui_ctx:put(?CONFIG_ID, Config#?CONFIG{main_ccm = undefined,
                                ccms = lists:delete(Host, CCMs), workers = lists:delete(Host, Workers)});
                        _ ->
                            gui_ctx:put(?CONFIG_ID, Config#?CONFIG{ccms = lists:delete(Host, CCMs),
                                workers = lists:delete(Host, Workers)})
                    end;
                false ->
                    gui_ctx:put(?CONFIG_ID, Config#?CONFIG{workers = lists:delete(Host, Workers)})
            end;
        _ ->
            gui_ctx:put(?CONFIG_ID, Config#?CONFIG{workers = [Host | Workers]})
    end;

event({db_checkbox_toggled, _, _, true}) ->
    ok;

event({db_checkbox_toggled, Host, _, _}) ->
    #?CONFIG{dbs = Dbs} = Config = gui_ctx:get(?CONFIG_ID),
    case lists:member(Host, Dbs) of
        true ->
            gui_ctx:put(?CONFIG_ID, Config#?CONFIG{dbs = lists:delete(Host, Dbs)});
        _ ->
            gui_ctx:put(?CONFIG_ID, Config#?CONFIG{dbs = [Host | Dbs]})
    end;

event(next) ->
    #?CONFIG{main_ccm = MainCCM, ccms = CCMs, dbs = Dbs} = Config = gui_ctx:get(?CONFIG_ID),
    case Dbs of
        [] ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Please select at least one host for database node.">>);
        _ ->
            case MainCCM of
                undefined ->
                    case CCMs of
                        [_ | _] ->
                            gui_ctx:put(?CONFIG_ID, Config#?CONFIG{main_ccm = hd(lists:sort(CCMs))}),
                            onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_MAIN_CCM_SELECTION);
                        _ ->
                            onepanel_gui_utils:message(<<"error_message">>, <<"Please select at least one host for CCM node.">>)
                    end;
                _ ->
                    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_MAIN_CCM_SELECTION)
            end
    end;

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.