%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page allows to select host for main CCM node in third step of
%% VeilCluster nodes installation.
%% @end
%% ===================================================================

-module(page_main_ccm_selection).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").

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
            case onepanel_gui_utils:maybe_redirect(?INSTALL_PAGE, "/main_ccm_selection", "/hosts_selection") of
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
    <<"Main CCM selection">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
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
                        body = <<"Step 2: Select main Central Cluster Manager host.">>
                    },
                    #panel{
                        class = <<"btn-group">>,
                        style = <<"margin: 12px 15px;">>,
                        body = main_ccm_body()
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
                                postback = next,
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


%% main_ccm_body/0
%% ====================================================================
%% @doc Renders main CCM dropdown body and highlights current choice.
-spec main_ccm_body() -> Result when
    Result :: [term()].
%% ====================================================================
main_ccm_body() ->
    try
        {ok, Db} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, Session} = onepanel_gui_utils:get_installation_state(),

        [
            <<"<i class=\"dropdown-arrow dropdown-arrow-inverse\"></i>">>,
            #button{
                id = <<"ccms_button">>,
                disabled = Db#?CONFIG.main_ccm =/= undefined,
                class = <<"btn btn-inverse btn-small dropdown-toggle">>,
                style = <<"width: 280px;">>,
                data_fields = [{<<"data-toggle">>, <<"dropdown">>}],
                body = [
                    #span{
                        id = <<"ccms_label">>,
                        class = <<"filter-option pull-left">>,
                        body = <<"Primary CCM host: <b>", (list_to_binary(Session#?CONFIG.main_ccm))/binary, "</b>">>
                    },
                    #span{
                        class = <<"caret pull-right">>
                    }
                ]
            },
            #list{
                id = <<"ccms_dropdown">>,
                class = <<"dropdown-menu dropdown-inverse">>,
                style = <<"overflow-y: auto; max-height: 200px;">>,
                body = ccms_list_body(Session#?CONFIG.main_ccm, Session#?CONFIG.ccms)
            }
        ]
    catch
        _:_ -> []
    end.


%% ccms_list_body/2
%% ====================================================================
%% @doc Renders CCMs' list body.
-spec ccms_list_body(MainCCM :: string(), CCMs :: [string()]) -> Result when
    Result :: [#li{}].
%% ====================================================================
ccms_list_body(MainCCM, CCMs) ->
    {Body, _} = lists:foldl(fun(CCM, {List, Id}) ->
        CCMId = <<"ccm_li_", (integer_to_binary(Id))/binary>>,
        {
            [#li{
                id = CCMId,
                actions = gui_jq:postback_action(CCMId, {set_main_ccm, CCM, CCMs}),
                class = case CCM of
                            MainCCM -> <<"active">>;
                            _ -> <<"">>
                        end,
                body = #link{
                    style = <<"text-align: left;">>,
                    body = CCM
                }
            }, List],
            Id + 1
        }
    end, {[], 1}, lists:sort(fun(CCM1, CCM2) -> CCM1 > CCM2 end, CCMs)),
    Body.


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
    onepanel_gui_utils:change_page(?INSTALL_PAGE, "/hosts_selection");

event(next) ->
    onepanel_gui_utils:change_page(?INSTALL_PAGE, "/ulimits");

event({set_main_ccm, MainCCM, CCMs}) ->
    Config = gui_ctx:get(?CONFIG_ID),
    gui_ctx:put(?CONFIG_ID, Config#?CONFIG{main_ccm = MainCCM}),
    gui_jq:update(<<"ccms_label">>, <<"Primary CCM host: <b>", (list_to_binary(MainCCM))/binary, "</b>">>),
    gui_jq:update(<<"ccms_dropdown">>, ccms_list_body(MainCCM, CCMs));

event(terminate) ->
    ok.