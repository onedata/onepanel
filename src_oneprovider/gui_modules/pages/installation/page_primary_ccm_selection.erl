%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to select host for main CCM node during oneprovider
%% nodes installation.
%% @end
%% ===================================================================
-module(page_primary_ccm_selection).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1]).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CCM_SELECTION, ?PAGE_INSTALLATION) of
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
    <<"Primary CCM selection">>.


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
        {<<"Primary CCM selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CCM_SELECTION}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, installation_link, Breadcrumbs),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 2: Primary Central Cluster Manager selection.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"<i>Central Cluster Manager</i> components control and organize work of other"
                " application components. However, it is not possible for more than one <i>CCM</i> component"
                " to run simultaneously. Therefore, it is required to select primary <i>CCM</i> component"
                " which will execute aforementioned tasks, while other <i>CCM</i> components will wait"
                " and in case of primary <i>CCM</i> breakdown take over its duties.">>
            },
            #panel{
                class = <<"btn-group">>,
                body = main_ccm()
            },
            onepanel_gui_utils:nav_buttons([
                {<<"back_button">>, {postback, back}, false, <<"Back">>},
                {<<"next_button">>, {postback, next}, false, <<"Next">>}
            ])
        ]
    },
    onepanel_gui_utils:body(108, Header, Main, onepanel_gui_utils:logotype_footer()).


%% main_ccm/0
%% ====================================================================
%% @doc Renders main CCM dropdown body and highlights current choice.
%% @end
-spec main_ccm() -> Result when
    Result :: [term()].
%% ====================================================================
main_ccm() ->
    try
        {ok, DbConfig} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, SessionConfig} = onepanel_gui_utils:get_session_config(),

        [
            <<"<i class=\"dropdown-arrow dropdown-arrow-inverse\"></i>">>,
            #button{
                disabled = DbConfig#?CONFIG.main_ccm =/= undefined,
                class = <<"btn btn-inverse btn-small dropdown-toggle">>,
                data_fields = [{<<"data-toggle">>, <<"dropdown">>}],
                body = [
                    #span{
                        id = <<"ccms_label">>,
                        style = <<"padding-right: 1em;">>,
                        class = <<"filter-option pull-left">>,
                        body = <<"Primary CCM host: <b>", (gui_str:html_encode(SessionConfig#?CONFIG.main_ccm))/binary, "</b>">>
                    },
                    #span{
                        class = <<"caret pull-right">>
                    }
                ]
            },
            #list{
                id = <<"ccms_dropdown">>,
                class = <<"dropdown-menu dropdown-inverse">>,
                style = <<"overflow-y: auto; max-height: 20em;">>,
                body = ccms_list(SessionConfig#?CONFIG.main_ccm, SessionConfig#?CONFIG.ccms)
            }
        ]
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>),
            []
    end.


%% ccms_list/2
%% ====================================================================
%% @doc Renders CCMs' list body.
%% @end
-spec ccms_list(MainCCM :: string(), CCMs :: [string()]) -> Result when
    Result :: [#li{}].
%% ====================================================================
ccms_list(MainCCM, CCMs) ->
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
                    body = gui_str:html_encode(CCM)
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
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),
    ok;

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_HOST_SELECTION);

event(next) ->
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_SYSTEM_LIMITS);

event({set_main_ccm, MainCCM, CCMs}) ->
    Config = gui_ctx:get(?CONFIG_ID),
    gui_ctx:put(?CONFIG_ID, Config#?CONFIG{main_ccm = MainCCM}),
    gui_jq:update(<<"ccms_label">>, <<"Primary CCM host: <b>", (gui_str:html_encode(MainCCM))/binary, "</b>">>),
    gui_jq:update(<<"ccms_dropdown">>, ccms_list(MainCCM, CCMs));

event(terminate) ->
    ok.