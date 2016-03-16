%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to select host for main CM node during oneprovider
%% nodes installation.
%% @end
%% ===================================================================
-module(page_primary_cm_selection).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CM_SELECTION, ?PAGE_INSTALLATION) of
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
    <<"Primary CM selection">>.


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
        {<<"Primary CM selection">>, ?CURRENT_INSTALLATION_PAGE, ?PAGE_PRIMARY_CM_SELECTION}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(software_tab, installation_link, Breadcrumbs),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 2: Primary Cluster Manager selection.">>
            },
            #p{
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>,
                body = <<"<i>Cluster Manager</i> components control and organize work of other"
                " application components. However, it is not possible for more than one <i>CM</i> component"
                " to run simultaneously. Therefore, it is required to select primary <i>CM</i> component"
                " which will execute aforementioned tasks, while other <i>CM</i> components will wait"
                " and in case of primary <i>CM</i> breakdown take over its duties.">>
            },
            #panel{
                class = <<"btn-group">>,
                body = main_cm()
            },
            onepanel_gui_utils:nav_buttons([
                {<<"back_button">>, {postback, back}, false, <<"Back">>},
                {<<"next_button">>, {postback, next}, false, <<"Next">>}
            ])
        ]
    },
    onepanel_gui_utils:body(?SUBMENU_HEIGHT, Header, Main, onepanel_gui_utils:logotype_footer()).


%% main_cm/0
%% ====================================================================
%% @doc Renders main CM dropdown body and highlights current choice.
%% @end
-spec main_cm() -> Result when
    Result :: [term()].
%% ====================================================================
main_cm() ->
    try
        {ok, DbConfig} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, SessionConfig} = onepanel_gui_utils:get_session_config(),

        [
            <<"<i class=\"dropdown-arrow dropdown-arrow-inverse\"></i>">>,
            #button{
                disabled = DbConfig#?CONFIG.main_cm =/= undefined,
                class = <<"btn btn-inverse btn-small dropdown-toggle">>,
                data_fields = [{<<"data-toggle">>, <<"dropdown">>}],
                body = [
                    #span{
                        id = <<"cms_label">>,
                        style = <<"padding-right: 1em;">>,
                        class = <<"filter-option pull-left">>,
                        body = <<"Primary CM host: <b>", (http_utils:html_encode(SessionConfig#?CONFIG.main_cm))/binary, "</b>">>
                    },
                    #span{
                        class = <<"caret pull-right">>
                    }
                ]
            },
            #list{
                id = <<"cms_dropdown">>,
                class = <<"dropdown-menu dropdown-inverse">>,
                style = <<"overflow-y: auto; max-height: 20em;">>,
                body = cms_list(SessionConfig#?CONFIG.main_cm, SessionConfig#?CONFIG.cms)
            }
        ]
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>),
            []
    end.


%% cms_list/2
%% ====================================================================
%% @doc Renders CMs' list body.
%% @end
-spec cms_list(MainCM :: string(), CMs :: [string()]) -> Result when
    Result :: [#li{}].
%% ====================================================================
cms_list(MainCM, CMs) ->
    {Body, _} = lists:foldl(fun(CM, {List, Id}) ->
        CMId = <<"cm_li_", (integer_to_binary(Id))/binary>>,
        {
            [#li{
                id = CMId,
                actions = gui_jq:postback_action(CMId, {set_main_cm, CM, CMs}),
                class = case CM of
                            MainCM -> <<"active">>;
                            _ -> <<"">>
                        end,
                body = #link{
                    style = <<"text-align: left;">>,
                    body = http_utils:html_encode(CM)
                }
            }, List],
            Id + 1
        }
    end, {[], 1}, lists:sort(fun(CM1, CM2) -> CM1 > CM2 end, CMs)),
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
    onepanel_gui_utils:change_page(?CURRENT_INSTALLATION_PAGE, ?PAGE_APP_PORTS_CHECK);

event({set_main_cm, MainCM, CMs}) ->
    Config = gui_ctx:get(?CONFIG_ID),
    gui_ctx:put(?CONFIG_ID, Config#?CONFIG{main_cm = MainCM}),
    gui_jq:update(<<"cms_label">>, <<"Primary CM host: <b>", (http_utils:html_encode(MainCM))/binary, "</b>">>),
    gui_jq:update(<<"cms_dropdown">>, cms_list(MainCM, CMs));

event(terminate) ->
    ok.