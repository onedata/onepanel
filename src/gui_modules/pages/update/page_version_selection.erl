%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to select VeilCluster update version.
%% @end
%% ===================================================================
-module(page_version_selection).

-include("gui_modules/common.hrl").
-include("onepanel_modules/updater/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1]).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_UPDATE_PAGE, ?PAGE_VERSION_SELECTION, ?PAGE_UPDATE) of
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
%% @doc This will be placed instead of {{title}} tag in template.
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Version selection">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(software_tab, update_link),
    Main = #panel{
        style = <<"margin-top: 10em;">>,
        body = [
            #panel{
                style = <<"text-align: center;">>,
                body = [
                    #h6{
                        style = <<"font-size: x-large; margin-bottom: 1em;">>,
                        body = <<"Step 1: Version selection.">>
                    },
                    #p{
                        style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 1em;">>,
                        body = case onepanel_utils:get_software_version() of
                                   undefined ->
                                       <<"Please select software version to update to.">>;
                                   Version ->
                                       <<"Please select software version to update to.<br>
                                       Current software version: <b>", Version/binary, "</b>">>
                               end
                    },
                    #panel{
                        class = <<"btn-group">>,
                        body = versions()
                    },
                    onepanel_gui_utils:nav_buttons([{<<"next_button">>, {postback, next}, false, <<"Next">>}])
                ]
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% versions/0
%% ====================================================================
%% @doc Renders software version dropdown body and highlights current choice.
%% @end
-spec versions() -> Result when
    Result :: [term()].
%% ====================================================================
versions() ->
    try
        AvailableVersions = onepanel_utils:get_available_software_versions(),
        ChosenVersion = case gui_ctx:get(?CHOSEN_VERSION) of
                            undefined -> hd(AvailableVersions);
                            Version -> Version
                        end,
        gui_ctx:put(?CHOSEN_VERSION, ChosenVersion),

        [
            <<"<i class=\"dropdown-arrow dropdown-arrow-inverse\"></i>">>,
            #button{
                class = <<"btn btn-inverse btn-small dropdown-toggle">>,
                data_fields = [{<<"data-toggle">>, <<"dropdown">>}],
                body = [
                    #span{
                        id = <<"version_label">>,
                        style = <<"padding-right: 1em;">>,
                        class = <<"filter-option pull-left">>,
                        body = <<"Version: <b>", (onepanel_utils:get_software_version_name(ChosenVersion))/binary, "</b>">>
                    },
                    #span{
                        class = <<"caret pull-right">>
                    }
                ]
            },
            #list{
                id = <<"version_dropdown">>,
                class = <<"dropdown-menu dropdown-inverse">>,
                style = <<"overflow-y: auto; max-height: 20em;">>,
                body = versions_list(ChosenVersion, lists:reverse(AvailableVersions))
            }
        ]
    catch
        _:Reason ->
            ?error("Cannot fetch available software versions from remote repository: ~p", [Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch available software versions from remote repository.">>),
            []
    end.


%% versions_list/2
%% ====================================================================
%% @doc Renders software versions list body.
%% @end
-spec versions_list(MainCCM :: string(), CCMs :: [string()]) -> Result when
    Result :: [#li{}].
%% ====================================================================
versions_list(ChosenVersion, Versions) ->
    {Body, _} = lists:foldl(fun(Version, {List, Id}) ->
        VersionId = <<"version_li_", (integer_to_binary(Id))/binary>>,
        {
            [#li{
                id = VersionId,
                actions = gui_jq:postback_action(VersionId, {set_version, Version, Versions}),
                class = case Version of
                            ChosenVersion -> <<"active">>;
                            _ -> <<"">>
                        end,
                body = #link{
                    style = <<"text-align: left;">>,
                    body = onepanel_utils:get_software_version_name(Version)
                }
            }, List],
            Id + 1
        }
    end, {[], 1}, Versions),
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

event({set_version, ChosenVersion, Versions}) ->
    gui_ctx:put(?CHOSEN_VERSION, ChosenVersion),
    gui_jq:update(<<"version_label">>, <<"Version: <b>", (onepanel_utils:get_software_version_name(ChosenVersion))/binary, "</b>">>),
    gui_jq:update(<<"version_dropdown">>, versions_list(ChosenVersion, Versions));

event(next) ->
    ChosenVersion = gui_ctx:get(?CHOSEN_VERSION),
    ChosenVersionName = onepanel_utils:get_software_version_name(ChosenVersion),
    case onepanel_utils:get_software_version() of
        ChosenVersionName -> onepanel_gui_utils:message(<<"error_message">>,
            <<"Nothing to do.<br>This software version is currently installed.">>);
        _ ->
            onepanel_gui_utils:change_page(?CURRENT_UPDATE_PAGE, ?PAGE_UPDATE_SUMMARY)
    end;

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.