%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page allows to select VeilCluster update version.
%% @end
%% ===================================================================

-module(page_version_selection).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include("onepanel_modules/updater/common.hrl").
-include_lib("ctool/include/logging.hrl").

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
%% @doc Page title.
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Version selection">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(software_tab, update_link),
    Main = #panel{
        style = <<"margin-top: 10em;">>,
        body = [
            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #panel{
                style = <<"text-align: center;">>,
                body = [
                    #h6{
                        style = <<"font-size: x-large; margin-bottom: 3em;">>,
                        body = <<"Step 1: Select software version to update to.">>
                    },
                    #h6{
                        style = <<"font-size: large; margin-bottom: 3em;">>,
                        body = case onepanel_utils:get_software_version() of
                                   undefined -> [];
                                   Version ->
                                       <<"Current software version: <b>", (list_to_binary(Version))/binary, "</b>">>
                               end
                    },
                    #panel{
                        class = <<"btn-group">>,
                        body = version_body()
                    },
                    #panel{
                        style = <<"margin-top: 3em;">>,
                        body = #button{
                            id = <<"next_button">>,
                            postback = next,
                            class = <<"btn btn-inverse btn-small">>,
                            style = <<"width: 80px; font-weight: bold;">>,
                            body = <<"Next">>
                        }
                    }
                ]
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% version_body/0
%% ====================================================================
%% @doc Renders software version dropdown body and highlights current choice.
-spec version_body() -> Result when
    Result :: [term()].
%% ====================================================================
version_body() ->
    try
        AvailableVersions = get_available_versions(),
        SortedAvailableVersions = sort_versions(AvailableVersions),

        ChosenVersion = case gui_ctx:get(?CHOSEN_VERSION) of
                            undefined -> hd(SortedAvailableVersions);
                            Version -> Version
                        end,
        gui_ctx:put(?CHOSEN_VERSION, ChosenVersion),

        [
            <<"<i class=\"dropdown-arrow dropdown-arrow-inverse\"></i>">>,
            #button{
                id = <<"version_button">>,
                class = <<"btn btn-inverse btn-small dropdown-toggle">>,
                style = <<"width: 180px;">>,
                data_fields = [{<<"data-toggle">>, <<"dropdown">>}],
                body = [
                    #span{
                        id = <<"version_label">>,
                        class = <<"filter-option pull-left">>,
                        body = <<"Version: <b>", (get_version_name(ChosenVersion))/binary, "</b>">>
                    },
                    #span{
                        class = <<"caret pull-right">>
                    }
                ]
            },
            #list{
                id = <<"version_dropdown">>,
                class = <<"dropdown-menu dropdown-inverse">>,
                style = <<"overflow-y: auto; max-height: 200px;">>,
                body = version_list_body(ChosenVersion, lists:reverse(SortedAvailableVersions))
            }
        ]
    catch
        _:_ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch available software versions from remote repository.">>),
            []
    end.


%% version_list_body/2
%% ====================================================================
%% @doc Renders software versions list body.
-spec version_list_body(MainCCM :: string(), CCMs :: [string()]) -> Result when
    Result :: [#li{}].
%% ====================================================================
version_list_body(ChosenVersion, Versions) ->
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
                    body = get_version_name(Version)
                }
            }, List],
            Id + 1
        }
    end, {[], 1}, Versions),
    Body.


%% get_version_name/1
%% ====================================================================
%% @doc Returns version in binary form.
-spec get_version_name(Version :: #version{}) -> Result when
    Result :: binary().
%% ====================================================================
get_version_name(#version{major = Major, minor = Minor, patch = Patch}) ->
    <<(integer_to_binary(Major))/binary, ".", (integer_to_binary(Minor))/binary, ".", (integer_to_binary(Patch))/binary>>.


%% get_version_record/0
%% ====================================================================
%% @doc Returns version in record form.
-spec get_version_record(Version :: binary()) -> Result when
    Result :: #version{}.
%% ====================================================================
get_version_record(Version) ->
    [Major, Minor, Patch | _] = binary:split(Version, <<".">>, [global]),
    #version{major = binary_to_integer(Major), minor = binary_to_integer(Minor), patch = binary_to_integer(Patch)}.


%% sort_versions/0
%% ====================================================================
%% @doc Sorts versions in descending order and eliminates duplicates.
-spec sort_versions(Versions :: [#version{}]) -> Result when
    Result :: [#version{}].
%% ====================================================================
sort_versions(Versions) ->
    CmpPatch = fun
        (#version{patch = PatchA}, #version{patch = PatchB}) ->
            PatchA >= PatchB
    end,
    CmpMinor = fun
        (#version{minor = Minor} = A, #version{minor = Minor} = B) ->
            CmpPatch(A, B);
        (#version{minor = MinorA}, #version{minor = MinorB}) ->
            MinorA > MinorB
    end,
    CmpMajor = fun
        (#version{major = Major} = A, #version{major = Major} = B) ->
            CmpMinor(A, B);
        (#version{major = MajorA}, #version{major = MajorB}) ->
            MajorA > MajorB
    end,
    lists:usort(CmpMajor, lists:map(fun(Version) ->
        get_version_record(Version)
    end, Versions)).


%% get_available_versions/0
%% ====================================================================
%% @doc Returns available software versions read from repository.
-spec get_available_versions() -> Result when
    Result :: [binary()] | undefined.
%% ====================================================================
get_available_versions() ->
    try
        {ok, URL} = application:get_env(?APP_NAME, get_versions_url),
        Options = [{connect_timeout, ?CONNECTION_TIMEOUT}],
        {ok, "200", _ResHeaders, ResBody} = ibrowse:send_req(URL, [{content_type, "application/json"}], get, "{}", Options),
        {_, List} = mochijson2:decode(ResBody),
        proplists:get_value(<<"VeilCluster-Linux.rpm">>, List)
    catch
        _:Reason ->
            ?error("Cannot get available software versions from repository: ~p", [Reason]),
            undefined
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

event({set_version, ChosenVersion, Versions}) ->
    gui_ctx:put(?CHOSEN_VERSION, ChosenVersion),
    gui_jq:update(<<"version_label">>, <<"Version: <b>", (get_version_name(ChosenVersion))/binary, "</b>">>),
    gui_jq:update(<<"version_dropdown">>, version_list_body(ChosenVersion, Versions));

event(next) ->
    #version{major = Major, minor = Minor, patch = Patch} = gui_ctx:get(?CHOSEN_VERSION),
    ChosenVersionName = integer_to_list(Major) ++ "." ++ integer_to_list(Minor) ++ "." ++ integer_to_list(Patch),
    case onepanel_utils:get_software_version() of
        ChosenVersionName -> onepanel_gui_utils:message(<<"error_message">>,
            <<"Nothing to do.<br>This software version is currently installed.">>);
        _ ->
            onepanel_gui_utils:change_page(?CURRENT_UPDATE_PAGE, ?PAGE_UPDATE_SUMMARY)
    end;

event(terminate) ->
    ok.