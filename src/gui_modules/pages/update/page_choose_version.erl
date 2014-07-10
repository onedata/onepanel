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

-module(page_choose_version).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include("onepanel_modules/updater/common.hrl").
-include_lib("ctool/include/logging.hrl").

%% Currently selected version's checkbox ID
-define(CURRENT_VERSION_ID, current_version_id).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_UPDATE_PAGE, ?PAGE_CHOOSE_VERSION, ?PAGE_UPDATE) of
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
    <<"Choose version">>.


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
            onepanel_gui_utils:top_menu(update_tab),

            #panel{
                id = <<"error_message">>,
                style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
                class = <<"dialog dialog-danger">>
            },
            #panel{
                style = <<"margin-top: 150px; text-align: center;">>,
                body = case installer_utils:get_workers() of
                           [] ->
                               #panel{
                                   style = <<"width: 50%; margin: 0 auto;">>,
                                   class = <<"alert alert-info">>,
                                   body = [
                                       #h3{
                                           body = <<"Software is not installed.">>
                                       },
                                       #p{
                                           body = <<"Please complete installation process to proceed with update.">>
                                       },
                                       #link{
                                           id = <<"next_button">>,
                                           postback = to_main_page,
                                           class = <<"btn btn-info">>,
                                           style = <<"width: 80px; font-weight: bold;">>,
                                           body = <<"OK">>
                                       }
                                   ]
                               };
                           _ ->
                               [
                                   #h6{
                                       style = <<"font-size: 18px;">>,
                                       body = <<"Step 1: Select VeilCluster version to update to.">>
                                   },
                                   #h6{
                                       style = <<"font-size: 16px; margin-top: 30px;">>,
                                       body = case get_software_version() of
                                                  undefined -> [];
                                                  Version ->
                                                      <<"Current software version: <b>", (list_to_binary(Version))/binary, "</b>">>
                                              end
                                   },
                                   #table{
                                       class = <<"table table-bordered">>,
                                       style = <<"width: 300px; margin: 0 auto; margin-top: 30px;">>,
                                       body = versions_table_body()
                                   },
                                   #panel{
                                       style = <<"margin-top: 30px; margin-bottom: 30px;">>,
                                       body = #button{
                                           id = <<"next_button">>,
                                           postback = next,
                                           class = <<"btn btn-inverse btn-small">>,
                                           style = <<"width: 80px; font-weight: bold;">>,
                                           body = <<"Next">>
                                       }
                                   }
                               ]
                       end

            }
        ] ++ onepanel_gui_utils:logotype_footer(120)
    }.


%% versions_table_body/0
%% ====================================================================
%% @doc Renders versions table body.
-spec versions_table_body() -> Result when
    Result :: [#tr{}].
%% ====================================================================
versions_table_body() ->
    VersionColumnStyle = <<"text-align: center; vertical-align: inherit; width: 80%;">>,
    CheckboxColumnStyle = <<"text-align: center; vertical-align: inherit; width: 20%;">>,

    Header = #tr{
        cells = [
            #th{
                body = <<"Available versions">>,
                style = VersionColumnStyle
            },
            #th{
                body = <<"">>,
                style = CheckboxColumnStyle
            }
        ]
    },

    try
        GetVersionName = fun(#version{major = Major, minor = Minor, patch = Patch}) ->
            <<(integer_to_binary(Major))/binary, ".", (integer_to_binary(Minor))/binary, ".", (integer_to_binary(Patch))/binary>>
        end,

        GetVersionId = fun(#version{major = Major, minor = Minor, patch = Patch}) ->
            <<"version_", (integer_to_binary(Major))/binary, "_", (integer_to_binary(Minor))/binary, "_", (integer_to_binary(Patch))/binary>>
        end,

        AvailableVersions = get_available_versions(),
        SortedAvailableVersions = sort_versions(AvailableVersions),

        CurrentVersionId = case gui_ctx:get(?CURRENT_VERSION_ID) of
                               undefined ->
                                   gui_ctx:put(?CURRENT_VERSION, hd(SortedAvailableVersions)),
                                   gui_ctx:put(?CURRENT_VERSION_ID, GetVersionId(hd(SortedAvailableVersions))),
                                   GetVersionId(hd(SortedAvailableVersions));
                               Version -> Version
                           end,

        Rows = lists:map(fun(Version) ->
            VersionId = GetVersionId(Version),
            #tr{cells = [
                #td{
                    body = <<"<b>", (GetVersionName(Version))/binary, "</b>">>,
                    style = VersionColumnStyle
                },
                #td{
                    style = CheckboxColumnStyle,
                    body = #label{
                        id = VersionId,
                        class = <<"checkbox no-label">>,
                        for = VersionId,
                        style = <<"width: 20px; margin: 0 auto;">>,
                        actions = gui_jq:postback_action(VersionId, {version_toggled, Version, VersionId}),
                        body = [
                            #span{
                                class = <<"icons">>
                            },
                            #custom_checkbox{
                                id = VersionId,
                                data_fields = [{<<"data-toggle">>, <<"checkbox">>}],
                                value = <<"">>,
                                checked = CurrentVersionId =:= VersionId
                            }
                        ]
                    }
                }
            ]
            }
        end, SortedAvailableVersions),

        [Header | Rows]
    catch
        _:_ -> [Header]
    end.


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


%% get_software_version/0
%% ====================================================================
%% @doc Returns current software version.
-spec get_software_version() -> Result when
    Result :: string() | undefined.
%% ====================================================================
get_software_version() ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{workers = [Worker | _]}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        rpc:call(list_to_atom("worker@" ++ Worker), node_manager, check_vsn, [], ?RPC_TIMEOUT)
    catch
        _:Reason ->
            ?error("Cannot get current software version: ~p", [Reason]),
            undefined
    end.


%% get_available_versions/0
%% ====================================================================
%% @doc Returns available software versions read from repository.
-spec get_available_versions() -> Result when
    Result :: [binary()] | undefined.
%% ====================================================================
get_available_versions() ->
    try
        {ok, Url} = application:get_env(?APP_NAME, get_versions_url),
        {ok, "200", _ResHeaders, ResBody} = ibrowse:send_req(Url, [{content_type, "application/json"}], get),
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

event({version_toggled, Version, VersionId}) ->
    PreviousVersionId = gui_ctx:get(?CURRENT_VERSION_ID),
    gui_jq:click(PreviousVersionId),
    gui_ctx:put(?CURRENT_VERSION, Version),
    gui_ctx:put(?CURRENT_VERSION_ID, VersionId);

event(next) ->
    onepanel_gui_utils:change_page(?CURRENT_UPDATE_PAGE, ?PAGE_UPDATE_SUCCESS);

event(to_main_page) ->
    gui_jq:redirect(?PAGE_ROOT);

event(terminate) ->
    ok.