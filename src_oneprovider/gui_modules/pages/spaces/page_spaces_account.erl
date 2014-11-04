%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to manage provider account.
%% @end
%% ===================================================================
-module(page_spaces_account).

-include("gui_modules/common.hrl").
-include("onepanel_modules/logic/provider_logic.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, api_event/3, comet_loop/1]).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, ?PROVIDER_RECORD).

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
            case installer_utils_adapter:get_workers() of
                [] ->
                    page_error:redirect_with_error(?SOFTWARE_NOT_INSTALLED_ERROR),
                    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                _ ->
                    case gui_ctx:get(?CURRENT_REGISTRATION_PAGE) of
                        undefined ->
                            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, custom()}]};
                        Page ->
                            gui_jq:redirect(Page),
                            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
                    end
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
    <<"Account settings">>.


%% custom/0
%% ====================================================================
%% @doc This will be placed instead of {{custom}} tag in template.
%% @end
-spec custom() -> binary().
%% ====================================================================
custom() ->
    <<"<script src='/flatui/bootbox.min.js' type='text/javascript' charset='utf-8'></script>">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils_adapter:top_menu(spaces_tab, spaces_account_link, [], true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 3em;">>,
                body = <<"Account settings">>
            },
            #panel{
                id = <<"account_table">>
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% account_table/1
%% ====================================================================
%% @doc Renders the body of account table.
%% @end
-spec account_table(ProviderDetails :: #?PROVIDER_RECORD{}) -> Result when
    Result :: #table{}.
%% ====================================================================
account_table(#?PROVIDER_RECORD{id = ProviderId, name = ProviderName, urls = URLs, redirection_point = RedirectionPoint}) ->
    #table{
        style = <<"border-width: 0; width: 100%;">>,
        body = lists:map(fun({TooltipId, TooltipStyle, TooltipBody, LabelId, LabelBody, CellId, CellBody}) ->
            #tr{
                cells = [
                    #td{
                        style = <<"border-width: 0; text-align: right; padding: 1em 1em; width: 50%; vertical-align: top; position: relative;">>,
                        body = [
                            #panel{
                                id = TooltipId,
                                class = <<"tooltip left in tooltip-light">>,
                                style = TooltipStyle,
                                body = [
                                    #panel{
                                        class = <<"tooltip-arrow">>
                                    },
                                    #panel{
                                        class = <<"tooltip-inner">>,
                                        style = <<"font-size: small; width: 200px;">>,
                                        body = TooltipBody
                                    }
                                ]
                            },
                            #flatui_label{
                                id = LabelId,
                                style = <<"margin: 0 auto; cursor: auto;">>,
                                class = <<"label label-large label-inverse label-tooltip">>,
                                body = LabelBody
                            }
                        ]
                    },
                    #td{
                        id = CellId,
                        style = <<"border-width: 0;  text-align: left; padding: 1em 1em;">>,
                        body = CellBody
                    }
                ]
            }
        end, [
            {<<"provider_id_tooltip">>, <<"top: 0px; right: 110px; display: none;">>, <<"Globally unique identifier assigned by Global Registry.">>,
                <<"provider_id_label">>, <<"Provider ID">>, <<"provider_id">>, providerId(ProviderId)},
            {<<"provider_name_tooltip">>, <<"top: 5px; right: 77px; display: none;">>, <<"Provider's name in <i>onedata</i>.">>,
                <<"provider_name_label">>, <<"Name">>, <<"provider_name">>, provider_name(ProviderName)},
            {<<"urls_tooltip">>, <<"top: -10px; right: 73px; display: none;">>, <<"List of <i>worker</i> components' IP addresses visible for Global Registry.">>,
                <<"urls_label">>, <<"URLs">>, <<"urls">>, urls(URLs)},
            {<<"redirection_point_tooltip">>, <<"top: -10px; right: 143px; display: none;">>, <<"Web address used by Global Registry to redirect users to provider.">>,
                <<"redirection_point_label">>, <<"Redirection point">>, <<"redirection_point">>, redirection_point(RedirectionPoint)}
        ])
    }.


%% providerId/1
%% ====================================================================
%% @doc Renders provider ID.
%% @end
-spec providerId(ProviderId :: binary() | undefined) -> Result when
    Result :: #span{}.
%% ====================================================================
providerId(undefined) ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>,
            #link{
                id = <<"register_link">>,
                title = <<"Register">>,
                style = <<"margin-left: 1em;">>,
                class = <<"glyph-link">>,
                postback = register,
                body = #span{
                    class = <<"fui-plus-inverted">>
                }
            }
        ]
    };

providerId(ProviderId) ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            ProviderId,
            #link{
                title = <<"Unregister">>,
                style = <<"margin-left: 1em;">>,
                class = <<"glyph-link">>,
                postback = unregister,
                body = #span{
                    class = <<"fui-cross-inverted">>
                }
            }
        ]
    }.


%% provider_name/1
%% ====================================================================
%% @doc Renders provider's name.
%% @end
-spec provider_name(ProviderName :: binary() | undefined) -> Result when
    Result :: #p{}.
%% ====================================================================
provider_name(undefined) ->
    #span{
        style = <<"font-size: large;">>,
        body = <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>
    };

provider_name(ProviderName) ->
    #span{
        style = <<"font-size: large;">>,
        body = ProviderName
    }.


%% urls/1
%% ====================================================================
%% @doc Renders urls.
%% @end
-spec urls(URLs :: [binary()] | undefined) -> Result when
    Result :: #p{}.
%% ====================================================================
urls(undefined) ->
    #span{
        style = <<"font-size: large;">>,
        body = <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>
    };

urls([]) ->
    #span{
        style = <<"font-size: large;">>,
        body = <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>
    };

urls(URLs) ->
    #list{
        style = <<"list-style-type: none; margin: 0 auto;">>,
        body = lists:map(fun(URL) ->
            #li{
                body = #span{
                    style = <<"font-size: large;">>,
                    body = URL
                }
            }
        end, lists:sort(URLs))
    }.


%% redirection_point/1
%% ====================================================================
%% @doc Renders redirection point.
%% @end
-spec redirection_point(RedirectionPoint :: binary() | undefined) -> Result when
    Result :: #p{}.
%% ====================================================================
redirection_point(undefined) ->
    #span{
        style = <<"font-size: large;">>,
        body = <<"&#8212&#8212&#8212&#8212&#8212&#8212&#8212&#8212">>
    };

redirection_point(RedirectionPoint) ->
    #span{
        style = <<"font-size: large;">>,
        body = [
            RedirectionPoint,
            #link{
                title = <<"Edit">>,
                style = <<"margin-left: 1em;">>,
                class = <<"glyph-link">>,
                postback = {change_redirection_point, RedirectionPoint},
                body = #span{
                    class = <<"icomoon-pencil2">>
                }
            }
        ]
    }.


%% change_redirection_point/1
%% ====================================================================
%% @doc Renders textbox used to change redirection point.
-spec change_redirection_point(RedirectionPoint :: binary()) -> Result when
    Result :: list().
%% ====================================================================
change_redirection_point(RedirectionPoint) ->
    [
        #textbox{
            id = <<"new_redirection_point_textbox">>,
            style = <<"margin: 0 auto; padding: 1px;">>,
            class = <<"span">>,
            placeholder = <<"New redirection point">>
        },
        #link{
            id = <<"new_redirection_point_submit">>,
            style = <<"margin-left: 10px;">>,
            class = <<"glyph-link">>,
            title = <<"Submit">>,

            actions = gui_jq:form_submit_action(<<"new_redirection_point_submit">>,
                {submit_new_redirection_point, RedirectionPoint}, <<"new_redirection_point_textbox">>),
            body = #span{
                class = <<"fui-check-inverted">>,
                style = <<"font-size: large; vertical-align: middle;">>
            }
        },
        #link{
            style = <<"margin-left: 10px;">>,
            class = <<"glyph-link">>,
            title = <<"Cancel">>,
            postback = {cancel_new_redirection_point_submit, RedirectionPoint},
            body = #span{
                class = <<"fui-cross-inverted">>,
                style = <<"font-size: large; vertical-align: middle;">>
            }
        }
    ].


%% ====================================================================
%% Events handling
%% ====================================================================

%% comet_loop/1
%% ====================================================================
%% @doc Handles space management actions.
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{id = ProviderId} = State) ->
    NewCometLoopState =
        try
            receive
                unregister ->
                    case provider_logic:unregister() of
                        ok ->
                            gui_jq:update(<<"provider_id">>, providerId(undefined)),
                            gui_jq:update(<<"provider_name">>, provider_name(undefined)),
                            gui_jq:update(<<"urls">>, urls(undefined)),
                            gui_jq:update(<<"redirection_point">>, redirection_point(undefined)),
                            onepanel_gui_utils:message(success, <<"You have been successfully unregistered from Global Registry.">>);
                        _ ->
                            onepanel_gui_utils:message(error, <<"Cannot unregister from Global Registry.">>)
                    end,
                    State;

                {change_redirection_point, _, <<>>} ->
                    onepanel_gui_utils:message(error, <<"Redirection point cannot be empty.">>),
                    State;

                {change_redirection_point, RedirectionPoint, RedirectionPoint} ->
                    gui_jq:update(<<"redirection_point">>, redirection_point(RedirectionPoint)),
                    State;

                {change_redirection_point, OldRedirectionPoint, RedirectionPoint} ->
                    {NewRedirectionPoint, MessageType, Message} =
                        try
                            {host_and_port, {ok, Host, Port}} = {host_and_port, onepanel_utils_adapter:get_host_and_port(RedirectionPoint)},
                            {check_redirection_point, ok} = {check_redirection_point, gr_providers:check_port(provider, Host, Port, <<"gui">>)},
                            {modify_refirection_point, ok} = {modify_refirection_point, gr_providers:modify_details(provider, [{<<"redirectionPoint">>, RedirectionPoint}])},
                            {modify_refirection_point, ok} = {modify_refirection_point, dao:update_record(?PROVIDER_TABLE, ProviderId, [{redirection_point, RedirectionPoint}])},
                            {RedirectionPoint, success, <<"Redirection point changed successfully.">>}
                        catch
                            error:{badmatch, {host_and_port, {error, Error}}} when is_list(Error) ->
                                {OldRedirectionPoint, error, list_to_binary(Error)};
                            error:{badmatch, {host_and_port, _}} ->
                                {OldRedirectionPoint, error, <<"Invalid redirection point.">>};
                            error:{badmatch, {check_redirection_point, _}} ->
                                {OldRedirectionPoint, error, <<"Redirection point is not available for <i>Global Registry</i>">>};
                            error:{badmatch, {modify_refirection_point, Error}} ->
                                ?error("Cannot change redirection point: ~p", [Error]),
                                {OldRedirectionPoint, error, <<"Cannot change redirection point.<br>Please try again later.">>};
                            _:Other ->
                                ?error("Cannot change redirection point: ~p", [Other]),
                                {OldRedirectionPoint, error, <<"Cannot change redirection point.<br>Please try again later.">>}
                        end,
                    gui_jq:update(<<"redirection_point">>, redirection_point(NewRedirectionPoint)),
                    onepanel_gui_utils:message(MessageType, Message),
                    State#?STATE{redirection_point = NewRedirectionPoint}
            end
        catch Type:Reason ->
            ?error_stacktrace("Comet process exception: ~p:~p", [Type, Reason]),
            onepanel_gui_utils:message(error, <<"There has been an error in comet process. Please refresh the page.">>),
            {error, Reason}
        end,
    gui_jq:wire(<<"$('#main_spinner').delay(300).hide(0);">>, false),
    gui_comet:flush(),
    ?MODULE:comet_loop(NewCometLoopState).


%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    try
        ProviderDetails = case dao:get_records(?PROVIDER_TABLE) of
                              {ok, []} -> #?PROVIDER_RECORD{};
                              {ok, [ProviderRecord]} -> ProviderRecord
                          end,
        gui_jq:update(<<"account_table">>, account_table(ProviderDetails)),
        gui_jq:hide(<<"main_spinner">>),

        gui_jq:wire(#api{name = "unregister", tag = "unregister"}, false),
        gui_jq:bind_key_to_click(<<"13">>, <<"new_redirection_point_submit">>),
        lists:foreach(fun({LabelId, TooltipId}) ->
            gui_jq:wire(<<"$('#", LabelId/binary, "').hover(function() {"
            "       $('#", TooltipId/binary, "').show();"
            "   }, function() {"
            "       $('#", TooltipId/binary, "').hide();"
            "   }"
            ");">>)
        end, [
            {<<"provider_id_label">>, <<"provider_id_tooltip">>},
            {<<"provider_name_label">>, <<"provider_name_tooltip">>},
            {<<"urls_label">>, <<"urls_tooltip">>},
            {<<"redirection_point_label">>, <<"redirection_point_tooltip">>}
        ]),

        {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(ProviderDetails) end),
        put(?COMET_PID, Pid)
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            onepanel_gui_utils:message(error, <<"Cannot fetch provider details.<br>Please try again later.">>)
    end;

event(to_root_page) ->
    gui_jq:redirect(?PAGE_ROOT);

event(register) ->
    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK);

event(unregister) ->
    Title = <<"Unregister">>,
    Message = <<"Are you sure you want to unregister from Global Registry?">>,
    Script = <<"unregister();">>,
    ConfirmButtonClass = <<"btn-inverse">>,
    gui_jq:dialog_popup(Title, Message, Script, ConfirmButtonClass);

event({change_redirection_point, RedirectionPoint}) ->
    gui_jq:update(<<"redirection_point">>, change_redirection_point(RedirectionPoint)),
    gui_jq:focus(<<"new_redirection_point_textbox">>);

event({submit_new_redirection_point, RedirectionPoint}) ->
    NewRedirectionPoint = gui_ctx:postback_param(<<"new_redirection_point_textbox">>),
    get(?COMET_PID) ! {change_redirection_point, RedirectionPoint, NewRedirectionPoint},
    gui_jq:show(<<"main_spinner">>);

event({cancel_new_redirection_point_submit, RedirectionPoint}) ->
    gui_jq:update(<<"redirection_point">>, redirection_point(RedirectionPoint));

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.


%% api_event/3
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec api_event(Name :: string(), Args :: string(), Req :: string()) -> no_return().
%% ====================================================================
api_event("unregister", _, _) ->
    get(?COMET_PID) ! unregister,
    gui_jq:show(<<"main_spinner">>).