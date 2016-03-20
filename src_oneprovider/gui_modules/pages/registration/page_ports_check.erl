%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to check whether all oneprovider ports are available
%% for Global Registry.
%% @end
%% ===================================================================
-module(page_ports_check).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {ports}).

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK, ?PAGE_SPACES_ACCOUNT) of
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
    <<"Ports configuration">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Breadcrumbs = onepanel_gui_utils:breadcrumbs([
        {<<"Connection check">>, ?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK},
        {<<"Ports configuration">>, ?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK}
    ]),
    Header = onepanel_gui_utils_adapter:top_menu(spaces_tab, spaces_account_link, Breadcrumbs, true),
    Main = #panel{
        style = <<"margin-top: 2em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 1em;">>,
                body = <<"Step 2: Ports configuration.">>
            },
            #p{
                id = <<"ports_message">>,
                style = <<"font-size: medium; width: 50%; margin: 0 auto; margin-bottom: 3em;">>
            },
            #table{
                id = <<"redirection_point_table">>,
                style = <<"width: 50%; margin: 0 auto; margin-bottom: 3em; border-spacing: 1em; border-collapse: inherit;">>
            },
            #table{
                id = <<"ports_table">>,
                class = <<"table table-bordered">>,
                style = <<"width: 50%; margin: 0 auto; display: none;">>
            },
            #panel{
                id = <<"nav_buttons">>
            }
        ]
    },
    onepanel_gui_utils:body(?SUBMENU_HEIGHT, Header, Main, onepanel_gui_utils:logotype_footer()).


%% redirection_table_row/2
%% ====================================================================
%% @doc Renders redirection table row.
%% @end
-spec redirection_table_row(IpAddress :: binary(), Port :: integer()) -> Result
    when Result :: [#tr{}].
%% ====================================================================
redirection_table_row(IpAddress, Port) ->
    [
        #tr{
            cells = [
                #td{
                    style = <<"border-width: 0; width: 50%; text-align: right;">>,
                    body = #label{
                        style = <<"margin: 0 auto; cursor: auto;">>,
                        class = <<"label label-large label-inverse">>,
                        body = <<"Redirection point">>
                    }
                },
                #td{
                    style = <<"border-width: 0; width: 50%; text-align: left;">>,
                    body = #textbox{
                        id = <<"redirection_point_textbox">>,
                        style = <<"margin: 0 auto; padding: 1px;">>,
                        class = <<"span">>,
                        placeholder = <<"Redirection point">>,
                        value = <<"https://", IpAddress/binary, ":", (integer_to_binary(Port))/binary>>
                    }
                }
            ]
        }
    ].


%% ports_table/3
%% ====================================================================
%% @doc Renders system ports table body.
%% @end
-spec ports_table(Ports :: [{Host :: string(), Id :: binary(), GuiPort :: integer(), RestPort :: integer()}]) -> Result
    when Result :: [#tr{}].
%% ====================================================================
ports_table(Ports) ->
    ColumnStyle = <<"text-align: center; vertical-align: inherit;">>,

    Header = #tr{
        cells = lists:map(fun(Body) ->
            #th{
                body = Body,
                style = ColumnStyle
            }
        end, [<<"Host">>, <<"GUI port">>, <<"REST port">>])
    },

    Rows = lists:map(fun({Host, Id, GuiPort, RestPort}) ->
        #tr{
            cells = [
                #td{
                    body = <<"<b>", (list_to_binary(Host))/binary, "</b>">>,
                    style = ColumnStyle
                } | lists:map(fun({TextboxId, Port}) ->
                    #td{
                        style = ColumnStyle,
                        body = #textbox{
                            id = TextboxId,
                            style = <<"text-align: center; margin: 0 auto;">>,
                            class = <<"span1">>,
                            value = integer_to_binary(Port)
                        }
                    }
                end, [
                    {<<"gui_port_textbox_", Id/binary>>, GuiPort},
                    {<<"rest_port_textbox_", Id/binary>>, RestPort}
                ])
            ]
        }
    end, Ports),

    [Header | Rows].


%% validate_port/1
%% ====================================================================
%% @doc Checks whether given port is a positive number.
%% @end
-spec validate_port(Port :: string()) -> Result
    when Result :: true | false.
%% ====================================================================
validate_port(Port) ->
    Regex = <<"[1-9][0-9]*">>,
    case re:run(Port, Regex, [{capture, first, binary}]) of
        {match, [Port]} -> true;
        _ -> false
    end.


%% port_value/1
%% ====================================================================
%% @doc Returns default port in case of unsupported value.
%% @end
-spec port_value(Port :: term(), DefaultPort :: integer()) -> Result
    when Result :: integer().
%% ====================================================================
port_value(Port, _) when is_integer(Port) ->
    Port;

port_value(Port, _) when is_binary(Port) ->
    binary_to_integer(Port);

port_value(_, DefaultPort) ->
    DefaultPort.


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

comet_loop(#?STATE{ports = Ports} = State) ->
    NewState = try
                   receive
                       {render_ports_table, DefaultGuiPort, DefaultRestPort} ->
                           TextboxIds = [<<"redirection_point_textbox">> | lists:foldl(fun({_, Id, _, _}, TextboxIdsAcc) ->
                               [<<"gui_port_textbox_", Id/binary>>, <<"rest_port_textbox_", Id/binary>> | TextboxIdsAcc]
                           end, [], Ports)],
                           gui_jq:update(<<"ports_message">>, <<"By default provider's GUI Web pages are served on <b>",
                           DefaultGuiPort/binary, "</b> port.<br>REST API endpoints are available on <b>", DefaultRestPort/binary, "</b> port.<br>"
                           "If your network configuration differs please provide port translations in the table below.">>),
                           gui_jq:update(<<"ports_table">>, ports_table(Ports)),
                           gui_jq:update(<<"nav_buttons">>, onepanel_gui_utils:nav_buttons([
                               {<<"back_button">>, {postback, back}, false, <<"Back">>},
                               {<<"next_button">>, {actions, gui_jq:form_submit_action(<<"next_button">>, {set_ports, Ports}, TextboxIds)}, true, <<"Next">>}
                           ])),
                           gui_jq:fade_in(<<"ports_table">>, 500),
                           gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
                           State;

                       {set_ports, RedirectionPoint, NewPorts} ->
                           RedirectionPointStatus =
                               try
                                   {host_and_port, {ok, RedirectionPointHost, RedirectionPointPort}} = {host_and_port, onepanel_utils:get_host_and_port(RedirectionPoint)},
                                   %todo add port checking to op_worker and check 'ok' here
                                   {check_redirection_point, _} = {check_redirection_point, oz_providers:check_port(provider, RedirectionPointHost, RedirectionPointPort, <<"gui">>)},
                                   gui_ctx:put(redirection_point, RedirectionPoint),
                                   gui_jq:css(<<"redirection_point_textbox">>, <<"border-color">>, <<"green">>),
                                   ok
                               catch
                                   _:Reason ->
                                       ?error_stacktrace("Cannot set redirection point: ~p", [Reason]),
                                       onepanel_gui_utils:message(error, <<"Redirection point is not available for <i>Global Registry</i>.<br>
                                       This may occur due to NAT or PAT translation mechanisms. Please check your network configuration or try again later.">>),
                                       gui_jq:css(<<"redirection_point_textbox">>, <<"border-color">>, <<"red">>),
                                       error
                               end,
                           case lists:foldl(fun({Host, GuiPortId, GuiPort, RestPortId, RestPort}, Status) ->
                               lists:foldl(fun({PortId, Port, Type, Field}, HostStatus) ->
                                   try
                                       true = validate_port(Port),
                                       {ok, #?LOCAL_CONFIG_RECORD{ip_address = IpAddress}} = dao:get_record(?LOCAL_CONFIG_TABLE, Host),
                                       %todo add port checking to op_worker and check 'ok' here
                                       _ = oz_providers:check_port(provider, IpAddress, binary_to_integer(Port), Type),
                                       ok = dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{Field, binary_to_integer(Port)}]),
                                       gui_jq:css(PortId, <<"border-color">>, <<"green">>),
                                       HostStatus
                                   catch
                                       _:_ ->
                                           gui_jq:css(PortId, <<"border-color">>, <<"red">>),
                                           error
                                   end
                               end, Status, [
                                   {GuiPortId, GuiPort, <<"gui">>, gui_port},
                                   {RestPortId, RestPort, <<"rest">>, rest_port}
                               ])
                           end, RedirectionPointStatus, NewPorts) of
                               ok ->
                                   onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUMMARY);
                               _ ->
                                   onepanel_gui_utils:message(error, <<"Some ports are not available for <i>Global Registry</i>.<br>
                                   This may occur due to NAT or PAT translation mechanisms. Please check your network configuration or try again later.">>)
                           end,
                           gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"">>),
                           gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"">>),
                           State

                   after ?COMET_PROCESS_RELOAD_DELAY ->
                       State
                   end
               catch Type:Message ->
                   ?error_stacktrace("Comet process exception: ~p:~p", [Type, Message]),
                   onepanel_gui_utils:message(error, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Message}
               end,
    gui_jq:wire(<<"$('#main_spinner').delay(300).hide(0);">>, false),
    gui_comet:flush(),
    ?MODULE:comet_loop(NewState).


%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{workers = [Worker | _] = Workers}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        {ok, [{<<"gui">>, DefaultGuiPort}, {<<"rest">>, DefaultRestPort}]} = provider_logic:get_default_ports(),
        {ok, #?LOCAL_CONFIG_RECORD{ip_address = IpAddress}} = dao:get_record(?LOCAL_CONFIG_TABLE, Worker),
        gui_jq:update(<<"redirection_point_table">>, redirection_table_row(IpAddress, DefaultGuiPort)),

        Ports = lists:map(fun({Host, Id}) ->
            {GuiPort, RestPort} = case dao:get_record(?LOCAL_CONFIG_TABLE, Host) of
                                      {ok, #?LOCAL_CONFIG_RECORD{gui_port = Port1, rest_port = Port2}} ->
                                          {Port1, Port2};
                                      {error, <<"Record not found.">>} ->
                                          {DefaultGuiPort, DefaultRestPort};
                                      _ ->
                                          throw("Cannot get local configuration for host: " ++ Host)
                                  end,
            {Host, integer_to_binary(Id), port_value(GuiPort, DefaultGuiPort), port_value(RestPort, DefaultRestPort)}
        end, lists:zip(Workers, tl(lists:seq(0, length(Workers))))),

        gui_jq:bind_key_to_click(<<"13">>, <<"next_button">>),

        {ok, Pid} = gui_comet:spawn(fun() ->
            comet_loop(#?STATE{ports = Ports})
        end),
        put(?COMET_PID, Pid),
        Pid ! {render_ports_table, integer_to_binary(DefaultGuiPort), integer_to_binary(DefaultRestPort)}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            gui_jq:hide(<<"main_spinner">>),
            onepanel_gui_utils:message(error, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event(back) ->
    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK);

event({set_ports, Ports}) ->
    RedirectionPoint = gui_ctx:postback_param(<<"redirection_point_textbox">>),
    NewPorts = lists:map(fun({Host, Id, _, _}) ->
        GuiPortId = <<"gui_port_textbox_", Id/binary>>,
        GuiPort = gui_ctx:postback_param(GuiPortId),
        RestPortId = <<"rest_port_textbox_", Id/binary>>,
        RestPort = gui_ctx:postback_param(RestPortId),
        {Host, GuiPortId, GuiPort, RestPortId, RestPort}
    end, Ports),
    gui_jq:show(<<"main_spinner">>),
    gui_jq:prop(<<"next_button">>, <<"disabled">>, <<"disabled">>),
    gui_jq:prop(<<"back_button">>, <<"disabled">>, <<"disabled">>),
    get(?COMET_PID) ! {set_ports, RedirectionPoint, NewPorts};

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.