%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% This page allows to check whether all VeilCluster ports are available
%% for Global Registry.
%% @end
%% ===================================================================

-module(page_ports_check).
-export([main/0, event/1]).

-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").

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
            case onepanel_gui_utils:maybe_redirect(?CURRENT_REGISTRATION_PAGE, ?PAGE_PORTS_CHECK, ?PAGE_SPACES_ACCOUNT) of
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
    <<"Ports check">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    ControlPanelHosts = case gr_utils:get_control_panel_hosts() of
                            {ok, Hosts} -> Hosts;
                            _ -> []
                        end,
    {DefaultGuiPort, DefaultRestPort} = case gr_utils:get_ports_to_check() of
                                            {ok, [{"gui", GuiPort}, {"rest", RestPort}]} -> {GuiPort, RestPort};
                                            _ -> {0, 0}
                                        end,
    {TextboxIds, _} = lists:foldl(fun(_, {Ids, Id}) ->
        HostId = integer_to_binary(Id),
        {[<<"gui_port_textbox_", HostId/binary>>, <<"rest_port_textbox_", HostId/binary>> | Ids], Id + 1}
    end, {[], 1}, ControlPanelHosts),

    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_account_link),
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
                body = <<"Step 2: Check VeilCluster ports availability for Global Registry.">>
            },
            #table{
                class = <<"table table-bordered">>,
                style = <<"width: 50%; margin: 0 auto;">>,
                body = ports_table_body(ControlPanelHosts, DefaultGuiPort, DefaultRestPort)
            },
            #panel{
                style = <<"width: 50%; margin: 0 auto; margin-top: 3em;">>,
                body = [
                    #button{
                        id = <<"back_button">>,
                        postback = back,
                        class = <<"btn btn-inverse btn-small">>,
                        style = <<"float: left; width: 80px; font-weight: bold;">>,
                        body = <<"Back">>
                    },
                    #button{
                        id = <<"next_button">>,
                        actions = gui_jq:form_submit_action(<<"next_button">>, {check_ports, ControlPanelHosts}, TextboxIds),
                        class = <<"btn btn-inverse btn-small">>,
                        style = <<"float: right; width: 80px; font-weight: bold;">>,
                        body = <<"Next">>
                    }
                ]
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% ports_table_body/3
%% ====================================================================
%% @doc Renders system limits table body.
-spec ports_table_body(Hosts :: [string()], DefaultGuiPort :: integer(), DefaultRestPort :: integer()) -> Result
    when Result :: [#tr{}].
%% ====================================================================
ports_table_body(Hosts, DefaultGuiPort, DefaultRestPort) ->
    ColumnStyle = <<"text-align: center; vertical-align: inherit;">>,
    Header = #tr{
        cells = [
            #th{
                body = <<"Host">>,
                style = ColumnStyle
            },
            #th{
                body = <<"GUI port">>,
                style = ColumnStyle
            },
            #th{
                body = <<"REST port">>,
                style = ColumnStyle
            }
        ]
    },
    try
        Rows = lists:map(fun({Host, Id}) ->
            HostId = integer_to_binary(Id),
            {GuiPort, RestPort} =
                case dao:get_record(?LOCAL_CONFIG_TABLE, Host) of
                    {ok, #?LOCAL_CONFIG_RECORD{gui_port = undefined, rest_port = undefined}} ->
                        {DefaultGuiPort, DefaultRestPort};
                    {ok, #?LOCAL_CONFIG_RECORD{gui_port = Port, rest_port = undefined}} ->
                        {Port, DefaultRestPort};
                    {ok, #?LOCAL_CONFIG_RECORD{gui_port = undefined, rest_port = Port}} ->
                        {DefaultGuiPort, Port};
                    {ok, #?LOCAL_CONFIG_RECORD{gui_port = Port1, rest_port = Port2}} ->
                        {Port1, Port2};
                    _ ->
                        {DefaultGuiPort, DefaultRestPort}
                end,
            Textboxes = [
                {
                    <<"gui_port_textbox_">>,
                    GuiPort
                },
                {
                    <<"rest_port_textbox_">>,
                    RestPort
                }
            ],

            #tr{
                id = <<"row_", HostId/binary>>,
                cells = [
                    #td{
                        body = <<"<b>", (list_to_binary(Host))/binary, "</b>">>,
                        style = ColumnStyle
                    } | lists:map(fun({Prefix, Port}) ->
                        #td{
                            style = ColumnStyle,
                            body = #textbox{
                                id = <<Prefix/binary, HostId/binary>>,
                                style = <<"text-align: center;">>,
                                class = <<"span1">>,
                                value = integer_to_binary(Port)
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


%% validate_port/1
%% ====================================================================
%% @doc Checks whether given port is a positive number.
-spec validate_port(Port :: string()) -> Result
    when Result :: true | false.
%% ====================================================================
validate_port(Port) ->
    Regex = "[1-9][0-9]*",
    Length = length(Port),
    case re:run(Port, Regex) of
        {match, [{0, Length}]} -> true;
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
    onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_CONNECTION_CHECK);

event({check_ports, Hosts}) ->
    case lists:foldl(fun(Host, {PortsErrors, Id}) ->
        HostId = integer_to_binary(Id),
        Textboxes = [
            {<<"gui_port_textbox_", HostId/binary>>, <<"gui">>, gui_port},
            {<<"rest_port_textbox_", HostId/binary>>, <<"rest">>, rest_port}
        ],
        {
                lists:filter(fun({TextboxId, Type, Field}) ->
                    try
                        Port = gui_str:to_list(gui_ctx:postback_param(TextboxId)),
                        true = validate_port(Port),
                        ok = dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{Field, list_to_integer(Port)}]),
                        ok = gr_adapter:check_port(Host, list_to_integer(Port), Type),
                        gui_jq:css(TextboxId, <<"border-color">>, <<"green">>),
                        false
                    catch
                        _:_ ->
                            gui_jq:css(TextboxId, <<"border-color">>, <<"red">>),
                            true
                    end
                end, Textboxes) ++ PortsErrors,
            Id + 1
        }
    end, {[], 1}, Hosts) of
        {[], _} ->
            onepanel_gui_utils:change_page(?CURRENT_REGISTRATION_PAGE, ?PAGE_REGISTRATION_SUMMARY);
        _ ->
            onepanel_gui_utils:message(<<"error_message">>, <<"Some ports are not available for Global Registry.
            Please change them and try again.">>)
    end;

event(terminate) ->
    ok.