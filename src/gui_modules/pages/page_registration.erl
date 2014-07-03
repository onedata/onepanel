%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% The page handles registration in Global Registry.
%% @end
%% ===================================================================

-module(page_registration).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").
-include("onepanel_modules/db_logic.hrl").
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
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]};
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
    <<"Registration">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{} | no_return().
%% ====================================================================
body() ->
    #panel{style = <<"position: relative;">>, body = [
        onepanel_gui_utils:top_menu(registration_tab),
        #panel{id = <<"ok_message">>, style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
            class = <<"dialog dialog-success">>},
        #panel{id = <<"error_message">>, style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
            class = <<"dialog dialog-danger">>},

        #panel{id = <<"step_0">>, style = <<"margin-top: 150px; text-align: center;">>, body = [
            #panel{style = <<"width: 50%; margin: 0 auto;">>, body = check_connection_body()}
        ]},

        ports_body(),

        #panel{id = <<"step_2">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
            #panel{id = <<"registration_info">>, style = <<"width: 50%; margin: 0 auto;">>, body = []}
        ]}
    ] ++ onepanel_gui_utils:logotype_footer(120)}.


%% check_connection_body/0
%% ====================================================================
%% @doc Checks connection to Global Registry and in case of an error
%% renders appropriate message.
-spec check_connection_body() -> Result
    when Result :: [#panel{}].
%% ====================================================================
check_connection_body() ->
    #panel{class = <<"alert alert-info">>, body = [
        #h3{body = <<"Checking connection to Global Registry...">>}
    ]}.


%% registration_success/2
%% ====================================================================
%% @doc Renders registration message with provider ID.
-spec registration_success(Message :: binary(), ProviderId :: binary()) -> Result
    when Result :: [#panel{}].
%% ====================================================================
registration_success(Message, ProviderId) ->
    [
        #panel{class = <<"alert alert-success">>, body = [
            #h3{body = Message},
            #p{body = <<"Your provider ID: ", ProviderId/binary>>},
            #link{postback = to_main_page, class = <<"btn btn-primary">>, body = <<"OK">>}
        ]}
    ].


%% registration_success/2
%% ====================================================================
%% @doc Renders registration error message when cannot connect to
%% Global Registry.
-spec registration_failure() -> Result
    when Result :: [#panel{}].
%% ====================================================================
registration_failure() ->
    [
        #panel{class = <<"alert alert-danger">>, body = [
            #h3{body = <<"Cannot connect to Global Registry.">>},
            #p{body = <<"Check your network configuration or try again later.">>},
            #link{postback = to_main_page, style = <<"width: 80px;">>, class = <<"btn btn-danger">>, body = <<"OK">>}
        ]}
    ].


%% installation_failure/0
%% ====================================================================
%% @doc Renders installation error message when cannot find main CCM
%% in installation configuration.
-spec installation_failure() -> Result
    when Result :: [#panel{}].
%% ====================================================================
installation_failure() ->
    [
        #panel{class = <<"alert alert-danger">>, body = [
            #h3{body = <<"Cluster is not installed.">>},
            #p{body = <<"To register in Global Registry you have to complete installation process.">>},
            #link{postback = to_main_page, style = <<"width: 80px;">>, class = <<"btn btn-danger">>, body = <<"OK">>}
        ]}
    ].


%% ports_table_body/2
%% ====================================================================
%% @doc Renders ports body in first step of registration
-spec ports_body() -> Result when
    Result :: #panel{}.
%% ====================================================================
ports_body() ->
    {ControlPanelHosts, {ok, PortsToCheck}} =
        case install_utils:get_main_ccm() of
            undefined -> {[], {ok, []}};
            MainCCM -> case install_utils:get_control_panel_hosts(MainCCM) of
                           {ok, Hosts} ->
                               {Hosts, install_utils:get_ports_to_check(MainCCM)};
                           _ -> {[], {ok, []}}
                       end
        end,

    {PortIds, _} = lists:foldl(fun(_, {Acc, Id}) ->
        {Acc ++ lists:map(fun({Type, _}) ->
            <<"port_", (list_to_binary(Type))/binary, "_", (integer_to_binary(Id))/binary>>
        end, PortsToCheck), Id + 1}
    end, {[], 0}, ControlPanelHosts),

    #panel{id = <<"step_1">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
        #h6{style = <<"font-size: 18px;">>, body = <<"Step 1: Connectivity checkup.">>},
        #table{class = <<"table table-bordered">>, style = <<"width: 50%; margin: 0 auto; margin-top: 20px; margin-bottom: 50px;">>, body = [
            #tbody{id = <<"ports_table">>, body = ports_table_body(ControlPanelHosts, PortsToCheck)}
        ]},
        #panel{style = <<"margin-top: 30px; margin-bottom: 30px;">>, body = [
            #button{id = <<"check_ports">>, actions = gui_jq:form_submit_action(<<"check_ports">>,
                {check_ports, ControlPanelHosts, PortsToCheck}, PortIds),
                class = <<"btn btn-inverse btn-small">>, style = <<"width: 60px; font-weight: bold;">>, body = <<"Next">>}
        ]}
    ]}.


%% ports_table_body/2
%% ====================================================================
%% @doc Renders ports table body in first step of registration
-spec ports_table_body(ControlPanelHosts :: [string()], PortsToCheck :: [{Name :: string(), Value :: integer()}]) -> Result when
    Result :: [#tr{}].
%% ====================================================================
ports_table_body(ControlPanelHosts, PortsToCheck) ->
    Header = #tr{cells = [
        #th{body = <<"Host">>, style = <<"text-align: center; vertical-align: inherit;">>},
        #th{body = <<"Type">>, style = <<"text-align: center; vertical-align: inherit;">>},
        #th{body = <<"Port">>, style = <<"text-align: center; vertical-align: inherit;">>}
    ]},
    {Body, _} = lists:foldl(fun(Host, {Acc, Id}) ->
        {Acc ++ ports_table_rows(Host, PortsToCheck, Id), Id + 1}
    end, {[], 0}, ControlPanelHosts),
    [Header | Body].


%% ports_table_rows/3
%% ====================================================================
%% @doc Renders ports table row in first step of registration
-spec ports_table_rows(Host :: string(), PortsToCheck :: [{Name :: string(), Value :: integer()}], Id :: integer()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
ports_table_rows(ControlPanelHost, PortsToCheck, Id) ->
    lists:map(fun({Type, Port}) ->
        BinaryId = <<"port_", (list_to_binary(Type))/binary, "_", (integer_to_binary(Id))/binary>>,
        #tr{cells = [
            #th{body = list_to_binary(ControlPanelHost), style = <<"text-align: center; vertical-align: inherit;">>},
            #th{body = case Type of
                           "gui" -> "GUI";
                           "rest" -> "REST"
                       end,
                style = <<"text-align: center; vertical-align: inherit;">>},
            #th{body = #textbox{id = BinaryId,
                value = integer_to_binary(Port), style = <<"width: 80px; margin-bottom: 0px; text-align: center;">>},
                style = <<"text-align: center; vertical-align: inherit;">>}
        ]}
    end, PortsToCheck).


%% comet_loop/0
%% ====================================================================
%% @doc Handles messages that change ports configuration.
-spec comet_loop() -> no_return().
%% ====================================================================
comet_loop() ->
    try
        receive
            {check_ports, ErrorPorts} ->
                case ErrorPorts of
                    [] ->
                        gui_jq:fade_out(<<"error_message">>, 300),
                        gui_jq:update(<<"ok_message">>, <<"All ports are available for Global Registry. Please wait while finalizing registration.">>),
                        gui_jq:fade_in(<<"ok_message">>, 300),
                        erlang:send_after(1000, self(), register);
                    _ ->
                        gui_jq:fade_out(<<"ok_message">>, 300),
                        gui_jq:update(<<"error_message">>, <<"Some ports are not available for Global Registry. Please change them and try again.">>),
                        gui_jq:fade_in(<<"error_message">>, 300)
                end,
                gui_comet:flush(),
                comet_loop();

            register ->
                case gr_adapter:register() of
                    {ok, ProviderId} ->
                        gui_jq:fade_out(<<"ok_message">>, 300),
                        dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{providerId, ProviderId}]),
                        gui_jq:update(<<"registration_info">>, registration_success(<<"Successful registration.">>, ProviderId)),
                        onepanel_gui_utils:change_step(1, 1);
                    _ ->
                        gui_jq:update(<<"error_message">>, <<"Cannot register in Global Registry. Please try again later.">>),
                        gui_jq:fade_in(<<"error_message">>, 300)
                end,
                gui_comet:flush(),
                comet_loop()
        end
    catch Type:Reason ->
        ?error("Comet process exception: ~p:~p", [Type, Reason]),
        gui_jq:update(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>),
        gui_jq:fade_in(<<"error_message">>, 300),
        gui_comet:flush()
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
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop() end),
    put(comet_pid, Pid),

    case install_utils:get_main_ccm() of
        undefined ->
            gui_jq:update(<<"registration_info">>, installation_failure()),
            onepanel_gui_utils:change_step(0, 2);
        _ ->
            case install_utils:get_provider_id() of
                undefined ->
                    timer:sleep(2000),
                    case gr_adapter:check_ip_address() of
                        {ok, _} ->
                            onepanel_gui_utils:change_step(0, 1);
                        _ ->
                            gui_jq:update(<<"registration_info">>, registration_failure()),
                            onepanel_gui_utils:change_step(0, 2)
                    end;
                ProviderId ->
                    gui_jq:update(<<"registration_info">>, registration_success(<<"You are already registerd in Global Registry.">>, ProviderId)),
                    onepanel_gui_utils:change_step(0, 2)
            end
    end;

event(to_main_page) ->
    gui_jq:redirect(<<"/">>);

event({check_ports, ControlPanelHosts, PortsToCheck}) ->
    {ErrorPorts, _} = lists:foldl(fun(Host, {Acc, Id}) ->
        {Acc ++ lists:filter(fun({Type, _}) ->
            PortId = <<"port_", (list_to_binary(Type))/binary, "_", (integer_to_binary(Id))/binary>>,
            try
                Port = binary_to_integer(gui_ctx:postback_param(PortId)),
                ok = gr_adapter:check_port(Host, Port, Type),
                dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{list_to_atom(Type ++ "_port"), Port}]),
                gui_jq:wire(<<"$('#", PortId/binary, "').css('border-color', 'green');">>),
                false
            catch
                _:_ ->
                    gui_jq:wire(<<"$('#", PortId/binary, "').css('border-color', 'red');">>),
                    true
            end
        end, PortsToCheck), Id + 1}
    end, {[], 0}, ControlPanelHosts),

    get(comet_pid) ! {check_ports, ErrorPorts};

event(terminate) ->
    ok.