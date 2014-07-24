%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains Global Registry interaction functions.
%% It allows to create Certificate Signing Request and register in
%% Global Registry (gr for short).
%% @end
%% ===================================================================
-module(gr_adapter).

-include("registered_names.hrl").
-include("onepanel_modules/user_logic.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([register/0, unregister/0, create_csr/3, check_ip_address/0, check_port/3]).

-on_load(init/0).

%% ====================================================================
%% API functions
%% ====================================================================

%% init/0
%% ====================================================================
%% @doc Initializes NIF library used to create Certificate Signing Request.
%% @end
-spec init() -> ok | no_return().
%% ====================================================================
init() ->
    ok = erlang:load_nif("c_lib/gr_adapter_drv", 0).


%% create_csr/3
%% ====================================================================
%% @doc Call underlying NIF function. Creates private key and certificate
%% signing request and saves them at given paths.
%% Returns 0 in case of success and 1 in case of failure.
%% Can throw an exception if nif was not properly loaded.
%% @end
-spec create_csr(Password :: string(), KeyFile :: string(), CsrPath :: string()) -> Result when
    Result :: 0 | 1 | no_return().
%% ====================================================================
create_csr(_, _, _) ->
    throw("NIF library not loaded.").


%% register/0
%% ====================================================================
%% @doc Registers provider in Global Registry. In case of successful
%% registration generated private key and certificate are save on all
%% hosts. Returns provider ID or an error.
%% @end
-spec register() -> Result when
    Result :: {ok, ProviderId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
register() ->
    try
        {ok, KeyFile} = application:get_env(?APP_NAME, grpkey_file),
        {ok, KeyName} = application:get_env(?APP_NAME, grpkey_name),
        {ok, CsrPath} = application:get_env(?APP_NAME, grpcsr_file),
        {ok, CertName} = application:get_env(?APP_NAME, grpcert_name),
        {ok, CertFile} = application:get_env(?APP_NAME, grpcert_file),
        Path = filename:join([?DEFAULT_NODES_INSTALL_PATH, ?DEFAULT_WORKER_NAME, "certs"]),

        0 = create_csr("", KeyFile, CsrPath),

        %% Save private key on all hosts
        {ok, Key} = file:read_file(KeyFile),
        ok = onepanel_utils:save_file_on_hosts(Path, KeyName, Key),

        {ok, ProviderId, Cert} = send_csr(CsrPath),

        %% Save provider ID and certifiacte on all hosts
        ok = file:write_file(CertFile, Cert),
        ok = onepanel_utils:save_file_on_hosts(Path, CertName, Cert),

        {ok, ProviderId}
    catch
        _:Reason ->
            ?error("Cannot register in Global Registry: ~p", [Reason]),
            {error, Reason}
    end.


%% unregister/0
%% ====================================================================
%% @doc Unregisters provider from Global Registry and removes his details
%% from database.
%% @end
-spec unregister() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
unregister() ->
    try
        ProviderId = gr_utils:get_provider_id(),
        {ok, "204", _ResHeaders, _ResBody} = gr_utils:send_req("/provider", delete),
        ok = dao:delete_record(?PROVIDER_TABLE, ProviderId)
    catch
        _:Reason ->
            ?error("Cannot unregister from Global Registry: ~p", [Reason]),
            {error, Reason}
    end.


%% check_ip_address/0
%% ====================================================================
%% @doc Returns ip address that is visible for Global Registry.
%% @end
-spec check_ip_address() -> Result when
    Result :: {ok, IpAddress :: binary()} | {error, Reason :: term()}.
%% ====================================================================
check_ip_address() ->
    try
        {ok, Url} = application:get_env(?APP_NAME, global_registry_url),
        Options = [{connect_timeout, ?CONNECTION_TIMEOUT}],
        {ok, "200", _ResHeaders, ResBody} =
            ibrowse:send_req(Url ++ "/provider/test/check_my_ip", [{content_type, "application/json"}], get, "{}", Options),
        {ok, mochijson2:decode(ResBody)}
    catch
        _:Reason ->
            ?error("Cannot get ip address that is visible for Global Registry: ~p", [Reason]),
            {error, Reason}
    end.


%% check_port/0
%% ====================================================================
%% @doc Checks VeilCluster port availability for Global Registry on
%% given host.
%% @end
-spec check_port(Host :: string(), Port :: integer(), Type :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
check_port(Host, Port, Type) ->
    try
        Node = onepanel_utils:get_node(Host),
        {ok, IpAddress} = rpc:call(Node, ?MODULE, check_ip_address, [], ?RPC_TIMEOUT),
        {ok, Url} = application:get_env(?APP_NAME, global_registry_url),
        TestUrl = Url ++ "/provider/test/check_my_ports",
        Resource = case Type of
                       <<"gui">> -> <<"/connection_check">>;
                       <<"rest">> -> <<"/rest/latest/connection_check">>
                   end,
        CheckUrl = <<"https://", IpAddress/binary, ":", (integer_to_binary(Port))/binary, Resource/binary>>,
        ReqBody = iolist_to_binary(mochijson2:encode({struct, [{Type, CheckUrl}]})),

        {ok, "200", _ResHeaders, ResBody} = ibrowse:send_req(TestUrl, [{content_type, "application/json"}], get, ReqBody),

        [{_, <<"ok">>}] = mochijson2:decode(ResBody, [{format, proplist}]),
        ok
    catch
        _:Reason ->
            ?error("Cannot check port ~p on host ~p: ~p", [Port, Host, Reason]),
            {error, Reason}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% send_csr/1
%% ====================================================================
%% @doc Reads Certificate Signing Request from given path and sends it
%% to Global Registry. Returns provider ID and signed certificate if
%% successful or an error.
%% @end
-spec send_csr(CsrPath :: string()) -> Result when
    Result :: {ok, ProviderId :: binary(), Cert :: binary()} | {error, Reason :: term()}.
%% ====================================================================
send_csr(CsrPath) ->
    {ok, Url} = application:get_env(?APP_NAME, global_registry_url),
    {ok, Csr} = file:read_file(CsrPath),
    {ok, [ControlPanelHost | _]} = gr_utils:get_control_panel_hosts(),
    {ok, ControlPanelHostIpAddress} = rpc:call(onepanel_utils:get_node(ControlPanelHost), ?MODULE, check_ip_address, []),
    {ok, #?LOCAL_CONFIG_RECORD{gui_port = GuiPort}} = dao:get_record(?LOCAL_CONFIG_TABLE, ControlPanelHost),
    Urls = lists:map(fun(Host) ->
        {ok, IpAddress} = rpc:call(onepanel_utils:get_node(Host), ?MODULE, check_ip_address, [], ?RPC_TIMEOUT),
        <<"https://", IpAddress/binary, ":", (integer_to_binary(GuiPort))/binary>>
    end, onepanel_utils:get_hosts()),
    RedirectionPoint = <<"https://", ControlPanelHostIpAddress/binary, ":", (integer_to_binary(GuiPort))/binary>>,
    ReqBody = iolist_to_binary(mochijson2:encode({struct, [{urls, Urls}, {csr, Csr}, {redirectionPoint, RedirectionPoint}]})),

    {ok, "200", _ResHeaders, ResBody} = ibrowse:send_req(Url ++ "/provider", [{content_type, "application/json"}], post, ReqBody),

    List = mochijson2:decode(ResBody, [{format, proplist}]),
    ProviderId = proplists:get_value(<<"providerId">>, List),
    Cert = proplists:get_value(<<"certificate">>, List),

    ok = dao:save_record(?PROVIDER_TABLE, #?PROVIDER_RECORD{id = ProviderId, urls = Urls, redirectionPoint = RedirectionPoint}),

    case {ProviderId, Cert} of
        {undefined, _} -> {error, "Provider ID not found in response body."};
        {_, undefined} -> {error, "Certificate not found in response body."};
        {_, _} -> {ok, ProviderId, Cert}
    end.
