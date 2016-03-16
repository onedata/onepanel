%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains Global Registry interaction functions.
%% It allows to create Certificate Signing Request and register in
%% Global Registry (gr for short).
%% @end
%% ===================================================================
-module(provider_logic).

-include("registered_names.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include("onepanel_modules/logic/provider_logic.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([register/2, unregister/0, create_csr/3]).
-export([get_default_ports/0, get_provider_id/0, get_provider_name/0]).

-on_load(init/0).

%% ====================================================================
%% API functions
%% ====================================================================

%% init/0
%% ====================================================================
%% @doc Initializes NIF library.
%% @end
-spec init() -> ok | no_return().
%% ====================================================================
init() ->
    {ok, NifPrefix} = application:get_env(?APP_NAME, nif_prefix_dir),
    ok = erlang:load_nif(filename:join(NifPrefix, "provider_logic_drv"), 0).


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


%% register/2
%% ====================================================================
%% @doc Registers provider in Global Registry. In case of successful
%% registration generated private key and certificate are save on all
%% hosts. Returns provider ID or an error.
%% @end
-spec register(RedirectionPoint :: binary(), ClientName :: binary()) -> Result when
    Result :: {ok, ProviderId :: binary()} | {error, Reason :: term()}.
%% ====================================================================
register(RedirectionPoint, ClientName) ->
    try
        {ok, EtcDir} = application:get_env(?APP_NAME, platform_etc_dir),
        {ok, KeyFile} = application:get_env(?APP_NAME, ozpkey_path),
        {ok, KeyName} = application:get_env(?APP_NAME, ozpkey_name),
        {ok, CsrPath} = application:get_env(?APP_NAME, ozpcsr_path),
        {ok, CertName} = application:get_env(?APP_NAME, ozpcert_name),
        {ok, CertFile} = application:get_env(?APP_NAME, ozpcert_path),
        Path = filename:join([EtcDir, ?SOFTWARE_NAME, "certs"]),

        {ok, _} = create_csr("", KeyFile, CsrPath),

        %% Save private key on all hosts
        {ok, Key} = file:read_file(KeyFile),
        ok = onepanel_utils:save_file_on_hosts(Path, KeyName, Key),

        %% Register in Global Registry
        {ok, CSR} = file:read_file(CsrPath),
        {ok, #?GLOBAL_CONFIG_RECORD{workers = Workers}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        URLs = lists:map(fun(Host) ->
            {ok, #?LOCAL_CONFIG_RECORD{ip_address = URL}} = dao:get_record(?LOCAL_CONFIG_TABLE, Host),
            URL
        end, Workers),
        Parameters = [{<<"urls">>, URLs}, {<<"csr">>, CSR}, {<<"redirectionPoint">>, RedirectionPoint}, {<<"clientName">>, ClientName}],
        {ok, ProviderId, Cert} = gr_providers:register(provider, Parameters),

        %% Save provider ID and certificate on all hosts
        ok = file:write_file(CertFile, <<Cert/binary, Key/binary>>),
        ok = onepanel_utils:save_file_on_hosts(Path, CertName, <<Cert/binary, Key/binary>>),

        ok = dao:save_record(?PROVIDER_TABLE, #?PROVIDER_RECORD{id = ProviderId, name = ClientName, urls = URLs, redirection_point = RedirectionPoint}),

        {ok, ProviderId}
    catch
        _:Reason ->
            ?error_stacktrace("Cannot register in Global Registry: ~p", [Reason]),
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
        {ok, EtcDir} = application:get_env(?APP_NAME, platform_etc_dir),
        {ok, KeyName} = application:get_env(?APP_NAME, ozpkey_name),
        {ok, CertName} = application:get_env(?APP_NAME, ozpcert_name),
        Path = filename:join([EtcDir, ?SOFTWARE_NAME, "certs"]),
        ProviderId = get_provider_id(),

        ok = gr_providers:unregister(provider),
        ok = onepanel_utils:delete_file_on_hosts(Path, KeyName),
        ok = onepanel_utils:delete_file_on_hosts(Path, CertName),
%%         todo enable when gr_channel will be present in new oneprovider
%%         Nodes = onepanel_utils:apply_on_worker(gen_server, call, [request_dispatcher, {get_workers, gr_channel}]),
%%         rpc:multicall(Nodes, gr_channel, disconnect, []),
        ok = dao:delete_record(?PROVIDER_TABLE, ProviderId)
    catch
        _:Reason ->
            ?error("Cannot unregister from Global Registry: ~p", [Reason]),
            {error, Reason}
    end.


%% get_default_ports/0
%% ====================================================================
%% @doc Returns default oneprovider ports that will be checked by Global Registry
%% @end
-spec get_default_ports() -> Result when
    Result :: {ok, Ports :: [{Type :: binary(), Port :: integer()}]} | {error, Reason :: term()}.
%% ====================================================================
get_default_ports() ->
    try
        {ok, GuiPort} = onepanel_utils:apply_on_worker(application, get_env, [?SOFTWARE_NAME, gui_https_port]),
        {ok, RestPort} = onepanel_utils:apply_on_worker(application, get_env, [?SOFTWARE_NAME, rest_port]),
        {ok, [{<<"gui">>, GuiPort}, {<<"rest">>, RestPort}]}
    catch
        _:Reason ->
            ?error("Cannot get ports to check: ~p", [Reason]),
            {error, Reason}
    end.


%% get_provider_id/0
%% ====================================================================
%% @doc Returns provider ID.
%% @end
-spec get_provider_id() -> Result when
    Result :: undefined | binary().
%% ====================================================================
get_provider_id() ->
    case dao:get_records(?PROVIDER_TABLE) of
        {ok, [#?PROVIDER_RECORD{id = ProviderId} | _]} ->
            ProviderId;
        _ ->
            undefined
    end.


%% get_provider_name/0
%% ====================================================================
%% @doc Returns provider name if registered or 'onepanel' otherwise.
%% @end
-spec get_provider_name() -> Result when
    Result :: binary().
%% ====================================================================
get_provider_name() ->
    case dao:get_records(?PROVIDER_TABLE) of
        {ok, [#?PROVIDER_RECORD{name = ProviderName} | _]} ->
            http_utils:html_encode(ProviderName);
        _ ->
            <<"onepanel">>
    end.
