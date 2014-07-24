%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains utility functions for registration
%% in Global Registry (gr for short).
%% @end
%% ===================================================================
-module(gr_utils).

-include("registered_names.hrl").
-include("onepanel_modules/user_logic.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([send_req/2, send_req/3, send_req/4]).
-export([get_ports_to_check/0, get_control_panel_hosts/0, get_provider_id/0]).

%% ====================================================================
%% API functions
%% ====================================================================


%% send_req/2
%% ====================================================================
%% @doc Sends given request to Global Registry with default options
%% and empty body using REST API.
-spec send_req(Uri :: string(), Method :: atom()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
send_req(Uri, Method) ->
    send_req(Uri, Method, []).


%% send_req/3
%% ====================================================================
%% @doc Sends given request to Global Registry with default options
%% using REST API.
-spec send_req(Uri :: string(), Method :: atom(), Body :: binary()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
send_req(Uri, Method, Body) ->
    send_req(Uri, Method, Body, []).


%% send_req/4
%% ====================================================================
%% @doc Sends given request to Global Registry using REST API.
-spec send_req(Uri :: string(), Method :: atom(), Body :: binary(), Options :: list()) -> Result when
    Result :: {ok, Status :: string(), ResponseHeaders :: binary(), ResponseBody :: binary()} | {error, Reason :: term()}.
%% ====================================================================
send_req(Uri, Method, Body, Options) ->
    {ok, Url} = application:get_env(?APP_NAME, global_registry_url),
    {ok, KeyFile} = application:get_env(?APP_NAME, grpkey_file),
    {ok, CertFile} = application:get_env(?APP_NAME, grpcert_file),
    {ok, CACertFile} = application:get_env(?APP_NAME, grpcacert_file),
    SSLOptions = {ssl_options, [{cacertfile, CACertFile}, {keyfile, KeyFile}, {certfile, CertFile}]},
    ibrowse:send_req(Url ++ Uri, [{content_type, "application/json"}], Method, Body, [SSLOptions | Options]).


%% get_ports_to_check/0
%% ====================================================================
%% @doc Returns default veilcluster ports that will be checked by Global Registry
-spec get_ports_to_check() -> Result when
    Result :: {ok, Ports :: [{Type :: string(), Port :: integer()}]} | {error, Reason :: term()}.
%% ====================================================================
get_ports_to_check() ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCM}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        Node = list_to_atom("ccm@" ++ MainCCM),
        {ok, GuiPort} = rpc:call(Node, application, get_env, [veil_cluster_node, control_panel_port]),
        {ok, RestPort} = rpc:call(Node, application, get_env, [veil_cluster_node, rest_port]),
        {ok, [{"gui", GuiPort}, {"rest", RestPort}]}
    catch
        _:Reason ->
            ?error("Cannot get ports to check: ~p", [Reason]),
            {error, Reason}
    end.


%% get_control_panel_hosts/0
%% ====================================================================
%% @doc Returns list of control panel hosts
-spec get_control_panel_hosts() -> Result when
    Result :: {ok, Hosts :: [string()]} | {error, Reason :: term()}.
%% ====================================================================
get_control_panel_hosts() ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCM}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        Node = list_to_atom("ccm@" ++ MainCCM),
        {Workers, _} = rpc:call(Node, gen_server, call, [{global, central_cluster_manager}, get_workers, 1000]),
        ControlPanelHosts = lists:foldl(fun
            ({WorkerNode, control_panel}, Acc) -> [onepanel_utils:get_host(WorkerNode) | Acc];
            (_, Acc) -> Acc
        end, [], Workers),
        {ok, ControlPanelHosts}
    catch
        _:Reason ->
            ?error("Cannot get control panel hosts: ~p", [Reason]),
            {error, Reason}
    end.


%% get_provider_id/0
%% ====================================================================
%% @doc Returns provider ID.
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

