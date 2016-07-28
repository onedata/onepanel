%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements oz_plugin_behaviour in order
%%% to adjust connection settings to onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_plugin).
-author("Krzysztof Trzepla").

-behaviour(oz_plugin_behaviour).

%% OZ behaviour callbacks
-export([get_oz_url/0, get_oz_rest_port/0, get_oz_rest_api_prefix/0]).
-export([get_key_path/0, get_csr_path/0, get_cert_path/0, get_cacert_path/0]).
-export([auth_to_rest_client/1]).

%%%===================================================================
%%% OZ behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:get_oz_url/0}
%%--------------------------------------------------------------------
-spec get_oz_url() -> string().
get_oz_url() ->
    "https://" ++ get_env(oz_domain, list).


%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:get_oz_rest_port/0}
%%--------------------------------------------------------------------
-spec get_oz_rest_port() -> integer().
get_oz_rest_port() ->
    get_env(oz_rest_port, integer).


%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:get_oz_rest_api_prefix/0}
%%--------------------------------------------------------------------
-spec get_oz_rest_api_prefix() -> string().
get_oz_rest_api_prefix() ->
    get_env(oz_rest_api_prefix, list).


%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:get_key_path/0}
%%--------------------------------------------------------------------
-spec get_key_path() -> file:name_all().
get_key_path() ->
    get_env(oz_provider_key_path, list).


%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:get_csr_path/0}
%%--------------------------------------------------------------------
-spec get_csr_path() -> file:name_all().
get_csr_path() ->
    get_env(oz_provider_csr_path, list).


%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:get_cert_path/0}
%%--------------------------------------------------------------------
-spec get_cert_path() -> file:name_all().
get_cert_path() ->
    get_env(oz_provider_cert_path, list).


%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:get_oz_url/0}
%%--------------------------------------------------------------------
-spec get_cacert_path() -> file:name_all().
get_cacert_path() ->
    get_env(oz_ca_cert_path, list).


%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:auth_to_rest_client/1}
%%--------------------------------------------------------------------
-spec auth_to_rest_client(Auth :: term()) -> file:name_all().
auth_to_rest_client(Auth) ->
    Auth.

%%%===================================================================
%%% Internal function
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Returns value from op_worker application configuration file and
%% converts it according to provided type.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Key :: onepanel_env:key(), Type :: onepanel_utils:type()) ->
    Value :: term().
get_env(Key, Type) ->
    Hosts = service_op_worker:get_hosts(),
    Nodes = onepanel_cluster:hosts_to_nodes(Hosts),
    Name = service_op_worker:name(),
    Path = onepanel_env:get(op_worker_app_config_path),
    {ok, Value} = onepanel_rpc:call_any(Nodes, onepanel_env, read, 
        [[Name, Key], Path]),
    onepanel_utils:convert(Value, Type).