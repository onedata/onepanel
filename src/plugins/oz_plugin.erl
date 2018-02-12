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
-export([get_cacerts_dir/0, get_provider_cacerts_dir/0]).
-export([auth_to_rest_client/1]).

%%%===================================================================
%%% OZ behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:get_oz_url/0}
%% @end
%%--------------------------------------------------------------------
-spec get_oz_url() -> string().
get_oz_url() ->
    "https://" ++ get_env(oz_domain, list).


%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:get_oz_rest_port/0}
%% @end
%%--------------------------------------------------------------------
-spec get_oz_rest_port() -> integer().
get_oz_rest_port() ->
    get_env(oz_rest_port, integer).


%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:get_oz_rest_api_prefix/0}
%% @end
%%--------------------------------------------------------------------
-spec get_oz_rest_api_prefix() -> string().
get_oz_rest_api_prefix() ->
    get_env(oz_rest_api_prefix, list).


%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:get_cacerts_dir/0}
%% @end
%%--------------------------------------------------------------------
-spec get_cacerts_dir() -> file:name_all().
get_cacerts_dir() ->
    onepanel_env:get(cacerts_dir).


%%--------------------------------------------------------------------
%% @doc
%% Returns the path to cacerts dir for underlying oneprovider instance.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_cacerts_dir() -> file:name_all().
get_provider_cacerts_dir() ->
    get_env(cacerts_dir, path).


%%--------------------------------------------------------------------
%% @doc {@link oz_plugin_behaviour:auth_to_rest_client/1}
%% @end
%%--------------------------------------------------------------------
-spec auth_to_rest_client(Auth :: term()) -> {user, token, binary()} |
{user, macaroon, {Macaroon :: binary(), DischargeMacaroons :: [binary()]}} |
{user, basic, binary()} | {provider, Macaroon :: binary()} | none.
auth_to_rest_client(none) ->
    none;
auth_to_rest_client(provider) ->
    [Node | _] = service_op_worker:get_nodes(),
    {ok, ProviderMacaroon} = rpc:call(Node, provider_auth, get_auth_macaroon, []),
    {provider, ProviderMacaroon}.

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
get_env(Key, path) ->
    Path = get_env(Key, list),
    case filename:absname(Path) of
        Path ->
            Path;
        _ ->
            [Node | _] = service_op_worker:get_nodes(),
            {ok, Cwd} = rpc:call(Node, file, get_cwd, []),
            filename:join(Cwd, Path)
    end;
get_env(Key, Type) ->
    Hosts = service_op_worker:get_hosts(),
    Nodes = onepanel_cluster:hosts_to_nodes(Hosts),
    Name = service_op_worker:name(),
    Path = onepanel_env:get(op_worker_app_config_file),
    {ok, Value} = onepanel_rpc:call_any(Nodes, onepanel_env, read,
        [[Name, Key], Path]),
    onepanel_utils:convert(Value, Type).