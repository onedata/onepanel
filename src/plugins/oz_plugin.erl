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

-include("names.hrl").
-include_lib("ctool/include/onedata.hrl").

-behaviour(oz_plugin_behaviour).

-type auth() :: none | op_panel | provider | {token, tokens:serialized()}.
-export_type([auth/0]).

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
    Domain = service_oneprovider:get_oz_domain(),
    "https://" ++ onepanel_utils:convert(Domain, list).


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
    % called by https_listener before op_worker is available
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
-spec auth_to_rest_client(Auth :: auth()) ->
    {headers, #{Header :: binary() => Value :: binary()}} |
    {provider, tokens:serialized()} |
    none.
auth_to_rest_client(none) ->
    none;

auth_to_rest_client({token, Token}) ->
    OneproviderIdentityToken = service_oneprovider:get_identity_token(),
    OpPanelIdentityToken = tokens:add_oneprovider_service_indication(
        ?OP_PANEL, OneproviderIdentityToken
    ),
    {headers, maps:merge(
        tokens:access_token_header(Token),
        tokens:service_token_header(OpPanelIdentityToken)
    )};

auth_to_rest_client(op_panel) ->
    OneproviderAccessToken = service_oneprovider:get_access_token(),
    OpPanelAccessToken = tokens:add_oneprovider_service_indication(
        ?OP_PANEL, OneproviderAccessToken
    ),
    {headers, tokens:access_token_header(OpPanelAccessToken)};

auth_to_rest_client(provider) ->
    ProviderAccessToken = service_oneprovider:get_access_token(),
    {provider, ProviderAccessToken}.


%%%===================================================================
%%% Internal function
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Returns value from op_worker application configuration file and
%% converts it according to provided type.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Key :: onepanel_env:key(), Type :: onepanel_utils:type() | path) ->
    Value :: term().
get_env(Key, path) ->
    Path = get_env(Key, list),
    service_utils:absolute_path(?SERVICE_OPW, Path);

get_env(Key, Type) ->
    Name = ?SERVICE_OPW,
    Nodes = nodes:all(#{service => ?SERVICE_PANEL, hosts => hosts:all(Name)}),
    {ok, Value} = onepanel_rpc:call_any(Nodes, onepanel_env, read_effective,
        [[Name, Key], Name]),
    onepanel_utils:convert(Value, Type).