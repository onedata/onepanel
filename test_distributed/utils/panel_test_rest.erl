%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for REST tests.
%%% @end
%%%--------------------------------------------------------------------
-module(panel_test_rest).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("names.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/test_utils.hrl").


%% API
-export([
    get/3,
    patch/3,

    request/2
]).

-type request_auth() ::
    root |
    {basic, Username :: binary(), Password :: binary()} |
    {token, Token :: binary()} |
    none.

-type request_args() :: #{
    method => http_client:method(),
    path => binary(),
    proxy => boolean(),

    auth => request_auth(),
    headers => http_client:headers(),

    json => json_utils:json_term(),
    body => http_client:request_body()
}.

-type response() ::
    {ok, http_client:code(), http_client:headers(), binary() | json_utils:json_term()} |
    {error, Reason :: term()}.


-export_type([request_auth/0, request_args/0, response/0]).


%%%===================================================================
%%% API
%%%===================================================================


-spec get(oct_background:node_selector(), binary(), request_args()) -> response().
get(PanelNodeSelector, Path, RequestArgs) ->
    request(PanelNodeSelector, RequestArgs#{method => get, path => Path}).


-spec patch(oct_background:node_selector(), binary(), request_args()) -> response().
patch(PanelNodeSelector, Path, RequestArgs) ->
    request(PanelNodeSelector, RequestArgs#{method => patch, path => Path}).


-spec request(oct_background:node_selector(), request_args()) -> response().
request(PanelNodeSelector, RequestArgs) ->
    PanelNode = select_node(PanelNodeSelector),

    Method = maps:get(method, RequestArgs),
    Url = build_url(PanelNode, RequestArgs),
    Headers = build_headers(RequestArgs),
    Body = build_body(RequestArgs),

    CaCerts = panel_test_rpc:get_cert_chain_ders(PanelNode),
    Opts = [{ssl_options, [{cacerts, CaCerts}]}, {recv_timeout, 60000}],

    case http_client:request(Method, Url, Headers, Body, Opts) of
        {ok, RespCode, RespHeaders, RespBody} ->
            case maps:get(?HDR_CONTENT_TYPE, RespHeaders, undefined) of
                <<"application/json">> ->
                    {ok, RespCode, RespHeaders, json_utils:decode(RespBody)};
                _ ->
                    {ok, RespCode, RespHeaders, RespBody}
            end;
        {error, _} = Error ->
            Error
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec select_node(oct_background:node_selector()) -> node().
select_node(NodeSelector) ->
    case is_known_node(NodeSelector) of
        true -> NodeSelector;
        false -> ?RAND_ELEMENT(oct_background:get_service_panels(NodeSelector))
    end.


%% @private
-spec is_known_node(oct_background:node_selector()) -> boolean().
is_known_node(NodeSelector) ->
    lists:member(NodeSelector, nodes(known)).


%% @private
-spec build_url(node(), request_args()) -> binary().
build_url(Node, RequestArgs) ->
    {ok, Domain} = test_utils:get_env(Node, ?APP_NAME, test_web_cert_domain),
    {ok, RestPrefix} = test_utils:get_env(Node, ?APP_NAME, rest_api_prefix),
    Path = maps:get(path, RequestArgs),

    PortBin = case maps:get(proxy, RequestArgs, false) of
        true ->
            <<>>;
        false ->
            Port = panel_test_rpc:call(Node, https_listener, port, []),
            str_utils:format_bin(":~B", [Port])
    end,

    str_utils:format_bin("https://~ts~ts~ts~ts", [Domain, PortBin, RestPrefix, Path]).


%% @private
-spec build_headers(request_args()) -> http_client:headers().
build_headers(RequestArgs) ->
    BasicHeaders = maps:get(headers, RequestArgs, #{}),

    Headers = case maps:is_key(json, RequestArgs) of
        true -> BasicHeaders#{?HDR_CONTENT_TYPE => <<"application/json">>};
        false -> BasicHeaders
    end,

    add_auth_header(Headers, maps:get(auth, RequestArgs, none)).


%% @private
-spec add_auth_header(http_client:headers(), request_auth()) ->
    http_client:headers().
add_auth_header(Headers, none) ->
    Headers;

add_auth_header(Headers, root) ->
    Username = ?LOCAL_USERNAME,
    Password = ?ONENV_EMERGENCY_PASSPHRASE,
    add_auth_header(Headers, {basic, Username, Password});

add_auth_header(Headers, {basic, Username, Password}) ->
    Hash = base64:encode(<<Username/binary, ":", Password/binary>>),
    Headers#{?HDR_AUTHORIZATION => <<"Basic ", Hash/binary>>};

add_auth_header(Headers, {token, AuthToken}) ->
    Headers#{?HDR_X_AUTH_TOKEN => AuthToken}.


%% @private
-spec build_body(request_args()) -> http_client:headers().
build_body(RequestArgs) ->
    case maps:get(json, RequestArgs, undefined) of
        undefined ->
            maps:get(body, RequestArgs, <<>>);
        JsonTerm ->
            json_utils:encode(JsonTerm)
    end.
