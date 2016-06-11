%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module implements oz_plugin_behaviour in order
%% to customize connection settings to onezone.
%% @end
%% ===================================================================
-module(oz_plugin).
-behaviour(oz_plugin_behaviour).

-include("registered_names.hrl").

%% oz_plugin_behaviour API
-export([get_oz_url/0, get_oz_rest_port/0, get_oz_rest_api_prefix/0,
    get_key_path/0, get_cert_path/0, get_cacert_path/0, get_csr_path/0]).

%% ====================================================================
%% oz_plugin_behaviour API functions
%% ====================================================================

%% get_oz_url/0
%% ====================================================================
%% @doc Should return a onezone URL.
%% @end
-spec get_oz_url() -> string().
%% ====================================================================
get_oz_url() ->
    {ok, Hostname} = application:get_env(?APP_NAME, oz_domain),
    "https://" ++ Hostname.


%%--------------------------------------------------------------------
%% @doc
%% Should return OZ REST port.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_rest_port() -> integer().
get_oz_rest_port() ->
    {ok, Port} = application:get_env(?APP_NAME, oz_rest_port),
    Port.


%%--------------------------------------------------------------------
%% @doc
%% @doc Should return OZ REST API prefix - for example /api/v3/onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_rest_api_prefix() -> string().
get_oz_rest_api_prefix() ->
    {ok, Prefix} = application:get_env(?APP_NAME, oz_rest_api_prefix),
    Prefix.


%% get_key_path/0
%% ====================================================================
%% @doc Should return a path to file containing provider's private key.
%% @end
-spec get_key_path() -> string().
%% ====================================================================
get_key_path() ->
    {ok, KeyFile} = application:get_env(?APP_NAME, oz_key_path),
    KeyFile.


%% get_cert_path/0
%% ====================================================================
%% @doc Should return a path to file containing provider's
%% public certificate signed by Global Registry.
%% @end
-spec get_cert_path() -> string().
%% ====================================================================
get_cert_path() ->
    {ok, CertFile} = application:get_env(?APP_NAME, oz_cert_path),
    CertFile.


%% get_cacert_path/0
%% ====================================================================
%% @doc Should return a path to file containing Global Registry
%% CA certificate.
%% @end
-spec get_cacert_path() -> string().
%% ====================================================================
get_cacert_path() ->
    {ok, CACertFile} = application:get_env(?APP_NAME, oz_cacert_path),
    CACertFile.

%% get_csr_path/0
%% ====================================================================
%% @doc Should return a path to file containing provider's
%% Certificate Signing Request.
%% @end
-spec get_csr_path() -> string().
%% ====================================================================
get_csr_path() ->
    {ok, CSRFile} = application:get_env(?APP_NAME, oz_csr_path),
    CSRFile.