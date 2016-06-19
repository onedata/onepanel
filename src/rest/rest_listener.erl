%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module is responsible for REST listener starting and stopping.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_listener).
-author("Krzysztof Trzepla").

-include("modules/logger.hrl").

-export([start/0, stop/0, get_status/0]).

-define(LISTENER, rest_listener).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec port() -> Port :: integer().
port() ->
    onepanel_env:get(rest_port).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | no_return().
start() ->
    Port = port(),
    HttpsAcceptors = onepanel_env:get(rest_https_acceptors),
    KeyPath = onepanel_env:get(rest_key_path),
    CertPath = onepanel_env:get(rest_cert_path),
    CaCertPath = onepanel_env:get(rest_cacert_path),

    {ok, CaCertPem} = file:read_file(CaCertPath),
    [{_, CaCertDer, _} | _] = public_key:pem_decode(CaCertPem),

    Routes = case onepanel_env:get(release) of
        oneprovider -> rest_api_oneprovider:routes();
        onezone -> rest_api_onezone:routes()
    end,

    Dispatch = cowboy_router:compile([{'_', Routes}]),

    {ok, _} = cowboy:start_https(?LISTENER, HttpsAcceptors,
        [
            {port, Port},
            {keyfile, KeyPath},
            {certfile, CertPath},
            {cacerts, [CaCertDer]},
            {verify, verify_peer},
            {ciphers, ssl:cipher_suites() -- weak_ciphers()},
            {versions, ['tlsv1.2', 'tlsv1.1']}
        ],
        [
            {env, [{dispatch, Dispatch}]}
        ]),

    ?log_info("REST listener successfully started").


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    case cowboy:stop_listener(?LISTENER) of
        ok ->
            ?log_info("REST listener stopped");
        {error, Reason} ->
            ?log_error("Cannot stop REST listener due to: ~p", [Reason]),
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_status() -> ok | {error, Reason :: term()}.
get_status() ->
    Endpoint = "https://127.0.0.1:" ++ integer_to_list(port()),
    case http_client:get(Endpoint, [], <<>>, [insecure]) of
        {ok, _, _, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns list of weak ciphers.
%% @end
-spec weak_ciphers() -> list().
%%--------------------------------------------------------------------
weak_ciphers() ->
    [{dhe_rsa, des_cbc, sha}, {rsa, des_cbc, sha}].