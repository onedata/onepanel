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

-include_lib("ctool/include/logging.hrl").

-export([start/0, stop/0, health_check/0]).

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
    onepanel:get_env(rest_port).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | no_return().
start() ->
    Port = port(),
    Prefix = erlang:list_to_binary(onepanel:get_env(rest_prefix)),
    HttpsAcceptors = onepanel:get_env(rest_https_acceptors),
    KeyPath = onepanel:get_env(rest_key_path),
    CertPath = onepanel:get_env(rest_cert_path),
    CaCertPath = onepanel:get_env(rest_cacert_path),

    {ok, CaCertPem} = file:read_file(CaCertPath),
    [{_, CaCertDer, _} | _] = public_key:pem_decode(CaCertPem),

    Routes = lists:append([
        rest_onedata_user:routes()
    ]),

    RoutesWithPrefix = lists:map(fun({Path, Module, State}) ->
        {<<Prefix/binary, Path/binary>>, Module, State}
    end, Routes),

    Dispatch = cowboy_router:compile([
        {'_', RoutesWithPrefix}
    ]),

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

    ?info("REST listener successfully started").


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    case cowboy:stop_listener(?LISTENER) of
        ok ->
            ?info("REST listener stopped");
        {error, Reason} ->
            ?error("Cannot stop REST listener due to: ~p", [Reason]),
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec health_check() -> ok | {error, server_not_responding}.
health_check() ->
    Endpoint = "https://127.0.0.1:" ++ integer_to_list(port()),
    case http_client:get(Endpoint, [], <<>>, [insecure]) of
        {ok, _, _, _} -> ok;
        _ -> {error, server_not_responding}
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