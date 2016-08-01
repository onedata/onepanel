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

-include("http/rest.hrl").
-include("modules/logger.hrl").

-export([start/0, stop/0, get_status/0]).

-define(LISTENER, rest_listener).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns REST listener port.
%% @end
%%--------------------------------------------------------------------
-spec port() -> Port :: integer().
port() ->
    onepanel_env:get(rest_port).


%%--------------------------------------------------------------------
%% @doc Starts REST listener.
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

    CommonRoutes = onepanel_api:routes(),
    SpecificRoutes = case onepanel_env:get(release) of
        oneprovider -> oneprovider_api:routes();
        onezone -> onezone_api:routes()
    end,
    Routes = merge_routes(CommonRoutes ++ SpecificRoutes),

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
%% @doc Stops REST listener.
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
%% @doc Checks whether REST listener is working.
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
%% @private @doc Returns list of weak ciphers.
%%--------------------------------------------------------------------
-spec weak_ciphers() -> list().
weak_ciphers() ->
    [{dhe_rsa, des_cbc, sha}, {rsa, des_cbc, sha}].


%%--------------------------------------------------------------------
%% @private @doc Converts routes generated by swagger to format expected
%% by cowboy router.
%% @end
%%--------------------------------------------------------------------
-spec merge_routes(Routes) -> Routes when
    Routes :: [{Path :: binary(), Module :: module(), State :: rest_handler:state()}].
merge_routes(Routes) ->
    lists:foldl(fun({Path, Handler, #rstate{methods = [Method]}} = Route, Acc) ->
        case lists:keyfind(Path, 1, Acc) of
            {Path, Handler, #rstate{methods = Methods} = State} ->
                lists:keyreplace(Path, 1, Acc, {Path, Handler,
                    State#rstate{methods = [Method | Methods]}});
            false ->
                [Route | Acc]
        end
    end, [], Routes).
