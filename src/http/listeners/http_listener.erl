%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module is responsible for starting and stopping listener that:
%%% - handles lets encrypt
%%% - redirects client from HTTP (port 80) to HTTPS
%%% - proxies oai requests to oz worker
%%% @end
%%%--------------------------------------------------------------------
-module(http_listener).
-author("Bartosz Walkowicz").

-behaviour(listener_behaviour).

-include("http/gui_paths.hrl").
-include_lib("ctool/include/logging.hrl").

% Cowboy listener reference
-define(HTTP_LISTENER, http_listener).

% Listener config
-define(PORT, 80).  % LE requires port number 80 to work so no other can be used
-define(ACCEPTORS_NUM, application:get_env(onepanel, rest_http_acceptors, 10)).
-define(REQUEST_TIMEOUT, application:get_env(onepanel, rest_http_request_timeout, timer:seconds(30))).

-define(WORKER_HTTPS_PORT, application:get_env(onepanel, worker_https_server_port, 443)).

-define(LE_CHALLENGE_PATH, application:get_env(
    onepanel,
    letsencrypt_challenge_api_prefix,
    "/.well-known/acme-challenge"
)).
-define(LE_CHALLENGE_ROOT, application:get_env(
    onepanel,
    letsencrypt_challenge_static_root,
    "/tmp/onepanel/http/.well-known/acme-challenge/"
)).

-define(OZ_WORKER_CONNECT_OPTS, fun() -> [
    {recv_timeout, timer:seconds(application:get_env(onepanel, oai_pmh_proxy_recv_timeout_sec, 30))},
    {ssl_options, [
        {secure, only_verify_peercert},
        {cacerts, https_listener:get_cert_chain_ders()}
    ]}
] end).
-define(OAI_PMH_PATH,  application:get_env(onepanel, oai_pmh_api_prefix, "/oai_pmh")).

%% listener_behaviour callbacks
-export([port/0, start/0, stop/0, reload_web_certs/0, healthcheck/0]).
-export([set_response_to_letsencrypt_challenge/2]).

%%%===================================================================
%%% listener_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback port/0.
%% @end
%%--------------------------------------------------------------------
-spec port() -> integer().
port() ->
    ?PORT.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback start/0.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, Reason :: term()}.
start() ->
    ?info("Starting '~tp' server...", [?HTTP_LISTENER]),

    BasicRoutes = [
        {?LE_CHALLENGE_PATH ++ "/[...]", cowboy_static, {dir, ?LE_CHALLENGE_ROOT}},
        {'_', redirector_handler, ?WORKER_HTTPS_PORT}
    ],
    Routes = case onepanel:is_oz_panel() of
        true ->
            [
                {?OAI_PMH_PATH ++ "/[...]", http_port_forwarder, [<<"https">>, ?WORKER_HTTPS_PORT, ?OZ_WORKER_CONNECT_OPTS]}
                | BasicRoutes
            ];
        false ->
            BasicRoutes
    end,
    Dispatch = cowboy_router:compile([{'_', Routes}]),

    Result = cowboy:start_clear(?HTTP_LISTENER,
        #{
            num_acceptors => ?ACCEPTORS_NUM,
            socket_opts => [
                {ip, any},
                {port, port()}
            ]
        },
        #{
            env => #{dispatch => Dispatch},
            max_keepalive => 1,
            request_timeout => ?REQUEST_TIMEOUT
        }
    ),
    case Result of
        {ok, _} ->
            ?info("Server '~tp' started successfully", [?HTTP_LISTENER]);
        _ ->
            ?error("Could not start server '~tp' - ~tp", [?HTTP_LISTENER, Result]),
            Result
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback stop/0.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    ?info("Stopping '~tp' server...", [?HTTP_LISTENER]),

    case cowboy:stop_listener(?HTTP_LISTENER) of
        ok ->
            ?info("Server '~tp' stopped", [?HTTP_LISTENER]);
        {error, Error} ->
            ?error("Error on stopping server ~tp: ~tp", [?HTTP_LISTENER, Error]),
            {error, redirector_stop_error}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback reload_web_certs/0.
%% @end
%%--------------------------------------------------------------------
-spec reload_web_certs() -> ok.
reload_web_certs() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_behaviour} callback healthcheck/0.
%% @end
%%--------------------------------------------------------------------
-spec healthcheck() -> ok | {error, server_not_responding}.
healthcheck() ->
    Endpoint = str_utils:format_bin("http://127.0.0.1:~B", [port()]),
    case http_client:get(Endpoint) of
        {ok, _, _, _} -> ok;
        _ -> {error, server_not_responding}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Writes a file served via HTTP in a directory expected by
%% Let's Encrypt HTTP authorization challenge.
%% @end
%%--------------------------------------------------------------------
-spec set_response_to_letsencrypt_challenge(Name :: file:name_all(), Content :: binary()) ->
    ok | {error, Reason}
    when Reason :: file:posix() | badarg | terminated | system_limit.
set_response_to_letsencrypt_challenge(Name, Content) ->
    Path = filename:join(?LE_CHALLENGE_ROOT, Name),
    case filelib:ensure_dir(Path) of
        ok -> file:write_file(Path, Content);
        {error, Reason} -> {error, Reason}
    end.
