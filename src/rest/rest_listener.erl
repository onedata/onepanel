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
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([get_port/0, get_prefix/1]).
-export([start/0, stop/0, status/0]).
-export([get_cert_chain/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns REST listener port.
%% @end
%%--------------------------------------------------------------------
-spec get_port() -> Port :: integer().
get_port() ->
    onepanel_env:get(rest_port).


%%--------------------------------------------------------------------
%% @doc Returns REST listener prefix.
%% @end
%%--------------------------------------------------------------------
-spec get_prefix(ApiVersion :: rest_handler:version()) -> Prefix :: binary().
get_prefix(ApiVersion) ->
    Template = onepanel_env:get(rest_api_prefix_template),
    re:replace(Template, "{version_number}",
        onepanel_utils:convert(ApiVersion, binary), [{return, binary}]).


%%--------------------------------------------------------------------
%% @doc Starts REST listener.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | no_return().
start() ->
    maybe_generate_web_cert(),
    maybe_trust_test_ca(),
    Port = get_port(),
    HttpsAcceptors = onepanel_env:get(rest_https_acceptors),
    KeyFile = onepanel_env:get(web_key_file),
    CertFile = onepanel_env:get(web_cert_file),
    ChainFile = onepanel_env:get(web_cert_chain_file),

    CommonRoutes = onepanel_api:routes(),
    SpecificRoutes = case onepanel_env:get(release_type) of
        oneprovider -> oneprovider_api:routes();
        onezone -> onezone_api:routes()
    end,
    Routes = merge_routes(CommonRoutes ++ SpecificRoutes) ++ static_gui_routes(),

    Dispatch = cowboy_router:compile([{'_', Routes}]),

    SslOpts = [
        {port, Port},
        {num_acceptors, HttpsAcceptors},
        {keyfile, KeyFile},
        {certfile, CertFile},
        {ciphers, ssl_utils:safe_ciphers()},
        {connection_type, supervisor},
        {next_protocols_advertised, [<<"http/1.1">>]},
        {alpn_preferred_protocols, [<<"http/1.1">>]}
    ],

    SslOptsWithChain = case filelib:is_regular(ChainFile) of
        true -> [{cacertfile, ChainFile} | SslOpts];
        _ -> SslOpts
    end,

    {ok, _} = ranch:start_listener(?MODULE, ranch_ssl, SslOptsWithChain,
        cowboy_tls, #{
            env => #{dispatch => Dispatch},
            connection_type => supervisor
        }
    ),

    ?info("REST listener successfully started").


%%--------------------------------------------------------------------
%% @doc Stops REST listener.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    case cowboy:stop_listener(?MODULE) of
        ok ->
            ?info("REST listener stopped");
        {error, Reason} ->
            ?error("Cannot stop REST listener due to: ~p", [Reason]),
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc Checks whether REST listener is working.
%% @end
%%--------------------------------------------------------------------
-spec status() -> ok | {error, Reason :: term()}.
status() ->
    Endpoint = str_utils:format_bin("https://127.0.0.1:~B", [get_port()]),
    Opts = [{ssl_options, [{secure, only_verify_peercert}, {cacerts, get_cert_chain()}]}],
    case http_client:get(Endpoint, #{}, <<>>, Opts) of
        {ok, _, _, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns intermediate CA chain for the web cert used in gui listener.
%% @end
%%--------------------------------------------------------------------
-spec get_cert_chain() -> [public_key:der_encoded()].
get_cert_chain() ->
    ChainFile = onepanel_env:get(web_cert_chain_file),
    case filelib:is_regular(ChainFile) of
        true -> cert_utils:load_ders(ChainFile);
        _ -> []
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

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


%%--------------------------------------------------------------------
%% @private @doc Returns Cowboy router compliant routes for static gui files.
%% @end
%%--------------------------------------------------------------------
-spec static_gui_routes() -> [{Path :: string(), Module :: module(), State :: term()}].
static_gui_routes() ->
    % Resolve static files root. First, check if there is a non-empty dir
    % located in gui_custom_static_root. If not, use default.
    CustomRoot = onepanel_env:get(gui_custom_static_root),
    DefaultRoot = onepanel_env:get(gui_default_static_root),
    StaticFilesRoot = case file:list_dir_all(CustomRoot) of
        {error, enoent} -> DefaultRoot;
        {ok, []} -> DefaultRoot;
        {ok, _} -> CustomRoot
    end,
    [{"/[...]", gui_static_handler, {dir, StaticFilesRoot}}].


%%--------------------------------------------------------------------
%% @private @doc
%% Generates a new test web server cert, given that this option is enabled in
%% env config. The generated cert should be used only for test purposes.
%% NOTE: for multi-node onepanel cluster, each node will generate its own cert
%% (for the same domain) - this is not a problem since these are test
%% certificates.
%% @end
%%--------------------------------------------------------------------
-spec maybe_generate_web_cert() -> ok.
maybe_generate_web_cert() ->
    WebKeyPath = onepanel_env:get(web_key_file),
    WebCertPath = onepanel_env:get(web_cert_file),
    WebChainPath = onepanel_env:get(web_cert_chain_file),

    case onepanel_env:get(generate_test_web_cert) of
        false ->
            ok;
        true ->
            % Back up any pre-existing certs
            onepanel_ssl:backup_exisiting_certs(),
            % Both key and cert are expected in the same file
            CAPath = onepanel_env:get(test_web_cert_ca_path),
            Domain = onepanel_env:get(test_web_cert_domain),
            cert_utils:create_signed_webcert(
                WebKeyPath, WebCertPath, Domain, CAPath, CAPath
            ),
            file:copy(CAPath, WebChainPath),
            ?warning(
                "Generated a new cert for domain '~s'. "
                "Use only for test purposes.~n"
                "    ~s~n"
                "    ~s~n"
                "    ~s",
                [Domain, WebKeyPath, WebCertPath, WebChainPath]
            ),
            % Do not generate new certificates upon listener restart
            onepanel_env:set(generate_test_web_cert, false),
            onepanel_env:write([?APP_NAME, generate_test_web_cert], false),
            ok
    end.


%%--------------------------------------------------------------------
%% @private @doc
%% Adds Onedata test CA to trusted certificates, given that this option is
%% enabled in env config.
%% @end
%%--------------------------------------------------------------------
-spec maybe_trust_test_ca() -> ok.
maybe_trust_test_ca() ->
    case onepanel_env:get(treat_test_ca_as_trusted) of
        false ->
            ok;
        true ->
            CAPath = onepanel_env:get(test_web_cert_ca_path),
            CaFile = filename:basename(CAPath),
            file:copy(CAPath, filename:join(oz_plugin:get_cacerts_dir(), CaFile)),
            ?warning("Added '~s' to trusted certificates. Use only for test purposes.", [
                CaFile
            ])
    end.
