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
-module(https_listener).
-author("Krzysztof Trzepla").

-behaviour(listener_behaviour).

-include("http/rest.hrl").
-include("names.hrl").
-include("http/gui_paths.hrl").
-include_lib("gui/include/gui.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PORT, application:get_env(onepanel, rest_port, 443)).
-define(ACCEPTORS_NUM, application:get_env(onepanel, rest_https_acceptors, 100)).
-define(REQUEST_TIMEOUT, application:get_env(onepanel, rest_https_request_timeout, timer:minutes(5))).
-define(INACTIVITY_TIMEOUT, application:get_env(onepanel, rest_https_inactivity_timeout, timer:minutes(10))).
-define(GUI_PACKAGE_PATH, onepanel_env:get(gui_package_path)).

-export([port/0, start/0, stop/0, healthcheck/0]).
-export([get_cert_chain_pems/0, get_prefix/0]).
-export([gui_package_path/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns REST listener port.
%% @end
%%--------------------------------------------------------------------
-spec port() -> Port :: integer().
port() ->
    ?PORT.


%%--------------------------------------------------------------------
%% @doc Starts REST listener.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | no_return().
start() ->
    deploy_standalone_gui_files(),
    maybe_generate_web_cert(),
    maybe_trust_test_ca(),
    KeyFile = onepanel_env:get(web_key_file),
    CertFile = onepanel_env:get(web_cert_file),
    ChainFile = onepanel_env:get(web_cert_chain_file),
    GuiStaticRoot = onepanel_env:get(gui_static_root),

    CommonRoutes = cluster_rest_routes:routes()
        ++ security_rest_routes:routes()
        ++ dns_and_web_certificates_rest_routes:routes()
        ++ current_user_rest_routes:routes()
        ++ internal_rest_routes:routes(),
    SpecificRoutes = case onepanel_env:get_cluster_type() of
        oneprovider ->
            oneprovider_cluster_rest_routes:routes()
            ++ registration_and_identity_rest_routes:routes()
            ++ storages_rest_routes:routes()
            ++ space_support_rest_routes:routes()
            ++ luma_db_rest_routes:routes()
            ++ luma_db_local_feed_rest_routes:routes()
            ++ file_popularity_and_auto_cleaning_rest_routes:routes()
            ++ ceph_rest_routes:routes()
            ++ debug_rest_routes:routes();
        onezone ->
            onezone_cluster_rest_routes:routes()
                 ++ service_configuration_rest_routes:routes()
                 ++ user_management_rest_routes:routes()
    end,
    Routes = merge_routes(CommonRoutes ++ SpecificRoutes),
    DynamicPages = [
        {?CONFIGURATION_PATH, [<<"GET">>], page_panel_configuration},
        {?LOGIN_PATH, [<<"POST">>], page_basic_auth_login},
        {?LOGOUT_PATH, [<<"POST">>], page_logout},
        {?GUI_CONTEXT_PATH, [<<"GET">>], page_gui_context},
        {?GUI_PREAUTHORIZE_PATH, [<<"POST">>], page_gui_preauthorize}
    ],

    ok = gui:start(#gui_config{
        port = port(),
        key_file = KeyFile,
        cert_file = CertFile,
        chain_file = ChainFile,
        number_of_acceptors = ?ACCEPTORS_NUM,
        request_timeout = ?REQUEST_TIMEOUT,
        inactivity_timeout = ?INACTIVITY_TIMEOUT,
        custom_cowboy_routes = Routes,
        dynamic_pages = DynamicPages,
        static_root = GuiStaticRoot,
        custom_response_headers = fun common_response_headers/1
    }),

    ?info("REST listener successfully started").


%%--------------------------------------------------------------------
%% @doc Stops REST listener.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    gui:stop().


%%--------------------------------------------------------------------
%% @doc Checks whether REST listener is working.
%% @end
%%--------------------------------------------------------------------
-spec healthcheck() -> ok | {error, server_not_responding}.
healthcheck() ->
    gui:healthcheck().


%%--------------------------------------------------------------------
%% @doc
%% Returns intermediate CA chain for the web cert used in gui listener.
%% @end
%%--------------------------------------------------------------------
-spec get_cert_chain_pems() -> [public_key:der_encoded()].
get_cert_chain_pems() ->
    gui:get_cert_chain_pems().


%%--------------------------------------------------------------------
%% @doc Returns REST listener prefix.
%% @end
%%--------------------------------------------------------------------
-spec get_prefix() -> Prefix :: binary().
get_prefix() ->
    onepanel_utils:convert(onepanel_env:get(rest_api_prefix), binary).


%%--------------------------------------------------------------------
%% @doc
%% Returns intermediate CA chain for the web cert used in gui listener.
%% @end
%%--------------------------------------------------------------------
-spec gui_package_path() -> string().
gui_package_path() ->
    ?GUI_PACKAGE_PATH.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Converts routes generated by swagger to format expected
%% by cowboy router.
%% @end
%%--------------------------------------------------------------------
-spec merge_routes([{Path, #rest_req{}}]) -> Routes when
    Routes :: [{Path, module(), #{rest_handler:method() => #rest_req{}}}],
    Path :: binary().
merge_routes(AllRoutes) ->
    % Aggregate routes that share the same path
    AggregatedRoutes = lists:foldl(
        fun({Path, #rest_req{method = Method} = RestReq}, RoutesAcc) ->
            RoutesForPath = proplists:get_value(Path, RoutesAcc, #{}),
            lists:keystore(
                Path, 1, RoutesAcc,
                {Path, RoutesForPath#{Method => RestReq}}
            )
        end, [], AllRoutes),
    % Convert all routes to cowboy-compliant routes
    % - prepend REST prefix to every route
    % - rest handler module must be added as second element to the tuples
    % - the result will serve as an argument to rest_handler:init.
    Prefix = str_utils:to_binary(onepanel_env:get(rest_api_prefix)),
    lists:map(fun({Path, RoutesForPath}) ->
        {<<Prefix/binary, Path/binary>>, ?REST_HANDLER_MODULE, RoutesForPath}
    end, AggregatedRoutes).


%%--------------------------------------------------------------------
%% @private @doc Returns headers which should be added to each response.
%% @end
%%--------------------------------------------------------------------
-spec common_response_headers(cowboy_req:req()) -> cowboy:http_headers().
common_response_headers(_Req) ->
    case rest_handler:allowed_origin() of
        undefined -> #{};
        Origin -> #{<<"access-control-allow-origin">> => Origin}
    end.


%%--------------------------------------------------------------------
%% @private @doc
%% Deploys standalone GUI - the GUI served by onepanel and reachable on port 9443.
%% Static GUI files are taken from GUI package tarball.
%% @end
%%--------------------------------------------------------------------
-spec deploy_standalone_gui_files() -> ok.
deploy_standalone_gui_files() ->
    utils:run_with_tempdir(fun(TempDir) ->
        GuiRoot = onepanel_env:get(gui_static_root),
        {ok, ExtractedPath} = gui:extract_package(gui_package_path(), TempDir),

        file_utils:recursive_del(GuiRoot),
        ok = file_utils:move(ExtractedPath, GuiRoot),
        ?info("Deployed standalone GUI files in ~s", [GuiRoot])
    end),
    ok.


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
            onepanel_cert:backup_exisiting_certs(),
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
            TargetCaFile = filename:join(oz_plugin:get_cacerts_dir(), CaFile),
            file:copy(CAPath, TargetCaFile),
            ?warning("Added '~s' to trusted certificates. Use only for test purposes.", [
                CaFile
            ])
    end.
