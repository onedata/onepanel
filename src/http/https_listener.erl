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
-export([get_cert_chain_pems/0, get_prefix/1]).
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

    CommonRoutes = onepanel_api:routes(),
    SpecificRoutes = case onepanel_env:get_release_type() of
        oneprovider -> oneprovider_api:routes();
        onezone -> onezone_api:routes()
    end,
    Routes = merge_routes(CommonRoutes ++ SpecificRoutes),
    DynamicPages = [
        {?LOGIN_PATH, [<<"POST">>], page_basic_auth_login},
        {?LOGOUT_PATH, [<<"POST">>], page_logout},
        {?GUI_TOKEN_PATH, [<<"POST">>], page_gui_token},
        {?CONFIGURATION_PATH, [<<"GET">>], page_panel_configuration},
        {?ONEZONE_LOGIN_PATH, [<<"GET">>], page_consume_onezone_login}
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
        custom_response_headers = fun response_headers/0
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
-spec get_prefix(ApiVersion :: rest_handler:version()) -> Prefix :: binary().
get_prefix(ApiVersion) ->
    Template = onepanel_env:get(rest_api_prefix_template),
    re:replace(Template, "{version_number}",
        onepanel_utils:convert(ApiVersion, binary), [{return, binary}]).


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
%% @private @doc Returns headers which should be added to each response.
%% @end
%%--------------------------------------------------------------------
-spec response_headers() -> #{binary() => binary()}.
response_headers() ->
    case rest_utils:allowed_origin() of
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
    TempDir = mochitemp:mkdtemp(),
    GuiRoot = onepanel_env:get(gui_static_root),
    GuiDirName = get_archive_top_dir(gui_package_path()),

    ok = erl_tar:extract(gui_package_path(), [compressed, {cwd, TempDir}]),
    filelib:is_file(GuiRoot) andalso file:rename(GuiRoot, filename:join(TempDir, "old_gui_static")),
    ok = file:rename(filename:join(TempDir, GuiDirName), GuiRoot),

    mochitemp:rmtempdir(TempDir),
    ?info("Deployed standalone GUI files in ~s", [GuiRoot]).


%%--------------------------------------------------------------------
%% @private @doc
%% Returns first path in a .tar.gz archive.
%% When a single directory has been compressed, as is the case
%% with gui packages, this is the top directory of the archive.
%% @end
%%--------------------------------------------------------------------
-spec get_archive_top_dir(TarGzPath :: file:filename()) -> string() | no_return().
get_archive_top_dir(TarGzPath) ->
    % @TODO Remove verbose mode after migration to OTP 21
    % In OTP 20 the spec for erl_tar:table/2 only describes the verbose
    % return format. Therefore it has to be used to appease dialyzer.
    {ok, [{TopDir, _, _, _, _, _, _} | _]} =
        erl_tar:table(TarGzPath, [compressed, verbose]),
    TopDir.


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
            file:copy(CAPath, filename:join(oz_plugin:get_cacerts_dir(), CaFile)),
            ?warning("Added '~s' to trusted certificates. Use only for test purposes.", [
                CaFile
            ])
    end.
