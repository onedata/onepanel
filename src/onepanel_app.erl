%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This is the main module of application. It lunches
%% supervisor which then initializes appropriate components of node.
%% @end
%% ===================================================================
-module(onepanel_app).

-behaviour(application).

-include("registered_names.hrl").

%% Application callbacks
-export([start/2, stop/1]).

% Cowboy listener reference
-define(HTTPS_LISTENER, https).
-define(HTTP_REDIRECTOR_LISTENER, http).

% Session logic module
-define(SESSION_LOGIC_MODULE, session_logic).

% GUI routing module
-define(GUI_ROUTING_MODULE, routes).

% Paths in gui static directory
-define(STATIC_PATHS, ["/css/", "/fonts/", "/images/", "/js/", "/n2o/"]).

%% ====================================================================
%% Application callbacks
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
-spec start(StartType :: normal | {takeover, node()} | {failover, node()}, StartArgs :: term()) -> Result when
    Result :: {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.
%% ====================================================================
start(_StartType, _StartArgs) ->
    {ok, GuiPort} = application:get_env(?APP_NAME, gui_port),
    {ok, HttpsAcceptors} = application:get_env(?APP_NAME, https_acceptors),
    {ok, Timeout} = application:get_env(?APP_NAME, socket_timeout),
    {ok, MaxKeepalive} = application:get_env(?APP_NAME, max_keepalive),
    {ok, GuiStaticRoot} = application:get_env(?APP_NAME, gui_static_root),
    {ok, CACertFile} = application:get_env(?APP_NAME, ca_cert_file),
    {ok, CertFile} = application:get_env(?APP_NAME, cert_file),
    {ok, KeyFile} = application:get_env(?APP_NAME, key_file),

    gui_utils:init_n2o_ets_and_envs(GuiPort, ?GUI_ROUTING_MODULE, ?SESSION_LOGIC_MODULE),

    Dispatch = cowboy_router:compile(
        [{'_',
                static_dispatches(GuiStaticRoot, ?STATIC_PATHS) ++ [
                {"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]},
                {'_', n2o_cowboy, []}
            ]}
        ]),

    case cowboy:start_https(?HTTPS_LISTENER, HttpsAcceptors,
        [
            {port, GuiPort},
            {cacertfile, CACertFile},
            {certfile, CertFile},
            {keyfile, KeyFile}
        ],
        [
            {env, [{dispatch, Dispatch}]},
            {max_keepalive, MaxKeepalive},
            {timeout, Timeout}
        ])
    of
        {ok, _} -> onepanel_sup:start_link();
        {error, Reason} -> {error, Reason}
    end.


%% stop/2
%% ====================================================================
%% @doc This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
-spec stop(State :: term()) -> Result when
    Result :: term().
%% ====================================================================
stop(_State) ->
    cowboy:stop_listener(?HTTPS_LISTENER),
    % Clean up after n2o.
    gui_utils:cleanup_n2o(?SESSION_LOGIC_MODULE),
    ok.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% static_dispatches/2
%% ====================================================================
%% @doc Generates static file routing for cowboy.
%% @end
-spec static_dispatches(DocRoot :: string(), StaticPaths :: [string()]) -> Result when
    Result :: [term()].
%% ====================================================================
static_dispatches(DocRoot, StaticPaths) ->
    _StaticDispatches = lists:map(fun(Dir) ->
        {Dir ++ "[...]", cowboy_static, {dir, DocRoot ++ Dir}}
    end, StaticPaths).

