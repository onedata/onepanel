%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc:
%% @end
%% ===================================================================
-module(spanel_app).

-behaviour(application).

-include("registered_names.hrl").

%% Application callbacks
-export([start/2,
  stop/1]).

% Cowboy listener reference
-define(https_listener, https).

% Paths in gui static directory
-define(static_paths, ["/css/", "/fonts/", "/images/", "/js/", "/n2o/"]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  {ok, Port} = application:get_env(?APP_NAME, gui_port),
  {ok, HttpsAcceptors} = application:get_env(?APP_NAME, https_acceptors),
  {ok, Timeout} = application:get_env(?APP_NAME, socket_timeout),
  {ok, MaxKeepalive} = application:get_env(?APP_NAME, max_keepalive),
  {ok, GuiStaticRoot} = application:get_env(?APP_NAME, gui_static_root),
  {ok, CACertFile} = application:get_env(?APP_NAME, ca_cert_file),
  {ok, CertFile} = application:get_env(?APP_NAME, cert_file),
  {ok, KeyFile} = application:get_env(?APP_NAME, key_file),

  % Set envs needed by n2o
  % Transition port - the same as ?APP_NAME port
  ok = application:set_env(n2o, transition_port, Port),
  % Custom route handler
  ok = application:set_env(n2o, route, routes),

  % Ets table needed by n2o
  ets:insert(globals, {onlineusers, 0}),

  Dispatch = cowboy_router:compile(
    [{'_',
        static_dispatches(GuiStaticRoot, ?static_paths) ++ [
        {"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]},
        {'_', n2o_cowboy, []}
      ]}
    ]),

  {ok, _} = cowboy:start_https(?https_listener, HttpsAcceptors,
    [
      {port, Port},
      {cacertfile, CACertFile},
      {certfile, CertFile},
      {keyfile, KeyFile}
    ],
    [
      {env, [{dispatch, Dispatch}]},
      {max_keepalive, MaxKeepalive},
      {timeout, Timeout}
    ]),
  spanel_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  cowboy:stop_listener(?https_listener),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Generates static file routing for cowboy.
static_dispatches(DocRoot, StaticPaths) ->
  _StaticDispatches = lists:map(fun(Dir) ->
    Opts = [
      {mimetypes, {fun mimetypes:path_to_mimes/2, default}},
      {directory, DocRoot ++ Dir}
    ],
    {Dir ++ "[...]", cowboy_static, Opts}
  end, StaticPaths).
