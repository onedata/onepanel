%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements gui_route_plugin_behaviour. It decides on:
%%%   - mapping of URLs to pages (routes)
%%%   - logic and requirements on different routes
%%%   - what pages are used for login, logout, displaying errors
%%%   - what modules handles server logic of WebSocket connection with
%%%         the client (data and callback backends)
%%% @end
%%%-------------------------------------------------------------------
-module(gui_route_plugin).
-author("Lukasz Opiola").
-behaviour(gui_route_plugin_behaviour).

-include_lib("gui/include/gui.hrl").
-include_lib("ctool/include/logging.hrl").

-export([route/1, data_backend/2, private_rpc_backend/0, public_rpc_backend/0]).
-export([session_details/0]).
-export([login_page_path/0, default_page_path/0]).
-export([error_404_html_file/0, error_500_html_file/0]).
-export([response_headers/0]).

%% Convenience macros for defining routes.

-define(LOGOUT, #gui_route{
    requires_session = ?SESSION_LOGGED_IN,
    html_file = undefined,
    page_backend = logout_backend
}).

-define(BASIC_LOGIN, #gui_route{
    requires_session = ?SESSION_NOT_LOGGED_IN,
    html_file = undefined,
    page_backend = basic_login_backend
}).

-define(INDEX, #gui_route{
    requires_session = ?SESSION_ANY,
    websocket = ?SESSION_ANY,
    html_file = <<"index.html">>,
    page_backend = undefined
}).


%% ====================================================================
%% API
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link gui_route_plugin_behaviour} callback route/1.
%% @end
%%--------------------------------------------------------------------
-spec route(Path :: binary()) -> #gui_route{}.
route(<<"/do_logout">>) -> ?LOGOUT;
route(<<"/do_login">>) -> ?BASIC_LOGIN;
route(<<"/">>) -> ?INDEX;
route(<<"/index.html">>) -> ?INDEX;
route(_) -> undefined.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_route_plugin_behaviour} callback data_backend/2
%% @end
%%--------------------------------------------------------------------
-spec data_backend(HasSession :: boolean(), Identifier :: binary()) ->
    no_return().
data_backend(_, _) -> error(unimplemented).


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_route_plugin_behaviour} callback private_rpc_backend/0
%% @end
%%--------------------------------------------------------------------
-spec private_rpc_backend() -> no_return().
private_rpc_backend() -> error(unimplemented).


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_route_plugin_behaviour} callback public_rpc_backend/0
%% @end
%%--------------------------------------------------------------------
-spec public_rpc_backend() -> no_return().
public_rpc_backend() -> error(unimplemented).


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_route_plugin_behaviour} callback get_session_details/0
%% @end
%%--------------------------------------------------------------------
-spec session_details() -> no_return().
session_details() ->
    error(unimplemented).


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_route_plugin_behaviour} callback login_page_path/0
%% @end
%%--------------------------------------------------------------------
-spec login_page_path() -> Path :: binary().
login_page_path() ->
    <<"/login">>.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_route_plugin_behaviour} callback default_page_path/0
%% @end
%%--------------------------------------------------------------------
-spec default_page_path() -> Path :: binary().
default_page_path() ->
    <<"/">>.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_route_plugin_behaviour} callback error_404_html_file/0
%% @end
%%--------------------------------------------------------------------
-spec error_404_html_file() -> FileName :: binary().
error_404_html_file() ->
    <<"page404.html">>.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_route_plugin_behaviour} callback error_500_html_file/0
%% @end
%%--------------------------------------------------------------------
-spec error_500_html_file() -> FileName :: binary().
error_500_html_file() ->
    <<"page500.html">>.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_route_plugin_behaviour} callback response_headers/0
%% @end
%%--------------------------------------------------------------------
-spec response_headers() -> [{Key :: binary(), Value :: binary()}].
response_headers() ->
    [].
