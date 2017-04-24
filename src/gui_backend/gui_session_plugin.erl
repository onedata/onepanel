%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements session_logic_behaviour and
%%% is capable of persisting GUI sessions in datastore.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_session_plugin).
-author("Lukasz Opiola").
-behaviour(gui_session_plugin_behaviour).


-include_lib("gui/include/gui.hrl").
-include_lib("ctool/include/logging.hrl").


%% session_logic_behaviour API
-export([init/0, cleanup/0]).
-export([create_session/2, update_session/2, lookup_session/1]).
-export([delete_session/1]).
-export([get_cookie_ttl/0]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback init/1.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback cleanup/1.
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok.
cleanup() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback create_session/1.
%% @end
%%--------------------------------------------------------------------
-spec create_session(UserId :: term(), CustomArgs :: [term()]) ->
    {ok, SessionId :: binary()} | {error, term()}.
create_session(UserId, _CustomArgs) ->
    {ok, UserId}.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback update_session/2.
%% @end
%%--------------------------------------------------------------------
-spec update_session(SessId :: binary(),
    MemoryUpdateFun :: fun((maps:map()) -> maps:map())) ->
    ok | {error, term()}.
update_session(_SessionId, _MemoryUpdateFun) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback lookup_session/1.
%% @end
%%--------------------------------------------------------------------
-spec lookup_session(SessionId :: binary()) ->
    {ok, Memory :: maps:map()} | undefined.
lookup_session(_SessionId) ->
    {ok, #{}}.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback delete_session/1.
%% @end
%%--------------------------------------------------------------------
-spec delete_session(SessionId :: binary()) -> ok | {error, term()}.
delete_session(_SessionId) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback get_cookie_ttl/0.
%% @end
%%--------------------------------------------------------------------
-spec get_cookie_ttl() -> integer() | {error, term()}.
get_cookie_ttl() ->
    3600 * 5.
