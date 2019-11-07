%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements gui_session_plugin behaviour and integrates GUI
%%% session handling with internal Onezone session.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_session_plugin).
-behavior(gui_session_plugin_behaviour).

-author("Lukasz Opiola").

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("gui/include/gui_session.hrl").

-define(RETURN_DETAILS(__Term), case __Term of
    {ok, __Session} -> {ok, session_to_details(__Session)};
    {error, _} = __Error -> __Error
    #error{} = __Error -> {error, __Error}
end).

-define(RETURN_OK(__Term), case __Term of
    ok -> ok;
    {error, _} = __Error -> __Error
    #error{} = __Error -> {error, __Error}
end).

-export([
    create/2,
    get/1,
    update/2,
    delete/1,
    timestamp/0,
    session_cookie_key/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback create/2.
%% @end
%%--------------------------------------------------------------------
-spec create(gui_session:id(), gui_session:details()) -> ok | {error, term()}.
create(Id, Details) ->
    ?RETURN_OK(onepanel_session:create(Id, details_to_session(Details, #onepanel_session{}))).


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback get/1.
%% @end
%%--------------------------------------------------------------------
-spec get(gui_session:id()) -> {ok, gui_session:details()} | {error, term()}.
get(Id) ->
    ?RETURN_DETAILS(onepanel_session:get(Id)).


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback update/2.
%% @end
%%--------------------------------------------------------------------
-spec update(gui_session:id(), fun((gui_session:details()) -> gui_session:details())) ->
    {ok, gui_session:details()} | {error, term()}.
update(Id, Diff) ->
    UpdateFun = fun(#onepanel_session{} = OnepanelSession) ->
        details_to_session(Diff(session_to_details(OnepanelSession)), OnepanelSession)
    end,
    onepanel_session:update(Id, UpdateFun),
    ?MODULE:get(Id).


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback delete/1.
%% @end
%%--------------------------------------------------------------------
-spec delete(gui_session:id()) -> ok | {error, term()}.
delete(Id) ->
    onepanel_session:delete(Id).


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback timestamp/0.
%% @end
%%--------------------------------------------------------------------
-spec timestamp() -> non_neg_integer().
timestamp() ->
    time_utils:system_time_seconds().


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback session_cookie_key/0.
%% @end
%%--------------------------------------------------------------------
-spec session_cookie_key() -> binary().
session_cookie_key() ->
    % Must be different than op/oz_worker's cookie key
    % as cookies are shared between applications served on different
    % ports.
    <<"PSID">>.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec session_to_details(onepanel_session:record()) -> gui_session:details().
session_to_details(#onepanel_session{username = UN, last_refresh = LF, nonce = No, previous_nonce = PN}) ->
    #gui_session{client = UN, last_refresh = LF, nonce = No, previous_nonce = PN}.


-spec details_to_session(gui_session:details(), onepanel_session:record()) -> onepanel_session:record().
details_to_session(#gui_session{client = UN, last_refresh = LF, nonce = No, previous_nonce = PN}, OnepanelSession) ->
    OnepanelSession#onepanel_session{username = UN, last_refresh = LF, nonce = No, previous_nonce = PN}.