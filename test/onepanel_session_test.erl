%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for onepanel_session module.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_session_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TOKEN_TTL, 6).
-define(SESSION_ID, <<"MySessionId">>).

%%%===================================================================
%%% Test generators
%%%===================================================================

onepanel_session_test_() ->
    {setup, fun node_cache:init/0, fun(_) -> ets:delete(node_cache) end, 
        {foreach,
            fun start/0,
            fun stop/1,
            [{timeout, 30, Testcase} || Testcase <- [
                fun expired_token_is_invalid/0,
                fun token_is_reused/0,
                fun token_is_renewed/0,
                fun expired_tokens_are_cleaned/0
            ]]
        }
    }.

%%%===================================================================
%%% Test functions
%%%===================================================================

expired_token_is_invalid() ->
    {ok, #onepanel_session{auth_tokens = [{Token, _} | _]} = Session} = get_new_session(),

    ?assertEqual({ok, Session}, onepanel_session:find_by_valid_auth_token(Token)),
    timer:sleep(timer:seconds(?TOKEN_TTL + 1)),
    ?assertMatch(error, onepanel_session:find_by_valid_auth_token(Token)).


token_is_reused() ->
    {ok, #onepanel_session{auth_tokens = [_Token]} = Session} = get_new_session(),
    ?assertEqual(Session, onepanel_session:ensure_fresh_token(Session)).


token_is_renewed() ->
    {ok, #onepanel_session{auth_tokens = [Token]} = Session} = get_new_session(),
    timer:sleep(timer:seconds((?TOKEN_TTL div 2) + 1)),
    ?assertMatch(#onepanel_session{auth_tokens = [_NewToken, Token]},
        onepanel_session:ensure_fresh_token(Session)).


expired_tokens_are_cleaned() ->
    {ok, #onepanel_session{auth_tokens = [_Token]} = Session} = get_new_session(),
    timer:sleep(timer:seconds(?TOKEN_TTL + 1)),
    ?assertMatch(#onepanel_session{auth_tokens = []},
        onepanel_session:remove_expired_tokens(Session)).

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start() ->
    error_logger:tty(false),
    onepanel_env:set(rpc_timeout, 1000),
    onepanel_env:set(create_tables_timeout, 10000),
    onepanel_env:set(auth_token_ttl, ?TOKEN_TTL),
    ?assertEqual(ok, service_onepanel:init_cluster(#{})),
    ok.

stop(_) ->
    ?assertEqual(ok, service_onepanel:reset_node(#{})),
    meck:unload().


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_new_session() ->
    onepanel_session:create(?SESSION_ID, #onepanel_session{}),
    {ok, #onepanel_session{}} = onepanel_session:get(?SESSION_ID).


-endif.