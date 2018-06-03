%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for onepanel_user module.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_deployment_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(M1, cluster).
-define(M2, certificate).

%%%===================================================================
%%% Test generators
%%%===================================================================

onepanel_deployment_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun step_is_completed_only_when_marked/0
        ]
    }.

%%%===================================================================
%%% Test functions
%%%===================================================================

step_is_completed_only_when_marked() ->
    ?assertNot(onepanel_deployment:is_completed(?M1)),
    ?assertMatch(ok, onepanel_deployment:mark_completed(?M1)),
    ?_assert(onepanel_deployment:is_completed(?M1)).

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start() ->
    error_logger:tty(false),
    onepanel_env:set(rpc_timeout, 1000),
    onepanel_env:set(create_tables_timeout, 10000),
    ?assertEqual(ok, service_onepanel:init_cluster(#{})),
    ok.

stop(_) ->
    ?assertEqual(ok, service_onepanel:reset_node(#{})),
    meck:unload().

-endif.