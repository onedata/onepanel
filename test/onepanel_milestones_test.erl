%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for onepanel_user module.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_milestones_test).
-author("Krzysztof Trzepla").

-ifdef(TEST).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(M1, cluster).
-define(M2, certificate).

%%%===================================================================
%%% Test generators
%%%===================================================================

onepanel_milestone_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun milestone_is_configured_only_when_marked/0
        ]
    }.

%%%===================================================================
%%% Test functions
%%%===================================================================

milestone_is_configured_only_when_marked() ->
    ?assertNot(onepanel_milestones:is_configured(?M1)),
    ?assertMatch(ok, onepanel_milestones:mark_configured(?M1)),
    ?_assert(onepanel_milestones:is_configured(?M1)).

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start() ->
    error_logger:tty(false),
    onepanel_env:set(rpc_timeout, 1000),
    onepanel_env:set(create_tables_timeout, 10000),
    onepanel_env:set(default_users, []),
    ?assertEqual(ok, service_onepanel:init_cluster(#{})),
    ok.

stop(_) ->
    ?assertEqual(ok, service_onepanel:reset_node(#{})),
    meck:unload().

-endif.