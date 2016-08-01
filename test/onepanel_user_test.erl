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
-module(onepanel_user_test).
-author("Krzysztof Trzepla").

-ifdef(TEST).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(U, <<"user1">>).
-define(P, <<"Password1">>).
-define(R, admin).

%%%===================================================================
%%% Test generators
%%%===================================================================

onepanel_user_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun new_should_validate_username/1,
            fun new_should_validate_password/1,
            fun new_should_validate_role/1,
            fun new_should_create_user/1,
            fun new_should_reject_existing_user/1,
            fun authenticate_should_return_user/1,
            fun authenticate_should_pass_errors/1,
            fun change_password_should_work/1,
            fun change_password_should_validate_password/1,
            fun validate_username_should_reject_empty/1,
            fun validate_username_should_reject_short/1,
            fun validate_username_should_reject_invalid/1,
            fun validate_password_should_reject_empty/1,
            fun validate_password_should_reject_short/1,
            fun validate_password_should_reject_invalid/1,
            fun validate_role_should_reject_invalid/1
        ]
    }.

%%%===================================================================
%%% Test functions
%%%===================================================================

new_should_validate_username(_) ->
    meck:new(onepanel_user, [passthrough]),
    meck:new(onepanel_user_nif),
    meck:expect(onepanel_user_nif, hash_password, fun(_, _) -> ok end),
    onepanel_user:new(?U, ?P, ?R),
    ?_assert(meck:called(onepanel_user, validate_username, [?U])).

new_should_validate_password(_) ->
    meck:new(onepanel_user, [passthrough]),
    meck:new(onepanel_user_nif),
    meck:expect(onepanel_user_nif, hash_password, fun(_, _) -> ok end),
    onepanel_user:new(?U, ?P, ?R),
    ?_assert(meck:called(onepanel_user, validate_password, [?P])).

new_should_validate_role(_) ->
    meck:new(onepanel_user, [passthrough]),
    meck:new(onepanel_user_nif),
    meck:expect(onepanel_user_nif, hash_password, fun(_, _) -> ok end),
    onepanel_user:new(?U, ?P, ?R),
    ?_assert(meck:called(onepanel_user, validate_role, [?R])).

new_should_create_user(_) ->
    ?_assertEqual(ok, onepanel_user:new(?U, ?P, ?R)).

new_should_reject_existing_user(_) ->
    ?assertEqual(ok, onepanel_user:new(?U, ?P, ?R)),
    ?_assertThrow(#error{reason = ?ERR_USERNAME_NOT_AVAILABLE},
        onepanel_user:new(?U, ?P, ?R)).

authenticate_should_return_user(_) ->
    ?assertEqual(ok, onepanel_user:new(?U, ?P, ?R)),
    ?_assertMatch({ok, #onepanel_user{
        username = ?U, password_hash = <<_/binary>>, role = ?R,
        uuid = <<_/binary>>}
    }, onepanel_user:authenticate(?U, ?P)).

authenticate_should_pass_errors(_) ->
    ?assertEqual(ok, onepanel_user:new(?U, ?P, ?R)),
    ?_assertMatch(#error{reason = ?ERR_INVALID_USERNAME_OR_PASSWORD},
        onepanel_user:authenticate(?U, <<"password">>)).

change_password_should_validate_password(_) ->
    meck:new(onepanel_user, [passthrough]),
    meck:new(onepanel_user_nif),
    meck:expect(onepanel_user_nif, hash_password, fun(_, _) -> ok end),
    onepanel_user:new(?U, ?P, ?R),
    ?_assert(meck:called(onepanel_user, validate_password, [?P])).

change_password_should_work(_) ->
    NewPassword = <<"Password2">>,
    ?assertEqual(ok, onepanel_user:new(?U, ?P, ?R)),
    ?assertEqual(ok, onepanel_user:change_password(?U, NewPassword)),
    ?assertMatch(#error{reason = ?ERR_INVALID_USERNAME_OR_PASSWORD},
        onepanel_user:authenticate(?U, ?P)),
    ?_assertMatch({ok, _}, onepanel_user:authenticate(?U, NewPassword)).

validate_username_should_reject_empty(_) ->
    ?_assertThrow(#error{reason = ?ERR_INVALID_USERNAME},
        onepanel_user:validate_username(<<>>)).

validate_username_should_reject_short(_) ->
    ?_assertThrow(#error{reason = ?ERR_INVALID_USERNAME},
        onepanel_user:validate_username(<<"u">>)).

validate_username_should_reject_invalid(_) ->
    ?_assertThrow(#error{reason = ?ERR_INVALID_USERNAME},
        onepanel_user:validate_username(<<"user:">>)).

validate_password_should_reject_empty(_) ->
    ?_assertThrow(#error{reason = ?ERR_INVALID_PASSWORD},
        onepanel_user:validate_password(<<>>)).

validate_password_should_reject_short(_) ->
    ?_assertThrow(#error{reason = ?ERR_INVALID_PASSWORD},
        onepanel_user:validate_password(<<"p">>)).

validate_password_should_reject_invalid(_) ->
    ?_assertThrow(#error{reason = ?ERR_INVALID_PASSWORD},
        onepanel_user:validate_password(<<"Pass:word1">>)).

validate_role_should_reject_invalid(_) ->
    ?_assertThrow(#error{reason = ?ERR_INVALID_ROLE},
        onepanel_user:validate_role(role)).

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start() ->
    error_logger:tty(false),
    onepanel_env:set(rpc_timeout, 1000),
    onepanel_env:set(min_username_length, 4),
    onepanel_env:set(min_password_length, 8),
    onepanel_env:set(becrypt_work_factor, 4),
    onepanel_env:set(create_tables_timeout, 10000),
    ?assertEqual(ok, service_onepanel:purge_node(#{})),
    ?assertEqual(ok, service_onepanel:create_tables(#{})),
    ok.

stop(_) ->
    ?assertEqual(ok, service_onepanel:purge_node(#{})),
    meck:unload().

-endif.