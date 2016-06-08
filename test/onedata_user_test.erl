%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for onedata_user module.
%%% @end
%%%--------------------------------------------------------------------
-module(onedata_user_test).
-author("Krzysztof Trzepla").

-ifdef(TEST).

-include("db/models.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test generators
%%%===================================================================

onedata_user_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun new_should_create_user/1,
            fun new_should_not_create_user_with_empty_username/1,
            fun new_should_not_create_user_with_short_password/1,
            fun new_should_not_overwrite_existing_user/1,
            fun authenticate_should_return_user/1,
            fun authenticate_should_pass_errors/1,
            fun change_password_should_work/1
        ]
    }.

%%%===================================================================
%%% Test functions
%%%===================================================================

new_should_create_user(_) ->
    ?_assertEqual(ok, onedata_user:new(<<"u">>, <<"p">>, r)).

new_should_not_create_user_with_empty_username(_) ->
    ?_assertEqual({error, empty_username}, onedata_user:new(<<>>, <<"p">>, r)).

new_should_not_create_user_with_short_password(_) ->
    onepanel:set_env(min_password_length, 2),
    ?_assertEqual({error, {password, {too_short, 2}}},
        onedata_user:new(<<"u">>, <<"p">>, r)).

new_should_not_overwrite_existing_user(_) ->
    ?assertEqual(ok, onedata_user:new(<<"u">>, <<"p">>, r)),
    ?_assertEqual({error, already_exists},
        onedata_user:new(<<"u">>, <<"p">>, r)).

authenticate_should_return_user(_) ->
    ?assertEqual(ok, onedata_user:new(<<"u">>, <<"p">>, r)),
    Hash = onedata_user:hash_password(<<"p">>),
    ?_assertMatch({ok, #onedata_user{
        username = <<"u">>, password_hash = Hash, role = r, uuid = <<_/binary>>}
    }, onedata_user:authenticate(<<"u">>, <<"p">>)).

authenticate_should_pass_errors(_) ->
    ?assertEqual(ok, onedata_user:new(<<"u">>, <<"p">>, r)),
    ?_assertMatch({error, invalid_username_or_password},
        onedata_user:authenticate(<<"u">>, <<"p2">>)).

change_password_should_work(_) ->
    ?assertEqual(ok, onedata_user:new(<<"u">>, <<"p">>, r)),
    ?assertEqual(ok, onedata_user:change_password(<<"u">>, <<"p2">>)),
    ?assertEqual({error, invalid_username_or_password},
        onedata_user:authenticate(<<"u">>, <<"p">>)),
    ?_assertMatch({ok, _}, onedata_user:authenticate(<<"u">>, <<"p2">>)).

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start() ->
    error_logger:tty(false),
    onepanel:set_env(min_password_length, 1),
    onepanel:set_env(create_tables_timeout, 10000),
    ?assertEqual(ok, db_manager:create_db()),
    ?assertEqual(ok, db_manager:empty_db()),
    ok.

stop(_) ->
    ?assertEqual(ok, db_manager:delete_db()).

-endif.