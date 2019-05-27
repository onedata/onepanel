%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for emergency_passphrase module.
%%% @end
%%%--------------------------------------------------------------------
-module(emergency_passphrase_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("modules/kv_keys.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(GOOD, <<"strongPassword">>).
-define(OTHER, <<"otherPassword">>).

%%%===================================================================
%%% Test generators
%%%===================================================================

root_passphrase_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [
            fun verify_accepts_correct_passphrase/0,
            fun verify_rejects_when_passphrase_is_not_set/0,
            fun validation_rejects_empty/0,
            fun validation_rejects_short/0,
            fun is_set_checks_passphrase_presence/0,
            fun migrate_selects_username_admin/0,
            fun migrate_selects_role_admin/0,
            fun migrate_leaves_passphrase_unset/0
        ]
    }.

%%%===================================================================
%%% Test functions
%%%===================================================================


verify_accepts_correct_passphrase() ->
    ?assertEqual(ok, emergency_passphrase:set(?GOOD)),
    ?assert(emergency_passphrase:verify(?GOOD)).


verify_rejects_when_passphrase_is_not_set() ->
    ?assertNot(onepanel_kv:exists(?KV_EMERGENCY_PASSPHRASE)),
    ?assertNot(emergency_passphrase:verify(?GOOD)).


validation_rejects_empty() ->
    ?assertMatch(#error{reason = ?ERR_INVALID_NEW_PASSPHRASE},
        emergency_passphrase:set(<<>>)).


validation_rejects_short() ->
    ?assertMatch(#error{reason = ?ERR_INVALID_NEW_PASSPHRASE},
        emergency_passphrase:set(<<"short">>)).


is_set_checks_passphrase_presence() ->
    ?assertNot(emergency_passphrase:is_set()),
    ?assertEqual(ok, emergency_passphrase:set(?GOOD)),
    ?assert(emergency_passphrase:is_set()).


migrate_selects_username_admin() ->
    add_user(<<"someAdmin">>, ?OTHER, admin),
    add_user(<<"admin">>, ?GOOD, admin),

    emergency_passphrase:migrate_from_users(),
    ?assert(emergency_passphrase:verify(?GOOD)).


migrate_selects_role_admin() ->
    add_user(<<"someUser">>, ?OTHER, regular),
    add_user(<<"someAdmin">>, ?GOOD, admin),

    emergency_passphrase:migrate_from_users(),
    ?assert(emergency_passphrase:verify(?GOOD)).


migrate_leaves_passphrase_unset() ->
    ?assertNot(onepanel_user:any_user_exists()),

    emergency_passphrase:migrate_from_users(),
    ?assertNot(emergency_passphrase:is_set()).


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


%%%===================================================================
%%% Helpers
%%%===================================================================


-spec add_user(Username :: binary(), Password :: binary(),
    Role :: admin | regular) -> #onepanel_user{}.
add_user(Username, Password, Role) ->
    Record = #onepanel_user{
        username = Username, role = Role, uuid = onepanel_utils:gen_uuid(),
        password_hash = onedata_passwords:create_hash(Password)
    },
    ok = onepanel_user:save(Record),
    Record.

-endif.