%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of user management endpoints.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_onepanel_user_test_SUITE).
-author("Krzysztof Trzepla").

-include("names.hrl").
-include("modules/errors.hrl").
-include("onepanel_test_utils.hrl").
-include("onepanel_test_rest.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").
-include_lib("ctool/include/http/codes.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    end_per_suite/1]).

%% tests
-export([
    method_should_return_not_found_error/1,
    method_should_return_unauthorized_error/1,
    get_current_user_as_oz_user_should_return_privileges/1,
    get_current_user_as_root_should_fail/1,
    get_should_list_oz_users/1,
    get_should_describe_oz_user/1,
    post_should_create_oz_user/1,
    patch_should_change_user_password/1
]).

all() ->
    ?ALL([
        method_should_return_not_found_error,
        method_should_return_unauthorized_error,
        get_current_user_as_oz_user_should_return_privileges,
        get_current_user_as_root_should_fail,
        get_should_list_oz_users,
        get_should_describe_oz_user,
        post_should_create_oz_user,
        patch_should_change_user_password
    ]).

-define(USER_ID1, <<"joeId">>).
-define(USER_ID2, <<"kyleId">>).
-define(USERNAME1, <<"joe">>).
-define(USERNAME2, <<"kyle">>).
-define(FULL_NAME1, <<"Joe Kowalski">>).
-define(FULL_NAME2, <<"Kyle Katarn">>).

-define(PASSWORD1, <<"onePassword">>).
-define(PASSWORD2, <<"otherpassword">>).

%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_not_found_error(Config) ->
    lists:foreach(fun({Method, Body}) ->
        ?assertMatch({ok, ?HTTP_404_NOT_FOUND, _, _}, onepanel_test_rest:auth_request(
            Config, <<"/zone/users/someUser">>, Method,
            ?OZ_OR_ROOT_AUTHS(Config, [?CLUSTER_UPDATE]), Body
        ))
    end, [{get, []}, {patch, #{newPassword => <<"SomePassword1">>}}]).


method_should_return_unauthorized_error(Config) ->
    lists:foreach(fun({Method, Body}) ->
        lists:foreach(fun(Auth) ->
            ?assertMatch({ok, ?HTTP_401_UNAUTHORIZED, _, _}, onepanel_test_rest:auth_request(
                Config, <<"/zone/users/someUser">>, Method, Auth, Body
            ))
        end, ?INCORRECT_AUTHS() ++ ?NONE_AUTHS())
    end, [{get, []}, {patch, #{newPassword => <<"SomePassword1">>}}]).


get_current_user_as_oz_user_should_return_privileges(Config) ->
    lists:foreach(fun(PrivilegeSet) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Config, <<"/user">>, get,
                ?OZ_AUTHS(Config, PrivilegeSet)
            )
        ),
        onepanel_test_rest:assert_body_fields(JsonBody,
            [<<"username">>, <<"userId">>, <<"clusterPrivileges">>]),

        Expected = lists:sort([atom_to_binary(P, utf8) || P <- PrivilegeSet]),
        #{<<"clusterPrivileges">> := ReturnedPrivileges} = json_utils:decode(JsonBody),
        ?assertEqual(Expected, lists:sort(ReturnedPrivileges))
    end, [
        [],
        [?CLUSTER_VIEW],
        [?CLUSTER_UPDATE, ?CLUSTER_VIEW],
        privileges:cluster_admin()
    ]).


get_current_user_as_root_should_fail(Config) ->
    ?assertMatch({ok, ?HTTP_404_NOT_FOUND, _, _}, onepanel_test_rest:auth_request(
        Config, <<"/user">>, get, ?ROOT_AUTHS(Config)
    )).


get_should_list_oz_users(Config) ->
    Expected = lists:sort(?config(oz_user_ids, Config)),

    {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/zone/users">>, get,
            ?OZ_OR_ROOT_AUTHS(Config, [])
        )
    ),
    onepanel_test_rest:assert_body_fields(JsonBody, [<<"ids">>]),
    #{<<"ids">> := ReturnedIds} = json_utils:decode(JsonBody),
    ?assertEqual(Expected, lists:sort(ReturnedIds)).


get_should_describe_oz_user(Config) ->
    Expected = #{
        <<"userId">> => ?USER_ID1, <<"username">> => ?USERNAME1,
        <<"fullName">> => ?FULL_NAME1
    },
    {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/zone/users/", (?USER_ID1)/binary>>, get,
            ?OZ_OR_ROOT_AUTHS(Config, [])
        )
    ),
    onepanel_test_rest:assert_body(JsonBody, Expected).


post_should_create_oz_user(Config) ->
    Bodies = [
        #{<<"username">> => ?USERNAME1, <<"password">> => ?PASSWORD1,
            <<"groups">> => [<<"admins">>]},
        #{<<"username">> => ?USERNAME2, <<"password">> => ?PASSWORD2,
            <<"groups">> => []}
    ],

    lists:foreach(fun({ReqBody, Auth}) ->
        {ok, _, Headers, JsonBody} = ?assertMatch({ok, ?HTTP_201_CREATED, _, _},
            onepanel_test_rest:auth_request(
                Config, <<"/zone/users">>, post,
                Auth, ReqBody
            )),
        onepanel_test_rest:assert_body_fields(JsonBody, [<<"id">>]),
        #{<<"id">> := Id} = ?assertMatch(#{<<"id">> := <<_/binary>>},
            json_utils:decode(JsonBody)),
        ?assertMatch(#{
            ?HDR_LOCATION := <<"/api/v3/onepanel/zone/users/", Id/binary>>
        }, Headers)
    end, zip_auths(Bodies, [
        hd(?OZ_AUTHS(Config, [?CLUSTER_UPDATE])),
        hd(?ROOT_AUTHS(Config))
    ])).


patch_should_change_user_password(Config) ->
    ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/zone/users/", (?USER_ID1)/binary>>, patch,
            ?OZ_OR_ROOT_AUTHS(Config, [?CLUSTER_UPDATE]),
            #{<<"newPassword">> => ?PASSWORD1}
        )
    ).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) -> onepanel_test_utils:init(NewConfig) end,
    [{?LOAD_MODULES, [onepanel_test_rest]}, {?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(method_should_return_unauthorized_error, Config) ->
    Config2 = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_new(Nodes, [onepanel_parser, user_middleware]),
    % do not require valid payload in requests
    test_utils:mock_expect(Nodes, onepanel_parser, parse, fun(_, _) -> #{} end),
    test_utils:mock_expect(Nodes, user_middleware, fetch_entity, fun
        (_) -> {ok, {undefined, 1}}
    end),
    Config2;

init_per_testcase(method_should_return_not_found_error, Config) ->
    Config2 = init_per_testcase(default, Config),
    Nodes = ?config(onepanel_nodes, Config),

    test_utils:mock_expect(Nodes, rpc, call, fun
        (_, rpc_api, apply, [get_user_details, [_Auth, UserId]]) ->
            ?ERROR_NOT_FOUND;
        (Node, M, F, A) -> meck:passthrough([Node, M, F, A])
    end),
    Config2;

init_per_testcase(get_should_list_oz_users, Config) ->
    Config2 = init_per_testcase(default, Config),
    Nodes = ?config(onepanel_nodes, Config),
    UserIds = [?USER_ID1, ?USER_ID2],
    test_utils:mock_new(Nodes, [onezone_users]),
    test_utils:mock_expect(Nodes, rpc, call, fun
        (_, rpc_api, apply, [list_users, [?ROOT]]) -> {ok, UserIds};
        (Node, M, F, A) -> meck:passthrough([Node, M, F, A])
    end),
    [{oz_user_ids, UserIds} | Config2];

init_per_testcase(Case, Config) when
    Case == get_should_describe_oz_user;
    Case == patch_should_change_user_password
->
    Config2 = init_per_testcase(default, Config),
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_new(Nodes, [onezone_users]),
    test_utils:mock_expect(Nodes, rpc, call, fun
        (_, rpc_api, apply, [user_exists, [UserId]]) -> UserId == ?USER_ID1;
        (_, rpc_api, apply, [set_user_password, [?ROOT, _UserId, _Password]]) -> ok;
        (_, rpc_api, apply, [get_user_details, [?ROOT, ?USER_ID1]]) ->
            {ok, #user_details{id = ?USER_ID1, username = ?USERNAME1, full_name = ?FULL_NAME1}};
        (Node, M, F, A) -> meck:passthrough([Node, M, F, A])
    end),
    Config2;

init_per_testcase(post_should_create_oz_user, Config) ->
    Config2 = init_per_testcase(default, Config),
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_new(Nodes, [onezone_users]),
    test_utils:mock_expect(Nodes, rpc, call, fun
        (_, rpc_api, apply, [create_user, [?ROOT, #{<<"username">> := ?USERNAME1}]]) ->
            {ok, ?USER_ID1};
        (_, rpc_api, apply, [create_user, [?ROOT, #{<<"username">> := ?USERNAME2}]]) ->
            {ok, ?USER_ID2};
        (_, rpc_api, apply, [create_user, [?ROOT, _]]) ->
            error(badarg);
        (_, rpc_api, apply, [add_user_to_group, [?ROOT, _GroupId, _UserId]]) ->
            {ok, <<"groupId">>};
        (Node, M, F, A) ->
            meck:passthrough([Node, M, F, A])
    end),
    Config2;

init_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    onepanel_test_rest:set_default_passphrase(Config),
    onepanel_test_rest:mock_token_authentication(Config),
    test_utils:mock_new(Nodes, [service, service_onezone, service_oz_worker, rpc],
        [passthrough, unstick]),
    test_utils:mock_expect(Nodes, service, is_healthy, fun(_) -> true end),
    test_utils:mock_expect(Nodes, service, all_healthy, fun() -> true end),
    test_utils:mock_expect(Nodes, service_onezone, get_hosts,
        fun() -> ?config(onepanel_hosts, Config) end),
    test_utils:mock_expect(Nodes, service_oz_worker, get_hosts,
        fun() -> ?config(onepanel_hosts, Config) end),
    Config.


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes),
    ?call(Config, model, clear, [onepanel_user]).


end_per_suite(_Config) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Assigns one of the auth objects to each request variant.
%% @end
%%--------------------------------------------------------------------
-spec zip_auths([Parameter], Auths :: [onepanel_test_rest:auth(), ...]) ->
    [{Parameter, onepanel_test_rest:auth()}].
zip_auths(Parameters, Auths) ->
    Auths2 = lists_utils:ensure_length(length(Parameters), Auths),
    lists:zip(Parameters, Auths2).
