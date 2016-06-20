%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(rest_onedata_user_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
    end_per_testcase/2]).

%% tests
-export([
    get_should_return_user/1,
    get_should_return_authorization_error/1,
    noauth_post_should_create_first_admin_user/1,
    post_should_create_user/1,
    post_should_return_authorization_error/1,
    post_should_not_create_existing_user/1,
    put_should_change_password/1,
    put_should_return_authorization_error/1,
    put_should_report_missing_key/1,
    put_should_report_invalid_value/1
]).

all() ->
    ?ALL([
        get_should_return_user,
        get_should_return_authorization_error,
        noauth_post_should_create_first_admin_user,
        post_should_create_user,
        post_should_return_authorization_error,
        post_should_not_create_existing_user,
        put_should_change_password,
        put_should_return_authorization_error,
        put_should_report_missing_key,
        put_should_report_invalid_value
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

get_should_return_user(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),
    Id = ?config(id, Config),
    Role = ?config(role, Config),

    ?assertMatch({Id, Role}, get_user(Node, Username, Password, 200)).


get_should_return_authorization_error(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),
    NewUsername = <<Username/binary, "_other">>,
    NewPassword = <<Password/binary, "_other">>,

    get_user(Node, NewUsername, Password, 401),
    get_user(Node, Username, NewPassword, 401).


noauth_post_should_create_first_admin_user(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = <<"username">>,
    Password = <<"Password1">>,

    post_user(Node, [
        {username, Username},
        {password, Password},
        {userRole, regular}
    ], 204),

    ?assertMatch({_, admin}, get_user(Node, Username, Password, 200)).


post_should_create_user(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),
    NewUsername = <<"username2">>,
    NewPassword = <<"Password2">>,
    NewRole = regular,

    post_user(Node,
        [onepanel_utils:get_basic_auth_header(Username, Password)], [
            {username, NewUsername},
            {password, NewPassword},
            {userRole, NewRole}
        ], 204
    ),

    ?assertMatch({_, NewRole}, get_user(Node, NewUsername, NewPassword, 200)).


post_should_return_authorization_error(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    NewUsername = <<"username2">>,
    NewPassword = <<"Password2">>,
    NewRole = regular,

    post_user(Node, [
        {username, NewUsername},
        {password, NewPassword},
        {userRole, NewRole}
    ], 403).


post_should_not_create_existing_user(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),

    Error = rpc:call(Node, onepanel_errors, translate, [throw,
        #error{reason = ?ERR_USERNAME_NOT_AVAILABLE}]),
    ?assertMatch(Error,
        post_user(Node,
            [onepanel_utils:get_basic_auth_header(Username, Password)], [
                {username, Username},
                {password, Password},
                {userRole, regular}
            ], 400
        )
    ).


put_should_change_password(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),
    Id = ?config(id, Config),
    Role = ?config(role, Config),
    NewPassword = <<Password/binary, "new">>,

    put_user(Node, Username, Password, [{password, NewPassword}], 204),
    ?assertMatch({Id, Role}, get_user(Node, Username, NewPassword, 200)).


put_should_return_authorization_error(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),
    NewPassword = <<Password/binary, "new">>,

    put_user(Node, Username, NewPassword, [{password, NewPassword}], 401).


put_should_report_missing_key(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),

    Error = rpc:call(Node, onepanel_errors, translate, [throw,
        #error{reason = {?ERR_MISSING_KEY, [password]}}]),
    ?assertEqual(Error,
        put_user(Node, Username, Password, [], 400)).


put_should_report_invalid_value(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),

    Error = rpc:call(Node, onepanel_errors, translate, [throw,
        #error{reason = ?ERR_INVALID_PASSWORD}]),
    ?assertEqual(Error, put_user(Node, Username, Password,
        [{password, <<"password">>}], 400)).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(ssl2),
    hackney:start(),
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


init_per_testcase(noauth_post_should_create_first_admin_user, Config) ->
    onepanel_test_utils:init(Config);

init_per_testcase(_Case, Config) ->
    NewConfig = onepanel_test_utils:init(Config),
    [Node | _] = ?config(onepanel_nodes, NewConfig),
    Username = <<"username">>,
    Password = <<"Password1">>,
    Role = admin,

    ?assertEqual(ok, rpc:call(Node, onedata_user, new,
        [Username, Password, Role])),
    {ok, #onedata_user{uuid = Id}} = ?assertMatch({ok, _},
        rpc:call(Node, onedata_user, get, [Username])),

    [{username, Username}, {password, Password}, {id, Id}, {role, Role} |
        NewConfig].


end_per_testcase(_Case, _Config) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

post_user(Node, Params, Code) ->
    post_user(Node, [], Params, Code).

post_user(Node, Headers, Params, Code) ->
    {ok, _, _, Body} = ?assertMatch({ok, Code, _, _},
        do_request(Node, "/user", post, Headers,
            json_utils:encode(Params))),

    case Code of
        204 -> ok;
        _ ->
            Data = json_utils:decode(Body),
            Error = proplists:get_value(<<"error">>, Data),
            Description = proplists:get_value(<<"description">>, Data),
            {Error, Description}
    end.


get_user(Node, Username, Password, Code) ->
    {ok, _, _, Body} = ?assertMatch({ok, Code, _, _},
        do_request(Node, "/user", get,
            [onepanel_utils:get_basic_auth_header(Username, Password)])),
    Data = json_utils:decode(Body),

    case Code of
        200 ->
            Id = proplists:get_value(<<"userId">>, Data),
            Role = proplists:get_value(<<"userRole">>, Data),
            {Id, erlang:binary_to_atom(Role, utf8)};
        _ ->
            Error = proplists:get_value(<<"error">>, Data),
            Description = proplists:get_value(<<"description">>, Data),
            {Error, Description}
    end.


put_user(Node, Username, Password, Params, Code) ->
    {ok, _, _, Body} = ?assertMatch({ok, Code, _, _},
        do_request(Node, "/user", put,
            [onepanel_utils:get_basic_auth_header(Username, Password)],
            json_utils:encode(Params))),

    case Code of
        204 -> ok;
        _ ->
            Data = json_utils:decode(Body),
            Error = proplists:get_value(<<"error">>, Data),
            Description = proplists:get_value(<<"description">>, Data),
            {Error, Description}
    end.


do_request(Node, Endpoint, Method, Headers) ->
    do_request(Node, Endpoint, Method, Headers, <<>>).


do_request(Node, Endpoint, Method, Headers, Body) ->
    Host = onepanel_cluster:node_to_host(Node),
    Prefix = "/api/v3/onepanel",
    Port = rpc:call(Node, onepanel_env, get, [rest_port]),
    Url = onepanel_utils:join(["https://", Host,":", Port, Prefix, Endpoint]),

    http_client:request(Method, Url, [{<<"Content-Type">>,
        <<"application/json">>} | Headers], Body, [insecure]).