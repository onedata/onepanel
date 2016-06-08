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

-include("db/models.hrl").
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
    put_should_report_invalid_value/1,
    put_should_report_password_too_short/1
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
        put_should_report_invalid_value,
        put_should_report_password_too_short
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
    Password = <<"password">>,

    post_user(Node, [
        {username, Username},
        {password, base64:encode(Password)},
        {userRole, regular}
    ], 204),

    ?assertMatch({_, admin}, get_user(Node, Username, Password, 200)).


post_should_create_user(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),
    NewUsername = <<"username2">>,
    NewPassword = <<"password2">>,
    NewRole = regular,

    post_user(Node,
        [onepanel_utils:get_basic_auth_header(Username, Password)], [
            {username, NewUsername},
            {password, base64:encode(NewPassword)},
            {userRole, NewRole}
        ], 204
    ),

    ?assertMatch({_, NewRole}, get_user(Node, NewUsername, NewPassword, 200)).


post_should_return_authorization_error(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    NewUsername = <<"username2">>,
    NewPassword = <<"password2">>,
    NewRole = regular,

    post_user(Node, [
        {username, NewUsername},
        {password, base64:encode(NewPassword)},
        {userRole, NewRole}
    ], 403).


post_should_not_create_existing_user(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),

    ?assertMatch({<<"invalid_request">>, _},
        post_user(Node,
            [onepanel_utils:get_basic_auth_header(Username, Password)], [
                {username, Username},
                {password, base64:encode(Password)},
                {userRole, regular}
            ],400
        )
    ).


put_should_change_password(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),
    Id = ?config(id, Config),
    Role = ?config(role, Config),
    NewPassword = <<Password/binary, "_new">>,

    put_user(Node, Username, Password,
        [{password, base64:encode(NewPassword)}], 204),
    ?assertMatch({Id, Role}, get_user(Node, Username, NewPassword, 200)).


put_should_return_authorization_error(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),
    NewPassword = <<Password/binary, "_new">>,

    put_user(Node, Username, NewPassword,
        [{password, base64:encode(NewPassword)}], 401).


put_should_report_missing_key(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),

    ?assertMatch({<<"missing_key">>, _},
        put_user(Node, Username, Password, [], 400)).


put_should_report_invalid_value(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),

    ?assertMatch({<<"invalid_value">>, _}, put_user(Node, Username, Password,
        [{password, <<"not_a_base64_string">>}], 400)).


put_should_report_password_too_short(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),

    ?assertMatch({<<"invalid_value">>, <<"new password should be at least",
        _/binary>>}, put_user(Node, Username, Password, [{password, <<>>}], 400)).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(ssl2),
    hackney:start(),
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    onepanel_test_utils:ensure_initailized(NewConfig).


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


init_per_testcase(noauth_post_should_create_first_admin_user, Config) ->
    Config;

init_per_testcase(_Case, Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = <<"username">>,
    Password = <<"password">>,
    Role = admin,

    ?assertEqual(ok, rpc:call(Node, onedata_user, new,
        [Username, Password, Role])),
    {ok, #onedata_user{uuid = Id}} = ?assertMatch({ok, _},
        rpc:call(Node, onedata_user, get, [Username])),

    [{username, Username}, {password, Password}, {id, Id}, {role, Role} | Config].


end_per_testcase(_Case, Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    rpc:call(Node, db_manager, empty_db, []).


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
    Hostname = onepanel_utils:node_to_host(Node),
    Prefix = rpc:call(Node, onepanel, get_env, [rest_prefix]),
    Port = erlang:integer_to_list(rpc:call(Node, onepanel, get_env, [rest_port])),
    Url = "https://" ++ Hostname ++ ":" ++ Port ++ Prefix ++ Endpoint,

    http_client:request(Method, Url, [{<<"Content-Type">>,
        <<"application/json">>} | Headers], Body, [insecure]).