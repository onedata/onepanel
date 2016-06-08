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
-module(rest_cluster_manager_test_SUITE).
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
    get_should_return_service_status/1,
    get_should_return_service_status_for_host/1,
    get_should_return_host_not_found_error/1,
    patch_should_start_service/1,
    patch_should_start_service_on_host/1,
    patch_should_stop_service/1,
    patch_should_stop_service_on_host/1
]).

all() ->
    ?ALL([
        get_should_return_service_status,
        get_should_return_service_status_for_host,
        get_should_return_host_not_found_error,
        patch_should_start_service,
        patch_should_start_service_on_host,
        patch_should_stop_service,
        patch_should_stop_service_on_host
    ]).

-define(SERVICE_MODULE, service_cluster_manager).

%%%===================================================================
%%% Test functions
%%%===================================================================

get_should_return_service_status(Config) ->
    ?assertMatch({ok, 200, _, _},
        do_request(Config, "/cluster/managers", get)).


get_should_return_service_status_for_host(Config) ->
    Hosts = ?config(hosts, Config),

    lists:foreach(fun(Host) ->
        ?assertMatch({ok, 200, _, _},
            do_request(Config, "/cluster/managers/" ++ Host, get))
    end, Hosts).


get_should_return_host_not_found_error(Config) ->
    ?assertMatch({ok, 404, _, _},
        do_request(Config, "/cluster/managers/some_host", get)).


patch_should_start_service(Config) ->
    ?assertMatch({ok, 200, _, _}, do_request(Config,
            "/cluster/managers?started=true", patch)).


patch_should_start_service_on_host(Config) ->
    Hosts = ?config(hosts, Config),

    lists:foreach(fun(Host) ->
        ?assertMatch({ok, 200, _, _}, do_request(Config,
            "/cluster/managers/" ++ Host ++ "?started=true", patch))
    end, Hosts).


patch_should_stop_service(Config) ->
    ?assertMatch({ok, 200, _, _}, do_request(Config,
        "/cluster/managers?started=false", patch)).


patch_should_stop_service_on_host(Config) ->
    Hosts = ?config(hosts, Config),

    lists:foreach(fun(Host) ->
        ?assertMatch({ok, 200, _, _}, do_request(Config,
            "/cluster/managers/" ++ Host ++ "?started=false", patch))
    end, Hosts).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(ssl2),
    hackney:start(),
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    onepanel_test_utils:ensure_initailized(NewConfig),

    [Node | _] = ?config(onepanel_nodes, NewConfig),
    Username = <<"username">>,
    Password = <<"password">>,
    Role = admin,

    ?assertEqual(ok, rpc:call(Node, onedata_user, new,
        [Username, Password, Role])),

    [{username, Username}, {password, Password}, {role, Role} | NewConfig].


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


init_per_testcase(get_should_return_service_status, Config) ->
    mock_service_action(Config, status);

init_per_testcase(get_should_return_service_status_for_host, Config) ->
    mock_service_host_action(Config, status);

init_per_testcase(patch_should_start_service, Config) ->
    mock_service_action(Config, start);

init_per_testcase(patch_should_start_service_on_host, Config) ->
    mock_service_host_action(Config, start);

init_per_testcase(patch_should_stop_service, Config) ->
    mock_service_action(Config, stop);

init_per_testcase(patch_should_stop_service_on_host, Config) ->
    mock_service_host_action(Config, stop);

init_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_new(Nodes, [service, ?SERVICE_MODULE]),
    [{hosts, onepanel_utils:nodes_to_hosts(Nodes)} | Config].


end_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_validate_and_unload(Nodes, [service, ?SERVICE_MODULE]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_request(Config, Endpoint, Method) ->
    do_request(Config, Endpoint, Method, []).


do_request(Config, Endpoint, Method, Headers) ->
    do_request(Config, Endpoint, Method, Headers, <<>>).


do_request(Config, Endpoint, Method, Headers, Body) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Username = ?config(username, Config),
    Password = ?config(password, Config),

    Hostname = onepanel_utils:node_to_host(Node),
    Prefix = rpc:call(Node, onepanel, get_env, [rest_prefix]),
    Port = erlang:integer_to_list(rpc:call(Node, onepanel, get_env, [rest_port])),
    Url = "https://" ++ Hostname ++ ":" ++ Port ++ Prefix ++ Endpoint,

    Auth = onepanel_utils:get_basic_auth_header(Username, Password),

    http_client:request(Method, Url, [{<<"Content-Type">>,
        <<"application/json">>}, Auth | Headers], Body, [insecure]).


mock_service_action(Config, Action) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(onepanel_nodes, NewConfig),
    Hosts = ?config(hosts, NewConfig),
    test_utils:mock_expect(Nodes, service, exists, fun(_) -> true end),
    test_utils:mock_expect(Nodes, service, get, fun(_) ->
        {ok, #service{hosts = Hosts}}
    end),
    test_utils:mock_expect(Nodes, ?SERVICE_MODULE, Action, fun(_) -> ok end),
    NewConfig.


mock_service_host_action(Config, Action) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(onepanel_nodes, NewConfig),
    test_utils:mock_expect(Nodes, service, member, fun(_, _) -> true end),
    test_utils:mock_expect(Nodes, ?SERVICE_MODULE, Action, fun(_) -> ok end),
    NewConfig.
