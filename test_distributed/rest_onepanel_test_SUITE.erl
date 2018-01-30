%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of 'rest_onepanel' module.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_onepanel_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% tests
-export([
    method_should_return_unauthorized_error/1,
    method_should_return_forbidden_error/1,
    method_should_return_not_found_error/1,
    get_as_admin_should_return_hosts/1,
    get_as_admin_should_return_cookie/1,
    put_as_admin_should_init_cluster/1,
    put_as_admin_should_extend_cluster/1,
    delete_as_admin_should_remove_node_from_cluster/1
]).

-define(ADMIN_USER_NAME, <<"admin1">>).
-define(ADMIN_USER_PASSWORD, <<"Admin1Password">>).
-define(REG_USER_NAME, <<"user1">>).
-define(REG_USER_PASSWORD, <<"User1Password">>).
-define(COOKIE, someCookie).
-define(TIMEOUT, timer:seconds(5)).

all() ->
    ?ALL([
        method_should_return_unauthorized_error,
        method_should_return_forbidden_error,
        method_should_return_not_found_error,
        get_as_admin_should_return_hosts,
        get_as_admin_should_return_cookie,
        put_as_admin_should_init_cluster,
        put_as_admin_should_extend_cluster,
        delete_as_admin_should_remove_node_from_cluster
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_unauthorized_error(Config) ->
    lists:foreach(fun({Endpoint, Method}) ->
        ?assertMatch({ok, 401, _, _}, onepanel_test_rest:noauth_request(
            Config, Endpoint, Method
        )),
        ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method, {<<"someUser">>, <<"somePassword">>}
        ))
    end, [
        {<<"/cookie">>, get},
        {<<"/hosts/someHost">>, delete},
        {<<"/hosts">>, get},
        {<<"/hosts?discovered=true">>, get}
    ]).


method_should_return_forbidden_error(Config) ->
    lists:foreach(fun({Endpoint, Method}) ->
        ?assertMatch({ok, 403, _, _}, onepanel_test_rest:noauth_request(
            Config, Endpoint, Method
        )),
        ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method, {<<"someUser">>, <<"somePassword">>}
        )),
        ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method, {?REG_USER_NAME, ?REG_USER_PASSWORD}
        ))
    end, [
        {<<"/hosts">>, post},
        {<<"/hosts?clusterHost=someHost">>, post}
    ]),

    lists:foreach(fun({Endpoint, Method}) ->
        ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method, {?REG_USER_NAME, ?REG_USER_PASSWORD}
        ))
    end, [
        {<<"/cookie">>, get},
        {<<"/hosts/someHost">>, delete}
    ]).


method_should_return_not_found_error(Config) ->
    lists:foreach(fun({Endpoint, Method}) ->
        ?assertMatch({ok, 404, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
        ))
    end, [{<<"/hosts/someHost">>, delete}]).


get_as_admin_should_return_hosts(Config) ->
    lists:foreach(fun({Endpoint, HostsType}) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(Config, Endpoint, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        Hosts = onepanel_utils:typed_get(HostsType, Config, {seq, binary}),
        onepanel_test_rest:assert_body(JsonBody, Hosts)
    end, [
        {<<"/hosts">>, cluster_hosts},
        {<<"/hosts?discovered=true">>, discovered_hosts}
    ]).


get_as_admin_should_return_cookie(Config) ->
    {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(Config, <<"/cookie">>, get,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
        )
    ),
    Cookie = ?callAny(Config, erlang, get_cookie, []),
    onepanel_test_rest:assert_body(JsonBody, onepanel_utils:convert(Cookie, binary)).


put_as_admin_should_init_cluster(Config) ->
    ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
        Config, "/hosts", post, {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD},
        #{cookie => ?COOKIE}
    )),
    ?assertReceivedMatch({service, onepanel, init_cluster,
        #{cookie := ?COOKIE}}, ?TIMEOUT).


put_as_admin_should_extend_cluster(Config) ->
    ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
        Config, "/hosts?clusterHost=someHost", post,
        {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, #{cookie => ?COOKIE}
    )),
    ?assertReceivedMatch({service, onepanel, join_cluster,
        #{cookie := ?COOKIE, cluster_host := "someHost"}
    }, ?TIMEOUT).


delete_as_admin_should_remove_node_from_cluster(Config) ->
    lists:foreach(fun(Host) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Config, "/hosts/" ++ Host, delete,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
        )),
        ?assertReceivedMatch({service, onepanel, leave_cluster, #{hosts := [Host]}},
            ?TIMEOUT)
    end, ?config(cluster_hosts, Config)).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) -> onepanel_test_utils:init(NewConfig) end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].

init_per_testcase(Case, Config) when
    Case =:= put_as_admin_should_init_cluster;
    Case =:= put_as_admin_should_extend_cluster;
    Case =:= delete_as_admin_should_remove_node_from_cluster ->
    Nodes = ?config(onepanel_nodes, Config),
    Self = self(),
    test_utils:mock_new(Nodes, service),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {module, function, ok}}]
    end),
    init_per_testcase(admin_account_required, Config);

init_per_testcase(Case, Config) when
    Case =:= admin_account_required;
    Case =:= method_should_return_forbidden_error;
    Case =:= get_as_admin_should_return_hosts;
    Case =:= get_as_admin_should_return_cookie;
    Case =:= method_should_return_not_found_error ->
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD, admin])),
    init_per_testcase(default, Config);

init_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    ClusterHosts = ["cHost1", "cHost2", "cHost3"],
    DiscoveredHosts = ["dHost1", "dHost2", "dHost3"],
    test_utils:mock_new(Nodes, [onepanel_cluster, onepanel_discovery]),
    test_utils:mock_expect(Nodes, service_onepanel, get_hosts, fun() ->
        ClusterHosts
    end),
    test_utils:mock_expect(Nodes, onepanel_discovery, get_hosts, fun() ->
        DiscoveredHosts
    end),
    ?assertMatch({ok, _}, ?call(Config, onepanel_user, create,
        [?REG_USER_NAME, ?REG_USER_PASSWORD, regular])),
    [{cluster_hosts, ClusterHosts}, {discovered_hosts, DiscoveredHosts} | Config].


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes),
    ?call(Config, model, clear, [onepanel_user]).

