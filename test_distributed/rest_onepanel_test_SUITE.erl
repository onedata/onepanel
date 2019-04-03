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
-include("onepanel_test_rest.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/privileges.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2,
    end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    method_should_return_unauthorized_error/1,
    noauth_method_should_return_forbidden_error/1,
    method_should_return_forbidden_error/1,
    method_should_return_not_found_error/1,
    get_as_admin_should_return_hosts/1,
    get_as_admin_should_return_cookie/1,
    get_should_return_node_details/1,
    post_as_admin_should_extend_cluster_and_return_hostname/1,
    unauthorized_post_should_join_cluster/1,
    delete_as_admin_should_remove_node_from_cluster/1
]).

-define(COOKIE, someCookie).
-define(CLUSTER_HOST_HOSTNAME, "known.hostname").
-define(NEW_HOST_HOSTNAME, "someHostname").
-define(TIMEOUT, timer:seconds(5)).

-define(run(Fun, EndpointsWithMethods),
    lists:foreach(fun({_Endpoint, _Method}) ->
        try
            Fun({_Endpoint, _Method})
        catch
            error:{assertMatch_failed, _} = _Reason ->
                ct:print("Failed on: ~s ~s", [_Method, _Endpoint]),
                erlang:error(_Reason)
        end
    end, EndpointsWithMethods)).

all() ->
    ?ALL([
        method_should_return_unauthorized_error,
        noauth_method_should_return_forbidden_error,
        method_should_return_forbidden_error,
        method_should_return_not_found_error,
        get_as_admin_should_return_hosts,
        get_as_admin_should_return_cookie,
        get_should_return_node_details,
        post_as_admin_should_extend_cluster_and_return_hostname,
        unauthorized_post_should_join_cluster,
        delete_as_admin_should_remove_node_from_cluster
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_unauthorized_error(Config) ->
    ?run(fun({Endpoint, Method}) ->
        ?assertMatch({ok, 401, _, _}, onepanel_test_rest:noauth_request(
            Config, Endpoint, Method)),
        ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method, {<<"badUser">>, <<"somePassword">>}))
    end, [
        {<<"/cookie">>, get},
        {<<"/hosts/someHost">>, delete},
        {<<"/hosts">>, get},
        {<<"/hosts">>, post},
        {<<"/web_cert">>, get},
        {<<"/web_cert">>, patch},
        {<<"/progress">>, get},
        {<<"/progress">>, patch}
    ]).


noauth_method_should_return_forbidden_error(Config) ->
    ?run(fun({Endpoint, Method}) ->
        ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method,
            [{<<"inexistent">>, <<"somePassword">>} | ?NONE_AUTHS()]
        )),
        ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method, ?REGULAR_AUTHS(Config)
        ))
    end, [
        % forbidden when there are users present
        {<<"/join_cluster">>, post},
        % forbidden without auth when there are admin users present
        {<<"/users">>, get}
    ]).


method_should_return_forbidden_error(Config) ->
    ?run(fun({Endpoint, Method}) ->
        Auths = case Method of
            get -> ?REGULAR_AUTHS(Config);
            _ -> ?REGULAR_AUTHS(Config) ++ ?OZ_AUTHS(Config, [])
        end,
        ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method, Auths
        ))
    end, [
        {<<"/cookie">>, get},
        {<<"/hosts">>, get},
        {<<"/hosts/someHost">>, delete},
        {<<"/web_cert">>, get},
        {<<"/web_cert">>, patch},
        {<<"/progress">>, get},
        {<<"/progress">>, patch}
    ]).


method_should_return_not_found_error(Config) ->
    ?run(fun({Endpoint, Method}) ->
        ?assertMatch({ok, 404, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method,
            ?OZ_OR_ROOT_AUTHS(Config, [?CLUSTER_UPDATE])
        ))
    end, [{<<"/hosts/someHost">>, delete}]).


get_as_admin_should_return_hosts(Config) ->
    {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(Config, <<"/hosts">>, get,
            ?OZ_OR_ROOT_AUTHS(Config, [])
        )
    ),
    Hosts = onepanel_utils:typed_get(cluster_hosts, Config, {seq, binary}),
    onepanel_test_rest:assert_body(JsonBody, Hosts).


get_as_admin_should_return_cookie(Config) ->
    {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(Config, <<"/cookie">>, get,
            ?OZ_OR_ROOT_AUTHS(Config, [])
        )
    ),
    Cookie = ?callAny(Config, erlang, get_cookie, []),
    onepanel_test_rest:assert_body(JsonBody, onepanel_utils:convert(Cookie, binary)).


get_should_return_node_details(Config) ->
    [Host] = ?config(cluster_hosts, Config),
    Expected = #{
        <<"clusterType">> => <<"oneprovider">>,
        <<"hostname">> => onepanel_utils:convert(Host, binary)
    },
    {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(Config, <<"/node">>, get,
            ?NONE_AUTHS() ++ ?REGULAR_AUTHS(Config)
                ++ ?OZ_OR_ROOT_AUTHS(Config, []))
    ),
    onepanel_test_rest:assert_body(JsonBody, Expected).


post_as_admin_should_extend_cluster_and_return_hostname(Config) ->
    Auths = ?OZ_OR_ROOT_AUTHS(Config, [?CLUSTER_UPDATE]),
    {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(
            Config, "/hosts", post, Auths,
            #{address => <<"someAddress">>}
        )),
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_assert_num_calls(Nodes,
        service_onepanel, extend_cluster, '_', length(Auths)),

    Expected = #{<<"hostname">> => <<?NEW_HOST_HOSTNAME>>},
    onepanel_test_rest:assert_body(JsonBody, Expected).


unauthorized_post_should_join_cluster(Config) ->
    ?assertMatch({ok, 204, _, _}, onepanel_test_rest:noauth_request(
        Config, "/join_cluster?clusterHost=someHost", post,
        #{clusterHost => <<"someHost">>, cookie => ?COOKIE}
    )),
    ?assertReceivedMatch({service, onepanel, join_cluster,
        #{cookie := ?COOKIE, cluster_host := "someHost"}
    }, ?TIMEOUT).


delete_as_admin_should_remove_node_from_cluster(Config) ->
    ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
        Config, "/hosts/" ++ ?CLUSTER_HOST_HOSTNAME, delete,
        ?OZ_OR_ROOT_AUTHS(Config, [?CLUSTER_UPDATE])
    )),
    ?assertReceivedMatch({service, onepanel, leave_cluster,
        #{hosts := [?CLUSTER_HOST_HOSTNAME]}}, ?TIMEOUT).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) -> onepanel_test_utils:init(NewConfig) end,
    [{?LOAD_MODULES, [onepanel_test_rest]}, {?ENV_UP_POSTHOOK, Posthook} | Config].

init_per_testcase(Case, Config) when
    Case =:= delete_as_admin_should_remove_node_from_cluster ->
    Nodes = ?config(onepanel_nodes, Config),
    Self = self(),
    test_utils:mock_new(Nodes, [service, service_onepanel]),
    test_utils:mock_expect(Nodes, service_onepanel, get_hosts, fun() ->
        [?CLUSTER_HOST_HOSTNAME | hosts:from_nodes(Nodes)]
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {module, function, ok}}]
    end),
    init_per_testcase(default, Config);

init_per_testcase(post_as_admin_should_extend_cluster_and_return_hostname, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_new(Nodes, [service, service_onepanel], [passthrough]),
    test_utils:mock_expect(Nodes, service_onepanel, extend_cluster, fun
        (#{hostname := Hostname}) -> #{hostname => Hostname};
        (_Ctx) -> #{hostname => <<?NEW_HOST_HOSTNAME>>} end),
    init_per_testcase(default, Config);

init_per_testcase(unauthorized_post_should_join_cluster, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    Self = self(),
    test_utils:mock_new(Nodes, [service, service_onepanel]),
    test_utils:mock_expect(Nodes, service_onepanel, available_for_clustering,
        fun() -> true end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {module, function, ok}}]
    end),
    init_per_testcase(default, Config);

init_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    onepanel_test_rest:set_up_default_users(Config),
    onepanel_test_rest:mock_token_authentication(Config),
    [{cluster_hosts, hosts:from_nodes(Nodes)} | Config].


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes),
    ?call(Config, model, clear, [onepanel_user]).


end_per_suite(_Config) ->
    ok.
