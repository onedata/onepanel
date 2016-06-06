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
-module(service_couchbase_test_SUITE).
-author("Krzysztof Trzepla").

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
    end_per_testcase/2]).

%% tests
-export([
    configure_should_create_cluster/1,
    start_should_activate_service/1,
    stop_should_deactivate_service/1
]).

all() ->
    ?ALL([
        configure_should_create_cluster,
        start_should_activate_service,
        stop_should_deactivate_service
    ]).

-define(TIMEOUT, timer:seconds(10)).

%%%===================================================================
%%% Test functions
%%%===================================================================

configure_should_create_cluster(Config) ->
    [Node | _] = Nodes = ?config(onepanel_nodes, Config),
    Hosts = onepanel_utils:nodes_to_hosts(Nodes),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [couchbase, configure, #{hosts => Hosts}])).


start_should_activate_service(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    Host = onepanel_utils:node_to_host(Node),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [couchbase, start, #{hosts => [Host]}])),
    rpc:call(Node, service_couchbase, wait_for_start, [#{}]),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [couchbase, status, #{hosts => [Host]}])).


stop_should_deactivate_service(Config) ->
    start_should_activate_service(Config),

    [Node | _] = ?config(onepanel_nodes, Config),
    Host = onepanel_utils:node_to_host(Node),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [couchbase, stop, #{hosts => [Host]}])),
    ?assertMatch({error, {couchbase, status, {errors, _}}}, rpc:call(Node,
        service, apply, [couchbase, status, #{hosts => [Host]}])).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    onepanel_test_utils:ensure_initailized(NewConfig).


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


init_per_testcase(configure_should_create_cluster, Config) ->
    Config;

init_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    onepanel_rpc:call(Nodes, service_couchbase, pre_configure, [#{}]),
    Config.


end_per_testcase(_Case, _Config) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
