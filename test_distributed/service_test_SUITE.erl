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
-module(service_test_SUITE).
-author("Krzysztof Trzepla").

-include("service.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
    end_per_testcase/2]).

%% tests
-export([
    service_should_be_not_found/1, service_action_should_be_not_supported/1,
    service_should_request_action_steps/1, service_should_execute_steps/1,
    service_should_notify_caller/1, service_get_steps_should_pass_errors/1,
    service_action_should_pass_errors/1
]).

all() ->
    ?ALL([
        service_should_be_not_found, service_action_should_be_not_supported,
        service_should_request_action_steps, service_should_execute_steps,
        service_should_notify_caller, service_get_steps_should_pass_errors,
        service_action_should_pass_errors
    ]).

-define(TIMEOUT, timer:seconds(10)).

%%%===================================================================
%%% Test functions
%%%===================================================================

service_should_be_not_found(Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    lists:foreach(fun(Node) ->
        ?assertMatch({error, service_not_found, _},
            rpc:call(Node, service, apply, [example, some_action, #{}]))
    end, Nodes).


service_action_should_be_not_supported(Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    lists:foreach(fun(Node) ->
        ?assertMatch({error, action_not_supported, _},
            rpc:call(Node, service, apply, [example, other_action, #{}]))
    end, Nodes).


service_should_request_action_steps(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    ?assertEqual(ok,
        rpc:call(Node, service, apply, [example, some_action, #{}])),
    ?assertReceivedEqual(get_steps, ?TIMEOUT).


service_should_execute_steps(Config) ->
    Self = self(),
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    ?assertEqual(ok, rpc:call(Node1, service, apply,
        [example, some_action, #{notify => Self}])),
    ?assertReceivedEqual({Node1, step1}, ?TIMEOUT),
    ?assertReceivedEqual({Node2, step2}, ?TIMEOUT),
    ?assertReceivedEqual({Node1, step3}, ?TIMEOUT),
    ?assertReceivedEqual({Node2, step3}, ?TIMEOUT).


service_should_notify_caller(Config) ->
    Self = self(),
    [Node, _] = ?config(onepanel_nodes, Config),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [example, some_action, #{notify => Self}])),
    ?assertReceivedEqual({action_begin, {example, some_action}}, ?TIMEOUT),
    lists:foreach(fun(Step) ->
        ?assertReceivedEqual({step_begin, {example, Step}}, ?TIMEOUT),
        ?assertReceivedEqual({step_end, {example, Step, ok}}, ?TIMEOUT)
    end, [step1, step2, step3]),
    ?assertReceivedEqual({action_end, {example, some_action, ok}},
        ?TIMEOUT).


service_get_steps_should_pass_errors(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    ?assertMatch({error, get_steps_failure, _},
        rpc:call(Node, service, apply, [example, some_action, #{}])).


service_action_should_pass_errors(Config) ->
    Self = self(),
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    ?assertMatch({error, _}, rpc:call(Node1, service, apply,
        [example, some_action, #{notify => Self}])),
    ?assertReceivedMatch({step_end, {example, some_step,
        {errors, [{Node2, {error, step_failure, _}}]}}}, ?TIMEOUT).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    onepanel_test_utils:mock_start(
        onepanel_test_utils:ensure_initailized(NewConfig)).


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


init_per_testcase(service_action_should_be_not_supported, Config) ->
    mock_service(Config, get_steps, fun(some_action, _) -> [] end);

init_per_testcase(service_should_request_action_steps, Config) ->
    Self = self(),
    mock_service(Config, get_steps, fun(some_action, _) ->
        Self ! get_steps,
        []
    end);

init_per_testcase(Case, Config) when
    Case =:= service_should_execute_steps;
    Case =:= service_should_notify_caller ->
    [Node | _] = Nodes = ?config(onepanel_nodes, Config),
    [Host1, Host2 | _] = Hosts =
        rpc:call(Node, onepanel_utils, nodes_to_hosts, [Nodes]),
    Self = self(),
    mock_service(Config, get_steps, fun(some_action, _) ->
        [
            #step{hosts = [Host1], function = step1},
            #step{hosts = [Host2], function = step2},
            #step{hosts = Hosts, function = step3}
        ]
    end),
    lists:foreach(fun(Step) ->
        mock_service(Config, Step, fun(_) ->
            Self ! {node(), Step}
        end)
    end, [step1, step2, step3]),
    Config;

init_per_testcase(service_get_steps_should_pass_errors, Config) ->
    mock_service(Config, get_steps, fun(some_action, _) ->
        meck:exception(throw, get_steps_failure)
    end);

init_per_testcase(service_action_should_pass_errors, Config) ->
    [_, Node2 | _] = Nodes = ?config(onepanel_nodes, Config),
    Hosts = rpc:call(Node2, onepanel_utils, nodes_to_hosts, [Nodes]),
    mock_service(Config, get_steps, fun(some_action, _) ->
        [#step{hosts = Hosts, function = some_step}]
    end),
    mock_service(Config, some_step, fun(_) ->
        case node() of
            Node2 -> meck:exception(throw, step_failure);
            _ -> ok
        end
    end),
    Config;

init_per_testcase(_Case, Config) ->
    Config.


end_per_testcase(service_should_be_not_found, _Config) ->
    ok;

end_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_unload(Nodes, service_example).

%%%===================================================================
%%% Internal functions
%%%===================================================================


mock_service(Config, Function, Expectation) ->
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_new(Nodes, service_example, [non_strict]),
    test_utils:mock_expect(Nodes, service_example, Function, Expectation),
    Config.
