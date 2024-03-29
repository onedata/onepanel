%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of 'service' module.
%%% @end
%%%--------------------------------------------------------------------
-module(service_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("onepanel_test_utils.hrl").
-include("service.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    service_should_be_not_found/1,
    service_should_request_action_steps/1, service_should_execute_steps/1,
    service_should_notify_caller/1, service_get_steps_error_test/1,
    service_action_error_test/1
]).

all() ->
    ?ALL([
        service_should_be_not_found,
        service_should_request_action_steps, service_should_execute_steps,
        service_should_notify_caller, service_get_steps_error_test,
        service_action_error_test
    ]).

-define(TIMEOUT, timer:seconds(10)).

%%%===================================================================
%%% Test functions
%%%===================================================================

service_should_be_not_found(Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    lists:foreach(fun(Node) ->
        ?assertMatch(?ERROR_INTERNAL_SERVER_ERROR,
            rpc:call(Node, service, apply, [example, some_action, #{}]))
    end, Nodes).


service_should_request_action_steps(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    onepanel_test_utils:service_action(Node, example, some_action, #{}),
    ?assertReceivedEqual(get_steps, ?TIMEOUT).


service_should_execute_steps(Config) ->
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    onepanel_test_utils:service_action(Node1,
        example, some_action, #{}),
    ?assertReceivedEqual({Node1, step1}, ?TIMEOUT),
    ?assertReceivedEqual({Node2, step2}, ?TIMEOUT),
    ?assertReceivedEqual({Node1, step3}, ?TIMEOUT),
    ?assertReceivedEqual({Node2, step3}, ?TIMEOUT).


service_should_notify_caller(Config) ->
    Self = self(),
    [Node | _] = ?config(onepanel_nodes, Config),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [example, some_action, #{}, Self])),
    ?assertReceivedEqual(#action_begin{
        service = example, action = some_action
    }, ?TIMEOUT),
    lists:foreach(fun(Step) ->
        ?assertReceivedEqual(#step_begin{
            module = service_example, function = Step
        }, ?TIMEOUT),
        ?assertReceivedMatch(#step_end{
            module = service_example, function = Step,
            good_bad_results = {[_ | _], []}
        }, ?TIMEOUT)
    end, [step1, step2, step3]),
    ?assertReceivedEqual(#action_end{
        service = example, action = some_action, result = ok
    }, ?TIMEOUT).


service_get_steps_error_test(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    ?assertMatch(?ERROR_NOT_SUPPORTED,
        rpc:call(Node, service, apply, [example, some_action, #{}])).


service_action_error_test(Config) ->
    Self = self(),
    [Node1, Node2 | _] = ?config(onepanel_nodes, Config),
    ?assertMatch({error, _}, rpc:call(Node1, service, apply,
        [example, some_action, #{}, Self])),
    ?assertReceivedMatch(#step_end{
        module = service_example, function = some_step,
        good_bad_results = {[{Node1, ok}], [{Node2, {error, step_failure}}]}
    }, ?TIMEOUT).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    Posthook = fun(NewConfig) -> onepanel_test_utils:init(NewConfig) end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].

init_per_testcase(service_should_request_action_steps, Config) ->
    Self = self(),
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_new(Nodes, service_example, [non_strict]),
    test_utils:mock_expect(Nodes, service_example, get_steps, fun(some_action, _) ->
        Self ! get_steps,
        []
    end),
    Config;

init_per_testcase(Case, Config) when
    Case =:= service_should_execute_steps;
    Case =:= service_should_notify_caller
->
    Nodes = ?config(onepanel_nodes, Config),
    [Host1, Host2 | _] = Hosts = hosts:from_nodes(Nodes),
    Self = self(),
    test_utils:mock_new(Nodes, service_example, [non_strict]),
    test_utils:mock_expect(Nodes, service_example, get_steps, fun(some_action, _) ->
        [
            #step{hosts = [Host1], function = step1},
            #step{hosts = [Host2], function = step2},
            #step{hosts = Hosts, function = step3}
        ]
    end),
    lists:foreach(fun(Step) ->
        test_utils:mock_expect(Nodes, service_example, Step, fun(_) ->
            Self ! {node(), Step}
        end)
    end, [step1, step2, step3]),
    Config;

init_per_testcase(service_get_steps_error_test, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_new(Nodes, service_example, [non_strict]),
    test_utils:mock_expect(Nodes, service_example, get_steps, fun(some_action, _) ->
        meck:exception(throw, ?ERROR_NOT_SUPPORTED)
    end),
    Config;

init_per_testcase(service_action_error_test, Config) ->
    [_, Node2 | _] = Nodes = ?config(onepanel_nodes, Config),
    Hosts = hosts:from_nodes(Nodes),
    test_utils:mock_new(Nodes, service_example, [non_strict]),
    test_utils:mock_expect(Nodes, service_example, get_steps, fun(some_action, _) ->
        [#step{hosts = Hosts, function = some_step}]
    end),
    test_utils:mock_expect(Nodes, service_example, some_step, fun(_) ->
        case node() of
            Node2 -> meck:exception(throw, {error, step_failure});
            _ -> ok
        end
    end),
    Config;

init_per_testcase(_Case, Config) ->
    Config.


end_per_testcase(service_should_be_not_found, Config) ->
    Config;

end_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_unload(Nodes).

end_per_suite(_Config) ->
    ok.