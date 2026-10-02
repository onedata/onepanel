%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of 'rest_onezone' module.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_onezone_test_SUITE).
-author("Wojciech Geisler").

-include("authentication.hrl").
-include("deployment_progress.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("onepanel_test_rest.hrl").
-include("onepanel_test_utils.hrl").
-include("service.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").


%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2]).

%% tests
-export([
    method_should_return_service_unavailable_error/1
]).

all() ->
    ?ALL([
        method_should_return_service_unavailable_error
    ]).

-define(TIMEOUT, timer:seconds(5)).

-define(COMMON_ENDPOINTS_WITH_METHODS, [
    {<<"/zone/nagios">>, get},
    {<<"/zone/users">>, get},
    {<<"/zone/cluster_ips">>, get},
    {<<"/zone/cluster_ips">>, patch},
    {<<"/zone/gui_messages/privacy_policy">>, get},
    {<<"/zone/gui_messages/privacy_policy">>, patch}
]).

%%%===================================================================
%%% Test functions
%%%===================================================================


method_should_return_service_unavailable_error(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        ?assertMatch({ok, ?HTTP_503_SERVICE_UNAVAILABLE, _, _},
            onepanel_test_rest:auth_request(
                Host, Endpoint, Method,
                ?OZ_OR_ROOT_AUTHS(Host, privileges:cluster_admin())
            ))
    end, lists:subtract(?COMMON_ENDPOINTS_WITH_METHODS, [
        {<<"/zone/cluster_ips">>, get}

    ])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    Posthook = fun(NewConfig) ->
        NewConfig2 = onepanel_test_utils:init(NewConfig),
        onepanel_test_rest:set_default_passphrase(NewConfig2),
        NewConfig2
    end,
    [{?LOAD_MODULES, [onepanel_test_rest]}, {?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(method_should_return_service_unavailable_error, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_expect(Nodes, service, all_healthy_ignoring_ones3, fun() -> false end),
    % do not require valid payload in requests
    test_utils:mock_new(Nodes, [onepanel_parser]),
    test_utils:mock_expect(Nodes, onepanel_parser, parse, fun(_, _) -> #{} end),
    NewConfig;


init_per_testcase(_Case, Config) ->
    Nodes = ?config(onezone_nodes, Config),
    Hosts = ?config(onezone_hosts, Config),
    Domain = onepanel_test_utils:get_domain(hd(Hosts)),
    Self = self(),
    ?call(Config, onepanel_deployment, set_marker,
        [[?PROGRESS_READY, ?PROGRESS_CLUSTER]]),

    test_utils:mock_new(Nodes, [service, service_oz_worker, service_onezone,
        oz_worker_rpc]),
    test_utils:mock_expect(Nodes, service, exists, fun
        (onezone) -> true; (oz_worker) -> true
    end),
    test_utils:mock_expect(Nodes, service, exists, fun
        (onezone) -> true; (oz_worker) -> true
    end),
    test_utils:mock_expect(Nodes, service, get, fun
        (onezone) -> {ok, #service{}};
        (oz_worker) -> {ok, #service{hosts = Hosts}};
        (_) -> ?ONP_ERR_DOC_NOT_FOUND
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [
            % satisfy fetch_entity
            #step_end{module = onezone_users, function = get_user,
                good_bad_results = {[{'node@host1', #{}}], []}},
            #action_end{service = service, action = action, result = ok}
        ]
    end),
    test_utils:mock_expect(Nodes, service_oz_worker, get_domain, fun
        () -> Domain
    end),
    ok = onepanel_test_rest:mock_token_authentication(Nodes),

    Config.

end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes, [
        onepanel_parser, service, service_oz_worker, service_onezone, oz_worker_rpc
    ]).


end_per_suite(_Config) ->
    ok.

