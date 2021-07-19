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
    method_should_return_unauthorized_error/1,
    method_should_return_forbidden_error/1,
    method_should_return_service_unavailable_error/1,
    bad_gui_message_id_should_return_not_found/1,
    get_should_return_gui_message/1,
    patch_should_update_gui_message/1
]).

all() ->
    ?ALL([
        method_should_return_unauthorized_error,
        method_should_return_forbidden_error,
        method_should_return_service_unavailable_error,
        bad_gui_message_id_should_return_not_found,
        get_should_return_gui_message,
        patch_should_update_gui_message
    ]).

-define(TIMEOUT, timer:seconds(5)).

-define(COMMON_ENDPOINTS_WITH_METHODS, [
    {<<"/zone/nagios">>, get},
    {<<"/zone/users">>, get},
    {<<"/zone/users">>, post},
    {<<"/zone/users/someUserId">>, get},
    {<<"/zone/users/someUserId">>, patch},
    {<<"/zone/policies">>, get},
    {<<"/zone/policies">>, patch},
    {<<"/zone/cluster_ips">>, get},
    {<<"/zone/cluster_ips">>, patch},
    {<<"/zone/gui_messages/privacy_policy">>, get},
    {<<"/zone/gui_messages/privacy_policy">>, patch}
]).

-define(GOOD_GUI_MESSAGE_ID, (<<"privacy_policy">>)).
-define(BAD_GUI_MESSAGE_ID, (<<"nonexistent_gui_message">>)).
-define(GUI_MESSAGE_ENABLED, true).
-define(GUI_MESSAGE_BODY, (<<"some html">>)).

%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_unauthorized_error(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        lists:foreach(fun(Auth) ->
            ?assertMatch({ok, ?HTTP_401_UNAUTHORIZED, _, _}, onepanel_test_rest:auth_request(
                Host, Endpoint, Method, Auth
            ))
        end, ?INCORRECT_AUTHS() ++ ?NONE_AUTHS())
    end, ?COMMON_ENDPOINTS_WITH_METHODS).


method_should_return_forbidden_error(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        % highest rights which still should not grant access to these endpoints
        Auths = ?PEER_AUTHS(Host) ++ case {Endpoint, Method} of
            {_, get} ->
                [];
            _ ->
                ?OZ_AUTHS(Host, privileges:cluster_admin() -- [?CLUSTER_UPDATE])
        end,

        ?assertMatch({ok, ?HTTP_403_FORBIDDEN, _, _}, onepanel_test_rest:auth_request(
            Host, Endpoint, Method, Auths
        ))
    end, ?COMMON_ENDPOINTS_WITH_METHODS).


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


bad_gui_message_id_should_return_not_found(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        ?assertMatch({ok, ?HTTP_404_NOT_FOUND, _, _},
            onepanel_test_rest:auth_request(
                Host, Endpoint, Method, ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE])
            )
        )
    end, [
        {<<"/zone/gui_messages/", ?BAD_GUI_MESSAGE_ID/binary>>, get},
        {<<"/zone/gui_messages/", ?BAD_GUI_MESSAGE_ID/binary>>, patch}
    ]).


get_should_return_gui_message(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/zone/gui_messages/", ?GOOD_GUI_MESSAGE_ID/binary>>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, #{
            <<"body">> => ?GUI_MESSAGE_BODY, <<"enabled">> => ?GUI_MESSAGE_ENABLED
        })
    end).


patch_should_update_gui_message(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/zone/gui_messages/", ?GOOD_GUI_MESSAGE_ID/binary>>, patch,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
                #{<<"body">> => ?GUI_MESSAGE_BODY, <<"enabled">> => true}
            )
        ),
        ?assertReceivedNextEqual({service, onezone, update_gui_message, #{
            message_id => ?GOOD_GUI_MESSAGE_ID, body => ?GUI_MESSAGE_BODY,
            enabled => ?GUI_MESSAGE_ENABLED
        }}, ?TIMEOUT)
    end).


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


init_per_testcase(Case, Config) when
    Case == method_should_return_forbidden_error;
    Case == method_should_return_unauthorized_error
->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, Config),
    % do not require valid payload in requests
    test_utils:mock_new(Nodes, [onepanel_parser]),
    test_utils:mock_expect(Nodes, onepanel_parser, parse, fun(_, _) -> #{} end),
    NewConfig;


init_per_testcase(method_should_return_service_unavailable_error, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_expect(Nodes, service, all_healthy, fun() -> false end),
    % do not require valid payload in requests
    test_utils:mock_new(Nodes, [onepanel_parser]),
    test_utils:mock_expect(Nodes, onepanel_parser, parse, fun(_, _) -> #{} end),
    NewConfig;


init_per_testcase(Case, Config) when
    Case == bad_gui_message_id_should_return_not_found;
    Case == get_should_return_gui_message;
    Case == patch_should_update_gui_message
->
    Self = self(),
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_expect(Nodes, oz_worker_rpc, gui_message_exists,
        fun(MessageId) -> MessageId == ?GOOD_GUI_MESSAGE_ID end),
    test_utils:mock_expect(Nodes, oz_worker_rpc, gui_message_exists,
        fun(_Node, MessageId) -> MessageId == ?GOOD_GUI_MESSAGE_ID end),

    Result = #{body => ?GUI_MESSAGE_BODY, enabled => ?GUI_MESSAGE_ENABLED},
    test_utils:mock_expect(Nodes, service, apply_sync, fun
        (_, get_gui_message, _) ->
            [
                #step_end{module = service_onezone, function = get_gui_message,
                    good_bad_results = {[{'node@host1', Result}], []}},
                #action_end{service = service, action = action, result = ok}
            ];
        (Service, Action, Ctx) ->
            Self ! {service, Service, Action, Ctx},
            [#action_end{service = service, action = action, result = ok}]
    end),
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
        (_) -> ?ERR_DOC_NOT_FOUND
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
    test_utils:mock_unload(Nodes).


end_per_suite(_Config) ->
    ok.

