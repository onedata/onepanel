%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of 'rest_oneprovider' module.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_oneprovider_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/models.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% tests
-export([
    method_should_return_unauthorized_error/1,
    method_should_return_forbidden_error/1,
    get_should_return_provider_details/1,
    put_should_register_provider/1,
    patch_should_modify_provider_details/1,
    delete_should_unregister_provider/1,
    get_should_return_supported_spaces/1,
    put_should_create_or_support_space/1,
    get_should_return_space_details/1,
    delete_should_revoke_space_support/1
]).

-define(ADMIN_USER_NAME, <<"admin1">>).
-define(ADMIN_USER_PASSWORD, <<"Admin1Password">>).
-define(REG_USER_NAME, <<"user1">>).
-define(REG_USER_PASSWORD, <<"User1Password">>).
-define(TIMEOUT, timer:seconds(5)).

-define(COMMON_ENDPOINTS_WITH_METHODS, [
    {<<"/provider">>, get},
    {<<"/provider">>, post},
    {<<"/provider">>, patch},
    {<<"/provider">>, delete},
    {<<"/provider/spaces">>, get},
    {<<"/provider/spaces">>, post},
    {<<"/provider/spaces/someSpaceId">>, get},
    {<<"/provider/spaces/someSpaceId">>, delete}
]).

-define(PROVIDER_DETAILS_JSON, [
    {<<"id">>, <<"someId">>}, {<<"name">>, <<"someName">>},
    {<<"redirectionPoint">>, <<"someUrl">>}, {<<"urls">>, [<<"someUrl">>]},
    {<<"geoLatitude">>, 10.0}, {<<"geoLongitude">>, 20.0}
]).

-define(SPACE_JSON, [{<<"id">>, <<"someId1">>}]).

-define(SPACES_JSON, [
    {<<"ids">>, [<<"someId1">>, <<"someId2">>, <<"someId3">>]}
]).

-define(SPACE_DETAILS_JSON, [
    {<<"id">>, <<"someId">>}, {<<"name">>, <<"someName">>},
    {<<"supportingProviders">>, [
        {<<"someId1">>, 1024}, {<<"someId2">>, 2048}, {<<"someId3">>, 4096}
    ]}
]).

-define(run(Config, Function), Function(hd(?config(oneprovider_hosts, Config)))).

all() ->
    ?ALL([
        method_should_return_unauthorized_error,
        method_should_return_forbidden_error,
        get_should_return_provider_details,
        put_should_register_provider,
        patch_should_modify_provider_details,
        delete_should_unregister_provider,
        get_should_return_supported_spaces,
        put_should_create_or_support_space,
        get_should_return_space_details,
        delete_should_revoke_space_support
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_unauthorized_error(Config) ->
    ?run(Config, fun(Host) ->
        lists:foreach(fun({Endpoint, Method}) ->
            ?assertMatch({ok, 401, _, _}, onepanel_test_rest:noauth_request(
                Host, Endpoint, Method
            )),
            ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
                Host, Endpoint, Method, {<<"someUser">>, <<"somePassword">>}
            ))
        end, ?COMMON_ENDPOINTS_WITH_METHODS)
    end).


method_should_return_forbidden_error(Config) ->
    ?run(Config, fun(Host) ->
        lists:foreach(fun({Endpoint, Method}) ->
            ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
                Host, Endpoint, Method, {?REG_USER_NAME, ?REG_USER_PASSWORD}
            ))
        end, ?COMMON_ENDPOINTS_WITH_METHODS)
    end).


get_should_return_provider_details(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?PROVIDER_DETAILS_JSON)
    end).


put_should_register_provider(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider">>, post,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, [
                {<<"name">>, <<"someName">>},
                {<<"redirectionPoint">>, <<"someUrl">>},
                {<<"geoLongitude">>, 10.0},
                {<<"geoLatitude">>, 20.0},
                {<<"onezoneDomainName">>, <<"someDomain">>}
            ]
        )),
        ?assertReceivedMatch({service, oneprovider, register, #{
            onezone_domain := <<"someDomain">>,
            oneprovider_name := <<"someName">>,
            oneprovider_redirection_point := <<"someUrl">>,
            oneprovider_geo_latitude := 20.0,
            oneprovider_geo_longitude := 10.0
        }}, ?TIMEOUT)
    end).


patch_should_modify_provider_details(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider">>, patch,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, [
                {<<"name">>, <<"someName">>},
                {<<"redirectionPoint">>, <<"someUrl">>},
                {<<"geoLongitude">>, 10.0},
                {<<"geoLatitude">>, 20.0}
            ]
        )),
        ?assertReceivedMatch({service, oneprovider, modify_details, #{
            oneprovider_name := <<"someName">>,
            oneprovider_redirection_point := <<"someUrl">>,
            oneprovider_geo_latitude := 20.0,
            oneprovider_geo_longitude := 10.0
        }}, ?TIMEOUT)
    end).


delete_should_unregister_provider(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider">>, delete,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
        )),
        ?assertReceivedMatch({service, oneprovider, unregister, #{}}, ?TIMEOUT)
    end).


get_should_return_supported_spaces(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACES_JSON)
    end).


put_should_create_or_support_space(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces">>, post,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, [
                    {<<"token">>, <<"someToken">>},
                    {<<"size">>, 1024},
                    {<<"storageId">>, <<"someId">>}
                ]
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACE_JSON)
    end).


get_should_return_space_details(Config) ->
    ?run(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/spaces/someId">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, ?SPACE_DETAILS_JSON)
    end).


delete_should_revoke_space_support(Config) ->
    ?run(Config, fun(Host) ->
        ?assertMatch({ok, 204, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/provider/spaces/someId">>, delete,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
        )),
        ?assertReceivedMatch({service, oneprovider, revoke_space_support,
            #{id := <<"someId">>}
        }, ?TIMEOUT)
    end).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(etls),
    hackney:start(),
    Posthook = fun(NewConfig) ->
        NewConfig2 = onepanel_test_utils:init(NewConfig),
        ?assertAllMatch({ok, _}, ?callAll(NewConfig2, onepanel_user, create,
            [?REG_USER_NAME, ?REG_USER_PASSWORD, regular]
        )),
        ?assertAllMatch({ok, _}, ?callAll(NewConfig2, onepanel_user, create,
            [?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD, admin]
        )),
        NewConfig2
    end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(get_should_return_provider_details, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, get_details, {
            [{'node@host1', ?PROVIDER_DETAILS_JSON}], []
        }},
        {task_finished, {service, action, ok}}
    ] end),
    NewConfig;

init_per_testcase(get_should_return_supported_spaces, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, get_spaces, {
            [{'node@host1', ?SPACES_JSON}], []
        }},
        {task_finished, {service, action, ok}}
    ] end),
    NewConfig;

init_per_testcase(get_should_return_space_details, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, get_space_details, {
            [{'node@host1', ?SPACE_DETAILS_JSON}], []
        }},
        {task_finished, {service, action, ok}}
    ] end),
    NewConfig;

init_per_testcase(put_should_create_or_support_space, Config) ->
    NewConfig = init_per_testcase(default, Config),
    Nodes = ?config(oneprovider_nodes, Config),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(_, _, _) -> [
        {service_oneprovider, support_space, {
            [{'node@host1', ?SPACE_JSON}], []
        }},
        {task_finished, {service, action, ok}}
    ]
    end),
    NewConfig;

init_per_testcase(_Case, Config) ->
    Nodes = ?config(oneprovider_nodes, Config),
    Self = self(),
    test_utils:mock_new(Nodes, service),
    test_utils:mock_expect(Nodes, service, get, fun(oneprovider) ->
        {ok, #service{ctx = #{registered => true}}}
    end),
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {service, action, ok}}]
    end),
    Config.


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes).