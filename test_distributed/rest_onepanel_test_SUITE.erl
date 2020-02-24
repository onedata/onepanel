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
-include("authentication.hrl").
-include("onepanel_test_utils.hrl").
-include("onepanel_test_rest.hrl").
-include_lib("ctool/include/aai/caveats.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/logging.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2,
    end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    method_should_return_unauthorized_error/1,
    api_caveats_should_restrict_available_endpoints/1,
    noauth_method_should_return_unauthorized_error/1,
    method_should_return_forbidden_error/1,
    method_should_return_not_found_error/1,
    noauth_get_should_return_password_status/1,
    noauth_put_should_set_emergency_passphrase/1,
    put_should_update_emergency_passphrase/1,
    passphrase_update_requires_previous_passphrase/1,
    get_as_admin_should_return_hosts/1,
    get_as_admin_should_return_cookie/1,
    get_should_return_node_details/1,
    post_as_admin_should_return_invite_token/1,
    post_as_admin_should_extend_cluster_and_return_hostname/1,
    unauthorized_post_should_join_cluster/1,
    delete_as_admin_should_remove_node_from_cluster/1,
    delete_as_admin_should_fail_if_node_is_used/1
]).

-define(COOKIE, someCookie).
-define(CLUSTER_HOST_HOSTNAME, "known.hostname").
-define(NEW_HOST_HOSTNAME, "someHostname").
-define(TIMEOUT, timer:seconds(5)).

all() ->
    ?ALL([
        method_should_return_unauthorized_error,
        api_caveats_should_restrict_available_endpoints,
        noauth_method_should_return_unauthorized_error,
        method_should_return_forbidden_error,
        method_should_return_not_found_error,
        noauth_get_should_return_password_status,
        noauth_put_should_set_emergency_passphrase,
        put_should_update_emergency_passphrase,
        passphrase_update_requires_previous_passphrase,
        get_as_admin_should_return_hosts,
        get_as_admin_should_return_cookie,
        get_should_return_node_details,
        post_as_admin_should_return_invite_token,
        post_as_admin_should_extend_cluster_and_return_hostname,
        unauthorized_post_should_join_cluster,
        delete_as_admin_should_remove_node_from_cluster,
        delete_as_admin_should_fail_if_node_is_used
    ]).

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
    end, [
        {<<"/cookie">>, get},
        {<<"/hosts/someHost">>, delete},
        {<<"/hosts">>, get},
        {<<"/hosts">>, post},
        {<<"/invite_tokens">>, post},
        {<<"/web_cert">>, get},
        {<<"/web_cert">>, patch},
        {<<"/progress">>, get},
        {<<"/progress">>, patch}
    ]).


api_caveats_should_restrict_available_endpoints(Config) ->
    Caveat = #cv_api{whitelist = [
        {?OP_PANEL, get, #gri_pattern{type = 'onp_panel', id='*', aspect = 'cookie'}}
    ]},
    Token = onepanel_test_rest:construct_token([Caveat]),
    ?assertMatch({ok, ?HTTP_200_OK, _, _},
        onepanel_test_rest:auth_request(
            Config, <<"/cookie">>, get, {token, Token}
        )),
    ?assertMatch({ok, ?HTTP_401_UNAUTHORIZED, _, _},
        onepanel_test_rest:auth_request(
            % sample endpoint - all have common authorization code
            Config, <<"/progress">>, get, {token, Token}
        )).


noauth_method_should_return_unauthorized_error(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        lists:foreach(fun(Auth) ->
            ?assertMatch({ok, ?HTTP_401_UNAUTHORIZED, _, _}, onepanel_test_rest:auth_request(
                Host, Endpoint, Method, Auth
            ))
        end, ?INCORRECT_AUTHS() ++ ?NONE_AUTHS())
    end, [
        % endpoints forbidden without auth when emergency passphrase IS set
        {<<"/emergency_passphrase">>, put},
        {<<"/join_cluster">>, post}
    ]).


method_should_return_forbidden_error(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        Auths = case {Endpoint, Method} of
            {<<"/emergency_passphrase">>, put} ->
                % even admin coming from Onezone cannot change root password
                ?OZ_AUTHS(Config, privileges:cluster_admin());
            _ ->
                ?OZ_AUTHS(Config, privileges:cluster_admin() -- [?CLUSTER_UPDATE])
        end,
        ?assertMatch({ok, ?HTTP_403_FORBIDDEN, _, _}, onepanel_test_rest:auth_request(
            Host, Endpoint, Method, Auths
        ))
    end, [
        {<<"/hosts/someHost">>, delete},
        {<<"/web_cert">>, patch},
        {<<"/progress">>, patch},
        {<<"/emergency_passphrase">>, put}
    ]).


method_should_return_not_found_error(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        ?assertMatch({ok, ?HTTP_404_NOT_FOUND, _, _}, onepanel_test_rest:auth_request(
            Host, Endpoint, Method,
            ?OZ_OR_ROOT_AUTHS(Config, [?CLUSTER_UPDATE])
        ))
    end, [{<<"/hosts/someHost">>, delete}]).


noauth_get_should_return_password_status(Config) ->
    {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
        onepanel_test_rest:noauth_request(Config, <<"/emergency_passphrase">>, get)
    ),
    Expected = #{<<"isSet">> => true},
    onepanel_test_rest:assert_body(JsonBody, Expected).


noauth_put_should_set_emergency_passphrase(Config) ->
    NewPassphrase = <<"newPassphrase">>,

    ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:noauth_request(
        Config, "/emergency_passphrase", put,
        #{<<"newPassphrase">> => NewPassphrase}
    )).


put_should_update_emergency_passphrase(Config) ->
    OldPassphrase = ?EMERGENCY_PASSPHRASE,
    NewPassphrase = <<"newPassphrase">>,
    Auth = onepanel_test_rest:obtain_local_token(Config, OldPassphrase),

    ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
        Config, "/emergency_passphrase", put, Auth, #{
            <<"currentPassphrase">> => OldPassphrase,
            <<"newPassphrase">> => NewPassphrase
        }
    )),

    % ensure new password works
    ?assertMatch({token, _},
        onepanel_test_rest:obtain_local_token(Config, NewPassphrase)).


passphrase_update_requires_previous_passphrase(Config) ->
    CorrectAuths = ?ROOT_AUTHS(Config),
    IncorrectPassphrase = <<"IncorrectPassphrase">>,

    {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_401_UNAUTHORIZED, _, _}, onepanel_test_rest:auth_request(
        Config, "/emergency_passphrase", put, CorrectAuths, #{
            <<"currentPassphrase">> => IncorrectPassphrase,
            <<"newPassphrase">> => <<"willNotBeSet">>
        }
    )),
    onepanel_test_rest:assert_body(JsonBody,
        #{<<"error">> => errors:to_json(?ERROR_BAD_BASIC_CREDENTIALS)}),
    ?assertMatch({ok, ?HTTP_401_UNAUTHORIZED, _, _}, onepanel_test_rest:auth_request(
        Config, "/emergency_passphrase", put, CorrectAuths, #{
            <<"newPassphrase">> => <<"willNotBeSet">>
        }
    )).



get_as_admin_should_return_hosts(Config) ->
    {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
        onepanel_test_rest:auth_request(Config, <<"/hosts">>, get,
            ?OZ_OR_ROOT_AUTHS(Config, [])
        )
    ),
    Hosts = onepanel_utils:get_converted(cluster_hosts, Config, {seq, binary}),
    onepanel_test_rest:assert_body(JsonBody, Hosts).


get_as_admin_should_return_cookie(Config) ->
    {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
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
    {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
        onepanel_test_rest:auth_request(Config, <<"/node">>, get,
            ?NONE_AUTHS() ++ ?OZ_OR_ROOT_AUTHS(Config, []))
    ),
    onepanel_test_rest:assert_body(JsonBody, Expected).


post_as_admin_should_return_invite_token(Config) ->
    [Node | _] = ?config(onepanel_nodes, Config),
    InviteTokens = lists:map(fun(Auth) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(Config, <<"/invite_tokens">>, post, Auth)
        ),
        #{<<"inviteToken">> := InviteToken} = json_utils:decode(JsonBody),
        Nonce = rpc:call(Node, invite_tokens, get_nonce, [InviteToken]),
        ?assert(rpc:call(Node, authorization_nonce, verify, [Nonce])),
        InviteToken
    end, ?OZ_OR_ROOT_AUTHS(Config, [])),

    ?assertEqual(lists:sort(InviteTokens), lists:usort(InviteTokens)).


post_as_admin_should_extend_cluster_and_return_hostname(Config) ->
    Auths = ?OZ_OR_ROOT_AUTHS(Config, [?CLUSTER_UPDATE]),
    {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
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
    TokenDescBin = base64:encode(json_utils:encode(#{
        <<"nonce">> => <<"someNonce">>,
        <<"clusterHost">> => <<"someHost">>
    })),
    DummyInviteToken = onepanel_utils:join(
        [?ONEPANEL_INVITE_TOKEN_PREFIX, TokenDescBin],
        <<?ONEPANEL_TOKEN_SEPARATOR>>
    ),
    ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:noauth_request(
        Config, "/join_cluster", post, #{inviteToken => DummyInviteToken}
    )),
    ?assertReceivedMatch({service, onepanel, join_cluster,
        #{cluster_host := "someHost", invite_token := DummyInviteToken}
    }, ?TIMEOUT).


delete_as_admin_should_remove_node_from_cluster(Config) ->
    ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
        Config, "/hosts/" ++ ?CLUSTER_HOST_HOSTNAME, delete,
        ?OZ_OR_ROOT_AUTHS(Config, [?CLUSTER_UPDATE])
    )),
    ?assertReceivedMatch({service, onepanel, leave_cluster,
        #{hosts := [?CLUSTER_HOST_HOSTNAME]}}, ?TIMEOUT).


delete_as_admin_should_fail_if_node_is_used(Config) ->
    {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_400_BAD_REQUEST, _, _},
        onepanel_test_rest:auth_request(
        Config, "/hosts/" ++ ?CLUSTER_HOST_HOSTNAME, delete,
        ?OZ_OR_ROOT_AUTHS(Config, [?CLUSTER_UPDATE])
    )),
    onepanel_test_rest:assert_body(JsonBody,
        #{<<"error">> => errors:to_json(?ERROR_NOT_SUPPORTED)}).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) -> onepanel_test_utils:init(NewConfig) end,
    [{?LOAD_MODULES, [onepanel_test_rest]}, {?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(method_should_return_unauthorized_error, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_new(Nodes, [service, host_middleware, onepanel_parser]),
    test_utils:mock_expect(Nodes, service, is_healthy, fun(_) -> true end),
    test_utils:mock_expect(Nodes, service, all_healthy, fun() -> true end),
    test_utils:mock_expect(Nodes, host_middleware, fetch_entity, fun
        (_) -> {ok, {undefined, 1}}
    end),
    % do not require valid payload in requests
    test_utils:mock_expect(Nodes, onepanel_parser, parse, fun(_, _) -> #{} end),
    init_per_testcase(default, Config);

init_per_testcase(delete_as_admin_should_remove_node_from_cluster, Config) ->
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


init_per_testcase(delete_as_admin_should_fail_if_node_is_used , Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_new(Nodes, [service_onepanel]),
    test_utils:mock_expect(Nodes, service_onepanel, get_hosts, fun() ->
        [?CLUSTER_HOST_HOSTNAME | hosts:from_nodes(Nodes)]
    end),
    test_utils:mock_expect(Nodes, service_onepanel, is_host_used,
        fun(_) -> true end),
    init_per_testcase(default, Config);

init_per_testcase(post_as_admin_should_extend_cluster_and_return_hostname, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    test_utils:mock_new(Nodes, [service, service_onepanel, service_oneprovider],
        [passthrough]),
    test_utils:mock_expect(Nodes, service_onepanel, extend_cluster, fun
        (#{hostname := Hostname}) -> #{hostname => Hostname};
        (_Ctx) -> #{hostname => <<?NEW_HOST_HOSTNAME>>} end),
    % it should work even in presence of some deployed services
    test_utils:mock_new(Nodes, [service_oneprovider]),
    test_utils:mock_expect(Nodes, service_oneprovider, get_hosts,
        fun() -> [hosts:self()] end),

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

init_per_testcase(Case, Config) when
    Case == method_should_return_forbidden_error;
    Case == noauth_method_should_return_unauthorized_error
->
    Nodes = ?config(onepanel_nodes, Config),
    Hosts = hosts:from_nodes(Nodes),
    test_utils:mock_new(Nodes, [service_onepanel, onepanel_parser]),
    test_utils:mock_expect(Nodes, service_onepanel, get_hosts,
        fun() -> ["someHost" | Hosts] end),
    % do not require valid payload in requests
    test_utils:mock_expect(Nodes, onepanel_parser, parse, fun(_, _) -> #{} end),
    init_per_testcase(default, Config);

init_per_testcase(noauth_put_should_set_emergency_passphrase , Config) ->
    ?call(Config, model, clear, [onepanel_kv]),
    Config;

init_per_testcase(api_caveats_should_restrict_available_endpoints, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    % mocks for onezone_tokens:authenticate_user/3 to succeed
    test_utils:mock_new(Nodes, [oz_endpoint, service_oneprovider]),
    test_utils:mock_expect(Nodes, service_oneprovider, get_id,
        fun() -> <<"some-id">> end),
    test_utils:mock_expect(Nodes, service_oneprovider, get_identity_token,
        fun() -> <<"some-identity-token">> end),
    test_utils:mock_expect(Nodes, oz_endpoint, request, fun
        (_Auth, "/tokens/verify_access_token", post, _) ->
            ?info("verifying access token using auth ~p", [_Auth]),
            {ok, 200, #{}, json_utils:encode(#{
                subject => aai:subject_to_json(?SUB(user, <<"userId">>)),
                ttl => 3600
            })};
        (_Auth, "/clusters/" ++ _, get, _) ->
            % assume privileges check
            {ok, 200, #{}, json_utils:encode(#{
                <<"privileges">> => privileges:cluster_admin()
            })}
    end),
    test_utils:mock_expect(Nodes, oz_endpoint, request, fun
        (_, "/user", get) ->
            ?info("fetching user details"),
            {ok, 200, #{}, json_utils:encode(#{
                <<"userId">> => <<"userId">>,
                <<"name">> => <<"someUserName">>,
                <<"username">> => <<"someUserName">>,
                <<"linkedAccounts">> => [], <<"emails">> => []
            })}
    end),
    Config;

init_per_testcase(_Case, Config) ->
    Nodes = ?config(onepanel_nodes, Config),
    onepanel_test_rest:set_default_passphrase(Config),
    onepanel_test_rest:mock_token_authentication(Config),
    [{cluster_hosts, hosts:from_nodes(Nodes)} | Config].


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes),
    lists:foreach(fun(Model) ->
        ?callAll(Config, model, clear, [Model])
    end, [onepanel_user, onepanel_kv]).


end_per_suite(_Config) ->
    ok.
