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
-include_lib("ctool/include/aai/caveats.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2,
    end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    method_should_return_unauthorized_error/1,
    token_with_api_caveats_should_return_unauthorized_error/1,
    noauth_method_should_return_forbidden_error/1,
    method_should_return_forbidden_error/1,
    method_should_return_not_found_error/1,
    noauth_get_should_return_password_status/1,
    noauth_put_should_set_emergency_passphrase/1,
    put_should_update_emergency_passphrase/1,
    passphrase_update_requires_previous_passphrase/1,
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
                ct:pal("Failed on: ~s ~s", [_Method, _Endpoint]),
                erlang:error(_Reason)
        end
    end, EndpointsWithMethods)).

all() ->
    ?ALL([
        method_should_return_unauthorized_error,
        token_with_api_caveats_should_return_unauthorized_error,
        noauth_method_should_return_forbidden_error,
        method_should_return_forbidden_error,
        method_should_return_not_found_error,
        noauth_get_should_return_password_status,
        noauth_put_should_set_emergency_passphrase,
        put_should_update_emergency_passphrase,
        passphrase_update_requires_previous_passphrase,
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
        lists:foreach(fun(Auth) ->
            ?assertMatch({ok, ?HTTP_401_UNAUTHORIZED, _, _}, onepanel_test_rest:auth_request(
                Config, Endpoint, Method, Auth
            ))
        end, ?INCORRECT_AUTHS() ++ ?NONE_AUTHS())
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


token_with_api_caveats_should_return_unauthorized_error(Config) ->
    BadCaveats = [
        #cv_api{whitelist = [{all, all, #gri_pattern{type = '*', aspect = '*'}}]},
        #cv_api{whitelist = [{oz_worker, delete, #gri_pattern{type = '*', aspect = '*'}}]},
        #cv_api{whitelist = [{oz_panel, create, #gri_pattern{type = '*', aspect = '*'}}]},
        #cv_interface{interface = oneclient},
        #cv_data_readonly{},
        #cv_data_path{whitelist = [<<"/260a56d159a980ca0d645dd81/dir/file.txt">>]},
        #cv_data_objectid{whitelist = [<<"901823DEC57846DCFE">>]}
    ],
    % sample good caveats
    GoodCaveats = [
        #cv_time{valid_until = time_utils:system_time_seconds()},
        #cv_ip{whitelist = [{{1,2,3,4}, 32}]}
    ],
    lists:foreach(fun(Caveat) ->
        Caveats = GoodCaveats ++ [Caveat],
        Token = onepanel_test_rest:construct_token(Caveats),
        Expected = #{
            <<"error">> => errors:to_json(?ERROR_TOKEN_CAVEAT_UNVERIFIED(Caveat))
        },
        {ok, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_401_UNAUTHORIZED, _, _},
            onepanel_test_rest:auth_request(
                % sample endpoint - all have common authorization code
                Config, <<"/cookie">>, get, {token, Token}
            )),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end, BadCaveats).


noauth_method_should_return_forbidden_error(Config) ->
    ?run(fun({Endpoint, Method}) ->
        lists:foreach(fun(Auth) ->
            ?assertMatch({ok, ?HTTP_403_FORBIDDEN, _, _}, onepanel_test_rest:auth_request(
                Config, Endpoint, Method, Auth
            ))
        end, ?INCORRECT_AUTHS() ++ ?NONE_AUTHS())
    end, [
        % endpoints forbidden without auth when emergency passphrase IS set
        {<<"/emergency_passphrase">>, put},
        {<<"/join_cluster">>, post}
    ]).


method_should_return_forbidden_error(Config) ->
    ?run(fun({Endpoint, Method}) ->
        Auths = case {Endpoint, Method} of
            {<<"/emergency_passphrase">>, put} ->
                % even admin coming from Onezone cannot change root password
                ?OZ_AUTHS(Config, privileges:cluster_admin());
            _ ->
                ?OZ_AUTHS(Config, privileges:cluster_admin() -- [?CLUSTER_UPDATE])
        end,
        ?assertMatch({ok, ?HTTP_403_FORBIDDEN, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method, Auths
        ))
    end, [
        {<<"/hosts/someHost">>, delete},
        {<<"/web_cert">>, patch},
        {<<"/progress">>, patch},
        {<<"/emergency_passphrase">>, put}
    ]).


method_should_return_not_found_error(Config) ->
    ?run(fun({Endpoint, Method}) ->
        ?assertMatch({ok, ?HTTP_404_NOT_FOUND, _, _}, onepanel_test_rest:auth_request(
            Config, Endpoint, Method,
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
    ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:noauth_request(
        Config, "/join_cluster?clusterHost=someHost", post,
        #{clusterHost => <<"someHost">>, cookie => ?COOKIE}
    )),
    ?assertReceivedMatch({service, onepanel, join_cluster,
        #{cookie := ?COOKIE, cluster_host := "someHost"}
    }, ?TIMEOUT).


delete_as_admin_should_remove_node_from_cluster(Config) ->
    ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _}, onepanel_test_rest:auth_request(
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

init_per_testcase(Case, Config) when
    Case == token_with_api_caveats_should_return_unauthorized_error;
    Case == noauth_put_should_set_emergency_passphrase ->
    ?call(Config, model, clear, [onepanel_kv]),
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
