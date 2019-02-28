%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of 'service_letsencrypt' module.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_letsencrypt_test_SUITE).
-author("Wojciech Geisler").

-include("names.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2,
    end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    method_should_return_unauthorized_error/1,
    method_should_return_forbidden_error/1,
    get_should_return_letsencrypt_setting/1,
    get_should_return_cert_metedata/1,
    patch_should_enable_letsencrypt/1,
    patch_should_disable_letsencrypt/1,
    ssl_cache_is_cleared_after_certification/1,
    existing_certificates_are_reused/1,
    failed_patch_leaves_letsencrypt_disabled/1,
    failed_patch_leaves_letsencrypt_enabled/1
]).

all() ->
    ?ALL([
        method_should_return_unauthorized_error,
        method_should_return_forbidden_error,
        get_should_return_letsencrypt_setting,
        get_should_return_cert_metedata,
        patch_should_disable_letsencrypt,
        patch_should_enable_letsencrypt,
        ssl_cache_is_cleared_after_certification,
        existing_certificates_are_reused,
        failed_patch_leaves_letsencrypt_disabled,
        failed_patch_leaves_letsencrypt_enabled
    ]).

-define(LE_PLUGIN_MOCK, service_le_plugin_mock).
-define(LE_PLUGIN_MOCK_NAME, le_plugin_mock).

-define(ADMIN_USER_NAME, <<"admin1">>).
-define(ADMIN_USER_PASSWORD, <<"Admin1Password">>).
-define(REG_USER_NAME, <<"user1">>).
-define(REG_USER_PASSWORD, <<"User1Password">>).

-define(EXPIRATION, 15000000).
-define(OBTAINED, 14000900).

%% Sample certificate files
-define(TEST_CERT_PATHS, #{
    web_cert_file => "testca/web_cert.pem",
    web_key_file => "testca/web_key.pem",
    web_cert_chain_file => "testca/web_chain.pem"
}).

%% Let's Encrypt issued certificate files
-define(LE_CERT_PATHS, #{
    web_cert_file => "letsencrypt/web_cert.pem",
    web_key_file => "letsencrypt/web_key.pem",
    web_cert_chain_file => "letsencrypt/web_chain.pem"
}).

-define(WEB_CERT_JSON, #{
    <<"letsEncrypt">> := false,
    <<"expirationTime">> := _,
    <<"creationTime">> := _,
    <<"paths">> := #{
        <<"cert">> := _,
        <<"key">> := _,
        <<"chain">> := _
    },
    <<"domain">> := _,
    <<"issuer">> := _,
    <<"status">> := _
}).

-define(WEB_CERT_LE_JSON, #{
    <<"letsEncrypt">> := true,
    <<"expirationTime">> := _,
    <<"creationTime">> := _,
    <<"paths">> := #{
        <<"cert">> := _,
        <<"key">> := _,
        <<"chain">> := _
    },
    <<"domain">> := _,
    <<"issuer">> := _,
    <<"status">> := _,
    <<"lastRenewalFailure">> := _,
    <<"lastRenewalSuccess">> := _
}).

% Calls Function with one host from each cluster
-define(hostPerCluster(Config, Function), begin
    Function(utils:random_element(?config(oneprovider_hosts, Config))),
    Function(utils:random_element(?config(onezone_hosts, Config)))
end).

% Calls Function with one node from each cluster
-define(nodePerCluster(Config, Function), begin
    Function(utils:random_element(?config(oneprovider_nodes, Config))),
    Function(utils:random_element(?config(onezone_nodes, Config)))
end).

%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_unauthorized_error(Config) ->
    lists:foreach(fun(Host) ->
        lists:foreach(fun({Endpoint, Method}) ->
            ?assertMatch({ok, 401, _, _}, onepanel_test_rest:noauth_request(
                Host, <<Endpoint/binary>>, Method
            )),
            ?assertMatch({ok, 401, _, _}, onepanel_test_rest:auth_request(
                Host, <<Endpoint/binary>>, Method,
                {<<"someUser">>, <<"somePassword">>}
            ))
        end, [
            {<<"/web_cert">>, get},
            {<<"/web_cert">>, patch}
        ])
    end, ?config(all_hosts, Config)).


method_should_return_forbidden_error(Config) ->
    lists:foreach(fun(Host) ->
        lists:foreach(fun({Endpoint, Method}) ->
            ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
                Host, <<Endpoint/binary>>, Method,
                {?REG_USER_NAME, ?REG_USER_PASSWORD}
            ))
        end, [
            {<<"/web_cert">>, get},
            {<<"/web_cert">>, patch}
        ])
    end, ?config(all_hosts, Config)).


get_should_return_letsencrypt_setting(Config) ->
    lists:foreach(fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/web_cert">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        ?assertMatch(#{<<"letsEncrypt">> := _}, json_utils:decode(JsonBody))
    end, ?config(all_hosts, Config)).


get_should_return_cert_metedata(Config) ->
    lists:foreach(fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/web_cert">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        ),
        ?assertMatch(?WEB_CERT_LE_JSON, json_utils:decode(JsonBody))
    end, ?config(all_hosts, Config)).


patch_should_enable_letsencrypt(Config) ->
    ?hostPerCluster(Config, fun(Host) ->
        ?assertEqual(204, patch_web_cert(Host, #{letsEncrypt => true}))
    end),
    lists:foreach(fun(Host) ->
        Body = get_web_cert(Host),
        ?assertMatch(#{<<"letsEncrypt">> := true}, Body)
    end, ?config(all_hosts, Config)),

    % assert there was attempt to obtain certificates
    ?assertEqual(1,
        total_num_calls(?config(onezone_nodes, Config), letsencrypt_api, run_certification_flow, 2)),
    ?assertEqual(1,
        total_num_calls(?config(oneprovider_nodes, Config), letsencrypt_api, run_certification_flow, 2)).


patch_should_disable_letsencrypt(Config) ->
    % when
    ?hostPerCluster(Config, fun(Host) ->
        ?assertEqual(204, patch_web_cert(Host, #{letsEncrypt => false}))
    end),

    % then
    lists:foreach(fun(Host) ->
        Body = get_web_cert(Host),
        ?assertMatch(#{<<"letsEncrypt">> := false}, Body)
    end, ?config(all_hosts, Config)).


ssl_cache_is_cleared_after_certification(Config) ->
    ?hostPerCluster(Config, fun(Host) ->
        ?assertEqual(204, patch_web_cert(Host, #{letsEncrypt => true}))
    end),

    lists:foreach(fun(Node) ->
        ?assertEqual(1, rpc:call(Node, meck, num_calls, [service_oz_worker, reload_webcert, 1])),
        ?assertEqual(1, rpc:call(Node, meck, num_calls, [service_onepanel, reload_webcert, 1]))
    end, ?config(onezone_nodes, Config)),

    lists:foreach(fun(Node) ->
        ?assertEqual(1, rpc:call(Node, meck, num_calls, [service_op_worker, reload_webcert, 1])),
        ?assertEqual(1, rpc:call(Node, meck, num_calls, [service_onepanel, reload_webcert, 1]))
    end, ?config(oneprovider_nodes, Config)).


existing_certificates_are_reused(Config) ->
    %% Warning! This test is fragile as it depends on function checking cert issuer
    %% to be separate from checking that certs are up to date and with matching subject

    % when
    ?hostPerCluster(Config, fun(Host) ->
        ?assertEqual(204, patch_web_cert(Host, #{letsEncrypt => true}))
    end),

    % then
    % assert there were no attempts to obtain new certificates
    ?assertEqual(0,
        total_num_calls(?config(all_nodes, Config), letsencrypt_api, run_certification_flow, 2)).


failed_patch_leaves_letsencrypt_disabled(Config) ->
    % given
    lists:foreach(fun(Host) ->
        ?assertMatch(#{<<"letsEncrypt">> := false}, get_web_cert(Host))
    end, ?config(all_hosts, Config)),

    % when
    ?hostPerCluster(Config, fun(Host) ->
        ?assertEqual(400, patch_web_cert(Host, #{letsEncrypt => true}))
    end),

    % then
    lists:foreach(fun(Host) ->
        ?assertMatch(#{<<"letsEncrypt">> := false}, get_web_cert(Host))
    end, ?config(all_hosts, Config)).


failed_patch_leaves_letsencrypt_enabled(Config) ->
    % given
    lists:foreach(fun(Host) ->
        ?assertMatch(#{<<"letsEncrypt">> := true}, get_web_cert(Host))
    end, ?config(all_hosts, Config)),

    % when
    ?hostPerCluster(Config, fun(Host) ->
        ?assertEqual(400, patch_web_cert(Host, #{letsEncrypt => true}))
    end),

    % then
    lists:foreach(fun(Host) ->
        ?assertMatch(#{<<"letsEncrypt">> := true}, get_web_cert(Host))
    end, ?config(all_hosts, Config)).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) ->
        NewConfig2 = onepanel_test_utils:init(NewConfig),
        ?assertMatch({ok, _}, ?callAny(NewConfig2, oneprovider_nodes, onepanel_user, create,
            [?REG_USER_NAME, ?REG_USER_PASSWORD, regular]
        )),
        ?assertMatch({ok, _}, ?callAny(NewConfig2, onezone_nodes, onepanel_user, create,
            [?REG_USER_NAME, ?REG_USER_PASSWORD, regular]
        )),
        ?assertMatch({ok, _}, ?callAny(NewConfig2, oneprovider_nodes, onepanel_user, create,
            [?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD, admin]
        )),
        ?assertMatch({ok, _}, ?callAny(NewConfig2, onezone_nodes, onepanel_user, create,
            [?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD, admin]
        )),

        % deploy letsencrypt service
        OzHosts = ?config(onezone_hosts, NewConfig2),
        [OzNode | _] = ?config(onezone_nodes, NewConfig2),
        onepanel_test_utils:service_action(OzNode,
            letsencrypt, deploy,
            #{hosts => OzHosts, letsencrypt_plugin => ?SERVICE_OZW}
        ),

        OpHosts = ?config(oneprovider_hosts, NewConfig2),
        [OpNode | _] = ?config(oneprovider_nodes, NewConfig2),
        onepanel_test_utils:service_action(OpNode,
            letsencrypt, deploy,
            #{hosts => OpHosts, letsencrypt_plugin => ?SERVICE_OPW}
        ),

        NewConfig2
    end,

    [{?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(Case, Config) when
    Case == get_should_return_cert_metedata;
    Case == patch_should_disable_letsencrypt ->
    Config2 = init_per_testcase(default, Config),
    ?hostPerCluster(Config, fun(Host) ->
        ?assertEqual(204, patch_web_cert(Host, #{letsEncrypt => true}))
    end),
    Config2;

init_per_testcase(ssl_cache_is_cleared_after_certification, Config) ->
    % mock module to allow counting calls
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_new(Nodes, [service_onepanel], [passthrough]),

    deploy_certs(?TEST_CERT_PATHS, Config),

    init_per_testcase(default, Config);

init_per_testcase(existing_certificates_are_reused, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_new(Nodes, [service_letsencrypt]),
    test_utils:mock_expect(Nodes, service_letsencrypt, local_cert_status,
        fun(_) -> valid end),

    ?hostPerCluster(Config, fun(Host) ->
        ?assertEqual(204, patch_web_cert(Host, #{letsEncrypt => false}))
    end),

    deploy_certs(?LE_CERT_PATHS, Config),

    init_per_testcase(default, Config);

init_per_testcase(Case, Config) when
    Case == failed_patch_leaves_letsencrypt_disabled;
    Case == failed_patch_leaves_letsencrypt_enabled ->
    Nodes = ?config(all_nodes, Config),
    mock_plugin_modules(Config),
    test_utils:mock_new(Nodes, [letsencrypt_api], [passthrough]),

    % cause error on certification attempt
    test_utils:mock_expect(Nodes, letsencrypt_api, run_certification_flow,
        fun(_, _) -> ?throw_error(?ERR_LETSENCRYPT(<<"someURN">>, <<"some message">>)) end),
    test_utils:mock_expect(Nodes, letsencrypt_api, run_certification_flow,
        fun(_, _, _) -> ?throw_error(?ERR_LETSENCRYPT(<<"someURN">>, <<"some message">>)) end),

    Initial = case Case of
        failed_patch_leaves_letsencrypt_disabled -> false;
        failed_patch_leaves_letsencrypt_enabled -> true
    end,

    ?nodePerCluster(Config, fun(Node) ->
        rpc:call(Node, service, update,
            [?SERVICE_LE, fun(#service{ctx = C} = S) ->
                S#service{ctx = C#{letsencrypt_enabled => Initial}}
            end])
    end),

    Config;


init_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    mock_plugin_modules(Config),
    test_utils:mock_new(Nodes, [letsencrypt_api], [passthrough]),
    test_utils:mock_expect(Nodes, letsencrypt_api, run_certification_flow,
        fun(Domain, Plugin) when is_binary(Domain) and is_atom(Plugin) -> ok end),
    test_utils:mock_expect(Nodes, letsencrypt_api, run_certification_flow,
        fun(Domain, Plugin, _Mode) when is_binary(Domain) and is_atom(Plugin) -> ok end),
    Config.


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes).

end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Performs get on the /web_cert endpoint
%% @end
%%--------------------------------------------------------------------
-spec get_web_cert(Host :: service:host()) -> Response :: #{binary() => _}.
get_web_cert(Host) ->
    {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
        onepanel_test_rest:auth_request(
            Host, <<"/web_cert">>, get,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
        )
    ),
    json_utils:decode(JsonBody).


%%--------------------------------------------------------------------
%% @private
%% @doc Performs patch on the /web_cert endpoint
%% @end
%%--------------------------------------------------------------------
-spec patch_web_cert(Host :: service:host(), Data :: map()) -> Code :: non_neg_integer().
patch_web_cert(Host, Data) ->
    {_, Code, _, _} = ?assertMatch({ok, _, _, _},
        onepanel_test_rest:auth_request(
            Host, <<"/web_cert">>, patch,
            {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}, Data
        )),
    Code.


%%--------------------------------------------------------------------
%% @private
%% @doc Mocks functions used by service_letsencrypt
%% in the op and oz services.
%% @end
%%--------------------------------------------------------------------
-spec mock_plugin_modules(Config :: proplists:proplist()) -> ok.
mock_plugin_modules(Config) ->
    OzNodes = ?config(onezone_nodes, Config),
    OpNodes = ?config(oneprovider_nodes, Config),
    OzHosts = ?config(onezone_hosts, Config),
    OpHosts = ?config(oneprovider_hosts, Config),
    OzDomain = onepanel_test_utils:get_domain(hd(?config(onezone_hosts, Config))),
    OpDomain = onepanel_test_utils:get_domain(hd(?config(oneprovider_hosts, Config))),
    test_utils:mock_new(OpNodes, [service_op_worker], [passthrough]),
    test_utils:mock_new(OzNodes, [service_oz_worker], [passthrough]),

    test_utils:mock_expect(OpNodes, service_oneprovider, is_registered, fun() -> false end),
    test_utils:mock_expect(OpNodes, service_op_worker, get_domain, fun() -> OpDomain end),
    test_utils:mock_expect(OzNodes, service_oz_worker, get_domain, fun() -> OzDomain end),
    test_utils:mock_expect(OpNodes, service_op_worker, get_hosts, fun() -> OpHosts end),
    test_utils:mock_expect(OzNodes, service_oz_worker, get_hosts, fun() -> OzHosts end),
    test_utils:mock_expect(OpNodes, service_op_worker, reload_webcert, fun(_) -> ok end),
    test_utils:mock_expect(OzNodes, service_oz_worker, reload_webcert, fun(_) -> ok end).


%%--------------------------------------------------------------------
%% @private
%% @doc Writs predefined certificate files on all nodes.
%% @end
%%--------------------------------------------------------------------
-spec deploy_certs(map(), Config :: proplists:proplist()) -> ok.
deploy_certs(SourcePaths, Config) ->
    Nodes = ?config(all_nodes, Config),
    lists:foreach(fun({FileType, Path}) ->
        {ok, Content} = file:read_file(?TEST_FILE(Config, Path)),
        {Result, []} = rpc:multicall(Nodes, erlang, apply, [fun() ->
            Dest = onepanel_env:get(FileType),
            file:write_file(Dest, Content)
        end, []]),
        ?assertAllMatch(ok, Result)
    end, maps:to_list(SourcePaths)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns total count of calls to a function on given nodes.
%% @end
%%--------------------------------------------------------------------
-spec total_num_calls(Nodes :: [node()], Module :: module(), Function :: atom(),
    Arity :: non_neg_integer()) -> non_neg_integer().
total_num_calls(Nodes, Module, Function, Arity) ->
    lists:sum(
        [rpc:call(Node, meck, num_calls, [Module, Function, Arity]) || Node <- Nodes]
    ).
