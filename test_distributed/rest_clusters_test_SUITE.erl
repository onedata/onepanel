%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of 'rest_clusters' module.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_clusters_test_SUITE).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include("authentication.hrl").
-include("modules/models.hrl").
-include("modules/onepanel_dns.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2,
    end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    get_should_return_clusters_list_test/1,
    get_should_return_cluster_details_test/1
]).

all() ->
    ?ALL([
        get_should_return_clusters_list_test,
        get_should_return_cluster_details_test
    ]).

-define(USER_DETAILS, #user_details{
    id = <<"someUserId">>,
    name = <<"onezoneUser">>,
    alias = <<"onezoneUserLogin">>}).

-define(OP_CLIENT, #client{
    privileges = privileges:cluster_admin(),
    user = ?USER_DETAILS,
    zone_auth = {rest, {access_token, <<"someUserToken">>}},
    role = user
}).

-define(OZ_CLIENT, #client{
    privileges = privileges:cluster_admin(),
    user = ?USER_DETAILS,
    zone_auth = {rpc, opaque_client_record},
    role = user
}).

-define(ZONE_ID, <<"onezone">>).
-define(ZONE_CLUSTER_ID, <<"onezone">>).

-define(PROVIDER_ID, <<"providerId">>).
-define(PROVIDER_CLUSTER_ID, <<"providerClusterId">>).

-define(ACCESS_TOKEN, <<"accessTokenFromOnezone">>).

-define(CLUSTERS, #{
    % @fixme use new verison format
% @fixme make the test check something
    ?ZONE_CLUSTER_ID => #{
        version => <<"18.07">>,
        build => <<"zonebuildno">>,
        proxy => false,
        serviceId => ?ZONE_ID,
        type => <<"onezone">>,
        id => ?ZONE_CLUSTER_ID
    },
    ?PROVIDER_CLUSTER_ID => #{
        version => <<"18.07">>,
        build => <<"providerbuildno">>,
        proxy => false,
        serviceId => ?PROVIDER_CLUSTER_ID,
        type => <<"onezone">>,
        id => ?ZONE_CLUSTER_ID
    }
}).


%%%===================================================================
%%% Test functions
%%%===================================================================

get_should_return_clusters_list_test(Config) ->
    Hosts = ?config(all_hosts, Config),
    lists:foreach(fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/user/clusters/">>, get,
                {access_token, ?ACCESS_TOKEN}
            )
        ),
        Expected = #{ids => maps:keys(?CLUSTERS)},
        onepanel_test_rest:assert_body_values(JsonBody,
            maps:to_list(onepanel_utils:convert(Expected, {keys, binary})))
    end, Hosts).


get_should_return_cluster_details_test(Config) ->
    Hosts = ?config(all_hosts, Config),
    lists:foreach(fun(Host) ->
        lists:foreach(fun(ClusterId) ->
            {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
                onepanel_test_rest:auth_request(
                    Host, <<"/user/clusters/", ClusterId/binary>>, get,
                    {access_token, ?ACCESS_TOKEN}
                )
            ),
            Expected = maps:get(ClusterId, ?CLUSTERS),
            onepanel_test_rest:assert_body_values(JsonBody,
                maps:to_list(onepanel_utils:convert(Expected, {keys, binary})))
        end, maps:keys(?CLUSTERS))
    end, Hosts).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) -> onepanel_test_utils:init(NewConfig) end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    OpNodes = ?config(oneprovider_nodes, Config),
    OzNodes = ?config(onezone_nodes, Config),
    OpHosts = ?config(oneprovider_hosts, Config),
    OzHosts = ?config(onezone_hosts, Config),

    true = lists:usort(Nodes) == lists:usort(OpNodes ++ OzNodes),

    test_utils:mock_new(Nodes, [service, rest_auth, clusters, rpc, oz_endpoint],
        [passthrough, no_history, unstick]),

    test_utils:mock_expect(OpNodes, service, get, fun
        (op_worker) -> {ok, #service{hosts = OpHosts}};
        (oneprovider) -> {ok, #service{ctx = #{registered => true}}}
    end),
    test_utils:mock_expect(OzNodes, service, get,
        fun(oz_worker) -> {ok, #service{hosts = OzHosts}} end),

    test_utils:mock_expect(OpNodes, service_oneprovider, get_auth_token,
        fun () -> <<"providerMacaroon">> end),

    test_utils:mock_expect(OzNodes, rest_auth, authenticate_by_onezone_access_token,
        fun(Req) -> {?OZ_CLIENT, Req} end),
    test_utils:mock_expect(OpNodes, rest_auth, authenticate_by_onezone_access_token,
        fun(Req) -> {?OP_CLIENT, Req} end),

    test_utils:mock_expect(OzNodes, clusters, get_id,
        fun() -> ?ZONE_CLUSTER_ID end),
    test_utils:mock_expect(OpNodes, clusters, get_id,
        fun() -> ?PROVIDER_CLUSTER_ID end),

    GetDetailsFun = fun(ClusterId) ->
        case maps:find(ClusterId, ?CLUSTERS) of
            {ok, Details} -> {ok, Details};
            error -> ?make_error(?ERR_NOT_FOUND)
        end
    end,

    test_utils:mock_expect(OzNodes, clusters, get_details,
        fun({rpc, _}, ClusterId) -> GetDetailsFun(ClusterId) end),
    test_utils:mock_expect(OpNodes, clusters, get_details,
        fun({rest, _}, ClusterId) -> GetDetailsFun(ClusterId) end),

    test_utils:mock_expect(OzNodes, rpc, call, fun
        (_Node, user_logic, get_clusters, [_Client]) ->
            {ok, maps:keys(?CLUSTERS)};
        (Node, Module, Function, Args) ->
            meck:passthrough([Node, Module, Function, Args])
    end),

    test_utils:mock_expect(OpNodes, oz_endpoint, request, fun
        (_Auth, "/user/clusters/", get, _Headers, <<>>, _Opts) ->
            {ok, 200, 0, json_utils:encode(#{clusters => maps:keys(?CLUSTERS)})}
    end),

    Config.


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes).


end_per_suite(_Config) -> ok.
