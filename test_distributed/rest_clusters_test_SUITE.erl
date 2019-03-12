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
    get_should_return_cluster_details_test/1,
    get_should_return_current_cluster_details_test/1,
    current_cluster_should_work_for_local_user/1,
    get_should_return_provider_info/1
]).

all() ->
    ?ALL([
        get_should_return_clusters_list_test,
        get_should_return_cluster_details_test,
        get_should_return_current_cluster_details_test,
        current_cluster_should_work_for_local_user,
        get_should_return_provider_info
    ]).


%%%===================================================================
%%% Test data
%%%===================================================================

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

-define(ADMIN_USER_NAME, <<"admin1">>).
-define(ADMIN_USER_PASSWORD, <<"Admin1Password">>).


-define(ACCESS_TOKEN, <<"accessTokenFromOnezone">>).

-define(CLUSTERS, #{
    ?ZONE_CLUSTER_ID => #{
        <<"workerVersion">> => #{
            <<"release">> => <<"ozWorkerRelease">>,
            <<"build">> => <<"ozWorkerBuild">>,
            <<"gui">> => <<"ozWorkerGuiSha">>
        },
        <<"onepanelVersion">> => #{
            <<"release">> => <<"ozPanelRelease">>,
            <<"build">> => <<"ozPanelBuild">>,
            <<"gui">> => <<"ozPanelGuiSha">>
        },
        <<"onepanelProxy">> => false,
        <<"serviceId">> => ?ZONE_ID,
        <<"type">> => <<"onezone">>,
        <<"clusterId">> => ?ZONE_CLUSTER_ID
    },
    ?PROVIDER_CLUSTER_ID => #{
        <<"workerVersion">> => #{
            <<"release">> => <<"opWorkerRelease">>,
            <<"build">> => <<"opWorkerBuild">>,
            <<"gui">> => <<"opWorkerGuiSha">>
        },
        <<"onepanelVersion">> => #{
            <<"release">> => <<"opPanelRelease">>,
            <<"build">> => <<"opPanelBuild">>,
            <<"gui">> => <<"opPanelGuiSha">>
        },
        <<"onepanelProxy">> => false,
        <<"serviceId">> => ?PROVIDER_CLUSTER_ID,
        <<"type">> => <<"oneprovider">>,
        <<"clusterId">> => ?PROVIDER_CLUSTER_ID
    }
}).

-define(PROVIDER_DETAILS_REST, #{
    <<"providerId">> => ?PROVIDER_ID,
    <<"online">> => true,
    <<"name">> => <<"providerName">>,
    <<"longitude">> => 42.0,
    <<"latitude">> => 7.0,
    <<"domain">> => <<"providerDomain">>,
    <<"cluster">> => ?PROVIDER_CLUSTER_ID
}).

-define(PROVIDER_DETAILS_RPC, ?PROVIDER_DETAILS_REST#{
    <<"creationTime">> => 1551451677
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
        onepanel_test_rest:assert_body(JsonBody,
            #{<<"ids">> => maps:keys(?CLUSTERS)})
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
            Expected = onepanel_maps:get_store_multiple([
                {<<"type">>, <<"type">>},
                {<<"workerVersion">>, <<"workerVersion">>},
                {<<"onepanelVersion">>, <<"onepanelVersion">>},
                {<<"serviceId">>, <<"serviceId">>},
                {<<"onepanelProxy">>, <<"onepanelProxy">>}
            ], maps:get(ClusterId, ?CLUSTERS), #{<<"id">> => ClusterId}),
            onepanel_test_rest:assert_body(JsonBody, Expected)
        end, maps:keys(?CLUSTERS))
    end, Hosts).


get_should_return_current_cluster_details_test(Config) ->
    Hosts = ?config(all_hosts, Config),
    lists:foreach(fun(Host) ->
        ClusterId = get_cluster_id(Host, Config),
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/cluster">>, get,
                {access_token, ?ACCESS_TOKEN}
            )
        ),
        Body = json_utils:decode(JsonBody),
        {_, _, _, JsonBodyById} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/user/clusters/", ClusterId/binary>>, get,
                {access_token, ?ACCESS_TOKEN}
            )
        ),
        BodyById = json_utils:decode(JsonBodyById),
        ?assertEqual(BodyById, Body)
    end, Hosts).


current_cluster_should_work_for_local_user(Config) ->
    Hosts = ?config(all_hosts, Config),
    lists:foreach(fun(Host) ->
        ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/cluster">>, get,
                {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD}
            )
        )
    end, Hosts).


get_should_return_provider_info(Config) ->
    Hosts = ?config(all_hosts, Config),

    lists:foreach(fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/providers/", ?PROVIDER_ID/binary>>, get,
                {access_token, ?ACCESS_TOKEN}
            )
        ),
        Expected = #{
            <<"id">> => ?PROVIDER_ID,
            <<"name">> => <<"providerName">>,
            <<"online">> => true,
            <<"domain">> => <<"providerDomain">>,
            <<"geoLongitude">> => 42.0,
            <<"geoLatitude">> => 7.0,
            <<"cluster">> => ?PROVIDER_CLUSTER_ID
        },
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end, Hosts).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) ->
        NewConfig2 = onepanel_test_utils:init(NewConfig),
        ?assertAllMatch({ok, _}, ?callAll(NewConfig2, onepanel_user, create,
            [?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD, admin]
        )),
        NewConfig2
    end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    OpNodes = ?config(oneprovider_nodes, Config),
    OzNodes = ?config(onezone_nodes, Config),
    OpHosts = ?config(oneprovider_hosts, Config),
    OzHosts = ?config(onezone_hosts, Config),

    test_utils:mock_new(Nodes, [service, rest_auth, clusters, rpc, oz_endpoint,
        onepanel_deployment], [passthrough, no_history, unstick]),

    test_utils:mock_expect(OzNodes, onepanel_deployment, is_completed,
        fun(_) -> true end),


    {_, []} = rpc:multicall(OpNodes, service, save, [#service{
        name = op_worker, hosts = OpHosts}]),
    {_, []} = rpc:multicall(OzNodes, service, save, [#service{
        name = oz_worker, hosts = OzHosts}]),
    % models required for cluster data caching to work
    {_, []} = rpc:multicall(OpNodes, service, save, [#service{
        name = oneprovider, ctx = #{registered => true}}]),
    {_, []} = rpc:multicall(OzNodes, service, save, [#service{
        name = onezone}]),

    test_utils:mock_expect(OpNodes, service_oneprovider, get_auth_token,
        fun() -> <<"providerMacaroon">> end),

    test_utils:mock_expect(OzNodes, rest_auth, authenticate_by_onezone_access_token,
        fun(Req) -> {?OZ_CLIENT, Req} end),
    test_utils:mock_expect(OpNodes, rest_auth, authenticate_by_onezone_access_token,
        fun(Req) -> {?OP_CLIENT, Req} end),

    test_utils:mock_expect(OzNodes, service_oz_worker, get_details,
        fun() -> #{name => undefined, domain => undefined} end),

    test_utils:mock_expect(OzNodes, clusters, get_id,
        fun() -> ?ZONE_CLUSTER_ID end),
    test_utils:mock_expect(OpNodes, clusters, get_id,
        fun() -> ?PROVIDER_CLUSTER_ID end),

    test_utils:mock_expect(OzNodes, rpc, call, fun
        (_Node, user_logic, get_clusters, [_Client]) ->
            {ok, maps:keys(?CLUSTERS)};
        (_Node, cluster_logic, get_protected_data, [_Client, ClusterId]) ->
            {ok, maps:get(ClusterId, ?CLUSTERS)};
        (_Node, provider_logic, get_protected_data, [_Client, ProviderId]) ->
            ?assertEqual(?PROVIDER_ID, ProviderId),
            {ok, ?PROVIDER_DETAILS_RPC};
        (_Node, entity_logic, root_client, []) ->
            {ok, opaque_root_client};
        (_Node, auth_logic, authorize_by_zone_gui_macaroon, [_AccessToken, _]) ->
            {ok, opaque_logic_client};
        (Node, Module, Function, Args) ->
            meck:passthrough([Node, Module, Function, Args])
    end),

    test_utils:mock_expect(OpNodes, oz_endpoint, request, fun
        (_Auth, "/user/clusters/", get, _Headers, <<>>, _Opts) ->
            {ok, 200, 0, json_utils:encode(#{clusters => maps:keys(?CLUSTERS)})};
        (_Auth, "/clusters/" ++ ClusterId, get, _Headers, <<>>, _Opts) ->
            Data = maps:get(list_to_binary(ClusterId), ?CLUSTERS),
            {ok, 200, 0, json_utils:encode(Data)};
        (_Auth, "/providers/" ++ ProviderId, get, _Headers, <<>>, _Opts) ->
            ?assertEqual(?PROVIDER_ID, list_to_binary(ProviderId)),
            {ok, 200, 0, json_utils:encode(?PROVIDER_DETAILS_REST)}
    end),

    Config.


end_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    test_utils:mock_unload(Nodes).


end_per_suite(_Config) -> ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_cluster_id(Host :: string(), Config :: proplists:proplist()) -> binary().
get_cluster_id(Host, Config) ->
    OpHosts = ?config(oneprovider_hosts, Config),
    OzHosts = ?config(onezone_hosts, Config),
    case {lists:member(Host, OpHosts), lists:member(Host, OzHosts)} of
        {true, _} -> ?PROVIDER_CLUSTER_ID;
        {_, true} -> ?ZONE_CLUSTER_ID
    end.
