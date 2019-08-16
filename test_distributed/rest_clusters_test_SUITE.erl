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
-include("onepanel_test_rest.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/http/codes.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2,
    end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    method_should_return_forbidden_error_test/1,
    local_user_should_get_not_found_error_test/1,
    get_should_return_clusters_list_test/1,
    get_should_return_cluster_details_test/1,
    get_should_return_current_cluster_details_test/1,
    current_cluster_should_work_for_local_user_test/1,
    get_should_return_current_cluster_members_count_test/1,
    get_should_return_provider_info_test/1
]).

all() ->
    ?ALL([
        method_should_return_forbidden_error_test,
        local_user_should_get_not_found_error_test,
        get_should_return_clusters_list_test,
        get_should_return_cluster_details_test,
        get_should_return_current_cluster_details_test,
        current_cluster_should_work_for_local_user_test,
        get_should_return_current_cluster_members_count_test,
        get_should_return_provider_info_test
    ]).


%%%===================================================================
%%% Test data
%%%===================================================================

-define(ZONE_ID, <<"onezone">>).
-define(ZONE_CLUSTER_ID, <<"onezone">>).

-define(PROVIDER_ID, "providerId").
-define(PROVIDER_CLUSTER_ID, <<?PROVIDER_ID>>).

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
    <<"providerId">> => <<?PROVIDER_ID>>,
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
%%% Test macros
%%%===================================================================

-define(eachHost(Config, Fun), lists:foreach(Fun, ?config(all_hosts, Config))).

-define(eachEndpoint(Config, Fun, EndpointsWithMethods),
    lists:foreach(fun({_Host, _Endpoint, _Method}) ->
        try
            Fun(_Host, _Endpoint, _Method)
        catch
            error:{assertMatch_failed, _} = _Reason ->
                ct:pal("Failed on: ~s ~s (host ~s)", [_Method, _Endpoint, _Host]),
                erlang:error(_Reason)
        end
    end, [
        {_Host, _Endpoint, _Method} ||
        {_Endpoint, _Method} <- EndpointsWithMethods,
        _Host <- ?config(all_hosts, Config)
    ])
).



%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_forbidden_error_test(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_403_FORBIDDEN, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/cluster/invite_user_token">>, post,
            ?OZ_AUTHS(Host, privileges:cluster_admin() -- [?CLUSTER_ADD_USER])
        )),
        ?assertMatch({ok, ?HTTP_403_FORBIDDEN, _, _}, onepanel_test_rest:auth_request(
            Host, <<"/cluster/members_summary">>, get,
            ?OZ_AUTHS(Host, privileges:cluster_admin() -- [?CLUSTER_VIEW])
        ))
    end).


local_user_should_get_not_found_error_test(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        ?assertMatch({ok, ?HTTP_404_NOT_FOUND, _, _}, onepanel_test_rest:auth_request(
            Host, <<Endpoint/binary>>, Method,
            ?ROOT_AUTHS(Host)
        ))
    end, [
        {<<"/user/clusters/">>, get},
        {<<"/user/clusters/someCluster">>, get},
        {<<"/providers/someProvider">>, get}
    ]).


get_should_return_clusters_list_test(Config) ->
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/user/clusters/">>, get,
                ?OZ_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody,
            #{<<"ids">> => maps:keys(?CLUSTERS)})
    end).


get_should_return_cluster_details_test(Config) ->
    ?eachHost(Config, fun(Host) ->
        lists:foreach(fun(ClusterId) ->
            {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
                onepanel_test_rest:auth_request(
                    Host, <<"/user/clusters/", ClusterId/binary>>, get,
                    ?OZ_AUTHS(Host, [])
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
    end).


get_should_return_current_cluster_details_test(Config) ->
    ?eachHost(Config, fun(Host) ->
        ClusterId = get_cluster_id(Host, Config),
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/cluster">>, get,
                ?OZ_AUTHS(Host, [])
            )
        ),
        Body = json_utils:decode(JsonBody),
        {_, _, _, JsonBodyById} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/user/clusters/", ClusterId/binary>>, get,
                ?OZ_AUTHS(Host, [])
            )
        ),
        BodyById = json_utils:decode(JsonBodyById),
        ?assertEqual(BodyById, Body)
    end).


current_cluster_should_work_for_local_user_test(Config) ->
    ?eachHost(Config, fun(Host) ->
        ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/cluster">>, get,
                ?ROOT_AUTHS(Host)
            )
        )
    end).


get_should_return_current_cluster_members_count_test(Config) ->
    ?eachHost(Config, fun(Host) ->
        Expected = #{
            <<"usersCount">> => 1, <<"effectiveUsersCount">> => 2,
            <<"groupsCount">> => 3, <<"effectiveGroupsCount">> => 4
        },
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/cluster/members_summary">>, get,
                ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_VIEW])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end).


get_should_return_provider_info_test(Config) ->
    Expected = #{
        <<"id">> => <<?PROVIDER_ID>>,
        <<"name">> => <<"providerName">>,
        <<"online">> => true,
        <<"domain">> => <<"providerDomain">>,
        <<"geoLongitude">> => 42.0,
        <<"geoLatitude">> => 7.0,
        <<"cluster">> => ?PROVIDER_CLUSTER_ID
    },
    ?eachHost(Config, fun(Host) ->
        {_, _, _, JsonBody} = ?assertMatch({ok, ?HTTP_200_OK, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/providers/", ?PROVIDER_ID>>, get,
                ?OZ_AUTHS(Host, [])
            )
        ),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun onepanel_test_utils:init/1,
    [{?LOAD_MODULES, [onepanel_test_rest]}, {?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(_Case, Config) ->
    Nodes = ?config(all_nodes, Config),
    OpNodes = ?config(oneprovider_nodes, Config),
    OzNodes = ?config(onezone_nodes, Config),
    OpHosts = ?config(oneprovider_hosts, Config),
    OzHosts = ?config(onezone_hosts, Config),
    OzDomain = onepanel_test_utils:get_domain(hd(OzHosts)),

    test_utils:mock_new(Nodes, [service, rest_auth, clusters, rpc, oz_endpoint,
        onepanel_deployment], [passthrough, no_history, unstick]),

    test_utils:mock_expect(OzNodes, onepanel_deployment, is_set,
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

    test_utils:mock_expect(OpNodes, service_oneprovider, get_access_token,
        fun() -> <<"providerToken">> end),
    test_utils:mock_expect(OpNodes, service_oneprovider, get_id,
        fun() -> <<?PROVIDER_ID>> end),

    onepanel_test_rest:set_default_passphrase(Config),
    onepanel_test_rest:mock_token_authentication(Config),

    test_utils:mock_expect(OzNodes, service_oz_worker, get_details,
        fun() -> #{name => undefined, domain => OzDomain} end),

    test_utils:mock_expect(OzNodes, rpc, call, fun
        (_Node, rpc_api, get_clusters_by_user_auth, [_Client]) ->
            {ok, maps:keys(?CLUSTERS)};
        (_Node, rpc_api, get_protected_cluster_data, [_Client, ClusterId]) ->
            {ok, maps:get(ClusterId, ?CLUSTERS)};

        (_Node, rpc_api, cluster_logic_get_users, [_Client, _ClusterId]) ->
            {ok, [<<"userId1">>]};
        (_Node, rpc_api, cluster_logic_get_eff_users, [_Client, _ClusterId]) ->
            {ok, [<<"userId1">>, <<"userId2">>]};
        (_Node, rpc_api, cluster_logic_get_groups, [_Client, _ClusterId]) ->
            {ok, [<<"groupId1">>, <<"groupId2">>, <<"groupId3">>]};
        (_Node, rpc_api, cluster_logic_get_eff_groups, [_Client, _ClusterId]) ->
            {ok, [<<"groupId1">>, <<"groupId2">>, <<"groupId3">>, <<"groupId4">>]};

        (_Node, rpc_api, get_protected_provider_data, [_Client, ProviderId]) ->
            ?assertEqual(<<?PROVIDER_ID>>, ProviderId),
            {ok, ?PROVIDER_DETAILS_RPC};
        (Node, Module, Function, Args) ->
            meck:passthrough([Node, Module, Function, Args])
    end),

    test_utils:mock_expect(OpNodes, oz_endpoint, request, fun
        (_Auth, "/user/effective_clusters/", get, _Headers, <<>>, _Opts) ->
            {ok, ?HTTP_200_OK, 0, json_utils:encode(#{clusters => maps:keys(?CLUSTERS)})};
        (_Auth, "/clusters/" ++ ?PROVIDER_ID ++ "/users", get, _Headers, <<>>, _Opts) ->
            {ok, ?HTTP_200_OK, 0, json_utils:encode(#{
                users => [<<"userId1">>]
            })};
        (_Auth, "/clusters/" ++ ?PROVIDER_ID ++ "/effective_users", get, _Headers, <<>>, _Opts) ->
            {ok, ?HTTP_200_OK, 0, json_utils:encode(#{
                users => [<<"userId1">>, <<"userId2">>]
            })};
        (_Auth, "/clusters/" ++ ?PROVIDER_ID ++ "/groups", get, _Headers, <<>>, _Opts) ->
            {ok, ?HTTP_200_OK, 0, json_utils:encode(#{
                groups => [<<"groupId1">>, <<"groupId2">>, <<"groupId3">>]
            })};
        (_Auth, "/clusters/" ++ ?PROVIDER_ID ++ "/effective_groups", get, _Headers, <<>>, _Opts) ->
            {ok, ?HTTP_200_OK, 0, json_utils:encode(#{
                groups => [<<"groupId1">>, <<"groupId2">>, <<"groupId3">>, <<"groupId4">>]
            })};
        (_Auth, "/clusters/" ++ ClusterId, get, _Headers, <<>>, _Opts) ->
            Data = maps:get(list_to_binary(ClusterId), ?CLUSTERS),
            {ok, ?HTTP_200_OK, 0, json_utils:encode(Data)};
        (_Auth, "/providers/" ++ ?PROVIDER_ID, get, _Headers, <<>>, _Opts) ->
            {ok, ?HTTP_200_OK, 0, json_utils:encode(?PROVIDER_DETAILS_REST)}
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
